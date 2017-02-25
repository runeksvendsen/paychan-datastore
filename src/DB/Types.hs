{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, TypeFamilies     #-}
module DB.Types
(
  module DB.Types
, module DB.Model.Types.Entity
, module DB.Model.Types.Request
, module Ancestor
, module Datastore
, module Google
, module Export
, DBException(..)
, DatastoreConf(..)
, Tagged(..)
)
where

import LibPrelude
import           DB.Model.Types as Export
import           DB.Model.Types.Entity
import           DB.Model.Types.Request
import           DB.Model.Types.KeyPath as Ancestor
import           Data.Tagged (Tagged(..))

import Network.Google.Datastore as Datastore hiding (key)
import Network.Google           as Google

import qualified Control.Exception as Except
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.Resource   as Res
import           Control.Monad.Trans.Control    as Ctrl
import qualified Control.Monad.Catch            as Catch
import qualified Control.Monad.Base             as Base
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Applicative              (Alternative)
-- import qualified Control.Monad.Trans.Writer.Strict        as W
import qualified Control.Monad.Writer.Strict              as W


emptyQuery = query

runDatastore :: DatastoreConf -> Datastore a -> IO a
runDatastore cfg d = Res.runResourceT $ liftResourceT $ R.runReaderT (unDS d) cfg


class (Res.MonadResource m, MonadGoogle '[AuthDatastore] m) => DatastoreM m where
    getEnv          :: m (Env '[AuthDatastore])
    getPid          :: m ProjectId
    getConf         :: m DatastoreConf

-- | Create a 'PartitionId' using the project id in 'DatastoreConf'
mkPartitionId   :: DatastoreM m => NamespaceId -> m PartitionId
mkPartitionId nsId = do
    pid <- getPid
    return $ partitionId &
        piNamespaceId ?~ nsId &
        piProjectId ?~ pid

instance DatastoreM Datastore where
    getEnv = dcAuthEnv <$> R.ask
    getPid = dcProjId  <$> R.ask
    getConf = R.ask

--     liftDS d = do
--         cfg <- R.ask
--         liftResourceT $ R.runReaderT (unDS d) cfg



mkProjectReq :: ( DatastoreM m
                , HasProject a p )
             => a
             -> m p
mkProjectReq req = do
    pid <- getPid
    return $ _mkProjectReq req pid

newtype Datastore a = Datastore { unDS :: R.ReaderT DatastoreConf (Res.ResourceT IO) a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , R.MonadPlus
        , MonadIO
        , MonadThrow
        , Catch.MonadCatch
        , Catch.MonadMask
        , Base.MonadBase IO
        , R.MonadReader DatastoreConf
        , MonadResource
        )

data DatastoreConf = DatastoreConf
    { dcAuthEnv :: Env '[AuthDatastore]
    , dcProjId  :: ProjectId
    }

instance Ctrl.MonadBaseControl IO Datastore where
    type StM Datastore a = a
    liftBaseWith f = Datastore $ liftBaseWith $ \g -> f (g . unDS)
    restoreM       = Datastore . restoreM

instance MonadGoogle '[AuthDatastore] Datastore where
    liftGoogle g = do
        env <- getEnv
        runGoogle env g

-- Tx
class (DatastoreM m, Res.MonadResource m, MonadGoogle '[AuthDatastore] m) => DatastoreTxM m where
    getTxId :: m TxId
    getNSId :: m NamespaceId


newtype DatastoreTx a = DatastoreTx
    { unDSTx :: R.ReaderT TxDatastoreConf (W.WriterT CommitRequest (Res.ResourceT IO)) a
    }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , R.MonadPlus
        , MonadIO
        , MonadThrow
        , Catch.MonadCatch
        , Catch.MonadMask
        , Base.MonadBase IO
        , R.MonadReader TxDatastoreConf
        , W.MonadWriter CommitRequest
        , MonadResource
        )

data TxDatastoreConf = TxDatastoreConf
    { tdcDsConf     :: DatastoreConf
    , tdcTxId       :: TxToken
    , tdcNamespace  :: NamespaceId
    }


-- txCommit :: CommitRequest -> DatastoreTx ()
-- txCommit = W.tell

data TxToken = TxToken TxId ReleaseKey


instance DatastoreTxM DatastoreTx where
    getTxId = R.asks tdcTxId >>= \(TxToken txid _) -> return txid
    getNSId = R.asks tdcNamespace

-- instance Ctrl.MonadBaseControl IO DatastoreTx where
--     type StM DatastoreTx a = a
--     liftBaseWith f = DatastoreTx $ liftBaseWith $ \g -> f (g . unDSTx)
--     restoreM       = DatastoreTx . restoreM

instance MonadGoogle '[AuthDatastore] DatastoreTx where
    liftGoogle g = do
        env <- getEnv
        runGoogle env g

instance DatastoreM DatastoreTx where
    getEnv = R.asks $ dcAuthEnv . tdcDsConf
    getPid = R.asks $ dcProjId  . tdcDsConf
    getConf = R.asks tdcDsConf




data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException

-- Orphans
instance Monoid CommitRequest where
    mempty = mutationReq []
    mutReq1 `mappend` mutReq2 = mutationReq $
        (mutReq1 ^. crMutations) ++
        (mutReq2 ^. crMutations)

mutationReq :: [Mutation] -> CommitRequest
mutationReq mutL = commitRequest
    & crMutations .~ mutL