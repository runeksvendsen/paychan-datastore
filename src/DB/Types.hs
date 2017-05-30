{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, TypeFamilies     #-}
module DB.Types
(
  module DB.Types
, module DB.Model.Types.Entity
, module DB.Model.Types.Request
, module Ancestor
, module Datastore
, module Google
, module X
, DatastoreConf(..)
, Tagged(..)
, Handle
, Log.LogLevel(..)
)
where

import LibPrelude
import DB.Error.Types as X
import DB.Model.Types as X hiding (Key)
import DB.Model.Types.Entity
import DB.Model.Types.Request
import DB.Model.Types.KeyPath as Ancestor
import Data.Tagged (Tagged(..))

import Network.Google.Datastore                 as Datastore hiding (key, Key)
import Network.Google                           as Google    hiding (LogLevel)
import Control.Monad.Trans.Resource             as Res
import Control.Monad.Trans.Control              as Ctrl
import Control.Monad.IO.Class                   (MonadIO)
import Control.Applicative                      (Alternative)
import           System.IO                      (Handle)

import qualified Control.Monad.Catch            as Catch
import qualified Control.Monad.Base             as Base
import qualified Control.Exception              as Except
import qualified Control.Monad.Reader           as R
import qualified Control.Monad.Writer.Strict    as W
-- Logging
import qualified Control.Monad.Logger           as Log
import qualified System.Log.FastLogger          as FLog
import qualified Data.ByteString.Char8          as S8


emptyQuery = query

runDatastore :: DatastoreConf -> Datastore a -> IO (Either DBException a)
runDatastore cfg d =
    Res.runResourceT $ Catch.try $ liftResourceT $ R.runReaderT (runDSLogging cfg $ unDS d) cfg

runDSLogging cfg lM = Log.runLoggingT lM $ logDefaultOutput (dcLogHandle cfg)


logDefaultOutput :: Handle
              -> Log.Loc
              -> Log.LogSource
              -> Log.LogLevel
              -> Log.LogStr
              -> IO ()
logDefaultOutput h loc src level msg =
    S8.hPutStr h ls
  where
    ls = FLog.fromLogStr $ Log.defaultLogStr loc src level msg




-- Ctrl.MonadBaseControl IO m
class (MonadLogger m, Res.MonadResource m, MonadGoogle '[AuthDatastore] m) => DatastoreM m where
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

newtype Datastore a = Datastore
    { unDS :: Log.LoggingT (R.ReaderT DatastoreConf (Res.ResourceT IO)) a
    }
    deriving
        ( Functor
        , Applicative
        , Log.MonadLogger
        , Monad
        , MonadIO
        , MonadThrow
        , Catch.MonadCatch
        , Catch.MonadMask
        , Base.MonadBase IO
        , R.MonadReader DatastoreConf
        , MonadResource
        )

data DatastoreConf = DatastoreConf
    { dcAuthEnv   :: Env '[AuthDatastore]
    , dcProjId    :: ProjectId
    , dcLogLevel  :: Log.LogLevel
    , dcLogHandle :: Handle
    }

instance Ctrl.MonadBaseControl IO Datastore where
    type StM Datastore a = a
    liftBaseWith f = Datastore $ liftBaseWith $ \g -> f (g . unDS)
    restoreM       = Datastore . restoreM

instance MonadGoogle '[AuthDatastore] Datastore where
    liftGoogle g = do
        env <- getEnv
        runGoogle env g



-- | Tx
class ( DatastoreM m
      , W.MonadWriter CommitRequest m
      , MonadGoogle '[AuthDatastore] m
      , Log.MonadLogger m
      ) => DatastoreTxM m
    where
      getTxId :: m TxId
      getNSId :: m NamespaceId


newtype DatastoreTx a = DatastoreTx
    { unDSTx :: Log.LoggingT (R.ReaderT TxDatastoreConf (W.WriterT CommitRequest (Res.ResourceT IO))) a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , Log.MonadLogger
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

data TxToken = TxToken TxId ReleaseKey


instance DatastoreTxM DatastoreTx where
    getTxId = R.asks tdcTxId >>= \(TxToken txid _) -> return txid
    getNSId = R.asks tdcNamespace

instance MonadGoogle '[AuthDatastore] DatastoreTx where
    liftGoogle g = do
        env <- getEnv
        runGoogle env g


instance DatastoreM DatastoreTx where
    getEnv = R.asks $ dcAuthEnv . tdcDsConf
    getPid = R.asks $ dcProjId  . tdcDsConf
    getConf = R.asks tdcDsConf


data UpdateResult = Updated | NotUpdated deriving Show

-- Orphans
instance Monoid CommitRequest where
    mempty = mutationReq []
    mutReq1 `mappend` mutReq2 = mutationReq $
        (mutReq1 ^. crMutations) ++
        (mutReq2 ^. crMutations)

mutationReq :: [Mutation] -> CommitRequest
mutationReq mutL = commitRequest
    & crMutations .~ mutL
    & crMode ?~ NonTransactional
