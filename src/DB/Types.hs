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

import           Util
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


emptyQuery = query

runDatastore :: DatastoreConf -> Datastore a -> IO a
runDatastore cfg d = Res.runResourceT $ liftResourceT $ R.runReaderT (unDS d) cfg

class (Res.MonadResource m, MonadGoogle '[AuthDatastore] m) => DatastoreM m where
    getEnv          :: m (Env '[AuthDatastore])
    getPid          :: m ProjectId
    liftDS          :: Datastore a -> m a
    -- | Create a 'PartitionId' using the project id in 'DatastoreConf'
    mkPartitionId   :: NamespaceId -> m PartitionId
    mkPartitionId nsId = do
        pid <- getPid
        return $ partitionId &
            piNamespaceId ?~ nsId &
            piProjectId ?~ pid


instance DatastoreM Datastore where
    getEnv = dcAuthEnv <$> R.ask
    getPid = dcProjId  <$> R.ask
    liftDS d = do
        cfg <- R.ask
        liftResourceT $ R.runReaderT (unDS d) cfg

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

instance Ctrl.MonadBaseControl IO Datastore where
    type StM Datastore a = a
    liftBaseWith f = Datastore $ liftBaseWith $ \g -> f (g . unDS)
    restoreM       = Datastore . restoreM

instance MonadGoogle '[AuthDatastore] Datastore where
    liftGoogle g = do
        env <- getEnv
        runGoogle env g

data DatastoreConf = DatastoreConf
    { dcAuthEnv :: Env '[AuthDatastore]
    , dcProjId  :: ProjectId
    }

data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException

