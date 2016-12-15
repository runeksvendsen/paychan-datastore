{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DB.Run where

import DB.Types
import qualified Control.Monad.Trans.Resource   as Res
import qualified Control.Monad.Reader           as Reader


runDatastore :: Res.MonadResource m => DatastoreConf -> Datastore a -> m a
runDatastore e m = Res.liftResourceT $ Reader.runReaderT (unDS m) e


