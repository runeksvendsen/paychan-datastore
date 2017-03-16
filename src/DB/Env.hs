{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Env
( defaultAppDatastoreEnv )
where

import           ChanDB.Types
import           Control.Lens hiding (op)
import           System.IO                             (Handle)
import           Network.Google       as Google
import qualified Control.Monad.Logger as Log
import qualified Network.HTTP.Conduit as HTTPS
--import qualified Network.HTTP.Client as HTTP


fromMonadLog :: Log.LogLevel -> Google.LogLevel
fromMonadLog mLL = case mLL of
    Log.LevelDebug    -> Google.Debug
    Log.LevelInfo     -> Google.Info
    Log.LevelWarn     -> Google.Info
    Log.LevelError    -> Google.Error
    Log.LevelOther "trace" -> Google.Trace
    Log.LevelOther _  -> Google.Info

defaultAppDatastoreEnv :: Handle -> Log.LogLevel -> IO (Env '[AuthDatastore])
defaultAppDatastoreEnv h logLvl = do
    manager <- HTTPS.newManager HTTPS.tlsManagerSettings
    logger  <- Google.newLogger (fromMonadLog logLvl) h
    Google.newEnv <&>
        (envLogger .~ logger) .
        (envScopes .~ datastoreScope) .
        (envManager .~ manager)
