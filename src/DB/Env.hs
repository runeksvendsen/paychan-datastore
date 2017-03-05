{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Env
( defaultAppDatastoreEnv )
where

import           ChanDB.Types
import           Control.Lens                   hiding (op)
import           System.IO                             (stderr)
import qualified Network.HTTP.Conduit as HTTPS
--import qualified Network.HTTP.Client as HTTP
import           Network.Google as Google


defaultAppDatastoreEnv :: Google.LogLevel -> IO (Env '[AuthDatastore])
defaultAppDatastoreEnv logLvl = do
    manager <- HTTPS.newManager HTTPS.tlsManagerSettings
    logger  <- Google.newLogger logLvl stderr
    Google.newEnv <&>
        (envLogger .~ logger) .
        (envScopes .~ datastoreScope) .
        (envManager .~ manager)
