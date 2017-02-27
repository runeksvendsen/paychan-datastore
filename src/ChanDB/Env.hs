{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module ChanDB.Env
( defaultAppDatastoreEnv )
where

import           ChanDB.Types
import           Control.Lens                   hiding (op)
import           System.IO                  (stderr)
import qualified Network.HTTP.Conduit as HTTP
import           Network.Google as Google


defaultAppDatastoreEnv :: IO (Env '[AuthDatastore])
defaultAppDatastoreEnv = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    logger  <- Google.newLogger Google.Debug stderr
    Google.newEnv <&>
        (envLogger .~ logger) .
        (envScopes .~ datastoreScope) .
        (envManager .~ manager)
