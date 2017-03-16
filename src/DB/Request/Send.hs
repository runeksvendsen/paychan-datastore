{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Send where

import LibPrelude
import DB.Model.Types.Request
import DB.Types
import DB.Error.Util
import Network.Google as Google


sendReq :: ( DatastoreM m
           , HasScope '[AuthDatastore] a
           , GoogleRequest a
           )
         => (ProjectId -> a)
         -> m (Rs a)
sendReq mkReq = do
    pid <- getPid
    liftGoogle $ Google.send (mkReq pid)

