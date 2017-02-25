{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Send where

import LibPrelude
import DB.Model.Types.Request
import DB.Types
import DB.Util.Error
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


sendReq' :: ( DatastoreM m
           , HasScope '[AuthDatastore] a
           , GoogleRequest a
           )
         => a
         -> m (Rs a)
sendReq' req = do
--     pid <- getPid
    liftGoogle $ Google.send req



getFirstResult :: Either String [ (a, EntityVersion) ] -> Maybe a
getFirstResult resE =
    fst <$> either (const Nothing) listToMaybe resE
