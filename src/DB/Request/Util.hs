{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Util where

import DB.Types
import DB.Util.Error
import Network.Google as Google


sendReq :: (DatastoreM m, HasScope '[AuthDatastore] a, GoogleRequest a) => (ProjectId -> a) -> m (Rs a)
sendReq mkReq = do
    pid <- getPid
    liftGoogle $ Google.send (mkReq pid)


sendReq' :: (HasScope '[AuthDatastore] a, GoogleRequest a) => (ProjectId -> a) -> Datastore (Rs a)
sendReq' mkReq = do
    pid <- getPid
    liftGoogle $ Google.send (mkReq pid)



getFirstResult :: Either String [ ((a, Ident anc), EntityVersion) ] -> Maybe a
getFirstResult resE =
    either internalError id $
    fmap getFirst resE
  where
    getFirst res = case res of
        ( ((chan,_),_) : _ ) -> Just chan
        []                   -> Nothing

