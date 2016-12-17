{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Util where

import DB.Model.Types.Request
import DB.Types
import DB.Util.Error
import Network.Google as Google


sendReq' :: (DatastoreM m, HasScope '[AuthDatastore] a, GoogleRequest a) => (ProjectId -> a) -> m (Rs a)
sendReq' mkReq = do
    pid <- getPid
    liftGoogle $ Google.send (mkReq pid)

-- sendReq'' :: (DatastoreM m, HasProject req p, HasScope '[AuthDatastore] p, GoogleRequest p) => req -> m (Rs p)
-- sendReq'' req = mkProjectReq req >>= liftGoogle . Google.send


-- sendReq :: forall p. (HasScope '[AuthDatastore] p, GoogleRequest p) => p -> Datastore (Rs p)
-- sendReq = liftGoogle . Google.send



getFirstResult :: Either String [ ((a, Ident anc), EntityVersion) ] -> Maybe a
getFirstResult resE =
    either internalError id $
    fmap getFirst resE
  where
    getFirst res = case res of
        ( ((chan,_),_) : _ ) -> Just chan
        []                   -> Nothing

