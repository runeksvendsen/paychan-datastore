{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Lookup
(
  module DB.Request.Lookup
, parseLookupRes
)
where

import           DB.Model.Types
import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert


txLookup :: forall k e.
            ( HasScope '[AuthDatastore] ProjectsLookup
            , HasKeyPath k
            , IsEntity e
            )
           => NamespaceId
           -> TxId
           -> k
           -> Datastore ( Either String [(e, EntityVersion)] )
txLookup ns tx key = do
    partId <- mkPartitionId ns
    res <- Tagged <$> sendReq' (projectsLookup $ reqWithTx partId)
    return $ parseLookupRes (res :: Tagged e LookupResponse)    -- (EntityWithAnc e (EntityKey t))
  where
    reqWithTx pid = atomically tx $ unTagged (mkLookup (Just pid) key)
