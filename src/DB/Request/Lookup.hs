{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Lookup
(
  module DB.Request.Lookup
, parseLookupRes
)
where

import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert
import           Util


txLookup :: forall a anc.
            HasAncestor a anc
           => Maybe PartitionId
           -> TxId
           -> Ident anc
           -> Ident a
           -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
txLookup ns tx anc a =
    parseLookupRes <$> reqRes
        where
            reqRes = Tagged <$> sendReq' (projectsLookup reqWithTx) -- (nsProjectId ns))
            reqWithTx = atomically tx $ unTagged (mkLookup ns anc a :: Tagged a LookupRequest)
--                 lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
