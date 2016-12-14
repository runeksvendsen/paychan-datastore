{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Lookup
(
  module DB.Request.Lookup
, parseLookupRes
)
where

import           DB.Types
import           DB.Model.Convert
import           Util
import           Network.Google as Google


txLookup :: forall a anc m.
            ( MonadGoogle '[AuthDatastore] m
            , HasScope    '[AuthDatastore] ProjectsLookup
            , HasAncestor a anc)
           => NamespaceId
           -> TxId
           -> Ident anc
           -> Ident a
           -> m ( Either String [ ((a, Ident anc), EntityVersion) ] )
txLookup ns tx anc a =
    parseLookupRes <$> reqRes
        where
            reqRes :: m (Tagged a LookupResponse)
            reqRes = Tagged <$> Google.send (projectsLookup reqWithTx (nsProjectId ns))
            reqWithTx = unTagged (mkLookup ns anc a :: Tagged a LookupRequest) &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
