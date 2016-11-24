{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Tx.Lookup
(
  module DB.Tx.Lookup
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
           => ProjectId
           -> TxId
           -> Ident anc
           -> Ident a
           -> m ( Either String [ ((a, Ident anc), EntityVersion) ] )
txLookup projectId tx anc a =
    parseLookupRes <$> reqRes
        where
            reqRes = Tagged <$> Google.send (projectsLookup reqWithTx projectId) :: m (Tagged a LookupResponse)
            reqWithTx = unTagged (mkLookup anc a :: Tagged a LookupRequest) &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)


-- parseLookupRes :: HasProperties a k => LookupResponse -> Maybe (a, EntityVersion)
-- parseLookupRes lookupRes =
--     listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
--         case res ^. erEntity of
--             Nothing  -> internalError "LookupResponse: Empty entityResult"
--             Just ent -> Just
--                 ( decodeFromPropertyOrFail $
--                     fromMaybe (internalError "LookupResponse: No properties in entity")
--                     (ent ^. eProperties)
--                 , fromMaybe (internalError $
--                          "CloudStore API BUG. LookupResponse: " ++
--                          "Entity version not present")
--                     (res ^. erVersion)
--                 )
