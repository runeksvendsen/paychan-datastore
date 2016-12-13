{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Tx.Lookup
(
  module DB.Tx.Lookup
, parseLookupRes
)
where

import           DB.Types
import           DB.Util.Error
import           DB.Model.Convert
import           Util
import           Network.Google as Google


getFirstResult :: Either String [ ((a, Ident anc), EntityVersion) ] -> Maybe a
getFirstResult resE =
    either internalError id $
    fmap getFirst resE
  where
    getFirst res = case res of
        ( ((chan,_),_) : _ ) -> Just chan
        []                   -> Nothing

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
            reqRes :: m (Tagged a LookupResponse)
            reqRes = Tagged <$> Google.send (projectsLookup reqWithTx projectId)
            reqWithTx = unTagged (mkLookup anc a :: Tagged a LookupRequest) &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)

txAncestorQuery :: forall a anc m.
           ( MonadGoogle '[AuthDatastore] m
           , HasScope    '[AuthDatastore] ProjectsRunQuery
           , HasAncestor a anc)
          => ProjectId
          -> TxId
          -> Ident anc
          -> Text
          -> m ( Either String [ ((a, Ident anc), EntityVersion) ] )
txAncestorQuery projectId tx anc query =
    parseQueryRes <$> reqRes
        where
            reqRes :: m (Tagged a RunQueryResponse)
            reqRes = Tagged <$> Google.send (projectsRunQuery reqWithTx projectId)
            reqWithTx = unTagged (mkQueryReq (Just anc) query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)


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
