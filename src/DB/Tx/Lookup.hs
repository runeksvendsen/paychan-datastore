{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
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


txLookup :: ( MonadGoogle '[AuthDatastore] m
            , HasScope    '[AuthDatastore] ProjectsLookup
            , HasKey k)
           => ProjectId
           -> TxId
           -> k
           -> m LookupResponse
txLookup projectId tx key =
    Google.send (projectsLookup reqWithTx projectId)
        where reqWithTx = mkLookup key &
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
