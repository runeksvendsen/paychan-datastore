{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Tx.Lookup where

import           Util
-- import           Model.PayState
import           Data.Maybe                (listToMaybe, fromMaybe)
import           Network.Google as Google


-- |Perform lookup inside transaction
txLookup :: ( MonadGoogle '[AuthDatastore] m
            , HasScope    '[AuthDatastore] ProjectsLookup )
           => ProjectId
           -> SendPubKey
           -> TxId
           -> m LookupResponse
txLookup projectId sendPK tx =
    let lookupReq = lookupRequest &
                lrKeys .~ [mkKey projectId sendPK] &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
    in Google.send (projectsLookup lookupReq projectId)

parseLookupRes :: LookupResponse -> Maybe (RecvPayChan, EntityVersion)
parseLookupRes lookupRes =
    listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
        case res ^. erEntity of
            Nothing  -> internalError "LookupResponse: Empty entityResult"
            Just ent -> Just
                ( decodeFromPropertyOrFail $
                    fromMaybe (internalError "LookupResponse: No properties in entity")
                    (ent ^. eProperties)
                , fromMaybe (internalError $
                         "CloudStore API BUG. LookupResponse: " ++
                         "Entity version not present")
                    (res ^. erVersion)
                )
