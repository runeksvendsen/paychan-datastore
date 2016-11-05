{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Lookup where

import           Util
import           Model.PayState

import           Network.Google as Google


-- |Perform lookup inside transaction
txLookup :: ( MonadGoogle '[AuthDatastore] m
            , HasScope    '[AuthDatastore] ProjectsLookup
            )
           => ProjectId
           -> SendPubKey
           -> TxId
           -> m LookupResponse
txLookup projectId sendPK tx =
    let lookupReq = lookupRequest &
                lrKeys .~ [mkKey projectId sendPK] &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
    in Google.send (projectsLookup lookupReq projectId)

