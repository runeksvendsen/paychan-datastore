{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Fetch where

import           Types
import           Model.PayState
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import           Control.Lens



-- |Perform lookup inside transaction
txLookup :: ( MonadGoogle s m
            , HasScope s '["https://www.googleapis.com/auth/cloud-platform",
                           "https://www.googleapis.com/auth/datastore"]
            )
           => ProjectId
           -> Pay.SendPubKey
           -> TxId
           -> m LookupResponse
txLookup projectId sendPK tx =
    let lookupReq = lookupRequest &
                lrKeys .~ [mkKey projectId sendPK] &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
    in Google.send (projectsLookup lookupReq projectId)

