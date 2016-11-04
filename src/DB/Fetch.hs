{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Fetch where

import           Types
import           DB.Mutate
import           Model.PayState
import qualified Model.ChanIndex as Open
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import qualified Control.Monad as M
import qualified Control.Exception as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Catch    (MonadCatch)
import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy (Proxy)
import           Control.Lens
import           System.IO              (stderr)
import           Test.QuickCheck (Gen, sample', vectorOf, choose, generate)
import           Data.String.Conversions          (cs)

import qualified Network.HTTP.Conduit as HTTP



-- |Perform lookup inside transaction
txLookup :: ( MonadGoogle s m
            , HasScope s LookupResponse)
           => ProjectId
           -> Pay.SendPubKey
           -> TxId
           -> m LookupResponse
txLookup projectId sendPK tx =
    let lookupReq = lookupRequest &
                lrKeys .~ [mkKey projectId sendPK] &
                lrReadOptions ?~ (readOptions & roTransaction ?~ tx)
    in Google.send (projectsLookup lookupReq projectId)


--              HasScope s ["https://www.googleapis.com/auth/cloud-platform",
--                                             "https://www.googleapis.com/auth/datastore"]