{-# LANGUAGE DataKinds #-}
module Types
(
  module Types
, module DB.Types
-- , module Network.Google
, module Network.Google.Datastore
, Pay.SendPubKey
, Pay.RecvPayChan
, Pay.PayChanError
, Catch.MonadCatch
, cs
)
where

import DB.Types

import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified Data.Text                      as T
import           Data.String.Conversions          (cs)

import qualified Control.Monad.Catch as      Catch

import           Network.Google.Datastore

type ProjectId = T.Text

type AuthCloudPlatform = "https://www.googleapis.com/auth/cloud-platform"
type AuthDatastore = "https://www.googleapis.com/auth/datastore"
