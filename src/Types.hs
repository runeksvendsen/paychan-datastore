{-# LANGUAGE DataKinds #-}
module Types
(
  module Types
-- , module Network.Google
-- , module Network.Google.Datastore
, Pay.SendPubKey
, Pay.RecvPayChan
, Pay.PayChanError
, Pay.BitcoinAmount
, Catch.MonadCatch
, MonadIO
, BS.ByteString
, T.Text
, Int64
, Tagged(..)
)
where

import           Data.Tagged (Tagged(..))
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import           Data.ByteString as BS
import           Data.Int                         (Int64)
import qualified Data.Text                      as T
import           Control.Monad.IO.Class     (MonadIO)

import qualified Control.Monad.Catch as      Catch

import           Network.Google.Datastore

type ProjectId = T.Text

type AuthCloudPlatform = "https://www.googleapis.com/auth/cloud-platform"
type AuthDatastore = "https://www.googleapis.com/auth/datastore"
