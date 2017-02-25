{-# LANGUAGE DataKinds, RecordWildCards, DeriveAnyClass, DeriveGeneric #-}
module ChanDB.Types
(
  module DB
, module LibPrelude.Types
, module Convert
, module Tx
, UpdateErr(..)
, DBQuery(..)
-- , Key
, Note
, RecvPayChan
, Pay.SendPubKey
, Pay.PaymentChannel(..)
, PromissoryNote, StoredNote, UUID
, Pay.PayChanError
, Pay.BtcAmount
, UTCTime
)
where

import ChanDB.Types.StoredNote (StoredNote)
import LibPrelude.Types
import DB.Types as DB
import DB.Model.Convert as Convert
import Data.Time.Clock (UTCTime)

import DB.Tx.Safe as Tx

import           GHC.Generics
import qualified Data.Serialize as Bin
import           Data.Aeson (FromJSON, ToJSON)

import qualified PaymentChannel.Test as Pay
import           PromissoryNote                   (PromissoryNote, UUID) -- , setMostRecentNote)
import Text.Printf



data UpdateErr =
    PayError Pay.PayChanError
  | SpecValueMismatch { ueSpecVal :: Pay.BtcAmount, uePayVal :: Pay.BtcAmount }

  | ChannelNotFound
    deriving (Eq, Generic, ToJSON, FromJSON, Bin.Serialize)

instance Show UpdateErr where
    show (PayError pe) = "Payment error: " ++ show pe
    show (SpecValueMismatch specVal payVal ) =
        printf "Value of payment (%s) does not equal desired note value (%s)."
        (show payVal) (show specVal)
    show ChannelNotFound = "No such channel"

-- type Key = Pay.SendPubKey

-- | Translates into a GQL query
data DBQuery =
    GetAll
  | ExpiringBefore UTCTime
  | CoveringValue Pay.BtcAmount


type Note = PromissoryNote
type RecvPayChan = Pay.ServerPayChanX

