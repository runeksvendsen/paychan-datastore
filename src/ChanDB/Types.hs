{-# LANGUAGE DataKinds, RecordWildCards #-}
module ChanDB.Types
(
  module DB
, module Types
, UpdateErr(..)
, DBQuery(..)
, Key
, Note
, RecvPayChan
, Pay.SendPubKey
, PromissoryNote, StoredNote, UUID
, Pay.PayChanError
, Pay.BitcoinAmount
, UTCTime
)
where

import Types
import DB.Types as DB
import Data.Time.Clock (UTCTime)

import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import           PromissoryNote                   (PromissoryNote, StoredNote, UUID) -- , setMostRecentNote)


data UpdateErr =
    PayError Pay.PayChanError
  | ChannelNotFound


type Key = Pay.SendPubKey

-- | Translates into a GQL query
data DBQuery =
    GetAll
  | ExpiringBefore UTCTime
  | CoveringValue Pay.BitcoinAmount


type Note = PromissoryNote
type RecvPayChan = Pay.RecvPayChanX

