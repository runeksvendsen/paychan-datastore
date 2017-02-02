{-# LANGUAGE DataKinds, RecordWildCards #-}
module ChanDB.Types
(
  module DB
, module Types
, module Convert
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

import Types
import DB.Types as DB
import DB.Model.Convert as Convert
import Data.Time.Clock (UTCTime)

import qualified PaymentChannel.Test as Pay
import           PromissoryNote                   (PromissoryNote, StoredNote, UUID) -- , setMostRecentNote)


data UpdateErr =
    PayError Pay.PayChanError
  | ChannelNotFound


-- type Key = Pay.SendPubKey

-- | Translates into a GQL query
data DBQuery =
    GetAll
  | ExpiringBefore UTCTime
  | CoveringValue Pay.BtcAmount


type Note = PromissoryNote
type RecvPayChan = Pay.ServerPayChanX

