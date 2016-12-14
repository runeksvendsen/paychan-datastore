{-# LANGUAGE DataKinds, RecordWildCards #-}
module ChanDB.Types
(
  module DB
, module Types
, Note
, RecvPayChan
, Pay.SendPubKey
, PromissoryNote, StoredNote, UUID
, Pay.PayChanError
, Pay.BitcoinAmount
)
where

import Types
import DB.Types as DB

import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import           PromissoryNote                   (PromissoryNote, StoredNote, UUID) -- , setMostRecentNote)

type Note = PromissoryNote
type RecvPayChan = Pay.RecvPayChanX
-- type ProjectId = T.Text
