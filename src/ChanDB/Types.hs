{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ChanDB.Types
( module ChanDB.Types
, module DB.Types
, module LibPrelude.Types
, module Convert
, module Tx
, module X
, UpdateErr(..)
, DBQuery(..)
-- , Key
--, Note
--, RecvPayChan
, Pay.SendPubKey
, Pay.RecvPubKey(..)
, Pay.PaymentChannel(..)
, PromissoryNote, StoredNote, UUID
, Pay.PayChanError
, Pay.KeyDeriveIndex
, Pay.BtcAmount
, Pay.External(..)
, Pay.ChildPub(..)
, HC.XPubKey
, UTCTime
, Handle
, Log.LoggingT
)
where

import ChanDB.Orphans ()
import ChanDB.Types.StoredNote (StoredNote)
import LibPrelude.Types
import DB.Types
import DB.Model.Convert as Convert
import Data.Time.Clock (UTCTime)
import System.IO     (Handle)
import DB.Tx.Safe as Tx

import           GHC.Generics
import qualified Data.Serialize         as Bin
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as JSON
import qualified Network.Haskoin.Crypto as HC
import qualified Control.Exception      as Except
import qualified Control.Monad.Logger   as Log
import Control.Monad.Trans.Either       as X
import qualified PaymentChannel         as Pay
import           PromissoryNote         (PromissoryNote, UUID, HasUUID(..))
--import qualified Data.Text              as T
import Text.Printf


type Key = Pay.SharedSecret

data ChanDBException =
    DBException DBException
  | MissingXPub HC.XPubKey      -- ^ Database not initialized for XPub
  | InsufficientValue { cdbeValueMissing :: Pay.BtcAmount }
      deriving (Eq, Show)

instance HasNotFound ChanDBException where
    is404 (DBException e) = is404 e
    is404 _ = False

instance Except.Exception ChanDBException

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

-- | Translates into a GQL query
data DBQuery =
    GetAll
  | ExpiringBefore UTCTime
  | CoveringValue Pay.BtcAmount


type Note = PromissoryNote
type RecvPayChan = Pay.ServerPayChanX

