{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.Model.Identifier where

-- import           DB.Model.Storable
import           Types
-- import           Util

import           PromissoryNote                   (PromissoryNote, UUID)
import qualified PromissoryNote                 as Note
import qualified Data.Bitcoin.PaymentChannel    as Pay
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import           Data.Typeable





data Ident a = Ident
    { iId       :: Either Int64 Text
    } deriving Typeable

-- | Identifier for Datastore objects.
-- Two objects of type 'a' are considered equal if they return the same 'objectId'
-- So if "objectId a1 == objectId a2" then "upsert a2" will overwrite "a1".
-- See: https://cloud.google.com/datastore/docs/concepts/entities#assigning_identifiers
class Typeable a => Identifier a where
    objectId    :: a -> Either Int64 Text   -- ^ Unique object identifier

getIdent :: forall a. Identifier a => a -> Ident a
getIdent a = Ident (objectId a)

instance Identifier (Either Int64 Text) where
    objectId = id

instance Typeable a => Identifier (Ident a) where
    objectId (Ident iid) = iid

instance Identifier SendPubKey
    where objectId = Right . encodeHex

instance Identifier RecvPayChan
    where objectId = objectId . Pay.getSenderPubKey

instance Identifier UUID
    where objectId = Right . encodeHex

instance Identifier PromissoryNote
    where objectId = objectId . Note.getID





encodeHex :: Bin.Serialize a => a -> Text
encodeHex = cs . B16.encode . Bin.encode

-- showIdent :: Ident a -> String
-- showIdent (Ident i) = cs $ typeStr <> ":" <> either (cs . show) id i
--     where typeStr = cs $ show (typeOf (undefined :: a))


-- ^ Get type identifier from an object instance.
objectType :: Typeable a => a -> Text
objectType = cs . show . typeOf
