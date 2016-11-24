{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
module DB.Model.Types.Identifier where

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
import           Data.Void                        (Void)
import qualified Network.Google.Datastore   as DS


type Root = Ident Void

data Ident a = Ident
    { iId       :: Either Int64 Text
    } deriving (Eq, Typeable)

-- | Identifier for Datastore objects.
-- Two objects of type 'a' are considered equal if they return the same 'objectId'
-- So if "objectId a1 == objectId a2" then "upsert a2" will overwrite "a1".
-- See: https://cloud.google.com/datastore/docs/concepts/entities#assigning_identifiers
class Typeable a => Identifier a where
    objectId    :: a -> Either Int64 Text   -- ^ Unique object identifier

getIdent :: forall a. Identifier a => a -> Ident a
getIdent a = Ident (objectId a)

-- | Root entities have an ancestor 'Identifier' of @Ident Void@
instance Identifier Void where
    objectId _ = Left 0

instance Identifier (Either Int64 Text) where
    objectId = id

instance Typeable a => Identifier (Ident a) where
    objectId (Ident iid) = iid

instance Identifier SendPubKey
    where objectId = Right . encodeHex

instance Identifier UUID
    where objectId = Right . encodeHex

instance Identifier RecvPayChan
    where objectId = objectId . Pay.getSenderPubKey

instance Identifier PromissoryNote
    where objectId = objectId . Note.getID


encodeHex :: Bin.Serialize a => a -> Text
encodeHex = cs . B16.encode . Bin.encode

instance Typeable i => Show (Ident i) where
    show i
        | objectId i == Left 0 = "/"
        | otherwise = typeStr ++ ":" ++ either show show (objectId i)
      where typeStr = show (typeOf (undefined :: i))


-- ^ Get type identifier from an object instance.
objectType :: Typeable a => a -> Text
objectType = cs . show . typeOf
