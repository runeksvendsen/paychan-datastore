{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
module DB.Model.Types.Identifier where

import           Types
import           Util
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import           Data.Typeable
import           Data.Void                        (Void)
import           Network.Google.Datastore       as DS


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

ident :: Identifier a => a -> Ident b
ident a = Ident (objectId a)

identPathElem :: forall a. Identifier a => a -> DS.PathElement
identPathElem i = DS.pathElement &
        DS.peKind ?~ cs kindStr &
        either (DS.peId ?~) (DS.peName ?~) (objectId i)
  where
    kindStr = show (typeOf (undefined :: a))

castIdent :: Ident a -> Ident b
castIdent (Ident i) = Ident i


type Root = Ident Void
root :: Root
root = Ident $ Left 0


-- instance Typeable a => Identifier (Ident a) where
--     objectId (Ident id) = id

instance Identifier Void where
    objectId _ = Left 0

instance Identifier (Either Int64 Text) where
    objectId = id

encodeHex :: Bin.Serialize a => a -> Text
encodeHex = cs . B16.encode . Bin.encode

instance Identifier i => Show (Ident i) where
    show (Ident i)
        | objectId i == Left 0 = "/"
        | otherwise = typeStr ++ ":" ++ either show show (objectId i)
      where typeStr = show (typeOf (undefined :: i))


-- ^ Get type identifier from an object instance.
objectType :: Typeable a => a -> Text
objectType = cs . show . typeOf
