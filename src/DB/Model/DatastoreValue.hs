{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.DatastoreValue
(
  module DB.Model.DatastoreValue
, Text
, ByteString
, Int64
)
where

import Network.Google.Datastore
import Control.Lens

import qualified Network.Google.Datastore.Types as DS
import           Data.Text          (Text)
import           Data.ByteString    (ByteString)
import           Data.Int           (Int64)
import           Data.Maybe         (fromMaybe)
import           Data.Typeable


-- | A class for the value types inside a Datastore object
class Typeable a => DatastoreValue a where
    valueLens       :: Lens' DS.Value (Maybe a)
    encode          :: a -> DS.Value
    decode          :: DS.Value -> a
    decodeMaybe     :: DS.Value -> Maybe a

    encode a = set valueLens (Just a) value
    decodeMaybe = view valueLens
    decode v = fromMaybe
        ( error $ show (typeOf (undefined :: a)) ++ " not present in DS.Value: " ++ show v )
        (decodeMaybe v)

instance DatastoreValue Bool            where valueLens = vBooleanValue
instance DatastoreValue ByteString      where valueLens = vBlobValue
instance DatastoreValue Text            where valueLens = vStringValue
instance DatastoreValue Int64           where valueLens = vIntegerValue
instance DatastoreValue Double          where valueLens = vDoubleValue
instance DatastoreValue DS.Entity       where valueLens = vEntityValue
instance DatastoreValue DS.ArrayValue   where valueLens = vArrayValue
instance DatastoreValue DS.ValueNullValue where valueLens = vNullValue
