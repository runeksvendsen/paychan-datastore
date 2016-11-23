{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Value.Native
(
  module DB.Model.Convert.Value.Native
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
class Typeable a => NativeValue a where
    valueLens       :: Lens' DS.Value (Maybe a)

instance NativeValue Bool            where valueLens = vBooleanValue
instance NativeValue ByteString      where valueLens = vBlobValue
instance NativeValue Text            where valueLens = vStringValue
instance NativeValue Int64           where valueLens = vIntegerValue
instance NativeValue Double          where valueLens = vDoubleValue
instance NativeValue DS.Key          where valueLens = vKeyValue
instance NativeValue DS.Entity       where valueLens = vEntityValue
instance NativeValue DS.ArrayValue   where valueLens = vArrayValue
-- instance NativeValue [a]             where valueLens = fmap avValues vArrayValue
instance NativeValue DS.ValueNullValue where valueLens = vNullValue


encode :: NativeValue a => a -> DS.Value
encode a = set valueLens (Just a) value

decodeMaybe :: NativeValue a => DS.Value -> Maybe a
decodeMaybe = view valueLens

decode :: forall a. NativeValue a => DS.Value -> a
decode v = fromMaybe
    ( error $ (show . show $ typeOf (undefined :: a)) ++ " not present in DS.Value: " ++ show v )
    (decodeMaybe v)
