{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
module DB.Model.Convert.Value
(
  module DB.Model.Convert.Value
, module DB.Model.Convert.Value.Native
)
where

import DB.Model.Convert.Value.EncodeAsKey
import DB.Model.Convert.Value.Native -- (encode, decode, decodeMaybe)
import Util
import qualified Network.Google.Datastore as DS
import Data.Text (Text)
import qualified Data.Scientific        as Sci
import qualified Data.Vector            as Vec
import           Control.Applicative     ((<|>))
import           Debug.Trace


-- | Convert any value to a 'DS.Value'
class ToValue a where
    toValue :: a -> DS.Value

-- Native values
instance ToValue Bool            where toValue = encode
instance ToValue ByteString      where toValue = encode
instance ToValue Text            where toValue = encode
instance ToValue Int64           where toValue = encode
instance ToValue Double          where toValue = encode
instance ToValue DS.Key          where toValue = encode
instance ToValue DS.Entity       where toValue = encode
instance ToValue DS.ArrayValue   where toValue = encode
instance ToValue DS.ValueNullValue where toValue = encode

instance ToValue Sci.Scientific where
    toValue sci =
        case Sci.toBoundedInteger sci of
            Just i  -> encode (i :: Int64)
            Nothing -> case Sci.toBoundedRealFloat sci of
                   Left inf -> error $ "Number doesn't fit because it's " ++ show inf
                   Right d  ->
                        if Sci.fromFloatDigits d == sci then
                            encode (d :: Double)
                        else
                            toSciDiff sci

instance ToValue (Vec.Vector DS.Value) where
    toValue vec = toValue (Vec.toList vec)

instance ToValue [DS.Value] where
    toValue lst = toValue $
       DS.arrayValue & DS.avValues .~ lst


-- | Parse a value from a 'DS.Value'
class ToValue a => FromValue a where
    fromValue :: DS.Value -> Maybe a

-- Native values
instance FromValue Bool            where fromValue = decodeMaybe
instance FromValue ByteString      where fromValue = decodeMaybe
instance FromValue Text            where fromValue = decodeMaybe
instance FromValue Int64           where fromValue = decodeMaybe
instance FromValue Double          where fromValue = decodeMaybe
instance FromValue DS.Key          where fromValue = decodeMaybe
instance FromValue DS.Entity       where fromValue = decodeMaybe
instance FromValue DS.ArrayValue   where fromValue = decodeMaybe
instance FromValue [DS.Value]      where fromValue v = fmap (^. DS.avValues) (v ^. DS.vArrayValue)
instance FromValue DS.ValueNullValue where fromValue = decodeMaybe

instance FromValue Sci.Scientific where
    fromValue val =
            fromIntegral <$> (fromValue val :: Maybe Int64)
        <|> Sci.fromFloatDigits <$> (fromValue val :: Maybe Double)
        <|> fromSciDiff val

-- | Encode, inside a list, a 'Sci.Scientific' as a Double and a 'Sci.Scientific' remainder
toSciDiff :: Sci.Scientific -> DS.Value
toSciDiff sci = toValue [ encode sciDouble, encodeAsKey sciDiff ]
  where
    sciDouble = Sci.toRealFloat sci :: Double
    sciDiff = sci - Sci.fromFloatDigits sciDouble

fromSciDiff :: DS.Value -> Maybe Sci.Scientific
fromSciDiff val = fromValue val >>= \lst -> case lst of
    [ doubleV, remainderV ] -> sciDiff remainderV >>=
        \diff -> fmap (+ diff) (fromDouble doubleV)
    _ -> Nothing
  where
    fromDouble d = Sci.fromFloatDigits <$> (fromValue d :: Maybe Double)
    sciDiff d = decodeAsKey d :: Maybe Sci.Scientific
