{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module DB.Model.EntityProps
(
  -- * Util
  jsonToDS
, jsonFromDS
  -- * Type
, EntityProps
, NoIndexKey
)

where

import DB.Model.NativeValue

import Network.Google.Datastore
import Control.Lens

import qualified Network.Google.Datastore.Types as DS
import qualified Data.Aeson             as JSON
import qualified Data.Vector            as Vec
import qualified Data.Scientific        as Sci
import qualified Data.HashMap.Strict    as Map
import           Data.Maybe               (fromMaybe)
import           Control.Applicative     ((<|>))


type EntityProps = Map.HashMap Text DS.Value
type NoIndexKey = Text

-- |
class ToJSON' a where
    wrapJson :: a -> DS.Value

instance ToJSON' Sci.Scientific where
    wrapJson sci =
        case Sci.toBoundedInteger sci of
            Just i  -> encode (i :: Int64)
            Nothing -> case Sci.toBoundedRealFloat sci of
                   Right d -> if Sci.fromFloatDigits d /= sci then
                        error "Double doesn't fit!" else encode (d :: Double)
                   Left inf  -> error $ "Number doesn't fit because it's " ++ show inf

instance ToJSON' (Vec.Vector DS.Value) where
    wrapJson vec = encode $
       arrayValue & avValues .~ Vec.toList vec


-- | Convert a JSON.Value, excluding from indexing 'JSON.Object' keys in '[NoIndexKey]'
jsonToDS :: [NoIndexKey] -> JSON.Value -> DS.Value
jsonToDS _ JSON.Null         = encode NullValue
jsonToDS _ (JSON.Bool b)     = encode b
jsonToDS _ (JSON.String txt) = encode txt
jsonToDS _ (JSON.Number sci) = wrapJson sci

jsonToDS exL (JSON.Array vec) = wrapJson $ fmap (jsonToDS exL) vec
jsonToDS exL (JSON.Object hmap) =
    encode $
        entity & eProperties ?~ entityProperties
            (convertWithIndex exL hmap)

convertWithIndex :: [NoIndexKey] -> JSON.Object -> EntityProps
convertWithIndex indxExcl = Map.mapWithKey $ \k v -> convertValue v (k `elem` indxExcl)
    where
        convertValue :: JSON.Value -> Bool -> DS.Value
        convertValue val exclude = jsonToDS indxExcl val & vExcludeFromIndexes ?~ exclude


-- | Construct a 'JSON.Value' from any 'NativeValue'
class NativeValue a => MkJson a where
    mkJson :: a -> JSON.Value

instance MkJson Bool where
    mkJson = JSON.toJSON

instance MkJson Text where
    mkJson = JSON.toJSON

instance MkJson Int64 where
    mkJson = JSON.toJSON

instance MkJson Double where
    mkJson = JSON.toJSON

instance MkJson DS.ValueNullValue where
    mkJson _ = JSON.Null

instance MkJson DS.ArrayValue where
    mkJson  = JSON.toJSON . fmap jsonFromDS . (^. avValues)

instance MkJson DS.Entity where
    mkJson e = JSON.Object $
        fmap jsonFromDS . view epAddtional .
        fromMaybe (entityProperties Map.empty) $
        (e ^. eProperties)

jsonFromDS :: DS.Value -> JSON.Value
jsonFromDS val =
    let parseRes =
                mkJson <$> (decodeMaybe val :: Maybe Bool)
            <|> mkJson <$> (decodeMaybe val :: Maybe Text)
            <|> mkJson <$> (decodeMaybe val :: Maybe Double)
            <|> mkJson <$> (decodeMaybe val :: Maybe Int64)
            <|> mkJson <$> (decodeMaybe val :: Maybe DS.Entity)
            <|> mkJson <$> (decodeMaybe val :: Maybe DS.ArrayValue)
            <|> mkJson <$> (decodeMaybe val :: Maybe DS.ValueNullValue)
    in
        fromMaybe JSON.Null parseRes    -- (fail "DS.Value -> JSON parse fail")



