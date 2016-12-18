{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module DB.Model.Convert.Properties
(
  -- * Util
  jsonToDS
, jsonFromDS
, convertWithIndex
, decodeProperties
, decodeEntProps
  -- * Type
, EntityProps
, NoIndexKey
)

where

import DB.Model.Convert.Value
import Types
import Network.Google.Datastore
import Control.Lens

import qualified Network.Google.Datastore.Types as DS
import qualified Data.Aeson             as JSON
import qualified Data.Scientific        as Sci
import qualified Data.HashMap.Strict    as Map
import           Data.Maybe               (fromMaybe)
import           Control.Applicative     ((<|>))


type EntityProps = Map.HashMap Text DS.Value
type NoIndexKey = Text

-- | Convert a JSON.Value, excluding from indexing 'JSON.Object' keys in '[NoIndexKey]'
jsonToDS :: [NoIndexKey] -> JSON.Value -> DS.Value
jsonToDS _ JSON.Null         = toValue NullValue
jsonToDS _ (JSON.Bool b)     = toValue b
jsonToDS _ (JSON.String txt) = toValue txt
jsonToDS _ (JSON.Number sci) = toValue sci

jsonToDS exL (JSON.Array vec) = toValue $ fmap (jsonToDS exL) vec
jsonToDS exL (JSON.Object hmap) =
    toValue $
        entity & eProperties ?~ entityProperties
            (convertWithIndex exL hmap)

convertWithIndex :: [NoIndexKey] -> JSON.Object -> EntityProps
convertWithIndex indxExcl = Map.mapWithKey $ \k v -> convertValue v (k `elem` indxExcl)
    where
        convertValue :: JSON.Value -> Bool -> DS.Value
        convertValue val exclude = jsonToDS indxExcl val & vExcludeFromIndexes ?~ exclude


-- | Construct a 'JSON.Value' from any value
class ToJSON' a where
    mkJson :: a -> JSON.Value

instance ToJSON' Bool where
    mkJson = JSON.toJSON

instance ToJSON' Text where
    mkJson = JSON.toJSON

instance ToJSON' Int64 where
    mkJson = JSON.toJSON

instance ToJSON' Double where
    mkJson = JSON.toJSON

instance ToJSON' Sci.Scientific where
    mkJson = JSON.toJSON

instance ToJSON' DS.ValueNullValue where
    mkJson _ = JSON.Null

instance ToJSON' DS.ArrayValue where
    mkJson  = JSON.toJSON . fmap jsonFromDS . (^. avValues)

instance ToJSON' DS.Entity where
    mkJson e = JSON.Object $
        fmap jsonFromDS . view epAddtional .
        fromMaybe (entityProperties Map.empty) $
        (e ^. eProperties)

jsonFromDS :: DS.Value -> JSON.Value
jsonFromDS val =
    let parseRes =
                mkJson <$> (fromValue val :: Maybe Bool)
            <|> mkJson <$> (fromValue val :: Maybe DS.ValueNullValue)
            <|> mkJson <$> (fromValue val :: Maybe Text)
            <|> mkJson <$> (fromValue val :: Maybe DS.Entity)
            <|> mkJson <$> (fromValue val :: Maybe Sci.Scientific)
            <|> mkJson <$> (fromValue val :: Maybe DS.ArrayValue)

    in
        fromMaybe JSON.Null parseRes    -- (fail "DS.Value -> JSON parse fail")


decodeProperties :: forall a. JSON.FromJSON a => Tagged a EntityProps -> Either String a
decodeProperties propsT = decodeEntProps (unTagged propsT)

decodeEntProps :: forall a. JSON.FromJSON a => EntityProps -> Either String a
decodeEntProps props =
    case JSON.fromJSON . JSON.Object $ jsonFromDS <$> props of
        JSON.Success a -> Right a
        JSON.Error e        -> Left e

