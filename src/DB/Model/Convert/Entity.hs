{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Entity
(
  module DB.Model.Convert.Entity
, module DB.Model.Convert.Identifier
)
where

import DB.Model.Types.Entity
import DB.Model.Convert.Identifier
import Util
import qualified Data.HashMap.Strict        as Map
import qualified Network.Google.Datastore   as DS

import Debug.Trace


encodeKey :: HasAncestors a => a -> DS.Key
encodeKey a = DS.key &
    DS.kPath .~ ( show keyPath `trace` keyPath )
        where keyPath = ancestors a ++ [ toPathElem (getIdent a) ]

parseKey :: forall a. Identifier a => DS.Key -> Either String (Ident a, [DS.PathElement])
parseKey a = case a ^. DS.kPath of
    (idn : anc) -> (,) <$> parsePathElem idn <*> return anc
    []          -> Left "No PathElements in Key"


encodeEntity :: (HasAncestors a, IsEntity a) => a -> DS.Entity
encodeEntity a = DS.entity
    & DS.eKey ?~ encodeKey a
    & DS.eProperties ?~ DS.entityProperties props
        where props = jsonToDS (excludeKeys a) <$> properties a

parseEntity :: forall a. Identifier a => DS.Entity -> Either String (Entity a)
parseEntity e = parseRes >>= \(ident, ancestors) -> Right $
    Entity (EntityKey ident ancestors) properties
  where
    parseRes = parseKey =<< maybe (Left "No key in Entity") Right (e ^. DS.eKey)
    properties = fromMaybe Map.empty $ ( ^. DS.epAddtional ) <$> e ^. DS.eProperties

