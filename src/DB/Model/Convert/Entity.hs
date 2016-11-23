{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Entity
(
  module DB.Model.Convert.Entity
, module DB.Model.Convert.Identifier
)
where

import DB.Model.Types.Entity
import DB.Model.Convert.Identifier
import Types
import Util
import qualified Data.HashMap.Strict        as Map
import qualified Network.Google.Datastore   as DS

import Debug.Trace


encodeKey :: IsDescendant a k => k -> Tagged a DS.Key
encodeKey k = Tagged $ DS.key &
    DS.kPath .~ ( ("Key: " ++ show keyPath) `trace` keyPath )
        where keyPath = toPathElem (getIdent k) : descPathElem k

parseKey :: forall a k. IsDescendant a k => Tagged a DS.Key -> Either String (Ident a, [DS.PathElement])
parseKey a = case unTagged a ^. DS.kPath of
    (idn : anc) -> (,) <$> parsePathElem idn <*> return anc
    []          -> Left "No PathElements in Key"


encodeEntity :: forall a k. (IsDescendant a k) => a -> Tagged a DS.Entity
encodeEntity a = Tagged $ DS.entity
    & DS.eKey ?~ unTagged (encodeKey (getKey a) :: Tagged a DS.Key)
    & DS.eProperties ?~ DS.entityProperties props
        where props = jsonToDS (excludeKeys a) <$> properties a

parseEntity :: forall a k. (IsDescendant a k) => Tagged a DS.Entity -> Either String a
parseEntity e =
    getTagKey >>= parseKey >>= \(idnt, ancestors) -> entityFromJson $
        Entity (EntityKey idnt ancestors) props
  where
    getTagKey = Tagged <$> entKeyE :: Either String (Tagged a DS.Key)
    entKeyE = maybe (Left "No key in Entity") Right (unTagged e ^. DS.eKey)
    props = fromMaybe Map.empty $ ( ^. DS.epAddtional ) <$> unTagged e ^. DS.eProperties
