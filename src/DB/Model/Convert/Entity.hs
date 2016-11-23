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

-- import Debug.Trace


encodeKey :: IsDescendant a k => k -> Tagged a DS.Key
encodeKey k = Tagged $ DS.key &
    DS.kPath .~ keyPath
        where keyPath = toPathElem (getIdent k) : descPathElem k

parseKey :: forall a k. IsDescendant a k => Tagged a DS.Key -> Either String (Ident k, [DS.PathElement])
parseKey a = case unTagged a ^. DS.kPath of
    (idn : anc) -> (,) <$> parsePathElem idn <*> return anc
    []          -> Left "No PathElements in Key"


encodeEntity :: forall a k. (IsDescendant a k) => a -> Tagged a DS.Entity
encodeEntity a = Tagged $ DS.entity
    & DS.eKey ?~ unTagged (encodeKey (getKey a) :: Tagged a DS.Key)
    & DS.eProperties ?~ DS.entityProperties props
        where props = jsonToDS (excludeKeys a) <$> properties a

parseEntity :: forall a k. (IsDescendant a k) => Tagged a DS.Entity -> Either String a
parseEntity eT =
    getNativeKey >>= \k -> entityFromJson (Tagged props) >>= \a ->   --  parseKey >>= \(idnt, peL) ->
            if parseKey k == parseKey (reEncodeKey a) then -- reEncodeKey a == Right (idnt, peL) then
                Right a
            else
                Left $ "BUG: Native Entity's key does not match parsed entity's re-encoded key" ++
                    show (k, reEncodeKey a)
  where
    getNativeKey = Tagged <$> nativeKeyE :: Either String (Tagged a DS.Key)
    nativeKeyE = maybe (Left "No key in Entity") Right (unTagged eT ^. DS.eKey)
    props = fromMaybe Map.empty $ ( ^. DS.epAddtional ) <$> unTagged eT ^. DS.eProperties
    reEncodeKey a = encodeKey (getKey a) :: Tagged a DS.Key




