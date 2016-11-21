{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Storable where


import DB.Model.Descendant
import DB.Model.Convert
import DB.Types
import DB.Util.Error
import Util
import Data.Either (lefts, rights)
import Data.Typeable

import qualified Network.Google.Datastore as DS


-- type Ident' = Either Int64 Text

data Storable a = Storable
    { selfKey       :: Ident a
    , properties    :: Maybe EntityProps
    , ancestorKeys  :: [DS.PathElement]
    }

instance Typeable a => Identifier (Storable a) where
    objectId (Storable k _ _) = objectId k


class Identifier a => IsStorable a where
    props       :: a -> Maybe EntityProps
    ancestors   :: a -> [DS.PathElement]
    -- Defaults
    props _ = Nothing
    ancestors _ = []

getStorable :: IsStorable a => a -> Storable a
getStorable a = Storable (getIdent a) (props a) (ancestors a)


instance Identifier a => IsStorable (Ident a)

instance Identifier a => IsStorable (Entity a) where
    props (Entity _ p) = Just p

instance (IsStorable a, IsStorable anc) => IsStorable (Descendant a anc) where
    props (Descendant d _) = props d
    ancestors (Descendant _ anc) = [ toPathElem (getIdent anc) ]


encodeKey :: IsStorable a => a -> DS.Key
encodeKey a = DS.key &
    DS.kPath .~ ( ancestors a ++ [ toPathElem (getIdent a) ] )

encodeEntity :: IsStorable a => a -> DS.Entity
encodeEntity a = DS.entity
    & DS.eKey ?~ encodeKey a
    & DS.eProperties .~ ( DS.entityProperties <$> props a )


parseKey :: forall a. Identifier a => DS.Key -> Either String (Ident a, [DS.PathElement])
parseKey a = case a ^. DS.kPath of
    (idn : anc) -> (,) <$> parsePathElem idn <*> return anc
    []          -> Left "No PathElements in Key"

parseEntity :: forall a. Identifier a => DS.Entity -> Either String (Storable a)
parseEntity e = parseRes >>= \(ident, ancestors) -> Right $ Storable ident properties ancestors
  where
    parseRes = parseKey =<< maybe (Left "No key in Entity") Right (e ^. DS.eKey)
    properties = ( ^. DS.epAddtional ) <$> e ^. DS.eProperties


parseLookupRes :: forall a. Identifier a => DS.LookupResponse -> Either String [(Storable a, EntityVersion)]
parseLookupRes lookupRes = fmapL ("LookupResponse: " ++) $
    if parseErrors `is` not . null then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map parse (lookupRes ^. lrFound)
    ver er = fromMaybe (internalError "EntityResult: Empty version field") (er ^. DS.erVersion)
    parse entRes = case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> parseEntity ent >>= \stor -> Right (stor :: Storable a, ver entRes)

infixr 8 `is`
is a b = b a

