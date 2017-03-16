{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module DB.Model.Types.Entity
(
  module DB.Model.Types.Entity
, module DB.Model.Types.Identifier
, module DB.Model.Convert.Properties
)
where

import LibPrelude
import DB.Error
import DB.Model.Types
import DB.Model.Types.Identifier
import DB.Model.Types.KeyPath
import DB.Model.Convert.Properties

import qualified Network.Google.Datastore.Types as DS
import qualified Data.Aeson                     as JSON
import           Data.Void                        (Void)
import qualified Data.HashMap.Strict    as Map


data JustEntity e = JustEntity { justEntity :: e }
    deriving (Eq, Show)

data EntityWithAnc e k = EntityWithAnc e k
    deriving (Eq, Show)


class IsEntity e where
    -- | Root key is right-most list element: @/a/b/c -> [c,b,a]@
    entKeyPath :: e -> [DS.PathElement]
    entProps   :: e -> DS.EntityProperties
    entDecode  :: (DS.EntityProperties, [DS.PathElement]) -> Either DBException e

instance HasProperties a => IsEntity (JustEntity a) where
    entProps (JustEntity e) =
        DS.entityProperties $ convertWithIndex (excludeKeys e) (encodeProps e)
    entKeyPath (JustEntity _) = []
    entDecode (props, _) = fmapL (UserError . ParseFail) $
        decodeProps (fmap jsonFromDS $ props ^. DS.epAddtional) >>= \ent ->
            Right (JustEntity ent)

instance (Identifier a, HasProperties a, HasKeyPath k, Show k) => IsEntity (EntityWithAnc a k) where
    entProps (EntityWithAnc e _) = entProps (JustEntity e)
    entKeyPath (EntityWithAnc e k) = identPathElem e : pathElems k
    entDecode (props, peL) = let decodeProps' = fmapL (UserError . ParseFail) . decodeProps in
        decodeProps' (fmap jsonFromDS $ props ^. DS.epAddtional) >>= \ent ->
        -- First PathElement is entity key
        parseElems (tail peL) >>= \(k, leftovers) ->
        if not (null leftovers) then
            Left $ InternalError $ ParseError $ "Key not fully parsed: " ++ show (k, leftovers, peL)
        else
            Right (EntityWithAnc ent k)

instance IsEntity (EntityKey a) where
    entProps (EntityKey _) = DS.entityProperties Map.empty
    entKeyPath (EntityKey peL) = peL
    entDecode (_, peL) = Right (EntityKey peL)

mkEntity :: IsEntity e => Maybe DS.PartitionId -> e -> DS.Entity
mkEntity partM e = DS.entity
    & DS.eKey ?~ (DS.key
        & DS.kPath .~ reverse (entKeyPath e)
        & DS.kPartitionId .~ partM)
    & DS.eProperties ?~ entProps e

parseEntity :: forall e.
               IsEntity e
            => DS.Entity
            -> Either DBException e
parseEntity ent = fmapL (catErr "parseEntity:") $ do
    keyPath <- getKeyPath
    eProps  <- getEntProps
    entDecode (eProps, reverse keyPath)
  where
    getEntProps = maybe
        (Left $ InternalError $ ParseError "No EntityProperties in Entity")
        Right (ent ^. DS.eProperties)
    getKeyPath =
        maybe (Left $ InternalError $ ParseError "No Key in Entity") Right (ent ^. DS.eKey) >>=
        (Right . (^. DS.kPath))



class HasProperties a where
    encodeProps :: a -> JSON.Object
    decodeProps :: JSON.Object -> Either String a
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []


instance HasProperties Void where
    encodeProps = const Map.empty
    decodeProps = error "Void decodeProps"
instance JSON.FromJSON Void where
    parseJSON = mempty




class (HasProperties a, Identifier i) => HasIdentifier a i | a -> i

getIdentifier :: forall a i. HasIdentifier a i => i -> Ident a
getIdentifier k = Ident $ objectId k
