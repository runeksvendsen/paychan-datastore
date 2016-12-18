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

import Util
import DB.Model.Types.Identifier
import DB.Model.Types.KeyPath
import DB.Model.Convert.Properties

import qualified Network.Google.Datastore.Types as DS
import qualified Data.Aeson                     as JSON
import           Data.Void                        (Void)
import qualified Data.HashMap.Strict    as Map


data JustEntity e = JustEntity e
    deriving (Eq, Show)

data EntityAtKey e k = EntityAtKey e k
    deriving (Eq, Show)


class IsEntity e where
    entEncode :: e -> (DS.EntityProperties, [DS.PathElement])
    entDecode :: (DS.EntityProperties, [DS.PathElement]) -> Either String e

instance (HasProperties a, HasKeyPath k, Show k) => IsEntity (EntityAtKey a k) where
    entEncode (EntityAtKey e k) =
        ( fst $ entEncode (JustEntity e)
        , pathElems k
        )
    entDecode (props, peL) =
        decodeEntProps (props ^. DS.epAddtional) >>= \ent ->
        parseElems peL >>= \(k, leftovers) ->
        if not (null leftovers) then
            Left $ "Key not fully parsed: " ++ show (k, leftovers, peL)
        else
            Right (EntityAtKey ent k)

instance HasProperties a => IsEntity (JustEntity a) where
    entEncode (JustEntity e) =
        ( DS.entityProperties $ convertWithIndex (excludeKeys e) (encodeProps e)
        , []
        )
    entDecode (props, _) =
        decodeEntProps (props ^. DS.epAddtional) >>= \ent ->
            Right (JustEntity ent)


mkEntity :: IsEntity e => Maybe DS.PartitionId -> e -> DS.Entity
mkEntity partM e = DS.entity
    & DS.eKey ?~ (DS.key & DS.kPath .~ peL & DS.kPartitionId .~ partM)
    & DS.eProperties ?~ eProps
  where
    (eProps, peL) = entEncode e

parseEntity :: forall e.
               IsEntity e
            => DS.Entity
            -> Either String e
parseEntity ent = fmapL ("parseEntity:" ++) $ do
    peL <- getKeyPath
    eProps <- getEntProps
    entDecode (eProps, peL)
  where
    getEntProps = maybe
        (Left "No EntityProperties in Entity")
        Right (ent ^. DS.eProperties)
    getKeyPath =
        maybe (Left "No Key in Entity") Right (ent ^. DS.eKey) >>=
        (Right . (^. DS.kPath))



-- parseEntity :: IsEntity e => Maybe DS.PartitionId -> DS.Entity -> Either String e
-- parseEntity partM e =

-- DS.entity
--     & DS.eKey ?~ (DS.key & DS.kPath .~ peL & DS.kPartitionId .~ partM)
--     & DS.eProperties ?~ eProps
--   where
--     (eProps, peL) = entEncode e


class JSON.FromJSON a => HasProperties a where
    encodeProps :: a -> JSON.Object
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []


instance HasProperties Void
    where encodeProps _ = Map.empty
instance JSON.FromJSON Void where
    parseJSON = mempty




class (HasProperties a, Identifier i) => HasIdentifier a i | a -> i

getIdentifier :: forall a i. HasIdentifier a i => i -> Ident a
getIdentifier k = Ident $ objectId k
