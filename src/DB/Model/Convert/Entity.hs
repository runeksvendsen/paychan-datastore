{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Entity
(
  module DB.Model.Convert.Entity
, module DB.Model.Convert.Identifier
, module DB.Model.Types.Entity
)
where

import DB.Model.Types.KeyPath
import DB.Model.Types.Entity
import DB.Model.Convert.Identifier
import Types
import Util
import qualified Data.HashMap.Strict        as Map
import qualified Network.Google.Datastore.Types   as DS


-- encodeKey :: forall a anc. HasAncestor a anc
--           => Maybe DS.PartitionId
--           -> Ident anc
--           -> Ident a
--           -> Tagged a DS.Key
-- encodeKey partIdM anc a = Tagged $ unTagged bareKey & DS.kPartitionId .~ partIdM
--     where bareKey = identKey anc </> identKey a


-- parseKey :: forall a k. HasKeyPath k => Tagged a DS.Key -> Either String k
-- parseKey a = case unTagged a ^. DS.kPath of
--     [idn]       -> (,) (Ident $ Left 0) <$> parsePathElem (Tagged idn)
--     [anc , idn] -> (,) <$> parsePathElem (Tagged anc) <*> parsePathElem (Tagged idn)
--     path        -> Left $ "parseKey: Unexpected PathElements: " ++ show path


encodeKeyPath :: HasKeyPath k
          => Maybe DS.PartitionId
          -> k
          -> Tagged a DS.Key
encodeKeyPath partIdM k = Tagged $ DS.key
    & DS.kPartitionId .~ partIdM
    & DS.kPath .~ reverse (pathElems k)

-- encodeEntity :: forall a k.
--                 IsEntity a k
--              => Maybe DS.PartitionId
--              -> EntityWithAnc a k
--              -> Tagged a DS.Entity
-- encodeEntity partIdM ent = Tagged $ DS.entity
--     & DS.eKey ?~ unTagged (encodeKeyPath partIdM (a <//> anc))
--     & DS.eProperties ?~ DS.entityProperties props
--         where props = jsonToDS (excludeKeys a) <$> encodeProps a
--





