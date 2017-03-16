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
import LibPrelude.Types
import LibPrelude
import qualified Data.HashMap.Strict        as Map
import qualified Network.Google.Datastore.Types   as DS



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





