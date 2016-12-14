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

import DB.Model.Types.Identifier
import DB.Model.Convert.Properties

-- import           Types
import qualified Data.Aeson                     as JSON
import           Data.Void                        (Void)
import qualified Data.HashMap.Strict    as Map


class (HasProperties a, Identifier i) => HasIdentifier a i | a -> i where
    getIdentifier :: i -> Ident a
    getIdentifier k = Ident $ objectId k :: Ident a

class (HasProperties a, Identifier a) => IsEntity a

class (IsEntity a, IsEntity anc) => HasAncestor a anc


class (JSON.FromJSON a) => HasProperties a where
    properties  :: a -> JSON.Object
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []

instance HasProperties Void
    where properties _ = Map.empty
instance JSON.FromJSON Void where
    parseJSON = mempty

instance IsEntity Void
