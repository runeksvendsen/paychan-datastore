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

import           Types
import           PromissoryNote                   (PromissoryNote, UUID)
import qualified Data.Aeson                     as JSON
import qualified Network.Google.Datastore       as DS
import           Data.Void                        (Void)
import qualified Data.HashMap.Strict    as Map


class (HasProperties a, Identifier i) => HasIdentifier a i | a -> i where
    getIdentifier :: i -> Ident a
    getIdentifier k = Ident $ objectId (getIdent k) :: Ident a

instance HasIdentifier RecvPayChan SendPubKey
instance HasIdentifier PromissoryNote UUID


class (HasProperties a, Identifier a) => IsEntity a
instance IsEntity RecvPayChan
instance IsEntity PromissoryNote
instance IsEntity Void


class (IsEntity a, IsEntity anc) => HasAncestor a anc

instance HasAncestor RecvPayChan Void
instance HasAncestor PromissoryNote RecvPayChan


class (JSON.FromJSON a) => HasProperties a where
    properties  :: a -> JSON.Object
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []

instance HasProperties Void
    where properties _ = Map.empty
instance JSON.FromJSON Void where
    parseJSON = mempty

instance HasProperties RecvPayChan
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["pcsPaymentSignature"]

instance HasProperties PromissoryNote
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]



