{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module DB.Model.Types.Entity
(
--   IsEntity
-- , Entity(..), mkEntity, parseEntity'
  module DB.Model.Types.Entity
, module DB.Model.Types.Identifier
, module DB.Model.Convert.Properties
)
where

import DB.Model.Types.Identifier
import DB.Model.Convert.Properties
import DB.Model.Convert.Identifier

import           Types
import           Util
import           PromissoryNote                   (PromissoryNote, UUID)
import           PromissoryNote                 as Note
import qualified Data.Bitcoin.PaymentChannel    as Pay
import qualified Data.Aeson                     as JSON
import           Data.Typeable
import qualified Network.Google.Datastore as DS


type NativeEntity a = Tagged a DS.Entity

-- | Isomorphic to 'DS.Key' (contains: "kind" + id/name + optional ancesor path)
data EntityKey a = EntityKey
    { ident         :: Ident a
    , ancestors'    :: [DS.PathElement]
    }

-- | Isomorphic to 'DS.Entity' (contains: key + properties)
data Entity a = Entity
    { entKey        :: EntityKey a
    , entityProps'  :: EntityProps
    }

class (IsEntity a, HasDescendant k) => IsDescendant a k | a -> k where
    getKey           :: a -> k
    ancestorIdent    :: a -> Ident k
    entityIdent      :: a -> [DS.PathElement]

instance IsDescendant RecvPayChan SendPubKey where
    entityIdent = descPathElem . getKey
    ancestorIdent = getIdent . getKey
    getKey = Pay.getSenderPubKey


class Identifier k => HasDescendant k where
    descPathElem :: k -> [DS.PathElement]

instance HasDescendant SendPubKey where
    descPathElem _ = [ toPathElem (Ident $ Left 1 :: Ident RecvPayChan) ]


class (Identifier a, JSON.FromJSON a) => IsEntity a where
    properties  :: a -> JSON.Object
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []

instance IsEntity RecvPayChan
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["pcsPaymentSignature"]

instance IsEntity PromissoryNote
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]


-- mkChanKey :: forall a. Identifier a => a -> EntityKey RecvPayChan
-- mkChanKey sendPK =
--     EntityKey chanKey [ toPathElem $ getIdent sendPK ]
--         where chanKey = Ident $ Left 1 :: Ident RecvPayChan

entityFromJson :: forall a. IsEntity a => Entity a -> Either String a
entityFromJson (Entity _ props) =
    case JSON.fromJSON . JSON.Object $ jsonFromDS <$> props of
        JSON.Success a -> Right a
        JSON.Error e        -> Left e

