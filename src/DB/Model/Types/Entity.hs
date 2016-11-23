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

class Identifier a => HasKey a where
    ancestors :: a -> [DS.PathElement]
--     key       :: a -> Ident a
    ancestors _ = []


instance Identifier a => HasKey (EntityKey a) where
    ancestors (EntityKey _ anc) = anc
--     key (EntityKey i _) = getIdent i

instance Identifier a => HasKey (Entity a) where
    ancestors (Entity k _) = ancestors k

instance Typeable a => Identifier (EntityKey a)
    where objectId (EntityKey i _) = objectId i

instance Typeable a => Identifier (Entity a)
    where objectId (Entity k _) = objectId k


class (Identifier a, JSON.FromJSON a) => IsEntity a where
    properties  :: a -> JSON.Object
    excludeKeys :: a -> [NoIndexKey]
    excludeKeys _ = []

instance HasKey SendPubKey where
    ancestors a = [toPathElem $ getIdent a]
--     key _ = Ident $ Left 1 :: Ident RecvPayChan

instance HasKey RecvPayChan where
    ancestors a = [toPathElem $ getIdent $ Pay.getSenderPubKey a]

instance IsEntity RecvPayChan
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["pcsPaymentSignature"]

instance IsEntity PromissoryNote
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]


mkChanKey :: forall a. Identifier a => a -> EntityKey RecvPayChan
mkChanKey sendPK =
    EntityKey chanKey [ toPathElem $ getIdent sendPK ]
        where chanKey = Ident $ Left 1 :: Ident RecvPayChan

parseEntity' :: forall a. IsEntity a => Entity a -> Either String a
parseEntity' (Entity _ props) =
    case JSON.fromJSON . JSON.Object $ jsonFromDS <$> props of
        JSON.Success a -> Right a
        JSON.Error e        -> Left e

