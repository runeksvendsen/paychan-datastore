{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
module DB.Model.Entity
(
  IsEntity
, Entity(..), mkEntity, parseEntity'
, module DB.Model.Identifier
, module DB.Model.EntityProps
)
where

import DB.Model.Identifier
import DB.Model.EntityProps

import           Types
import           Util
import           PromissoryNote                   (PromissoryNote, UUID)
import           PromissoryNote                 as Note
import qualified Data.Bitcoin.PaymentChannel    as Pay
import qualified Data.Aeson                     as JSON
import           Data.Typeable


-- | An e
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


data Entity a = Entity
    { ident         :: Ident a
    , entityProps   :: EntityProps
    }

instance Typeable a => Identifier (Entity a)
    where objectId (Entity i _) = objectId i

mkEntity :: IsEntity a => a -> Entity a
mkEntity a = Entity (getIdent a) $ jsonToDS (excludeKeys a) <$> properties a


parseEntity' :: forall a. IsEntity a => Entity a -> JSON.Result a
parseEntity' (Entity _ props) = JSON.fromJSON . JSON.Object $ jsonFromDS <$> props

