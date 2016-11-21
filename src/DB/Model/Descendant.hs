{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Descendant
(
  module DB.Model.Descendant
, module DB.Model.Entity
)
where

import DB.Model.Entity
-- import DB.Model.Convert

import Types
import Util
import PromissoryNote
import qualified Network.Google.Datastore as DS
import Data.Typeable
import qualified Data.Bitcoin.PaymentChannel    as Pay


data Descendant d a = Descendant
    { desc      :: d    -- PromissoryNote
    , ancestor  :: a    -- SendPubKey
    }

class (Identifier d, Identifier a) => HasAncestor d a


instance (Identifier d, Identifier a) => Identifier (Descendant d a) where
    objectId (Descendant d _) = objectId d


test = Descendant (undefined :: RecvPayChan) (Pay.MkSendPubKey undefined)


 -- where
--     path :: d -> a -> [Ident]
--     path d a = [ getIdent a, getIdent d ]

--     lookupDescendants :: a -> m [d]
--     lookupDescendants a = error $
--         "SELECT " <> show (typeOf (undefined :: d)) <> " WHERE " <>
--             "__key__" <> " HAS ANCESTOR " <> showIdent a

-- instance (IsEntity d, HasIdentifier a) => HasAncestor d a where
--
-- lookupAncestor :: HasAncestor d a => d -> m (Maybe a)
-- lookupAncestor  = undefined
--
-- mkKey :: HasAncestor d a  => d -> a -> DS.Key
-- mkKey d a = DS.key & DS.kPath .~
--     [ idPathElem a, idPathElem d ]
--
-- instance HasAncestor PromissoryNote SendPubKey



