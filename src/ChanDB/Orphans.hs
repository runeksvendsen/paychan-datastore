module ChanDB.Orphans where

import ChanDB.Types
import qualified Data.Bitcoin.PaymentChannel    as Pay
import qualified PromissoryNote                 as Note
import qualified Data.Aeson as JSON
import           Data.Void                        (Void)
import PromissoryNote                             (PromissoryNote, StoredNote, UUID)


-- Identifiers
instance Identifier SendPubKey
    where objectId = Right . encodeHex

instance Identifier UUID
    where objectId = Right . encodeHex

instance Identifier RecvPayChan
    where objectId = objectId . Pay.getSenderPubKey

instance Identifier PromissoryNote
    where objectId = objectId . Note.getID

instance Identifier StoredNote
    where objectId = objectId . Note.getID


-- Entities
instance HasIdentifier RecvPayChan SendPubKey
instance HasIdentifier PromissoryNote UUID

instance IsEntity RecvPayChan
instance IsEntity PromissoryNote
instance IsEntity StoredNote

instance HasAncestor RecvPayChan Void
instance HasAncestor PromissoryNote RecvPayChan
instance HasAncestor StoredNote RecvPayChan


instance HasProperties RecvPayChan
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["pcsPaymentSignature"]

instance HasProperties PromissoryNote
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]

instance HasProperties StoredNote
    where properties = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]

