module ChanDB.Orphans where

import ChanDB.Types
import qualified Data.Bitcoin.PaymentChannel    as Pay
import qualified PromissoryNote                 as Note
import qualified Data.Aeson as JSON
import           Data.Void                        (Void)
import PromissoryNote                             (PromissoryNote, StoredNote, UUID)


-- New stuff
-- instance IsKind SendPubKey
-- instance IsKind RecvPayChan
-- instance IsKind StoredNote
-- instance IsKind PromissoryNote

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

-- instance IsEntity RecvPayChan
-- instance IsEntity PromissoryNote
-- instance IsEntity StoredNote



instance HasProperties RecvPayChan
    where encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["pcsPaymentSignature"]

instance HasProperties PromissoryNote
    where encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]

instance HasProperties StoredNote
    where encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
          excludeKeys _ = ["server_sig"]

