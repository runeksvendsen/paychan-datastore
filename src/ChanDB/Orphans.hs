module ChanDB.Orphans where

import ChanDB.Types
import qualified PaymentChannel    as Pay
import qualified PromissoryNote                 as Note
import qualified Data.Aeson as JSON
import           Data.Void                        (Void)
import PromissoryNote                             (PromissoryNote, StoredNote, UUID)


instance ToValue UUID where
    toValue = toValue . jsonStr . JSON.toJSON
        where jsonStr (JSON.String txt) = txt
              jsonStr n = error $ "UUID JSON should be String but it's: " ++ show n

-- Identifiers
instance Identifier SendPubKey
    where objectId = Right . encodeHex

instance Identifier UUID
    where objectId = Right . encodeHex

instance Identifier RecvPayChan
    where objectId = objectId . Pay.getSendPubKey

instance Identifier PromissoryNote
    where objectId = objectId . Note.getUUID

instance Identifier StoredNote
    where objectId = objectId . Note.getUUID


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

