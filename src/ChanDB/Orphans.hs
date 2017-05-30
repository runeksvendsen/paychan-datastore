module ChanDB.Orphans where

import Datastore
import LibPrelude
import qualified PaymentChannel     as Pay
import qualified PromissoryNote     as Note
import qualified Data.Aeson         as JSON
import qualified Data.Serialize     as Bin
import qualified Data.ByteString      as BS
import qualified Network.Haskoin.Crypto as HC
import           Data.Void          (Void)
import PromissoryNote               (PromissoryNote, HasUUID(..), UUID)
import PaymentChannel               (SendPubKey, RecvPubKey)
import           Data.Monoid            ((<>))


instance ToValue UUID where
    toValue = toValue . jsonStr . JSON.toJSON
        where jsonStr (JSON.String txt) = txt
              jsonStr n = error $ "UUID JSON should be String but it's: " ++ show n

-- Only identifies the root pubkey, not any derived key/key index inside it
instance HasUUID HC.XPubKey where
    serializeForID xPub =
           Bin.encode (HC.xPubDepth  xPub)
        <> Bin.encode (HC.xPubParent xPub)
        <> Bin.encode (HC.xPubChain  xPub)


type RecvPayChan = Pay.ServerPayChanX

-- Identifiers
instance Identifier Pay.SharedSecret where
    objectId = Right . encodeHex

instance Identifier UUID where
    objectId = Right . encodeHex

instance Identifier RecvPayChan where
    objectId = objectId . Pay.getSecret

instance Identifier PromissoryNote where
    objectId = objectId . Note.getUUID

instance Identifier (Pay.External Pay.ChildPub) where
    objectId = objectId . getUUID . Pay.pairPub

instance Identifier Pay.KeyDeriveIndex where
    -- "Left 0" is not a valid identifier, so we start at 1
    objectId kdi = Left $ fromIntegral kdi + 1

instance Identifier RecvPubKey where
    objectId = Right . encodeHex


instance ToValue Pay.KeyDeriveIndex where
    toValue kdi = toValue (fromIntegral kdi :: Int64)
instance FromValue Pay.KeyDeriveIndex where
    fromValue val = fromIntegral <$> (fromValue val :: Maybe Int64)

-- Entities
instance HasIdentifier RecvPayChan Pay.SharedSecret
instance HasIdentifier PromissoryNote UUID


instance HasProperties RecvPayChan where
    encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
    decodeProps = decodeJsonObject
    excludeKeys _ = ["pcsPaymentSignature"]

instance HasProperties PromissoryNote where
    encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
    decodeProps = decodeJsonObject
    excludeKeys _ = ["server_sig"]


