{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ChanDB.Types.PubKey where

import LibPrelude
import Datastore                        (Identifier(..), HasProperties(..))
import ChanDB.Orphans                   ()
import           GHC.Generics
import qualified Data.Serialize         as Bin
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as JSON
import qualified Network.Haskoin.Crypto as HC
import qualified PaymentChannel         as Pay
import           PromissoryNote         (HasUUID(..))
import           Data.Monoid            ((<>))
import           Data.Typeable          (Typeable)


-- | A key that was previously sent to a client, and to which
--    funds may have been sent
data KeyAtIndex = KeyAtIndex
    { kaiIndex  :: Pay.KeyDeriveIndex
    , kaiPubKey :: Pay.RecvPubKey
    } deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

-- | The key we give out to clients when asked for a pubkey
newtype CurrentKey = CurrentKey
    { ckKey  :: KeyAtIndex
    } deriving (Eq, Show, Typeable, Generic, JSON.ToJSON, JSON.FromJSON, Bin.Serialize)

instance Identifier KeyAtIndex where
    objectId = objectId . kaiPubKey

instance Identifier CurrentKey where
    objectId = const $ Right "CurrentKey"

instance HasProperties KeyAtIndex where
    encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
    decodeProps = decodeJsonObject

instance HasProperties CurrentKey where
    encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
    decodeProps = decodeJsonObject
