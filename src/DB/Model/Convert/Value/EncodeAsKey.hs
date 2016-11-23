{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Value.EncodeAsKey where

import DB.Model.Convert.Value.Native (encode, decodeMaybe)
-- import Types
import Util
import qualified Network.Google.Datastore as DS
import Data.Typeable
import Data.Text (Text)
import qualified Data.Scientific        as Sci


-- | Encode any type in a 'DS.Key' as a string
class Typeable a => EncodeAsKey a where
    toStr   :: a -> Text
    fromStr :: Text -> Maybe a

toKey :: forall a. EncodeAsKey a => a -> DS.Key
toKey a = DS.key & DS.kPath .~
    [ DS.pathElement &
          DS.peKind ?~ cs typeStr &
          DS.peName ?~ toStr a
    ] where typeStr = show (typeOf (undefined :: a))

fromKey :: forall a. EncodeAsKey a => DS.Key -> Either String a
fromKey k = case k ^. DS.kPath of
    [elm] -> getKind elm >>= checkKind >> getName elm >>= maybe (Left "Decode fail") Right . fromStr
    x     -> Left $ "Not just 1 PathElement: " ++ show x
  where
    checkKind e = if e /= typeStr then Left "type mismatch" else Right ()
    getKind e = maybe (Left "Missing kind") Right $ e ^. DS.peKind
    getName e = maybe (Left "Missing name") Right $ e ^. DS.peName
    typeStr = cs $ show (typeOf (undefined :: a))

encodeAsKey :: EncodeAsKey a => a -> DS.Value
encodeAsKey = encode . toKey

decodeAsKey :: forall a. EncodeAsKey a => DS.Value -> Maybe a
decodeAsKey v = decodeMaybe v >>= either (const Nothing) Just . fromKey

instance EncodeAsKey Sci.Scientific where
    toStr = cs . show
    fromStr = Just . read . cs

