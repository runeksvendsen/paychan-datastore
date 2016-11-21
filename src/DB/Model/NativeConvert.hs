{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.NativeConvert where

import DB.Model.NativeValue (encode, decode)
import Util
import qualified Network.Google.Datastore as DS
import Data.Typeable
import Data.Text (Text)
import qualified Data.Scientific        as Sci


class Typeable a => ConvertNative a where
    toStr   :: a -> Text
    fromStr :: Text -> Maybe a

toKey :: forall a. ConvertNative a => a -> DS.Key
toKey a = DS.key & DS.kPath .~
    [ DS.pathElement &
          DS.peKind ?~ cs typeStr &
          DS.peName ?~ toStr a
    ] where typeStr = show (typeOf (undefined :: a))

fromKey :: forall a. ConvertNative a => DS.Key -> Either String a
fromKey k = case k ^. DS.kPath of
    [elm] -> getKind elm >>= checkKind >> getName elm >>= maybe (Left "Decode fail") Right . fromStr
    x     -> Left $ "Not just 1 PathElement: " ++ show x
  where
    checkKind e = if e /= typeStr then Left "type mismatch" else Right ()
    getKind e = maybe (Left "Missing kind") Right $ e ^. DS.peKind
    getName e = maybe (Left "Missing name") Right $ e ^. DS.peName
    typeStr = cs $ show (typeOf (undefined :: a))

encodeNative :: forall a. ConvertNative a => a -> DS.Value
encodeNative = encode . toKey

decodeNative :: forall a. ConvertNative a => DS.Value -> Maybe a
decodeNative = either (const Nothing) Just . fromKey . decode

instance ConvertNative Sci.Scientific where
    toStr = cs . show
    fromStr = Just . read . cs

