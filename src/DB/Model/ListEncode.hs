{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.ListEncode where

import Util
import qualified Network.Google.Datastore as DS
import Data.Typeable
import Data.Text (Text)


class Typeable a => KeyEncode a where
    toStr   :: a -> Text
    fromStr :: Text -> Maybe a


encode :: forall a. KeyEncode a => a -> DS.Key
encode a = DS.key & DS.kPath .~
    [ DS.pathElement &
          DS.peKind ?~ cs typeStr &
          DS.peName ?~ toStr a
    ] where typeStr = show (typeOf (undefined :: a))

decode :: forall a. KeyEncode a => DS.Key -> Either String a
decode k = case k ^. DS.kPath of
    [elm] -> getKind elm >>= checkKind >> getName elm >>= maybe (Left "Decode fail") Right . fromStr
    x     -> Left $ "Not just 1 PathElement: " ++ show x
  where
    checkKind e = if e /= typeStr then Left "type mismatch" else Right ()
    getKind e = maybe (Left "Missing kind") Right $ e ^. DS.peKind
    getName e = maybe (Left "Missing name") Right $ e ^. DS.peName
    typeStr = cs $ show (typeOf (undefined :: a))

