{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}
module DB.Model.Types.Identifier where

import DB.Error
import LibPrelude
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import           Data.Typeable
import           Data.Void                        (Void)
import           Network.Google.Datastore       as DS


newtype Ident a = Ident
    { iId       :: Either Int64 Text
    } deriving (Eq, Typeable)

class Typeable a => Identifier a where
    objectId    :: a -> Either Int64 Text   -- ^ Object identifier. Valid integer range: 1-maxBound

instance Typeable a => Identifier (Ident a) where
    objectId = iId

parseIdent :: Typeable a => DS.PathElement -> Either DBException (Ident a)
parseIdent = parsePathElem'

getIdent :: forall a. Identifier a => a -> Ident a
getIdent a = Ident (objectId a)

ident :: Identifier a => a -> Ident b
ident a = Ident (objectId a)

identPathElem :: forall a. Identifier a => a -> DS.PathElement
identPathElem i = DS.pathElement &
        DS.peKind ?~ cs kindStr &
        either (DS.peId ?~) (DS.peName ?~) (objectId i)
  where
    kindStr = show (typeOf (undefined :: a))

castIdent :: Ident a -> Ident b
castIdent (Ident i) = Ident i


root :: Void
root = undefined

encodeHex :: Bin.Serialize a => a -> Text
encodeHex = cs . B16.encode . Bin.encode

instance Typeable a => Show (Ident a) where
    show (Ident e)
        | e == Left 0 = "/"
        | otherwise = typeStr ++ ":" ++ either show show e
      where typeStr = show (typeOf (undefined :: a))


-- ^ Get type identifier from an object instance.
objectType :: Typeable a => a -> Text
objectType = cs . show . typeOf


parsePathElem' :: forall a. Typeable a => DS.PathElement -> Either DBException (Ident a)
parsePathElem' pe = fmapL (catErr "PathElement: ") $
    parseKind >>= check >>
        -- If a number ID is present use that, else use name.
        either (const identNameE) Right identNumE
    where identNumE  = parseNum  >>= Right . Ident . Left
          identNameE = parseName >>= Right . Ident . Right
          -- Parse fields
          parseNum  = labelErr "Missing ID" $ pe ^. DS.peId
          parseName = labelErr "Missing name" $ pe ^. DS.peName
          -- Type/kind check
          labelErr e = maybe (Left $ InternalError $ ParseError (e :: String)) Right
          parseKind = labelErr "Missing kind" $ pe ^. DS.peKind
          kindStr = cs $ show (typeOf (undefined :: a))
          check k = if k == kindStr then Right k else
                    Left $ InternalError $ ParseError $ "Type mismatch. Found " ++ cs k ++ " expected " ++ cs kindStr
