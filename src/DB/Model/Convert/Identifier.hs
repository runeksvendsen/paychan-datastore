{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module DB.Model.Convert.Identifier
(
  module DB.Model.Convert.Identifier
, module DB.Model.Types.Identifier
)

where

import DB.Model.Types.Identifier
import LibPrelude.Types
import LibPrelude

import qualified Network.Google.Datastore as DS
import           Data.Typeable
import Text.Printf (printf)


-- | Used when referencing ancestor keys in a GQL query string. Example string:
-- @Key(`ReceiverPaymentChannelI Metadata`, '0336a5e95a8e2e8928836e0e6c45ec39dd8cc292565636e61fa78140da97285361')@
gqlKeyString :: forall a. Identifier a => Ident a -> Text
gqlKeyString i =
    cs (printf "Key(`%s`, %s)" kind identifier :: String)
    where identifier = either show show (iId i)
          kind = show (typeOf (undefined :: a))

-- | Used when referencing kinds in a SELECT query.
gqlSelectString :: forall a. Identifier a => Text -> Ident a -> Text
gqlSelectString selection _ =
    cs (printf "SELECT %s FROM `%s`" selection kindStr :: String)
    where kindStr = show (typeOf (undefined :: a))

-- | Used when referencing kinds in a 'Query'.
gqlKind :: forall a. Typeable a => a -> DS.KindExpression
gqlKind _ =
    DS.kindExpression & DS.keName ?~ cs kindStr
        where kindStr = show (typeOf (undefined :: a))


identKey :: forall a. Typeable a => Ident a -> Tagged a DS.Key
identKey idn@(Ident i)
    | i == Left 0 = Tagged mempty
    | otherwise = Tagged $ DS.key & DS.kPath .~ [ unTagged (toPathElem idn :: Tagged a DS.PathElement) ]

instance Monoid DS.Key where
    mempty = DS.key
    key1 `mappend` key2 =
        key2 & DS.kPath .~
            (key1 ^. DS.kPath) ++
            (key2 ^. DS.kPath)

mkDSKey :: Tagged i DS.PathElement -> Tagged i DS.Key
mkDSKey peT = Tagged $ DS.key & DS.kPath .~ [ unTagged peT ]


toPathElem :: forall a. Typeable a => Ident a -> Tagged a DS.PathElement
toPathElem (Ident i) = Tagged $
    DS.pathElement &
        DS.peKind ?~ cs kindStr &
        either (DS.peId ?~) (DS.peName ?~) i
  where
    kindStr = show (typeOf (undefined :: a))


parsePathElem :: forall a. Identifier a => Tagged a DS.PathElement -> Either String (Ident a)
parsePathElem peT = fmapL ("PathElement: " ++) $
    parseKind >>= check >>
        -- If a number ID is present, use that, else use name.
        either (const identNameE) Right identNumE
    where identNumE  = parseNum  >>= Right . Ident . Left
          identNameE = parseName >>= Right . Ident . Right
          -- Parse fields
          parseNum  = labelErr "Missing ID" $ unTagged peT ^. DS.peId
          parseName = labelErr "Missing name" $ unTagged peT ^. DS.peName
          -- Type/kind check
          labelErr e = maybe (Left (e :: String)) Right
          parseKind = labelErr "Missing kind" $ unTagged peT ^. DS.peKind
          kindStr = cs $ show (typeOf (undefined :: a))
          check k = if k == kindStr then Right k else
                    Left $ "Type mismatch. Found " ++ cs k ++ " expected " ++ cs kindStr



