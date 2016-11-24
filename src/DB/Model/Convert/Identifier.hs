{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module DB.Model.Convert.Identifier where

import DB.Model.Types.Identifier
import Types
import Util

import qualified Network.Google.Datastore as DS
import           Data.Typeable


-- parsePathElemId :: Identifier a => DS.PathElement -> Either String (Ident a)
-- parsePathElemId a = parsePathElem a >> iId >>= \id -> Right $ Ident (Left id)
--     where

-- identKey Root </> identKey (getIdentifier sendPK)

identKey :: forall a. Identifier a => Ident a -> Tagged a DS.Key
identKey idn@(Ident i)
    | objectId i == Left 0 = Tagged mempty
    | otherwise = Tagged $ DS.key & DS.kPath .~ [ unTagged (toPathElem idn :: Tagged a DS.PathElement) ]

instance Monoid DS.Key where
    mempty = DS.key
    key1 `mappend` key2 =
        key2 & DS.kPath .~
            (key1 ^. DS.kPath) ++
            (key2 ^. DS.kPath)

(</>) ::
         Tagged a DS.Key
      -> Tagged b DS.Key
      -> Tagged b DS.Key
key1 </> key2 = Tagged $ unTagged key1 <> unTagged key2

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



