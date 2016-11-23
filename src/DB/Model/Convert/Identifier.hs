{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module DB.Model.Convert.Identifier where

import DB.Model.Types.Identifier
-- import DB.Types
import Util

import qualified Network.Google.Datastore as DS
import           Data.Typeable


-- parsePathElemId :: Identifier a => DS.PathElement -> Either String (Ident a)
-- parsePathElemId a = parsePathElem a >> iId >>= \id -> Right $ Ident (Left id)
--     where

parsePathElem :: forall a. Identifier a => DS.PathElement -> Either String (Ident a)
parsePathElem a = fmapL ("PathElement: " ++) $
    parseKind >>= check >>
        -- If a number ID is present, use that, else use name.
        either (const identNameE) Right identNumE
    where identNumE  = parseNum  >>= Right . Ident . Left
          identNameE = parseName >>= Right . Ident . Right
          -- Parse fields
          parseNum  = maybe (Left "Missing ID") Right $ a ^. DS.peId
          parseName = maybe (Left "Missing name") Right $ a ^. DS.peName
          -- Type/kind check
          parseKind = maybe (Left "Missing kind") Right $ a ^. DS.peKind
          kindStr = cs $ show (typeOf (undefined :: a))
          check k = if k == kindStr then Right k else
                    Left $ "Type mismatch. Found " ++ cs k ++ " expected " ++ cs kindStr

toPathElem :: forall a. Typeable a => Ident a -> DS.PathElement
toPathElem (Ident iId) =
    DS.pathElement &
        DS.peKind ?~ cs kindStr &
        either (DS.peId ?~) (DS.peName ?~) iId
  where
    kindStr = show (typeOf (undefined :: a))

