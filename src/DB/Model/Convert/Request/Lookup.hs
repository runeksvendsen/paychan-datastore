{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Lookup where

import DB.Types
import DB.Util.Error
import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Util

import qualified Network.Google.Datastore as DS


-- mkLookup :: HasAncestors k => k -> DS.LookupRequest
mkLookup :: HasKey a k => k -> DS.LookupRequest
mkLookup k = lookupRequest & lrKeys .~ [ encodeKey k ]

parseLookupRes :: forall a. Identifier a => DS.LookupResponse -> Either String [(Entity a, EntityVersion)]
parseLookupRes lookupRes = fmapL ("LookupResponse: " ++) $
    if parseErrors `is` not . null then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map parse (lookupRes ^. lrFound)
    ver er = fromMaybe (internalError "EntityResult: Empty version field") (er ^. DS.erVersion)
    parse entRes = case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> parseEntity ent >>= \stor -> Right (stor :: Entity a, ver entRes)


infixr 8 `is`
is a b = b a
