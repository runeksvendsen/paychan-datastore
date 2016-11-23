{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Lookup where

import DB.Types
import DB.Util.Error
import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Util

import qualified Network.Google.Datastore as DS


-- mkLookup :: HasAncestors k => k -> DS.LookupRequest
mkLookup :: forall a k. IsDescendant a k
         => k
         -> Tagged a DS.LookupRequest
mkLookup k = Tagged $ lookupRequest & lrKeys .~ [ unTagged (encodeKey k :: Tagged a DS.Key) ]

parseLookupRes :: forall a k. IsDescendant a k
               => Tagged a DS.LookupResponse
               -> Either String [(a, EntityVersion)]
parseLookupRes lookupRes = fmapL ("LookupResponse: " ++) $
    if parseErrors `is` not . null then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map parse (unTagged lookupRes ^. lrFound)
    ver er = fromMaybe (internalError "EntityResult: Empty version field") (er ^. DS.erVersion)
    parse entRes = case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> parseEntity (Tagged ent :: Tagged a DS.Entity) >>=
                    \orig -> Right (orig :: a, ver entRes)


infixr 8 `is`
is :: forall t t1. t -> (t -> t1) -> t1
is a b = b a
