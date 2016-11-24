{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Lookup where

import DB.Types
import DB.Util.Error
import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Util

import qualified Network.Google.Datastore as DS


mkLookup :: forall a anc.
            HasAncestor a anc
         => Ident anc
         -> Ident a
         -> Tagged a DS.LookupRequest
mkLookup anc a = Tagged $ lookupRequest & lrKeys .~
    [ unTagged (encodeKey anc a :: Tagged a DS.Key) ]

-- mkLookupDescendants :: forall a anc.
--             DeriveAncestor a anc
--          => Ident anc
--          -> Tagged a DS.LookupRequest
-- mkLookupDescendants anc = Tagged $ lookupRequest & lrKeys .~
--     [ unTagged (encodeAncestorKey anc :: Tagged anc DS.Key) ]


parseLookupRes :: forall a anc.
                  HasAncestor a anc
               => Tagged a DS.LookupResponse
               -> Either String [ ((a,Ident anc), EntityVersion) ]
parseLookupRes lookupRes = fmapL ("LookupResponse: " ++) $
    if parseErrors `is` not . null then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map parse (unTagged lookupRes ^. lrFound)
    ver er = fromMaybe (internalError "EntityResult: Empty version field") (er ^. DS.erVersion)
    parse entRes = case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> parseEntity (Tagged ent :: Tagged a DS.Entity) >>=
                    \(orig,anc) -> Right ((orig,anc), ver entRes)


infixr 8 `is`
is :: forall t t1. t -> (t -> t1) -> t1
is a b = b a
