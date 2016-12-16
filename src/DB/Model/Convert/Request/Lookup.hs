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
         => Maybe PartitionId
         -> Ident anc
         -> Ident a
         -> Tagged a DS.LookupRequest
mkLookup partM anc a = Tagged $ lookupRequest & lrKeys .~
    [ unTagged (encodeKey partM anc a :: Tagged a DS.Key) ]


parseLookupRes :: forall a anc.
                  HasAncestor a anc
               => Tagged a DS.LookupResponse
               -> Either String [ ((a,Ident anc), EntityVersion) ]
parseLookupRes lookupResT = fmapL ("LookupResponse: " ++) $
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) $
        unTagged lookupResT ^. lrFound


parseEntityResult :: forall a anc.
                     HasAncestor a anc
                  => Tagged a DS.EntityResult
                  -> Either String ((a,Ident anc), EntityVersion)
parseEntityResult entResT = fmapL ("parseEntityResult: " ++) $
    case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> parseEntity (Tagged ent :: Tagged a DS.Entity) >>=
                    \(orig,anc) -> Right ((orig,anc), ver entRes)
    where ver er = fromMaybe (internalError "EntityResult: Empty version field") (er ^. DS.erVersion)
          entRes = unTagged entResT

parseEntityResultKey :: forall a anc.
                     HasAncestor a anc
                  => Tagged a DS.EntityResult
                  -> Either String (Ident a, Ident anc, EntityVersion)
parseEntityResultKey entResT = fmapL ("parseEntityResultKey: " ++) $
    case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> case ent ^. DS.eKey of
                Nothing  -> Left "Missing key"
                Just key -> parseKey (Tagged key :: Tagged a DS.Key) >>=
                    \(ancKey,entKey) -> Right (entKey, ancKey, ver entRes)
    where ver er = fromMaybe (internalError "Empty version field") (er ^. DS.erVersion)
          entRes = unTagged entResT
