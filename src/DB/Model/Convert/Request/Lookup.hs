{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Lookup where

import DB.Types
import DB.Util.Error
import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import LibPrelude

import qualified Network.Google.Datastore as DS


mkLookup :: HasKeyPath k
         => Maybe PartitionId
         -> k
         -> Tagged a DS.LookupRequest
mkLookup partM k = Tagged $ lookupRequest & lrKeys .~
    [ unTagged (encodeKeyPath partM k :: Tagged a DS.Key) ]


parseLookupRes :: forall e.
                  IsEntity e
               => Tagged e DS.LookupResponse
               -> Either String [ (e, EntityVersion) ]
parseLookupRes lookupResT = fmapL ("LookupResponse: " ++) $
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) $
        unTagged lookupResT ^. lrFound


parseEntityResult :: forall e.
                    IsEntity e
                  => Tagged e DS.EntityResult
                  -> Either String (e, EntityVersion)
parseEntityResult entResT = fmapL ("parseEntityResult: " ++) $
    case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> do
                e <- parseEntity ent
                ver <- entResVer entRes
                Right (e, ver)
    where
          entRes = unTagged entResT

parseEntityResultKey :: forall k.
                    HasKeyPath k
                  => DS.EntityResult
                  -> Either String (k, EntityVersion)
parseEntityResultKey entRes = fmapL ("parseEntityResultKey: " ++) $
    case entRes ^. erEntity of
            Nothing  -> Left "Empty entityResult"
            Just ent -> case ent ^. DS.eKey of
                Nothing  -> Left "Missing key"
                Just key -> do
                    (k,leftovers) <- parseElems (key ^. DS.kPath)
                    ver <- entResVer entRes
                    if not (null leftovers) then
                            Left $ "Key not fully parsed: " ++ show (k, leftovers, key)
                        else
                            Right (k, ver)


entResVer er = maybe (Left "EntityResult: Empty version field") Right (er ^. DS.erVersion)
