{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Lookup where

import DB.Types
import DB.Error.Util
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
               -> Either DBException [ (e, EntityVersion) ]
parseLookupRes lookupResT =
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) $
        unTagged lookupResT ^. lrFound

parseEntityResult :: forall e.
                     IsEntity e
                  => Tagged e DS.EntityResult
                  -> Either DBException (e, EntityVersion)
parseEntityResult entResT = do
    dse <- resGetEntity entRes
    ent <- parseEntity dse
    ver <- entResVer entRes
    Right (ent, ver)
  where
    entRes = unTagged entResT

parseEntityResultKey :: forall k.
                     HasKeyPath k
                  => DS.EntityResult
                  -> Either DBException (k, EntityVersion)
parseEntityResultKey entRes = do
    key <- entGetKey =<< resGetEntity entRes
    (k,leftovers) <- parseElems (reverse $ key ^. DS.kPath)
    ver <- entResVer entRes
    if not (null leftovers) then
            Left $ InternalError $ ParseError $ "Key not fully parsed: " ++ show (k, leftovers, key)
        else
            Right (k, ver)

entGetKey ent =
    case ent ^. DS.eKey of
        Nothing -> Left $ InternalError $ ParseError "EntityResult: Missing key"
        Just k  -> Right k

resGetEntity :: DS.EntityResult -> Either DBException DS.Entity
resGetEntity entRes =
    case entRes ^. erEntity of
        Nothing -> Left $ InternalError $ ParseError "EntityResult: Missing entity"
        Just e  -> Right e

entResVer er =
    case er ^. DS.erVersion of
        Nothing -> Left $ InternalError $ ParseError "EntityResult: Empty version field"
        Just v  -> Right v
