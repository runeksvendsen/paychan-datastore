{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Tx.Lookup where

import           Util
import           DB.Types
import           Network.Google.Datastore

import           Data.Maybe                (listToMaybe, fromMaybe)
import           Network.Google as Google


txLookup :: ( MonadGoogle '[AuthDatastore] m
            , HasScope    '[AuthDatastore] ProjectsLookup
            , IsEntity a k)
           => ProjectId
           -> k
           -> m (Maybe a)
txLookup projectId tx key =
    let lookupReq = lookupRequest &
                lrKeys .~ [mkKey projectId sendPK]
    in Google.send (projectsLookup lookupReq projectId)

parseLookupRes :: IsEntity a k => LookupResponse -> Maybe (a, EntityVersion)
parseLookupRes lookupRes =
    listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
        case res ^. erEntity of
            Nothing  -> internalError "LookupResponse: Empty entityResult"
            Just ent -> Just
                ( decodeFromPropertyOrFail $
                    fromMaybe (internalError "LookupResponse: No properties in entity")
                    (ent ^. eProperties)
                , fromMaybe (internalError $
                         "CloudStore API BUG. LookupResponse: " ++
                         "Entity version not present")
                    (res ^. erVersion)
                )
