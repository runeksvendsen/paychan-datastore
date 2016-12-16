{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Query where

import DB.Model.Convert.Request.Lookup (parseEntityResult, parseEntityResultKey)
import DB.Types
import DB.Util.Error
import DB.Model.Convert.Entity
import Util
import Text.Printf

import qualified Network.Google.Datastore as DS



mkGQLQuery :: Text
           -> DS.GqlQuery
mkGQLQuery query =
    DS.gqlQuery &
        DS.gqQueryString ?~ query &
        gqAllowLiterals ?~ True

mkQueryReq :: Maybe PartitionId
           -> Text
           -> Tagged a DS.RunQueryRequest
mkQueryReq partM query = Tagged $
    DS.runQueryRequest
        & rqrPartitionId .~ partM
        & rqrGqlQuery ?~ mkGQLQuery query


mkAncQueryReq :: forall a anc.
              HasAncestor a anc
           => Maybe PartitionId
           -> Ident anc
           -> Text
           -> Tagged a DS.RunQueryRequest
mkAncQueryReq partM anc query =
    mkQueryReq partM completeQueryStr
        where completeQueryStr = query <> cs (mkAncestorStr anc)
              mkAncestorStr :: Ident anc -> String
              mkAncestorStr a = printf " AND __key__ HAS ANCESTOR %s" (gqlKeyString a)


parseQueryRes :: forall a anc.
                 HasAncestor a anc
              => Tagged (a,anc) DS.RunQueryResponse
              -> Either String [ ((a, Ident anc), EntityVersion) ]
parseQueryRes queryResT = fmapL ("parseQueryRes: " ++) $
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) entRes
    entRes = maybe (internalError "QueryResultBatch must always be present.") (^. DS.qrbEntityResults)
            (unTagged queryResT ^. rBatch)

parseQueryResKeys :: forall a anc.
                 HasAncestor a anc
              => Tagged (a,anc) DS.RunQueryResponse
              -> Either String [ (Ident a, Ident anc, EntityVersion) ]
parseQueryResKeys queryResT = fmapL ("parseQueryResKeys: " ++) $
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResultKey . Tagged) entRes
    entRes = maybe (internalError "QueryResultBatch must always be present.") (^. DS.qrbEntityResults)
            (unTagged queryResT ^. rBatch)

