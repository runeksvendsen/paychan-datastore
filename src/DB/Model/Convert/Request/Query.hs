{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Query where

import DB.Model.Convert.Request.Lookup (parseEntityResult)
import DB.Types
import DB.Util.Error
import DB.Model.Convert.Entity
import Util
import Text.Printf

import qualified Network.Google.Datastore as DS


mkQueryReq :: forall a.
              IsEntity a
           => Maybe PartitionId
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


mkGQLQuery :: Text
           -> DS.GqlQuery
mkGQLQuery query =
    DS.gqlQuery &
        DS.gqQueryString ?~ query &
        gqAllowLiterals ?~ True

parseQueryRes :: forall a anc.
                 HasAncestor a anc
              => Tagged a DS.RunQueryResponse
              -> Either String [ ((a,Ident anc), EntityVersion) ]
parseQueryRes queryResT = fmapL ("RunQueryResponse: " ++) $
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) entRes
    entRes = maybe (internalError "QueryResultBatch must always be present.") (^. DS.qrbEntityResults)
            (unTagged queryResT ^. rBatch)


