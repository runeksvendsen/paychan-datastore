{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Query where

import DB.Model.Convert.Request.Lookup (parseEntityResult)
import DB.Types
import DB.Util.Error
import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Util
import Text.Printf

import qualified Network.Google.Datastore as DS

import Debug.Trace


mkQueryReq :: forall a anc.
              HasAncestor a anc
           => Maybe (Ident anc)
           -> Text
           -> Tagged a DS.RunQueryRequest
mkQueryReq ancM query = Tagged $ DS.runQueryRequest & rqrGqlQuery ?~
    (DS.gqlQuery & DS.gqQueryString ?~ show completeQuery `trace` completeQuery)
        where completeQuery = query <> ancestorQueryStr
              ancestorQueryStr = cs $ maybe "" mkAncestorStr ancM
              mkAncestorStr :: Ident anc -> String
              mkAncestorStr anc = printf " AND __key__ HAS ANCESTOR %s" (gqlKeyString anc)

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


