{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import DB.Model.Types.Query
import DB.Request.Util
import DB.Types
import DB.Model.Convert
import Util



streamQueryBatch ::
    ( DatastoreM m
    , IsQuery q
    , HasScope '[AuthDatastore] ProjectsRunQuery
    )
    => Maybe NamespaceId
    -> q
    -> m [Entity]
streamQueryBatch nsM query = do
    res <- runQueryReq Nothing =<< mkQueryReq nsM query
    case parseRes (unTagged res) of
        Left e -> throw . InternalError $ "openChannelKeys: " ++ e
        Right (queryEnts, Nothing)   -> return queryEnts
        Right (queryEnts, Just curs) -> loop queryEnts curs
  where
    parseRes :: RunQueryResponse -> Either String ([Entity], Maybe Cursor)
    parseRes = parseQueryBatchRes >=> parseBatchResults
    loop queryEnts curs =
        streamQueryBatch nsM (StartAtCursor curs query) >>=
        \entLstAccum -> return (queryEnts ++ entLstAccum)

openChannelsQuery = undefined


runQueryReq ::
            ( DatastoreM m
            , HasScope '[AuthDatastore] ProjectsRunQuery
            )
         => Maybe TxId
         -> Tagged a RunQueryRequest
         -> m ( Tagged (a,anc) RunQueryResponse )
runQueryReq txM req =
    Tagged <$> sendReq' (projectsRunQuery txReq)
  where
    txReq = maybe uReq (`atomically` uReq) txM
    uReq = unTagged req


txQuery :: forall q a anc.
           ( IsQuery q
           , HasAncestor a anc
           , HasScope '[AuthDatastore] ProjectsRunQuery )
          => Maybe NamespaceId
          -> TxId
          -> q
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
txQuery nsM tx q = do
    req <- mkQueryReq nsM q
    parseQueryRes <$> runQueryReq (Just tx) req

entityQuery :: forall q a anc.
           ( IsQuery q
           , HasAncestor a anc
           , HasScope '[AuthDatastore] ProjectsRunQuery )
          => Maybe NamespaceId
          -> q
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
entityQuery nsM q = do
    req <- mkQueryReq nsM q
    parseQueryRes <$> runQueryReq Nothing req


keysOnlyQuery :: forall q a anc.
               ( IsQuery q
               , HasAncestor a anc
               , HasScope '[AuthDatastore] ProjectsRunQuery )
              => Maybe NamespaceId
              -> q
              -> Datastore ( Either String [ (Ident a, Ident anc) ] )
keysOnlyQuery nsM q = do
    req <- mkQueryReq nsM q
    identE <- parseQueryResKeys <$> runQueryReq Nothing req
    let rmVer (i,a,_) = (i,a)
    return $ fmap (map rmVer) identE



