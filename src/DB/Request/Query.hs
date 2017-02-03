{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import DB.Model.Types.Query
import DB.Request.Util
import DB.Types
import DB.Model.Convert
import LibPrelude


queryBatchEntities ::
    ( DatastoreM m
    , IsQuery q
    , HasProperties a
    , HasScope '[AuthDatastore] ProjectsRunQuery
    )
    => Maybe NamespaceId
    -> q
    -> m [JustEntity a]
queryBatchEntities ns q = do
    res <- map parseEntity <$> queryBatchEnts ns q
    if null (lefts res) then
            return (rights res)
        else
            throw . InternalError $ "streamQueryBatch parse error: " ++ show (lefts res)

queryBatchEnts ::
    ( DatastoreM m
    , IsQuery q
    , HasScope '[AuthDatastore] ProjectsRunQuery
    )
    => Maybe NamespaceId
    -> q
    -> m [Entity]
queryBatchEnts nsM query = do
    res <- runQueryReq Nothing =<< mkQueryReq nsM query
    case parseRes res of
        Left e -> throw . InternalError $ "streamQueryBatch: " ++ e
        Right (queryEnts, Nothing)   -> return queryEnts
        Right (queryEnts, Just curs) -> loop queryEnts curs
  where
    parseRes :: RunQueryResponse -> Either String ([Entity], Maybe Cursor)
    parseRes = parseQueryBatchRes >=> parseBatchResults
    loop queryEnts curs =
        queryBatchEnts nsM (StartAtCursor curs query) >>=
        \entLstAccum -> return (queryEnts ++ entLstAccum)


runQueryReq ::
            ( DatastoreM m
            , HasScope '[AuthDatastore] ProjectsRunQuery
            )
         => Maybe TxId
         -> Tagged a RunQueryRequest
         -> m RunQueryResponse
runQueryReq txM reqT =
    sendReq (projectsRunQuery txReq)
  where
    txReq = maybe req (`atomically` req) txM
    req = unTagged reqT

entityQuery :: forall q e.
           ( IsQuery q
           , IsEntity e
           , HasScope '[AuthDatastore] ProjectsRunQuery
           )
          => Maybe NamespaceId
          -> Maybe TxId
          -> q
          -> Datastore ( Either String [(e, EntityVersion)] )
entityQuery nsM txM q = do
    req <- mkQueryReq nsM q
    parseQueryRes <$> runQueryReq txM req

keysOnlyQuery :: forall q a.
               ( IsQuery q
               , Typeable a
               , HasScope '[AuthDatastore] ProjectsRunQuery
               )
              => Maybe NamespaceId
              -> q
              -> Datastore ( Either String [(EntityKey a, EntityVersion)] )
keysOnlyQuery nsM q = do
    req <- mkQueryReq nsM q
    parseQueryResKeys <$> runQueryReq Nothing req




