{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Query where

import DB.Model.Types.Query
import DB.Model.Convert.Request.Lookup (parseEntityResult, parseEntityResultKey)
import DB.Types
import DB.Error.Util
import DB.Model.Convert.Entity
import LibPrelude
import Text.Printf

import qualified Network.Google.Datastore as DS



mkQueryReq :: ( DatastoreM m
              , IsQuery q )
           => Maybe NamespaceId
           -> q
           -> m (Tagged a DS.RunQueryRequest)
mkQueryReq nsM q = do
    projId <- getPid
    let projPartId = return $ partitionId & piProjectId ?~ projId
    partId <- maybe projPartId mkPartitionId nsM
    return $ Tagged $ mkReq partId
  where
    mkReq pid = DS.runQueryRequest
        & rqrPartitionId .~ Just pid
        & rqrQuery ?~ mkQuery pid q



parseQueryRes :: IsEntity e
              => DS.RunQueryResponse
              -> Either DBException [ (e, EntityVersion) ]
parseQueryRes queryRes =
    if not (null parseErrors) then Left $ head parseErrors else Right $ rights parseRes
  where
    parseErrors = lefts parseRes
    parseRes = map (parseEntityResult . Tagged) entRes
    entRes = maybe (internalError $ Bug "QueryResultBatch must always be present.")
        (^. DS.qrbEntityResults)
        (queryRes ^. rBatch)

parseQueryResKeys :: HasKeyPath k
              => DS.RunQueryResponse
              -> Either DBException [ (k, EntityVersion) ]
parseQueryResKeys queryRes =
    entRes queryRes >>= parseEitherRes . map parseEntityResultKey
  where
    entRes er = fmap (^. DS.qrbEntityResults) (parseQueryBatchRes er)


parseQueryBatchRes :: RunQueryResponse -> Either DBException QueryResultBatch
parseQueryBatchRes  qr = maybe
    (Left $ InternalError $ ParseError "No QueryResultBatch in RunQueryResponse")
    Right
    (qr ^. rBatch)

parseBatchResults :: QueryResultBatch
                  -> Either DBException ([DS.Entity], Maybe Cursor)
parseBatchResults qrb = fmapL (catErr "QueryResultBatch: ") $
    case qrb ^. qrbMoreResults of
        Just mr ->
            parseBatchEntities qrb >>=
            \entRes -> maybeNextCursor qrb mr >>=
            \cursM -> Right (entRes, cursM)
        Nothing -> Left $ InternalError $ ParseError "Missing QueryResultBatchMoreResults"

parseBatchEntities :: QueryResultBatch -> Either DBException [DS.Entity]
parseBatchEntities qrb = fmapL (catErr "parseBatchEntities: ") $
    parseEitherRes (entitiesFromResult qrb)
  where
    entityFromResult er = maybe (Left $ InternalError $ ParseError "No Entity in EntityResult") Right (er ^. erEntity)
    entitiesFromResult qrb =
        map entityFromResult (qrb ^. qrbEntityResults)


maybeNextCursor :: QueryResultBatch -> QueryResultBatchMoreResults -> Either DBException (Maybe Cursor)
maybeNextCursor qrb mr = fmapL (catErr "maybeNextCursor: ") $
    maybeEndCursor qrb mr >>=
    \cursM -> Right cursM
  where
    maybeEndCursor qrb mr = case mr of
        NoMoreResults -> Right Nothing
        NotFinished ->
            maybe
                (Left $ InternalError $ ParseError "No end-cursor when NotFinished")
                (Right . Just)
                (qrb ^. qrbEndCursor)
        x -> Left $ InternalError $ ParseError $ "Unexpected QueryResultBatchMoreResults: " ++ show x

parseEitherRes :: [Either a b] -> Either a [b]
parseEitherRes resE =
    case lefts resE of
        []  -> Right $ rights resE
        err -> Left  $ head err
