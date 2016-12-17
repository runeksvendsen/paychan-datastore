{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Query where

import DB.Model.Types.Query
import DB.Model.Convert.Request.Lookup (parseEntityResult, parseEntityResultKey)
import DB.Types
import DB.Util.Error
import DB.Model.Convert.Entity
import Util
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


parseQueryBatchRes :: RunQueryResponse -> Either String QueryResultBatch
parseQueryBatchRes  qr = maybe
    (Left "No QueryResultBatch in RunQueryResponse")
    Right
    (qr ^. rBatch)

parseBatchResults :: QueryResultBatch
                  -> Either String ([DS.Entity], Maybe Cursor)
parseBatchResults qrb = fmapL ("QueryResultBatch: " ++) $
    case qrb ^. qrbMoreResults of
        Just mr ->
            parseBatchEntities qrb >>=
            \entRes -> maybeNextCursor qrb mr >>=
            \cursM -> Right (entRes, cursM)
        Nothing -> Left "Missing QueryResultBatchMoreResults"

parseBatchEntities :: QueryResultBatch -> Either String [DS.Entity]
parseBatchEntities qrb =
    entitiesFromResult qrb >>=
    parseEitherRes
  where
    entityFromResult er = maybe (Left "No Entity in EntityResult") Right (er ^. erEntity)
    entitiesFromResult qrb =
        case qrb ^. qrbEntityResults of
            []   -> Left "No EntityResults at all"
            resL -> Right $ map entityFromResult resL


maybeNextCursor :: QueryResultBatch -> QueryResultBatchMoreResults -> Either String (Maybe Cursor)
maybeNextCursor qrb mr = fmapL ("maybeNextCursor: " ++) $
    maybeEndCursor qrb mr >>=
    \cursM -> Right cursM
  where
    maybeEndCursor qrb mr = case mr of
        NoMoreResults -> Right Nothing
        NotFinished ->
            maybe
                (Left "No end-cursor when NotFinished")
                (Right . Just)
                (qrb ^. qrbEndCursor)
        x -> Left $ "Unexpected QueryResultBatchMoreResults: " ++ show x

parseEitherRes :: [Either a b] -> Either a [b]
parseEitherRes resE =
    case lefts resE of
        []  -> Right $ rights resE
        err -> Left  $ head err
