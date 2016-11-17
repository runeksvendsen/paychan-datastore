{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Index.Keys where

import           Util
import qualified Model.ChanIndex        as Index
import           Network.Google         as Google
import qualified Data.Either as Either


-- |Retrieve a list of keys for all open channels.
openChannelKeys pid = openChannelKeysImpl pid Nothing

openChannelKeysImpl
    :: ( MonadGoogle '[AuthDatastore] m
    ,    HasScope    '[AuthDatastore] ProjectsRunQuery )
    => ProjectId -> Maybe Cursor -> m [SendPubKey]
openChannelKeysImpl projectId cursM =
    openChannelsQuery projectId cursM >>=
    \qr -> case parseRes qr of
        Left e -> internalErrorM $ "openChannelKeys: " ++ e
        Right (queryKeys, Nothing)   -> return queryKeys
        Right (queryKeys, Just curs) -> loop queryKeys curs
  where
    parseRes = parseQueryBatchRes >=> parseBatchResults
    loop queryKeys curs =
        openChannelKeysImpl projectId (Just curs) >>=
        \keyLstAccum -> return (queryKeys ++ keyLstAccum)

-- |Retrieve a single batch of results. TODO: MonadRetry
openChannelsQuery
    :: ( MonadGoogle '[AuthDatastore] m
    ,    HasScope    '[AuthDatastore] ProjectsRunQuery )
    => ProjectId -> Maybe Cursor -> m RunQueryResponse
openChannelsQuery projectId cursM =
    Google.send (projectsRunQuery queryReq projectId)
  where
    queryReq = runQueryRequest &
        rqrPartitionId ?~ Index.mkPartitionId projectId &
        rqrQuery ?~ indexQuery &
        rqrReadOptions ?~ (readOptions & roReadConsistency ?~ Strong)
    indexQuery = query &
        qKind .~ [ kindExpression & keName ?~ Index.kindName ] &
        qStartCursor .~ cursM &
        -- Keys-only query:
        -- https://cloud.google.com/datastore/docs/concepts/queries#datastore_keys_only_query_gql
        qProjection .~ [ projection & pProperty ?~ (propertyReference & prName ?~ "__key__") ]

parseQueryBatchRes :: RunQueryResponse -> Either String QueryResultBatch
parseQueryBatchRes  qr = maybe
    (Left "No QueryResultBatch in RunQueryResponse")
    Right
    (qr ^. rBatch)

parseBatchResults :: QueryResultBatch -> Either String ([SendPubKey], Maybe Cursor)
parseBatchResults qrb = fmapL ("QueryResultBatch: " ++) $
    case qrb ^. qrbMoreResults of
        Just mr ->
            parseBatchEntities qrb >>=
            parseEntityKeys >>=
            \entRes -> maybeNextCursor qrb mr >>=
            \cursM -> Right (entRes, cursM)
        Nothing -> Left "Missing QueryResultBatchMoreResults"

parseBatchEntities :: QueryResultBatch -> Either String [Entity]
parseBatchEntities qrb =
    entitiesFromResult qrb >>=
    parseEitherRes
  where
    entityFromResult er = maybe (Left "No Entity in EntityResult") Right (er ^. erEntity)
    entitiesFromResult qrb =
        case qrb ^. qrbEntityResults of
            []   -> Left "No EntityResults at all"
            resL -> Right $ map entityFromResult resL

parseEntityKeys :: [Entity] -> Either String [SendPubKey]
parseEntityKeys entL = fmapL ("Failed to parse SendPubKey from Entity: " ++) $
    parseEitherRes $ map Index.keyFromEntity entL

maybeNextCursor :: QueryResultBatch -> QueryResultBatchMoreResults -> Either String (Maybe Cursor)
maybeNextCursor qrb mr =
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
    case Either.lefts resE of
        []  -> Right $ Either.rights resE
        err -> Left  $ head err

