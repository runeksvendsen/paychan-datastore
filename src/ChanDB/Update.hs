{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module ChanDB.Update
(
  withDBState
, withDBStateNote
, UpdateErr(..)
)
where

import           Util
import           DB.Types
import           PromissoryNote.StoredNote  (setMostRecentNote)
import           DB.Tx.Safe
import           DB.Request                 (txLookup, txAncestorQuery, getFirstResult)
import           DB.Model.Convert

import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)



data UpdateErr =
    PayError PayChanError
  | ChannelNotFound


txGetChanState :: ( MonadCatch m
                 , MonadGoogle '[AuthDatastore] m
                 ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
              => NamespaceId
              -> TxId
              -> SendPubKey
              -> m (Either UpdateErr RecvPayChan)
txGetChanState ns tx sendPK = do
    resM <- getFirstResult <$> txLookup ns tx root (getIdentifier sendPK)
    return $ maybe (Left $ ChannelNotFound) Right resM

txGetLastNote :: ( MonadCatch m
                 , MonadGoogle '[AuthDatastore] m
                 , HasScope    '[AuthDatastore] ProjectsRunQuery)
              => NamespaceId
              -> TxId
              -> SendPubKey
              -> m (Maybe StoredNote)
txGetLastNote ns tx k = do
    let query = "SELECT * FROM StoredNote WHERE most_recent_note = TRUE"
        payChanId = castIdent $ getIdent k :: Ident RecvPayChan
    getFirstResult <$> txAncestorQuery ns tx payChanId query


withDBState :: ( -- MonadIO m
                 MonadCatch m
               , MonadGoogle '[AuthDatastore] m
               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => NamespaceId
            -> SendPubKey
            -> (RecvPayChan -> m (Either PayChanError RecvPayChan))
            -> m (Either UpdateErr RecvPayChan)
withDBState ns sendPK f = do
    (eitherRes,_) <- withTx ns $ \tx -> do
        resE <- txGetChanState ns tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _        -> return ( applyResult, Nothing)
            Right newState -> return ( applyResult
                                     , Just $ unTagged $ mkUpdate ns root newState
                                     )
    return $ case eitherRes of
        Right state -> Right state
        Left e -> Left e

withDBStateNote :: ( -- MonadIO m
                     MonadCatch m
                   , MonadGoogle '[AuthDatastore] m
                   ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => NamespaceId
            -> SendPubKey
            -> (RecvPayChan -> Maybe StoredNote -> m (Either PayChanError (RecvPayChan,StoredNote)))
            -> m (Either UpdateErr RecvPayChan)
withDBStateNote ns sendPK f = do
    (eitherRes,_) <- withTx ns $ \tx -> do
        resE  <- txGetChanState ns tx sendPK
        noteM <- txGetLastNote ns tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan noteM
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _        -> return (applyResult, Nothing)
            Right (newState,newNote) ->
                return ( applyResult
                       , Just $ unTagged $
                                mkUpdate ns root newState
                            </> mkInsert ns (getIdent newState) newNote
                            -- Update fetched note so is_tip = False
                            </> maybe
                                    (Tagged mempty)
                                    (mkUpdate ns (getIdent newState) . setMostRecentNote False )
                                    noteM
                       )
    return $ case eitherRes of
        Right (state,_) -> Right state
        Left e -> Left e



traceCommit :: forall a. Show a => a -> a
traceCommit c = show c `trace` c

-- |Check entity version.
checkCommResponse :: EntityVersion -> CommitResponse -> UpdateResult
checkCommResponse prevVer commResp =
    case commResp ^. crMutationResults of
        [r]       -> if getMRVersion r > prevVer then Updated else NotUpdated
        []        -> NotUpdated
        n@(_:_:_) -> throw . InternalError $
            "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
  where
    getMRVersion r = fromMaybe
        (throw . InternalError $ "MutationResult: empty version field")
        (r ^. mrVersion)


--
-- withTxLookup :: ( MonadCatch m
--                , MonadGoogle '[AuthDatastore] m
--                ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
--             => ProjectId
--             -> SendPubKey
--             -> (ProjectId -> TxId -> SendPubKey -> m (Maybe a))
--             -> (a -> Maybe CommitRequest)
--             -> m (Either UpdateErr RecvPayChan)
-- withTxLookup ns sendPK lookupFunc commitFunc = do
--     eitherRes <- withTx ns $ \tx -> do
--         lookupResM <- lookupFunc ns tx sendPK
--         case lookupResM of
--             Nothing -> return $ Nothing
--             Just a  -> return (a, commitFunc a)
--
--
--     return $ case eitherRes of
--             Right (state,_) -> Right state
--             Left e -> Left e

