{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module ChanDB.Update
(
  withDBState
, withDBStateNote
)
where

import           ChanDB.Orphans ()
import           Util
import           ChanDB.Types
import           DB.Types
import           PromissoryNote.StoredNote  (setMostRecentNote)
import           DB.Tx.Safe
import           DB.Request                 (txLookup, txAncestorQuery, getFirstResult)
import           DB.Model.Convert
import qualified Network.Google.Datastore.Types as DS

import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)




txGetChanState :: ( MonadCatch m
                 , DatastoreM m
                 ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
              => NamespaceId
              -> TxId
              -> SendPubKey
              -> m (Either UpdateErr RecvPayChan)
txGetChanState ns tx sendPK = do
    partId <- mkPartitionId ns
    resM <- getFirstResult <$> txLookup (Just partId) tx root (getIdentifier sendPK)
    return $ maybe (Left $ ChannelNotFound) Right resM

txGetLastNote :: ( MonadCatch m
                 , DatastoreM m
                 , HasScope    '[AuthDatastore] ProjectsRunQuery)
              => NamespaceId
              -> TxId
              -> SendPubKey
              -> m (Maybe StoredNote)
txGetLastNote ns tx k = do
    partId <- mkPartitionId ns
    let query = "SELECT * FROM StoredNote WHERE most_recent_note = TRUE"
        payChanId = castIdent $ getIdent k :: Ident RecvPayChan
    getFirstResult <$> txAncestorQuery (Just partId) tx payChanId query

withDBState :: ( -- MonadIO m
                 MonadCatch m
               , DatastoreM m
               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => NamespaceId
            -> SendPubKey
            -> (RecvPayChan -> m (Either PayChanError RecvPayChan))
            -> m (Either UpdateErr RecvPayChan)
withDBState ns sendPK f = do
    (eitherRes,_) <- withTx $ \tx -> do
        resE <- txGetChanState ns tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _        -> return (applyResult, Nothing)
            Right newState -> mkPartitionId ns >>= \partId ->
                return (applyResult, Just $ unTagged $ mkUpdate (Just partId) root newState)
    return $ case eitherRes of
        Right state -> Right state
        Left e -> Left e

withDBStateNote :: (
                     MonadCatch m
                   , DatastoreM m
                   ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => NamespaceId
            -> SendPubKey
            -> (RecvPayChan -> Maybe StoredNote -> m (Either PayChanError (RecvPayChan,StoredNote)))
            -> m (Either UpdateErr RecvPayChan)
withDBStateNote ns sendPK f = do
    (eitherRes,_) <- withTx $ \tx -> do
        resE  <- txGetChanState ns tx sendPK
        noteM <- txGetLastNote ns tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan noteM
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _  ->
                return (applyResult , Nothing)
            Right (newState,newNote) -> mkPartitionId ns >>= \partId ->
                return (applyResult , Just $ mkNoteCommit (Just partId) newState newNote noteM)
    return $ case eitherRes of
        Right (state,_) -> Right state
        Left e -> Left e

mkNoteCommit :: Maybe PartitionId -> RecvPayChan -> StoredNote -> Maybe StoredNote -> DS.CommitRequest
mkNoteCommit partM payChanState newNote prevNoteM = unTagged $
        mkUpdate partM root payChanState
    </> mkInsert partM (getIdent payChanState) newNote
    -- If there's previous note, update it so its is_tip = False
    </> maybe
            (Tagged mempty)
            (mkUpdate partM (getIdent payChanState) . setMostRecentNote False )
            prevNoteM

