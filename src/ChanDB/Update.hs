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
import           DB.Util.Error
import           DB.Tx.Safe
import           DB.Tx.Lookup   (txLookup, txAncestorQuery, getFirstResult)
import           DB.Model.Convert

import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)

import Debug.Trace


data UpdateErr =
    PayError PayChanError
  | ChannelNotFound


txGetChanState :: ( MonadCatch m
                 , MonadGoogle '[AuthDatastore] m
                 ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
              => ProjectId
              -> TxId
              -> SendPubKey
              -> m (Either UpdateErr RecvPayChan)
txGetChanState pid tx sendPK = do
    resM <- getFirstResult <$> txLookup pid tx root (getIdentifier sendPK)
    return $ maybe (Left $ ChannelNotFound) Right resM

txGetLastNote :: ( MonadCatch m
                 , MonadGoogle '[AuthDatastore] m )
              => ProjectId
              -> TxId
              -> SendPubKey
              -> m (Maybe StoredNote)
txGetLastNote pid tx k = do
    let query = "SELECT * FROM StoredNote WHERE most_recent_note = TRUE"
    resE <- txAncestorQuery pid tx (castIdent $ getIdent k :: Ident RecvPayChan) query
    tipNoteM <- case resE of
        Right [((tipNote,_),_)] -> return $ Just tipNote
        Right []    -> return Nothing
        Right resL  -> internalError $ "tipNoteLookup: Multiple Notes." ++ show resL
        Left e      -> internalError $ "txAncestorQuery error:" ++ e
    return tipNoteM


withDBState :: ( -- MonadIO m
                 MonadCatch m
               , MonadGoogle '[AuthDatastore] m
               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => ProjectId
            -> SendPubKey
            -> (RecvPayChan -> m (Either PayChanError RecvPayChan))
            -> m (Either UpdateErr RecvPayChan)
withDBState pid sendPK f = do
    (eitherRes,_) <- withTx pid $ \tx -> do
        resE <- txGetChanState pid tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _        -> return ( applyResult, Nothing)
            Right newState -> return ( applyResult
                                     , Just $ unTagged $ mkUpdate root newState
                                     )
    return $ case eitherRes of
        Right state -> Right state
        Left e -> Left e

withDBStateNote :: ( -- MonadIO m
                     MonadCatch m
                   , MonadGoogle '[AuthDatastore] m
                   ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => ProjectId
            -> SendPubKey
            -> (RecvPayChan -> Maybe StoredNote -> m (Either PayChanError (RecvPayChan,StoredNote)))
            -> m (Either UpdateErr RecvPayChan)
withDBStateNote pid sendPK f = do
    (eitherRes,_) <- withTx pid $ \tx -> do
        resE  <- txGetChanState pid tx sendPK
        noteM <- txGetLastNote pid tx sendPK
        -- Apply user function
        let applyF chan = fmapL PayError <$> f chan noteM
        applyResult <- either (return . Left) applyF resE
        -- Commit/rollback
        case applyResult of
            Left  _        -> return (applyResult, Nothing)
            Right (newState,newNote) ->
                return ( applyResult
                       , Just $ unTagged $
                                mkUpdate root newState
                            </> mkInsert (getIdent newState) newNote
                            -- Update fetched note so is_tip = False
                            </> maybe
                                    (Tagged mempty)
                                    (mkUpdate (getIdent newState) . setMostRecentNote False )
                                    noteM
                       )
    return $ case eitherRes of
        Right (state,_) -> Right state
        Left e -> Left e
  where
    setMostRecentNote b n = n { most_recent_note = b }



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
-- withTxLookup pid sendPK lookupFunc commitFunc = do
--     eitherRes <- withTx pid $ \tx -> do
--         lookupResM <- lookupFunc pid tx sendPK
--         case lookupResM of
--             Nothing -> return $ Nothing
--             Just a  -> return (a, commitFunc a)
--
--
--     return $ case eitherRes of
--             Right (state,_) -> Right state
--             Left e -> Left e

