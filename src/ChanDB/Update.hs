{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module ChanDB.Update
(
  withDBState
, withDBStateNote
, UpdateErr(..)
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


