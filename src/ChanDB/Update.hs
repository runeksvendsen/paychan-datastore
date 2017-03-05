{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module ChanDB.Update
-- (
--   withDBState
-- , withDBStateNote
-- )
where

import LibPrelude
import           ChanDB.Orphans ()
import           ChanDB.Types
import           DB.Query
import           ChanDB.Types.StoredNote  (setMostRecentNote)
import           DB.Tx.Safe
import           DB.Request                 (txLookup, entityQuery, getFirstResult)
import qualified Network.Google.Datastore.Types as DS
import Control.Monad.Trans.Control
import Control.Monad.Base
import           Control.Monad.Trans.Resource


type PayChanEnt = EntityWithAnc RecvPayChan Void
type NoteEnt    = EntityWithAnc StoredNote (Ident RecvPayChan)


txGetChanState :: ( DatastoreTxM m
                  , HasScope '[AuthDatastore] ProjectsRunQuery
                  )
               => NamespaceId
               -> SendPubKey
               -> m (Either UpdateErr RecvPayChan)
txGetChanState ns sendPK = do
    tid <- getTxId
    getRes <$> mkLookup tid
  where
    mkLookup tx = txLookup ns tx (root <//> sendPK :: KeyWithAnc RecvPayChan Void)


getRes :: Either String [ (JustEntity a, EntityVersion) ]
       -> Either UpdateErr a
getRes r =
    maybe (Left ChannelNotFound) Right (getFirstResult r) >>=
    \(JustEntity e) -> Right e


txGetLastNote :: ( DatastoreTxM m
                 , HasScope '[AuthDatastore] ProjectsRunQuery
                 )
              => NamespaceId
              -> SendPubKey
              -> m (Maybe StoredNote)
txGetLastNote ns k = do
    tx <- getTxId
    res <- entityQuery (Just ns) (Just tx) (qMostRecentNote k)
    return $ getRes' (res :: Either String [(NoteEnt, EntityVersion) ])
  where
    getRes' = fmap (\(EntityWithAnc e _) -> e) . getFirstResult

qMostRecentNote :: SendPubKey
                -> AncestorQuery RecvPayChan (OfKind StoredNote (FilterProperty Bool Query))
qMostRecentNote k =
      AncestorQuery payChanId
    $ OfKind kind
    $ FilterProperty "most_recent_note" PFOEqual True
    query
  where
    kind = undefined :: StoredNote
    payChanId = getIdentifier k :: Ident RecvPayChan





-- withDBState :: ( MonadResource m
--                , DatastoreM m
--                , HasScope '[AuthDatastore] ProjectsBeginTransaction
--                )
--             => NamespaceId
--             -> SendPubKey
--             -> (RecvPayChan -> m (Either PayChanError RecvPayChan))
--             -> m (Either UpdateErr RecvPayChan)
-- withDBState ns sendPK f = do
--     (eitherRes,_) <- withTx $ \tx -> do
--         resE <- txGetChanState ns tx sendPK
--         -- Apply user function
--         let applyF chan = fmapL PayError <$> f chan
--         applyResult <- either (return . Left) applyF resE
--         -- Commit/rollback
--         case applyResult of
--             Left  _        -> return (applyResult, Nothing)
--             Right newState -> do
--                 updChan <- mkMutation ns
--                     (Update $ EntityWithAnc newState root)
--                 return ( applyResult, Just updChan )
--     return $ case eitherRes of
--         Right state -> Right state
--         Left e -> Left e
--
-- withDBStateNote :: ( MonadResource m
--                    , DatastoreM m
--                    , HasScope '[AuthDatastore] ProjectsRunQuery
--                    )
--                 => NamespaceId
--                 -> SendPubKey
--                 -> (RecvPayChan -> Maybe StoredNote -> m (Either UpdateErr (RecvPayChan,StoredNote)))
--                 -> m (Either UpdateErr StoredNote)
-- withDBStateNote ns sendPK f = do
--     rk@(TxToken tx _) <- txBegin
--     resE  <- txGetChanState ns tx sendPK
--     noteM <- txGetLastNote ns tx sendPK
--     -- Apply user function
--     let applyFunc chan = f chan noteM
--     applyResult <- either (return . Left) applyFunc resE
--     -- Commit/rollback
--     case applyResult of
--         Left  _  -> txFinish rk Nothing
--         Right (newState,newNote) -> do
--             updChanNotes <- mkNoteCommit ns newState newNote noteM
--             txFinish rk (Just updChanNotes)
--     return $ case applyResult of
--         Right (_,note) -> Right note
--         Left e -> Left e



