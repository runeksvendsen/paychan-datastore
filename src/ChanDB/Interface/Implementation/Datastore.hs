--{-# LANGUAGE UndecidableInstances, KindSignatures #-}
module ChanDB.Interface.Implementation.Datastore
( module ChanDB.Interface.Implementation.Datastore
, module ChanDB.Interface.Spec
)
where

import LibPrelude
import Datastore
import ChanDB.Types
import ChanDB.Interface.Spec
import ChanDB.Query
import ChanDB.Creation
import ChanDB.Update
import ChanDB.PubKey
import DB.Env as Env

import DB.Request.Query
import Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)
import qualified Control.Monad.Writer.Strict        as W


paychanNS :: NamespaceId
paychanNS = "paychan3"

clearingNS :: NamespaceId
clearingNS = "clearing3"

projectId :: ProjectId
projectId = "cloudstore-test"

toNamespace :: DBConsumer -> NamespaceId
toNamespace PayChanDB = paychanNS
toNamespace ClearingDB = clearingNS

instance DBHandle DatastoreConf where
    getHandle h logLvl = do
        env <- defaultAppDatastoreEnv h logLvl
        return $ DatastoreConf env projectId logLvl h

instance ChanDB Datastore DatastoreConf where
    runDB c d = fmapL DBException <$> runDatastore c d
    create = create'
    delete = delete'
    settleBegin = settleBegin'
    settleFin = settleFin'
    selectNotes = selectNotes'
    selectChannels = selectChannels'
    -- XPub
    pubKeySetup = liftTx (toNamespace PayChanDB) . getOrInitialize (toNamespace PayChanDB)
    pubKeyCurrent = getCurrent (toNamespace PayChanDB)
    pubKeyLookup = lookupKey (toNamespace PayChanDB)
    pubKeyMarkUsed xp = liftTx (toNamespace PayChanDB) . markAsUsed (toNamespace PayChanDB) xp
    pubKeyDELETE = deleteEverything (toNamespace PayChanDB)


create' :: RecvPayChan -> Datastore ()
create' rpc = do
    _ <- insertChan (toNamespace PayChanDB)  rpc
    _ <- insertChan (toNamespace ClearingDB) rpc
    return ()

delete' :: Key -> Datastore ()
delete' k = do
    _ <- removeChan (toNamespace PayChanDB)  k
    _ <- removeChan (toNamespace ClearingDB) k
    return ()

selectNotes' :: HasScope '[AuthDatastore] ProjectsRunQuery => [UUID] -> Datastore [EntityKey StoredNote]
selectNotes' uuidL = do
    keyL <- forM uuidL doQuery
    liftIO $ print keyL
    return $ concat keyL
  where
    doQuery uid = keysOnlyQuery (Just (toNamespace ClearingDB)) (q uid) >>= getResult
    q :: UUID -> OfKind StoredNote (FilterProperty UUID (KeysOnly Query))
    q uid = OfKind (undefined :: StoredNote)
        $ FilterProperty "previous_note_id" PFOEqual uid
        $ KeysOnly
          emptyQuery

selectChannels' :: HasScope '[AuthDatastore] ProjectsRunQuery => DBQuery -> Datastore [EntityKey RecvPayChan]
selectChannels' GetAll =
    keysOnlyQuery (Just (toNamespace PayChanDB)) q >>= getResult
  where
    q = OfKind (undefined :: RecvPayChan) emptyQuery

selectChannels' (ExpiringBefore t) =
    keysOnlyQuery (Just (toNamespace PayChanDB)) q >>= getResult
  where
    timestamp = round $ utcTimeToPOSIXSeconds t :: Int64
    q =   OfKind (undefined :: RecvPayChan)
        $ FilterProperty "state.pcsParameters.cpLockTime" PFOLessThanOrEqual timestamp
        $ FilterProperty "metadata.mdChannelStatus" PFOEqual ("ReadyForPayment" :: Text)
        $ OrderBy "state.pcsParameters.cpLockTime" Ascending emptyQuery

selectChannels' (CoveringValue val) = do
    resL <- queryBatchEntities (Just (toNamespace PayChanDB)) q
    chanL <- case collect [] resL of
        Left v    -> throwM $ InsufficientValue $ val - v
        Right chL -> return chL
    let retL = map (rootIdent . getIdent) chanL :: [EntityKey RecvPayChan]
    return retL
  where
    collect accum [] = Left $ sum $ map valueToMe accum
    collect accum (JustEntity chan:rem) =
        if sum (map valueToMe accum) >= val then
                Right accum
            else
                collect (chan : accum) rem
    q =   OfKind (undefined :: RecvPayChan)
        $ FilterProperty "metadata.mdChannelStatus" PFOEqual ("ReadyForPayment" :: Text)
        $ OrderBy "metadata.mdValueReceived" Descending emptyQuery

settleBegin' :: [EntityKey RecvPayChan]
                     -> Datastore [RecvPayChan]
settleBegin' = error "STUB"

settleFin' :: [RecvPayChan] -> Datastore ()
settleFin' = error "STUB"

getResult :: Monad m => [ (EntityKey a, EntityVersion) ] -> m [ EntityKey a ]
getResult = return . getResult'

getResult' :: [ (EntityKey a, EntityVersion) ] -> [ EntityKey a ]
getResult' = map fst


instance ChanDBTx DatastoreTx Datastore DatastoreConf where
    updatePayChan = commitChan'
    insertUpdNotes  = commitNote'
    -- TODO: ExceptT
    getPayChan = txGetChanState
    getNewestNote = txGetLastNote
    liftDbTx ns cfg = runTx cfg (toNamespace ns)
    atomically ns cfg m = fmapL DBException <$> runDatastoreTx cfg (toNamespace ns) m

commitChan' :: RecvPayChan -> DatastoreTx ()
commitChan' chan = do
    let updateChan = Update $ EntityWithAnc chan root
    ns <- getNSId
    mut <- mkMutation ns updateChan
    W.tell mut

commitNote' :: (Key, StoredNote, Maybe StoredNote) -> DatastoreTx ()
commitNote' (pk,newNote,prevNoteM) = do
    ns <- getNSId
    insNewNote  <- mkMutation ns $ Insert $ EntityWithAnc newNote (ident pk :: Ident RecvPayChan)
    -- If there's previous note: overwrite the old version (is_tip = False)
    let updateNote note = mkMutation ns $ Update $ EntityWithAnc note (ident pk :: Ident RecvPayChan)
    updPrevNote <- maybe (return mempty) updateNote prevNoteM
    W.tell $ insNewNote <> updPrevNote
