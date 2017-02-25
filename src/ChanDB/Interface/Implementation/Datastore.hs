{-# LANGUAGE UndecidableInstances, KindSignatures #-}
module ChanDB.Interface.Implementation.Datastore

where

import LibPrelude
import ChanDB.Types
import ChanDB.Interface.Spec
import ChanDB.Query
import ChanDB.Creation
import ChanDB.Update

import DB.Request.Query
import Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)
import qualified Control.Monad.Writer.Strict        as W


paychanNS :: NamespaceId
paychanNS = "paychan"

clearingNS :: NamespaceId
clearingNS = "clearing"


instance ChanDBTxRun DatastoreTx PayChan where
    atomically (PayChan cfg) = runDatastoreTx cfg paychanNS

instance ChanDBTxRun DatastoreTx Clearing where
    atomically (Clearing cfg) = runDatastoreTx cfg clearingNS


instance ChanDBTx DatastoreTx where
--     atomically = runDatastoreTx
    updatePayChan chan = getNSId >>= (`commitChan'` chan)
    insertUpdNotes  = commitNote'

    -- TODO: ExceptT
    getPayChan pk = getNSId >>= (`txGetChanState` pk) >>= tmpErrFix
    getNewestNote pk = getNSId >>= (`txGetLastNote` pk)


-- instance (HasScope '[AuthDatastore] ProjectsRunQuery) => ChanDBTx DatastoreTx Clearing where
--     atomically (Clearing cfg) = runDatastoreTx cfg clearingNS
--     updatePayChan (Clearing chan) = commitChan' clearingNS chan
--     insertUpdNotes (Clearing notePair) = commitNote' notePair
--
--     -- TODO: ExceptT
--     getPayChan (Clearing pk) = txGetChanState clearingNS pk >>= tmpErrFix
--     getNewestNote (Clearing pk) = txGetLastNote clearingNS pk


-- getPayChan' :: HasNamespace c => c SendPubKey ->
-- getPayChan'

commitChan' :: NamespaceId -> RecvPayChan -> DatastoreTx ()
commitChan' ns chan = do
--     ns <- getNSId
    let updateChan = Update $ EntityWithAnc chan root
    mut <- mkMutation ns updateChan
    W.tell mut


commitNote' :: (SendPubKey, StoredNote, Maybe StoredNote) -> DatastoreTx ()
commitNote' (pk,newNote,prevNoteM) = do
    ns <- getNSId
--     let ns = clearingNS
    insNewNote  <- mkMutation ns $ Insert $ EntityWithAnc newNote (ident pk :: Ident RecvPayChan)
    -- If there's previous note: overwrite the old version (is_tip = False)
    let updateNote note = mkMutation ns $ Update $ EntityWithAnc note (ident pk :: Ident RecvPayChan)
    updPrevNote <- maybe (return mempty) updateNote prevNoteM
    W.tell $ insNewNote <> updPrevNote

tmpErrFix :: Monad m => Either UpdateErr a -> m (Maybe a)
tmpErrFix e =
    case e of
        Right a ->
            return $ Just a
        Left ChannelNotFound ->
            return Nothing
        Left x ->
            throw $ InternalError (show x)


-- either (throw . InternalError) return

instance HasScope '[AuthDatastore] ProjectsRunQuery => ChanDB Datastore where
    runDB = runDB'
    create = create'
    delete = delete'
    selectNotes = selectNotes'
    selectChannels = selectChannels'
    settleBegin = settleBegin'
    settleFin = settleFin'

runDB' :: DatastoreConf -> Datastore a -> IO a
runDB' = runDatastore

create' :: RecvPayChan -> Datastore ()
create' rpc = do
    _ <- insertChan paychanNS  rpc
    _ <- insertChan clearingNS rpc
    return ()

delete' :: SendPubKey -> Datastore ()
delete' k = do
    _ <- removeChan paychanNS  k
    _ <- removeChan clearingNS k
    return ()

selectNotes' :: HasScope '[AuthDatastore] ProjectsRunQuery => [UUID] -> Datastore [EntityKey StoredNote]
selectNotes' uuidL = do
    keyL <- forM uuidL doQuery
    liftIO $ print keyL
    return $ concat keyL
  where
    doQuery uid = keysOnlyQuery (Just clearingNS) (q uid) >>= failOnErr >>= getResult
    q :: UUID -> OfKind StoredNote (FilterProperty UUID (KeysOnly Query))
    q uid = OfKind (undefined :: StoredNote)
        $ FilterProperty "previous_note_id" PFOEqual uid
        $ KeysOnly
          emptyQuery

selectChannels' :: HasScope '[AuthDatastore] ProjectsRunQuery => DBQuery -> Datastore [EntityKey RecvPayChan]
selectChannels' GetAll =
    keysOnlyQuery (Just paychanNS) q >>= failOnErr >>= getResult
  where
    q = OfKind (undefined :: RecvPayChan) emptyQuery

selectChannels' (ExpiringBefore t) =
    keysOnlyQuery (Just paychanNS) q >>= failOnErr >>= getResult
  where
    timestamp = round $ utcTimeToPOSIXSeconds t :: Int64
    q =   OfKind (undefined :: RecvPayChan)
        $ FilterProperty "state.pcsParameters.cpLockTime" PFOLessThanOrEqual timestamp
        $ FilterProperty "metadata.mdChannelStatus" PFOEqual ("ReadyForPayment" :: Text)
        $ OrderBy "state.pcsParameters.cpLockTime" Ascending emptyQuery

selectChannels' (CoveringValue val) = do
    resL <- queryBatchEntities (Just paychanNS) q
    chanL <- case collect [] resL of
        Left v -> return $ Left $ "Not enough available value. Have: " ++
            show v ++ " need: " ++ show val
        Right chL -> return $ Right chL
    let retL = map (rootIdent . getIdent) <$> chanL :: Either String [EntityKey RecvPayChan]
    liftIO $ print (resL :: [JustEntity RecvPayChan])
    either (throw . InternalError) return retL
  where
    collect accum [] = Left $ map valueToMe accum
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


failOnErr :: Monad m => Either String b -> m b
failOnErr = either (throw . InternalError) return

failOnErr' :: Either String b -> b
failOnErr' = either (throw . InternalError) id

getResult :: Monad m => [ (EntityKey a, EntityVersion) ] -> m [ EntityKey a ]
getResult = return . getResult'

getResult' :: [ (EntityKey a, EntityVersion) ] -> [ EntityKey a ]
getResult' = map fst



{-

test_ :: HasScope '[AuthDatastore] ProjectsRunQuery => NamespaceId -> DatastoreConf -> IO RecvPayChan
test_ ns cfg = atomically cfg ns something
    where
        something :: HasScope '[AuthDatastore] ProjectsRunQuery => DatastoreTx RecvPayChan
        something = do
            let pk :: SendPubKey
                pk = undefined
            Just chan <- getChan pk
            Just note <- getNewestNote pk
            commitChan chan
            commitNote pk note
            return chan


-}