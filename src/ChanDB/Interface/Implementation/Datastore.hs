{-# LANGUAGE UndecidableInstances #-}
module ChanDB.Interface.Implementation.Datastore where

import Util
import ChanDB.Types
import ChanDB.Interface.Spec
import ChanDB.Query
import ChanDB.Creation
import ChanDB.Update

import DB.Request.Query
import Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)


clearingNS :: NamespaceId
clearingNS = "clearing"

paychanNS :: NamespaceId
paychanNS = "paychan"


instance HasScope '[AuthDatastore] ProjectsRunQuery => ChanDB Datastore where
    runDB = runDatastore
    create rpc = do
        _ <- insertChan paychanNS  rpc
        _ <- insertChan clearingNS rpc
        return ()

    paychanWithState = withDBState paychanNS

    noteWithState = withDBStateNote clearingNS

    delete k = do
        _ <- removeChan paychanNS  k
        _ <- removeChan clearingNS k
        return ()

    selectNotes uuidL = do
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

    selectChannels GetAll =
        keysOnlyQuery (Just paychanNS) q >>= failOnErr >>= getResult
      where
        q = OfKind (undefined :: RecvPayChan) emptyQuery

    selectChannels (ExpiringBefore t) =
        keysOnlyQuery (Just paychanNS) q >>= failOnErr >>= getResult
      where
        timestamp = round $ utcTimeToPOSIXSeconds t :: Int64
        q =   OfKind (undefined :: RecvPayChan)
            $ FilterProperty "state.pcsParameters.cpLockTime" PFOLessThanOrEqual timestamp
            $ FilterProperty "metadata.mdChannelStatus" PFOEqual ("ReadyForPayment" :: Text)
            $ OrderBy "state.pcsParameters.cpLockTime" Ascending emptyQuery

    selectChannels (CoveringValue val) = do
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

    settleBegin     = error "STUB"
    settleFin       = error "STUB"


failOnErr :: Monad m => Either String b -> m b
failOnErr = either (throw . InternalError) return

failOnErr' :: Either String b -> b
failOnErr' = either (throw . InternalError) id

getResult :: Monad m => [ (EntityKey a, EntityVersion) ] -> m [ EntityKey a ]
getResult = return . getResult'

getResult' :: [ (EntityKey a, EntityVersion) ] -> [ EntityKey a ]
getResult' = map fst

