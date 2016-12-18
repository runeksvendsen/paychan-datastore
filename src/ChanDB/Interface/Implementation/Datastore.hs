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

    paychanWithState k = withDBState paychanNS k

    noteWithState k = withDBStateNote clearingNS k

    delete k = do
        _ <- removeChan paychanNS  k
        _ <- removeChan clearingNS k
        return ()

--     selectChannels :: HasScope '[AuthDatastore] ProjectsRunQuery => DBQuery -> m [EntityKey RecvPayChan]
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

    selectChannels (CoveringValue _) = do
--         let getChan :: (JustEntity RecvPayChan, EntityVersion) -> RecvPayChan
--             getChan ((chan,_),_) = chan
        resL <- failOnErr =<< entityQuery (Just paychanNS) Nothing q
        liftIO $ print (map fst resL :: [JustEntity RecvPayChan])
        error "STUB #2"
      where
        q =   OfKind (undefined :: RecvPayChan)
            $ FilterProperty "metadata.mdChannelStatus" PFOEqual ("ReadyForPayment" :: Text)
            $ OrderBy "metadata.mdValueReceived" Descending emptyQuery

    settleBegin     = error "STUB"
    settleFin       = error "STUB"


failOnErr :: Monad m => Either String b -> m b
failOnErr = either (throw . InternalError) return

getResult :: Monad m => [ (EntityKey a, EntityVersion) ] -> m [ EntityKey a ]
getResult = return . map fst
