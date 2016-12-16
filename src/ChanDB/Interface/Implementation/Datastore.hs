module ChanDB.Interface.Implementation.Datastore where

import Util
import ChanDB.Types
import ChanDB.Interface.Spec
import ChanDB.Creation
import ChanDB.Update

import DB.Request.Query
import Data.Time.Clock.POSIX    (utcTimeToPOSIXSeconds)

clearingNS :: NamespaceId
clearingNS = "clearing"

paychanNS :: NamespaceId
paychanNS = "paychan"


instance ChanDB Datastore where
    runDB = runDatastore
    create rpc = do
        _ <- insertChan paychanNS  rpc
        _ <- insertChan clearingNS rpc
        return ()

    paychanWithState k f = withDBState paychanNS k f

    noteWithState k f = withDBStateNote clearingNS k f

    delete k = do
        _ <- removeChan paychanNS  k
        _ <- removeChan clearingNS k
        return ()

    selectChannels GetAll =
        keysOnlyQuery (Just paychanNS) Nothing >>=
        either (throw . InternalError) return >>= getResult

    selectChannels (ExpiringBefore t) =
        keysOnlyQuery (Just paychanNS) (Just qfilter) >>=
        either (throw . InternalError) return >>= getResult
      where
        qfilter = "state.pcsParameters.cpLockTime <= " <>
            (cs . show) (round $ utcTimeToPOSIXSeconds t :: Integer)

    selectChannels (CoveringValue _) = error "STUB"

    settleBegin     = error "STUB"
    settleFin       = error "STUB"


getResult :: Monad m => [ (Ident RecvPayChan, Ident Void) ] -> m [ Ident RecvPayChan ]
getResult = return . map fst
