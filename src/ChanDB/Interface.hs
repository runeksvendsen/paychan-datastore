-- {-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, ScopedTypeVariables #-}
module ChanDB.Interface where

import DB.Run
import ChanDB.Types
import ChanDB.Creation
import ChanDB.Update


class Monad m => ChanDB m where
    -- | Create new state for both PayChanServer and ClearingServer
    create           :: RecvPayChan
                     -> m ()

    -- | PayChanServer: Provide a function in which we have access to the DB state
    paychanWithState :: SendPubKey
                     -> (RecvPayChan -> m (Either PayChanError RecvPayChan))
                     -> m (Either UpdateErr RecvPayChan)

    -- | ClearingServer: Provide a function in which we have access to the DB state + the most recent note (if present)
    noteWithState    :: SendPubKey
                     -> (RecvPayChan -> Maybe StoredNote -> m (Either PayChanError (RecvPayChan,StoredNote)))
                     -> m (Either UpdateErr RecvPayChan)

    -- | Select channel keys by properties
    selectChannels   :: DBQuery
                     -> m [Key]

    -- | Mark channel as in the process of being settled (unavailable for payment)
    settleBegin      :: [Key]
                     -> m [RecvPayChan]

    -- | Mark channel as settled (closed or again available for payment, depending on client change address).
    settleFin        :: [RecvPayChan]
                     -> m ()

     -- | Remove state for both PayChanServer and ClearingServer
    delete           :: SendPubKey
                     -> m ()


-- Implementation

clearingNS :: NamespaceId
clearingNS = "clearing"

paychanNS :: NamespaceId
paychanNS = "paychan"


-- instance ChanDB Datastore where
--     create rpc = do
--         env <- getEnv
--         runGoogle env $ do
--             _ <- insertChan paychanNS  rpc
--             _ <- insertChan clearingNS rpc
--             return ()
--
--     paychanWithState k f = do
--            env <- getEnv
--            runGoogle env $ withDBState paychanNS k f
--
--     noteWithState k f = do
--            env <- getEnv
--            runGoogle env $ withDBStateNote paychanNS k f
--
--
--     delete k = do
--         env <- getEnv
--         runGoogle env $ do
--             _ <- removeChan paychanNS k
--             _ <- removeChan clearingNS k
--             return ()

