{-# LANGUAGE FunctionalDependencies #-}
module ChanDB.Interface.Spec
( module ChanDB.Interface.Spec
, module ChanDB.Types
)
where

import ChanDB.Types


-- | Run database operations using this handle.
--   Acquired at application startup.
class DBHandle h where
    getHandle :: Handle -> LogLevel -> IO h

-- | Database interface for storage of ServerPayChan and PromissoryNote
class (DBHandle h, MonadIO m) => ChanDB m h | m -> h where
    -- | Run database operations
    runDB           :: h -> m a -> IO (Either ChanDBException a)
    -- | Create new state for both PayChanServer and ClearingServer
    create          :: RecvPayChan -> m ()
    -- | Delete/remove state for both PayChanServer and ClearingServer
    delete          :: Key -> m ()
    -- | Mark channel as in the process of being settled (unavailable for payment)
    settleBegin     :: [EntityKey RecvPayChan] -> m [RecvPayChan]
    -- | Mark channel as settled (status becomes "closed" or, again "available for payment",
    --    depending on client change address).
    settleFin       :: [RecvPayChan] -> m ()
    -- | Select channel keys by 'DBQuery' properties
    selectChannels  :: DBQuery -> m [EntityKey RecvPayChan]
    -- | Select note keys by note UUID
    selectNotes     :: [UUID] -> m [EntityKey StoredNote]


-- | Composable, atomic database operations
class (ChanDB dbM h, MonadIO m) => ChanDBTx m dbM h | m -> dbM where
    -- | Get paychan state
    getPayChan      :: Key -> m (Maybe RecvPayChan)
    -- | Save paychan state
    updatePayChan   :: RecvPayChan -> m ()
    -- | Save newly created promissory note
    --    and, for previous note if present, update isTip=False
    insertUpdNotes  :: (Key, StoredNote, Maybe StoredNote) -> m ()
    -- | Get most recently issued note, if one exists
    getNewestNote   :: Key -> m (Maybe StoredNote)
    -- | Atomically execute 'ChanDBTx' operations,
    --    converting a 'ChanDBTx' operation to a 'ChanDB' one.
    liftDbTx        :: DBConsumer -> h -> m a -> dbM a
    -- | Atomically execute 'ChanDbTx' operations as an IO action
    atomically      :: DBConsumer -> h -> m a -> IO (Either ChanDBException a)

data DBConsumer =
    PayChanDB
  | ClearingDB


