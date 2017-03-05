{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, KindSignatures, FunctionalDependencies #-}
module ChanDB.Interface.Spec
( module ChanDB.Interface.Spec
, module ChanDB.Types
)
where

import ChanDB.Types
import Control.Monad.Trans.Resource


-- | Run database operations using this handle. Acquired at application startup.
class DBHandle h where
    initDB :: LogLevel -> IO h

-- | Database interface for storage of ServerPayChan and PromissoryNote
class (DBHandle h, Monad m) => ChanDB m h | m -> h where
    -- | Run database operations
    runDB           :: h -> m a -> IO a
    -- | Create new state for both PayChanServer and ClearingServer
    create          :: RecvPayChan -> m ()
    -- | Delete/remove state for both PayChanServer and ClearingServer
    delete          :: SendPubKey -> m ()
    -- | Mark channel as in the process of being settled (unavailable for payment)
    settleBegin     :: [EntityKey RecvPayChan] -> m [RecvPayChan]
    -- | Mark channel as settled (status becomes "closed" or, again "available for payment",
    --    depending on client change address).
    settleFin       :: [RecvPayChan] -> m ()
    -- | Select channel keys by 'DBQuery' properties
    selectChannels  :: DBQuery -> m [EntityKey RecvPayChan]
    -- | Select note keys by note UUID
    selectNotes     :: [UUID] -> m [EntityKey StoredNote]


-- | Atomic database operations
class ChanDB dbM h => ChanDBTx m dbM h | m -> dbM where
    -- | Get paychan state
    getPayChan      :: SendPubKey -> m (Maybe RecvPayChan)
    -- | Save paychan state
    updatePayChan   :: RecvPayChan -> m ()
    -- | Save newly created promissory note
    --    and, for previous note if present, update isTip=False
    insertUpdNotes  :: (SendPubKey, StoredNote, Maybe StoredNote) -> m ()
    -- | Get most recently issued note, if one exists
    getNewestNote   :: SendPubKey -> m (Maybe StoredNote)
    -- | Atomically execute 'ChanDBTx' operations,
    --  reducing a 'ChanDBTx' operation to a 'ChanDB' one
    atomically      :: DBConsumer -> h -> m a -> dbM a

data DBConsumer =
    PayChanDB
  | ClearingDB


