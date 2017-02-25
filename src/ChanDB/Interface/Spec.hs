{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, KindSignatures #-}
module ChanDB.Interface.Spec
-- (
--   ChanDB(..)
-- , ChanDBTx(..)
-- , module ChanDB.Types
-- )
where

import ChanDB.Types
import Control.Monad.Trans.Resource


-- | Database interface for storage of ServerPayChan and PromissoryNote
class (Monad m, HasScope '[AuthDatastore] ProjectsRunQuery) => ChanDB m where
    -- |
    runDB            :: DatastoreConf
                     -> m a
                     -> IO a

    -- | Create new state for both PayChanServer and ClearingServer
    create           :: RecvPayChan
                     -> m ()

    -- | Delete/remove state for both PayChanServer and ClearingServer
    delete           :: SendPubKey
                     -> m ()

    -- | Select channel keys by 'DBQuery' properties
    selectChannels   :: DBQuery
                     -> m [EntityKey RecvPayChan]

    -- | Select note keys by note UUID
    selectNotes      :: [UUID]
                     -> m [EntityKey StoredNote]

    -- | Mark channel as in the process of being settled (unavailable for payment)
    settleBegin      :: [EntityKey RecvPayChan]
                     -> m [RecvPayChan]

    -- | Mark channel as settled (status becomes "closed" or, again, "available for payment",
    --    depending on client change address).
    settleFin        :: [RecvPayChan]
                     -> m ()

data PayChan a = PayChan a
data Clearing a = Clearing a
class IsDBConsumer (a :: * -> *)
instance IsDBConsumer Clearing
instance IsDBConsumer PayChan


class (MonadResource m) => ChanDBTx m where
--     atomically      :: DatastoreConf -> NamespaceId -> m a -> IO a

    -- | Get paychan state
    getPayChan      :: SendPubKey -> m (Maybe RecvPayChan)

    -- | Save paychan state
    updatePayChan   :: RecvPayChan -> m ()

    -- | Save newly created promissory note
    --    and, for previous note if present, update isTip=False
    insertUpdNotes  :: (SendPubKey, StoredNote, Maybe StoredNote) -> m ()

    -- | Get most recently issued note
    getNewestNote   :: SendPubKey -> m (Maybe StoredNote)


class (ChanDBTx m, IsDBConsumer c) => ChanDBTxRun m c where
    atomically      :: c DatastoreConf -> m a -> IO a

