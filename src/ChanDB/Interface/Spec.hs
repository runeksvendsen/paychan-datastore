{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
module ChanDB.Interface.Spec
(
  ChanDB(..)
, module ChanDB.Types
)
where

import ChanDB.Types


-- | Database interface
class (Monad m, HasScope '[AuthDatastore] ProjectsRunQuery) => ChanDB m where
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
                     -> m [EntityKey RecvPayChan]

    -- | Mark channel as in the process of being settled (unavailable for payment)
    settleBegin      :: [EntityKey RecvPayChan]
                     -> m [RecvPayChan]

    -- | Mark channel as settled (closed or again available for payment, depending on client change address).
    settleFin        :: [RecvPayChan]
                     -> m ()

     -- | Remove state for both PayChanServer and ClearingServer
    delete           :: SendPubKey
                     -> m ()

    runDB            :: DatastoreConf
                     -> m a
                     -> IO a

