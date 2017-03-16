{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Lookup
(
  LookupM(..)
, lookup'
, txLookup
, getFirstResult
, getSingleRes
  -- *Re-exports
, parseLookupRes
)
where

import LibPrelude
import DB.Error.Util
import DB.Model.Types
import DB.Request.Send
import DB.Types
import DB.Model.Convert


class ( HasKeyPath k
      , HasScope '[AuthDatastore] ProjectsLookup
      , IsEntity e
      , MonadThrow m
      )
      => LookupM m k e where
        dbLookup :: NamespaceId -> k -> m [(e, EntityVersion)]


instance ( HasKeyPath k
         , HasScope '[AuthDatastore] ProjectsLookup
         , IsEntity e
         )
         => LookupM Datastore k e where
            dbLookup = lookup'

instance ( HasKeyPath k
         , HasScope '[AuthDatastore] ProjectsLookup
         , IsEntity e
         )
         => LookupM DatastoreTx k e where
            dbLookup _ = txLookup

txLookup :: forall k e m.
            ( DatastoreTxM m
            , HasKeyPath k
            , HasScope '[AuthDatastore] ProjectsLookup
            , IsEntity e
            )
           => k
           -> m [(e, EntityVersion)]
txLookup key = do
    tx <- getTxId
    ns <- getNSId
    lookupG (Just tx) ns key

lookup' :: forall k e m.
            ( DatastoreM m
            , HasKeyPath k
            , HasScope '[AuthDatastore] ProjectsLookup
            , IsEntity e
            )
           => NamespaceId
           -> k
           -> m [(e, EntityVersion)]
lookup' = lookupG Nothing

lookupG :: forall k e m.
            ( DatastoreM m
            , HasKeyPath k
            , HasScope '[AuthDatastore] ProjectsLookup
            , IsEntity e
            )
           => Maybe TxId
           -> NamespaceId
           -> k
           -> m [(e, EntityVersion)]
lookupG tidM ns key = do
    partId <- mkPartitionId ns
    let lookupReq = projectsLookup . maybeAtomic . unTagged $ mkLookup (Just partId) key
    throwLeft =<< (parseLookupRes . Tagged <$> sendReq lookupReq)
  where
    maybeAtomic :: TransactionalReq a => a -> a
    maybeAtomic req = maybe req (`mkAtomicReq` req) tidM

getFirstResult :: [ (a, EntityVersion) ] -> Maybe a
getFirstResult resL =
    fst <$> listToMaybe resL

getSingleRes :: DatastoreM m => [ (a, EntityVersion) ] -> m a
getSingleRes = maybe (throwM NoSuchEntity) return . getFirstResult

