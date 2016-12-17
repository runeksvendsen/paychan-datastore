{-# LANGUAGE DataKinds #-}
module DB.Model.Types.Request
-- (
--   IsRequest
-- , TransactionalReq(..)
-- ,
-- )
where

import Util
import DB.Model.Types
import Network.Google.Datastore


class IsRequest a

instance IsRequest RollbackRequest
instance IsRequest CommitRequest
instance IsRequest LookupRequest
instance IsRequest RunQueryRequest
instance IsRequest BeginTransactionRequest


-- | Requests that can be executed within a transaction.
class TransactionalReq a where
    atomically :: TxId -> a -> a

-- data Atomically a = Atomically TxId a

-- instance TransactionalReq a => TransactionalReq (Atomically a) where
--     atomically (Atomically tx r) = atomically tx r

instance TransactionalReq RollbackRequest where
    atomically tx req = req
        & rrTransaction ?~ tx

instance TransactionalReq CommitRequest where
    atomically tx req = req
        & crTransaction ?~ tx
        & crMode ?~ Transactional

instance TransactionalReq LookupRequest where
    atomically tx req = req
        & lrReadOptions ?~ (readOptions & roTransaction ?~ tx)

instance TransactionalReq RunQueryRequest where
    atomically tx req = req
        & rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)


class IsRequest a => HasProject a p where
    _mkProjectReq :: a -> ProjectId -> p

instance HasProject RollbackRequest ProjectsRollback where
    _mkProjectReq = projectsRollback

instance HasProject CommitRequest ProjectsCommit where
    _mkProjectReq = projectsCommit

instance HasProject LookupRequest ProjectsLookup where
    _mkProjectReq = projectsLookup

instance HasProject RunQueryRequest ProjectsRunQuery where
    _mkProjectReq = projectsRunQuery

instance HasProject BeginTransactionRequest ProjectsBeginTransaction where
    _mkProjectReq = projectsBeginTransaction



