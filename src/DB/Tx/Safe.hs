{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Safe
(
  withTx
)
where

import Util
import DB.Tx.Util
import DB.Types
import qualified Control.Monad.Catch as      Catch
import           Control.Monad.Trans.Resource   as Res


-- |Safely acquire a transaction handle for use in a transaction.
--  The handle will be safely released both if an exception
--   occurs in "f", and if no 'CommitResponse' is returned by
--   "f".
withTx :: ( Catch.MonadCatch m
          , DatastoreM m
          )
       => (TxId -> m (a, Maybe CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx f =
    txBeginUnsafe >>= \tx -> do
        let rollback = txRollback tx
        (a,maybeCommReq) <- f tx `Catch.onException` rollback
        case maybeCommReq of
            Nothing  -> rollback >> return (a,Nothing)
            Just req -> txCommit tx req >>= \resp -> return (a, Just resp)

-- withTxN :: ( Res.MonadResource m
--           , MonadGoogle '[AuthDatastore] m
--           ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
--        => NamespaceId
--        -> (TxId -> m (a, Maybe CommitRequest))
--        -> m (a, Maybe CommitResponse)
-- withTxN ns f = do
--     let rollback tx = void $ txRollback ns tx
--
--     (rk,tx) <- Res.allocate (txBeginUnsafe ns) rollback
--     (a,maybeCommReq) <- f tx
--     case maybeCommReq of
--         Nothing  -> Res.release rk >> return (a,Nothing)
--         Just req -> txCommit ns tx req >>= \resp -> return (a, Just resp)

