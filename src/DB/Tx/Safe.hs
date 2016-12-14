{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Safe
(
  withTx
)
where

import DB.Tx.Util
import DB.Types
import qualified Control.Monad.Catch as      Catch


-- |Safely acquire a transaction handle for use in a transaction.
--  The handle will be safely released both if an exception
--   occurs in "f", and if no 'CommitResponse' is returned by
--   "f".
withTx :: ( Catch.MonadCatch m
          , MonadGoogle '[AuthDatastore] m
          ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
       => NamespaceId
       -> (TxId -> m (a, Maybe CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx ns f =
    txBeginUnsafe ns >>= \tx -> do
        let rollback = txRollback ns tx
        (a,maybeCommReq) <- f tx `Catch.onException` rollback
        case maybeCommReq of
            Nothing  -> rollback >> return (a,Nothing)
            Just req -> txCommit ns tx req >>= \resp -> return (a, Just resp)

