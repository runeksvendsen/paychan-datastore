{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx
(
  withTx
)
where

import Util
import Network.Google as Google
import qualified Control.Monad.Catch as      Catch


-- |Safely acquire a transaction handle for use in a transaction.
--  The handle will be safely released both if an exception
--   occurs in "f", and if no 'CommitResponse' is returned by
--   "f".
withTx :: ( MonadCatch m
          , MonadGoogle '[AuthDatastore] m
          ,    HasScope '[AuthDatastore] BeginTransactionResponse
          ,    HasScope '[AuthDatastore] RollbackResponse
          ,    HasScope '[AuthDatastore] CommitResponse )
       => ProjectId
       -> (TxId -> m (a, Maybe CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx pid f =
    txBeginUnsafe pid >>= \tx -> do
        let rollback = txRollback pid tx
        (a,maybeCommReq) <- f tx `Catch.onException` rollback
        case maybeCommReq of
            Nothing  -> return (a,Nothing)
            Just req -> txCommit pid tx req >>= \resp -> return (a, Just resp)


-- |Rollback. Finish the transaction without doing anything.
txRollback :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] RollbackResponse )
           => ProjectId -> TxId -> m RollbackResponse
txRollback projectId tx =
    Google.send (projectsRollback rollbackReq projectId)
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx


-- |Commit. Finish the transaction with an update.
txCommit :: ( MonadGoogle '[AuthDatastore] m
            ,    HasScope '[AuthDatastore] CommitResponse )
         => ProjectId
         -> TxId
         -> CommitRequest
         -> m CommitResponse
txCommit pid tx commReq =
    Google.send (projectsCommit txCommReq pid)
  where
    txCommReq = commReq & crMode ?~ Transactional & crTransaction ?~ tx


-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe :: ( MonadGoogle '[AuthDatastore] m
                 ,    HasScope '[AuthDatastore] BeginTransactionResponse )
              => ProjectId
              -> m TxId
txBeginUnsafe projectId = do
    txBeginRes <- Google.send (projectsBeginTransaction beginTransactionRequest projectId)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $
                "CloudStore API BUG. BeginTransactionResponse: " ++
                "Transaction identifier not present"

