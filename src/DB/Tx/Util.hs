{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Util
(
  txBeginUnsafe
, txRollback
, txCommit
)
where

import DB.Request.Send
import LibPrelude
import DB.Types
import qualified DB.Error.Util as Util
import Debug.Trace
import qualified Control.Monad.Logger as Log
import GHC.Stack


-- |Rollback. Finish the transaction without doing anything.
txRollback :: ( DatastoreM m
              , HasCallStack
              , HasScope '[AuthDatastore] ProjectsRollback
              )
           => TxId
           -> m RollbackResponse
txRollback tx =
    sendReq (projectsRollback rollbackReq) >>=
        \res -> Log.logInfoCS callStack "Transaction rolled back" >> return res
  where
    rollbackReq = mkAtomicReq tx rollbackRequest


-- |Commit. Finish the transaction with an update.
txCommit :: ( DatastoreM m
            , HasScope '[AuthDatastore] ProjectsCommit
            )
         => TxId
         -> CommitRequest
         -> m CommitResponse
txCommit tx commReq =
    sendReq (projectsCommit txCommReq)
  where
    txCommReq = mkAtomicReq tx commReq


-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe :: HasScope '[AuthDatastore] ProjectsBeginTransaction
              => Datastore TxId
txBeginUnsafe = do
    txBeginRes <- sendReq (projectsBeginTransaction beginTransactionRequest)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $ Bug $
                "Datastore BeginTransactionResponse: " ++
                "Transaction identifier not present " ++ show txBeginRes

