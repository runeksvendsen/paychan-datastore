{-# LANGUAGE OverloadedStrings #-}
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
import qualified DB.Util.Error as Util
import Debug.Trace


log_info = putStrLn

-- |Rollback. Finish the transaction without doing anything.
txRollback :: ( DatastoreM m
              , HasScope '[AuthDatastore] ProjectsRollback
              )
           => TxId
           -> m RollbackResponse
txRollback tx =
    sendReq (projectsRollback rollbackReq) >>=
        \res -> liftIO (log_info "INFO: Transaction rolled back.") >> return res
  where
    rollbackReq = mkAtomicReq tx rollbackRequest -- rollbackRequest & rrTransaction ?~ tx


-- |Commit. Finish the transaction with an update.
txCommit :: ( DatastoreM m
            , HasScope '[AuthDatastore] ProjectsCommit
            )
         => TxId
         -> CommitRequest
         -> m CommitResponse
txCommit tx commReq =
    "txCommit!" `trace` sendReq (projectsCommit txCommReq)
  where
    txCommReq = mkAtomicReq tx commReq -- commReq & crMode ?~ Transactional & crTransaction ?~ tx




-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe :: HasScope '[AuthDatastore] ProjectsBeginTransaction
              => Datastore TxId
txBeginUnsafe = do
    txBeginRes <- sendReq (projectsBeginTransaction beginTransactionRequest)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $
                "CloudStore API BUG: BeginTransactionResponse: " ++
                "Transaction identifier not present" ++ show txBeginRes

