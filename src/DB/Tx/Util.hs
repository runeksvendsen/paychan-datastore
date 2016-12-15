{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Util
(
  txBeginUnsafe
, txRollback
, txCommit
)
where

import           DB.Request.Util
import Util
import DB.Types
import qualified DB.Util.Error as Util


-- |Rollback. Finish the transaction without doing anything.
txRollback :: TxId -> Datastore RollbackResponse
txRollback tx =
    sendReq (projectsRollback rollbackReq) >>=
        \res -> liftIO (putStrLn "INFO: Transaction rolled back.") >> return res
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx


-- |Commit. Finish the transaction with an update.
txCommit :: TxId
         -> CommitRequest
         -> Datastore CommitResponse
txCommit tx commReq =
    sendReq (projectsCommit txCommReq)
  where
    txCommReq = commReq & crMode ?~ Transactional & crTransaction ?~ tx


-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe :: Datastore TxId
txBeginUnsafe = do
    txBeginRes <- sendReq (projectsBeginTransaction beginTransactionRequest)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $
                "CloudStore API BUG: BeginTransactionResponse: " ++
                "Transaction identifier not present" ++ show txBeginRes

