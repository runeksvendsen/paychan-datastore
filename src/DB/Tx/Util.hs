{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Util
(
  txBeginUnsafe
, txRollback
, txCommit
)
where

import Util
import DB.Types
import qualified DB.Util.Error as Util
import Network.Google as Google


-- |Rollback. Finish the transaction without doing anything.
txRollback :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsRollback )
           => ProjectId -> TxId -> m RollbackResponse
txRollback projectId tx =
    Google.send (projectsRollback rollbackReq projectId) >>=
        \res -> liftIO (putStrLn "INFO: Transaction rolled back.") >> return res
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx


-- |Commit. Finish the transaction with an update.
txCommit :: ( MonadGoogle '[AuthDatastore] m
            ,    HasScope '[AuthDatastore] ProjectsCommit )
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
                 ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
              => ProjectId
              -> m TxId
txBeginUnsafe projectId = do
    txBeginRes <- Google.send (projectsBeginTransaction beginTransactionRequest projectId)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $
                "CloudStore API BUG. BeginTransactionResponse: " ++
                "Transaction identifier not present" ++ show txBeginRes

