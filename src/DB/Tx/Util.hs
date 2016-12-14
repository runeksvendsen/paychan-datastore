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
           => NamespaceId -> TxId -> m RollbackResponse
txRollback nsId tx =
    Google.send (projectsRollback rollbackReq (nsProjectId nsId)) >>=
        \res -> liftIO (putStrLn "INFO: Transaction rolled back.") >> return res
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx


-- |Commit. Finish the transaction with an update.
txCommit :: ( MonadGoogle '[AuthDatastore] m
            ,    HasScope '[AuthDatastore] ProjectsCommit )
         => NamespaceId
         -> TxId
         -> CommitRequest
         -> m CommitResponse
txCommit ns tx commReq =
    Google.send (projectsCommit txCommReq (nsProjectId ns))
  where
    txCommReq = commReq & crMode ?~ Transactional & crTransaction ?~ tx


-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe :: ( MonadGoogle '[AuthDatastore] m
                 ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
              => NamespaceId
              -> m TxId
txBeginUnsafe nsId = do
    txBeginRes <- Google.send (projectsBeginTransaction beginTransactionRequest (nsProjectId nsId))
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Util.internalError $
                "CloudStore API BUG. BeginTransactionResponse: " ++
                "Transaction identifier not present" ++ show txBeginRes

