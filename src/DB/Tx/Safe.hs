{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Safe
-- (
--   withTx
-- )
where

import LibPrelude
import DB.Error
import DB.Tx.Util
import DB.Types
import qualified Control.Monad.Catch as      Catch
import           Control.Monad.Trans.Resource   as Res
import qualified Control.Monad.Reader           as R
import Control.Monad
import Control.Monad.Trans.Writer.Strict        as W
import Control.Monad.Trans.Control
import Control.Monad.Base

import qualified Control.Monad.Logger as Log


runDatastoreTx :: HasScope '[AuthDatastore] ProjectsBeginTransaction =>
    DatastoreConf -> NamespaceId -> DatastoreTx a -> IO (Either DBException a)
runDatastoreTx cfg ns txM =
    runDatastore cfg (runTx cfg ns txM)

runTx :: forall m a. (DatastoreM m, MonadBaseControl IO m, HasScope '[AuthDatastore] ProjectsBeginTransaction) =>
    DatastoreConf -> NamespaceId -> DatastoreTx a -> m a
runTx cfg ns txM = do
    (a,_) <- withTx runner
    return a
  where
    runner :: TxToken -> m (a, CommitRequest)
    runner tk = Res.runResourceT $ liftResourceT $ do
         let w = R.runReaderT (runDSLogging cfg $ unDSTx txM) (TxDatastoreConf cfg tk ns)
         W.runWriterT w

liftTx :: (DatastoreM m, MonadBaseControl IO m, HasScope '[AuthDatastore] ProjectsBeginTransaction) =>
    NamespaceId -> DatastoreTx a -> m a
liftTx ns txM = do
    cfg <- getConf
    runTx cfg ns txM

withTx :: ( MonadResource m
          , DatastoreM m
          , HasScope '[AuthDatastore] ProjectsBeginTransaction
          )
       => (TxToken -> m (a, CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx f = do
    tk <- txBegin
    (a,commReq) <- f tk
    resM <- txFinish tk commReq
    return (a,resM)

txBegin :: ( MonadResource m
           , DatastoreM m
           , HasScope '[AuthDatastore] ProjectsBeginTransaction
           )
        => m TxToken
txBegin = do
    cfg <- getConf
    (rk,tx) <- allocate
        (throwLeft =<< runDatastore cfg txBeginUnsafe)
        (void . runDatastore cfg . txRollback)
    return $ TxToken tx rk

txFinish :: ( MonadResource m
            , DatastoreM m
            , HasScope '[AuthDatastore] ProjectsBeginTransaction
            )
         => TxToken
         -> CommitRequest
         -> m (Maybe CommitResponse)
txFinish (TxToken tx rk) req
    | req == mempty =
        release rk >> return Nothing
    | otherwise     = do
        Log.logDebugN "Sending tx commit request..."
        resp <- txCommit tx req
        _ <- unprotect rk
        return (Just resp)




