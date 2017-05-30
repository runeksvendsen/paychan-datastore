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

runTx :: (DatastoreM m, MonadBaseControl IO m, HasScope '[AuthDatastore] ProjectsBeginTransaction) =>
    DatastoreConf -> NamespaceId -> DatastoreTx a -> m a
runTx cfg ns txM = do
    tx <- txBegin
    (a,req) <- Res.runResourceT $ liftResourceT $ do
        let w = R.runReaderT (runDSLogging cfg $ unDSTx txM) (TxDatastoreConf cfg tx ns)
        W.runWriterT w
    when (req /= mempty) $
        void $ txFinish tx (Just req)
    return a

liftTx :: (DatastoreM m, MonadBaseControl IO m, HasScope '[AuthDatastore] ProjectsBeginTransaction) =>
    NamespaceId -> DatastoreTx a -> m a
liftTx ns txM = do
    cfg <- getConf
    runTx cfg ns txM



withTx :: ( MonadResource m
          , DatastoreM m
          , HasScope '[AuthDatastore] ProjectsBeginTransaction
          )
       => (TxId -> m (a, Maybe CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx f = do
    tk@(TxToken tx rk) <- txBegin
    (a,maybeCommReq) <- f tx
    case maybeCommReq of
        Nothing  -> release rk >> return (a,Nothing)
        Just req -> do
            respM <- txFinish tk (Just req)
            return (a, respM)


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
         -> Maybe CommitRequest
         -> m (Maybe CommitResponse)
txFinish (TxToken _ rk) Nothing = release rk >> return Nothing
txFinish (TxToken tx rk) (Just req) = do
    Log.logDebugN "Sending tx commit request..."
    resp <- txCommit tx req
    _ <- unprotect rk
    return (Just resp)




