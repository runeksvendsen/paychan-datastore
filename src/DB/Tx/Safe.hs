{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx.Safe
-- (
--   withTx
-- )
where

import LibPrelude
import DB.Tx.Util
import DB.Types
import qualified Control.Monad.Catch as      Catch
import           Control.Monad.Trans.Resource   as Res
import qualified Control.Monad.Reader           as R
import Control.Monad
import Control.Monad.Trans.Writer.Strict        as W
import Control.Monad.Trans.Control
import Control.Monad.Base



runDatastoreTx :: HasScope '[AuthDatastore] ProjectsBeginTransaction =>
    DatastoreConf -> NamespaceId -> DatastoreTx a -> IO a
runDatastoreTx cfg ns d = runDatastore cfg $ do
    tx <- txBegin
    (a,req) <- Res.runResourceT $ liftResourceT $ do
        let w = R.runReaderT (unDSTx d) (TxDatastoreConf cfg tx ns)
        W.runWriterT w
    when (req /= mempty) $
        txFinish tx (Just req)
    return a



withTx :: ( MonadResource m
          , DatastoreM m
          , HasScope '[AuthDatastore] ProjectsBeginTransaction
          )
       => (TxId -> m (a, Maybe CommitRequest))
       -> m (a, Maybe CommitResponse)
withTx f = do
    cfg <- getConf
    (rk,tx) <- allocate
        (runDatastore cfg txBeginUnsafe)
        (void . runDatastore cfg . txRollback)

    (a,maybeCommReq) <- f tx

    case maybeCommReq of
        Nothing  -> release rk >> return (a,Nothing)
        Just req -> do
            resp <- txCommit tx req
            _ <- unprotect rk
            return (a, Just resp)


txBegin :: ( MonadResource m
           , DatastoreM m
           , HasScope '[AuthDatastore] ProjectsBeginTransaction
           )
        => m TxToken
txBegin = do
    cfg <- getConf
    (rk,tx) <- allocate
        (runDatastore cfg txBeginUnsafe)
        (void . runDatastore cfg . txRollback)
    return $ TxToken tx rk

txFinish :: ( MonadResource m
            , DatastoreM m
            , HasScope '[AuthDatastore] ProjectsBeginTransaction
            )
         => TxToken
         -> Maybe CommitRequest
         -> m ()
txFinish (TxToken _ rk) Nothing = release rk
txFinish (TxToken tx rk) (Just req) = do
    _ <- txCommit tx req
    _ <- unprotect rk
    return ()




-- test :: IO ()
-- test cfg = do
--     tx <- runDatastore cfg $ do
--         runResourceT $
--             withTx' $ \tx -> do
--                 return (tx, Nothing)
--     putStrLn $ "Got this tx: " ++ show tx




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

