{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, ScopedTypeVariables#-}
module ChanDB.Creation where

import ChanDB.Orphans ()
import ChanDB.Types
import DB.Types
import DB.Tx.Safe
import DB.Util.Error
import DB.Model.Convert

import Debug.Trace


insertChan :: HasScope '[AuthDatastore] ProjectsBeginTransaction =>
              NamespaceId
           -> RecvPayChan
           -> Datastore (Tagged RecvPayChan CommitResponse)
insertChan nsId chan =
    mkMutation nsId
        (Insert $ EntityWithAnc chan root) >>=
            \mut -> ("mkMut: " ++ show mut) `trace` return mut >>=
            runReqWithTx . Tagged


removeChan :: HasScope '[AuthDatastore] ProjectsBeginTransaction =>
              NamespaceId
           -> SendPubKey
           -> Datastore (Tagged RecvPayChan CommitResponse)
removeChan nsId key = do
    let fullKey = key <//> root :: RootKey RecvPayChan
    partId <- mkPartitionId nsId
    runReqWithTx $ mkDelete
        (Just partId)
        fullKey


runReqWithTx :: HasScope '[AuthDatastore] ProjectsBeginTransaction =>
    Tagged a CommitRequest -> Datastore (Tagged a CommitResponse)
runReqWithTx commitReq =
    withTx ( const $ return ((), Just (unTagged commitReq)) ) >>=
        \(_,responseM) -> maybe
           (internalErrorM "runReqWithTx: 'withTx' did not return CommitResponse")
           return
           (Tagged <$> responseM)
