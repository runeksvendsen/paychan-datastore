{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, ScopedTypeVariables#-}
module ChanDB.Creation where

import ChanDB.Orphans ()
import ChanDB.Types
import DB.Types
import DB.Tx.Safe
import DB.Util.Error
import DB.Model.Convert



insertChan :: -- DatastoreM m
              NamespaceId
           -> RecvPayChan
           -> Datastore (Tagged RecvPayChan CommitResponse)
insertChan nsId chan = do
    partId <- mkPartitionId nsId
    runReqWithTx (mkInsert (Just partId) root chan)

removeChan :: -- DatastoreM m
              NamespaceId
           -> SendPubKey
           -> Datastore (Tagged RecvPayChan CommitResponse)
removeChan nsId key = do
    partId <- mkPartitionId nsId
    runReqWithTx $ mkDelete (Just partId) root (getIdentifier key)


runReqWithTx :: -- forall a m.
--              ( DatastoreM m )
             Tagged a CommitRequest -> Datastore (Tagged a CommitResponse)
runReqWithTx commitReq =
    withTx ( const $ return ((), Just (unTagged commitReq)) ) >>=
        \(_,responseM) -> maybe
           (internalErrorM "runReqWithTx: 'withTx' did not return CommitResponse")
           return
           (Tagged <$> responseM)
