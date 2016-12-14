{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, ScopedTypeVariables#-}
module ChanDB.Creation where

import DB.Types
import DB.Tx.Safe
import DB.Util.Error
import DB.Model.Convert


insertChan :: ( MonadGoogle '[AuthDatastore] m
              , HasScope '[AuthDatastore] ProjectsBeginTransaction )
           => NamespaceId
           -> RecvPayChan
           -> m (Tagged RecvPayChan CommitResponse)
insertChan nsId chan =
    runReqWithTx nsId (mkInsert nsId root chan)

removeChan :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction
              )
           => NamespaceId
           -> SendPubKey
           -> m (Tagged RecvPayChan CommitResponse)
removeChan nsId key =
    runReqWithTx nsId $ mkDelete nsId root (getIdentifier key)

runReqWithTx :: forall a m.
             ( MonadGoogle '[AuthDatastore] m
             , HasScope '[AuthDatastore] ProjectsBeginTransaction )
             => NamespaceId -> Tagged a CommitRequest -> m (Tagged a CommitResponse)
runReqWithTx ns commitReq =
    withTx ns ( const $ return ((), Just (unTagged commitReq)) ) >>=
        \(_,responseM) -> maybe
           (internalErrorM "runReqWithTx: 'withTx' did not return CommitResponse")
           return
           (Tagged <$> responseM)
