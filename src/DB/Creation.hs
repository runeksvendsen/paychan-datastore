{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Creation where

import DB.Types
import DB.Tx.Safe
import DB.Util.Error
import DB.Model.Convert


--
-- dbInsert :: HasProperties a k => a -> m ()
-- dbInsert = error "STUB"
--
-- dbUpdate :: HasProperties a k => a -> m ()
-- dbUpdate = error "STUB"
--
-- dbRemove :: IsKey k  => k -> m ()
-- dbRemove = error "STUB"


insertChan :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
           => ProjectId
           -> RecvPayChan
           -> m CommitResponse
insertChan projectId chan =
    runReqWithTx projectId (mkInsert chan)
--     where insertRequest = commitRequest
--             & crMutations .~
--                 [ mutation & mInsert ?~ State.mkEntity projectId chan
--                 , mutation & mInsert ?~ Index.mkEntity  projectId chan ]

removeChan :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
           => ProjectId
           -> SendPubKey
           -> m CommitResponse
removeChan projectId key =
    runReqWithTx projectId (mkDelete $ mkChanKey key)

runReqWithTx :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
             => ProjectId -> CommitRequest -> m CommitResponse
runReqWithTx pid commitReq =
    withTx pid ( const $ return ((), Just commitReq) ) >>=
        \(_,responseM) -> maybe
           (internalErrorM "runReqWithTx: 'withTx' did not return CommitResponse")
           return
           responseM
