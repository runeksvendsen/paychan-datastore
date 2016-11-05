{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Creation where

import Util
import Network.Google as Google

import           DB.Tx
import qualified Model.PayState    as State
import qualified Model.ChanIndex     as Open


insertChan :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction
              ,    HasScope '[AuthDatastore] ProjectsRollback
              ,    HasScope '[AuthDatastore] ProjectsCommit )
           => ProjectId
           -> RecvPayChan
           -> m CommitResponse
insertChan projectId chan =
    runReqWithTx projectId insertRequest
    where insertRequest = commitRequest
            & crMutations .~
                [ mutation & mInsert ?~ State.mkEntity projectId chan
                , mutation & mInsert ?~ Open.mkEntity  projectId chan ]

removeChan :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction
              ,    HasScope '[AuthDatastore] ProjectsRollback
              ,    HasScope '[AuthDatastore] ProjectsCommit )
           => ProjectId
           -> SendPubKey
           -> m CommitResponse
removeChan projectId key =
    runReqWithTx projectId  chanDeleteRequest
    where chanDeleteRequest = commitRequest
            & crMutations .~
                [ mutation & mDelete ?~ State.mkKey projectId key
                , mutation & mDelete ?~ Open.mkKey  projectId key ]

runReqWithTx :: ( MonadGoogle '[AuthDatastore] m
              ,    HasScope '[AuthDatastore] ProjectsBeginTransaction
              ,    HasScope '[AuthDatastore] ProjectsRollback
              ,    HasScope '[AuthDatastore] ProjectsCommit )
             => ProjectId -> CommitRequest -> m CommitResponse
runReqWithTx pid commitReq =
    withTx pid ( const $ return ((), Just commitReq) ) >>=
        \(_,maybeResp) -> return $ fromMaybe
           (throw $ InternalError "runReqWithTx: 'withTx' did not return CommitResponse")
           maybeResp
