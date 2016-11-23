{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, ScopedTypeVariables#-}
module DB.Creation where

import DB.Types
import DB.Tx.Safe
import DB.Util.Error
import DB.Model.Convert



insertChan :: ( MonadGoogle '[AuthDatastore] m
              , HasScope '[AuthDatastore] ProjectsBeginTransaction )
           => ProjectId
           -> RecvPayChan
           -> m (Tagged RecvPayChan CommitResponse)
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
           -> m (Tagged RecvPayChan CommitResponse)
removeChan projectId key =
    runReqWithTx projectId (mkDelete key)

runReqWithTx :: forall a m.
             ( MonadGoogle '[AuthDatastore] m
             , HasScope '[AuthDatastore] ProjectsBeginTransaction )
             => ProjectId -> Tagged a CommitRequest -> m (Tagged a CommitResponse)
runReqWithTx pid commitReq =
    withTx pid ( const $ return ((), Just (unTagged commitReq)) ) >>=
        \(_,responseM) -> maybe
           (internalErrorM "runReqWithTx: 'withTx' did not return CommitResponse")
           return
           (Tagged <$> responseM)
