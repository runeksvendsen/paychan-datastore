{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Mutate where

import           Types
import qualified Model.PayState    as State
import qualified Model.ChanIndex     as Open
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified Data.Text                      as T
import           Network.Google as Google
import           Network.Google.Datastore
import           Control.Lens ((?~), (&), (.~))



-- |Delete DB item. Fail if item doesn't exist.
deleteChan ::
            ( MonadGoogle s m
            , HasScope s CommitResponse)
           => ProjectId
           -> TxId
           -> Pay.SendPubKey
           -> m CommitResponse
deleteChan projectId tx key =
    let chanDeleteRequest = commitRequest
            & crMode ?~ Transactional
            & crTransaction ?~ tx
            & crMutations .~ [ mutation & mDelete ?~ State.mkKey projectId key
                             , mutation & mDelete ?~ Open.mkKey projectId key ]
    in
        Google.send (projectsCommit chanDeleteRequest projectId)


-- |Create new DB item. Fail if item already exists.
createChan :: (MonadGoogle s m
             , HasScope s CommitResponse)
           => ProjectId
           -> TxId
           -> Pay.RecvPayChan
           -> m CommitResponse
createChan projectId tx chan =
    let chanInsertRequest = commitRequest
            & crMode ?~ Transactional
            & crTransaction ?~ tx
            & crMutations .~ [ mutation & mInsert ?~ State.mkEntity projectId chan
                             , mutation & mInsert ?~ Open.mkEntity projectId chan ]
    in
        Google.send (projectsCommit chanInsertRequest projectId)


