{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Creation where

import           DB.Tx
import qualified Model.PayState    as State
import qualified Model.ChanIndex     as Open

import           Network.Google as Google
import           Network.Google.Datastore
import           Control.Lens ((?~), (&), (.~))


-- deleteChan :: (MonadGoogle s m)
--            => ProjectId
--            -> TxId
--            -> Pay.SendPubKey
--            -> m CommitResponse
removeChan projectId tx key =
    let chanDeleteRequest = commitRequest
            & crMode ?~ Transactional
            & crTransaction ?~ tx
            & crMutations .~
                [ mutation & mDelete ?~ State.mkKey projectId key
                , mutation & mDelete ?~ Open.mkKey  projectId key ]
    in
        txBeginUnsafe projectId >>= \tx -> Google.send
            (projectsCommit chanDeleteRequest projectId)


-- createChan :: (MonadGoogle s m)
--            => ProjectId
--            -> Pay.RecvPayChan
--            -> m CommitResponse
insertChan projectId chan =
    let mkInsertRequest tx = commitRequest
            & crMode ?~ Transactional
            & crTransaction ?~ tx
            & crMutations .~
                [ mutation & mInsert ?~ State.mkEntity projectId chan
                , mutation & mInsert ?~ Open.mkEntity  projectId chan ]
    in
        txBeginUnsafe projectId >>= \tx -> Google.send
            (projectsCommit (mkInsertRequest tx) projectId)
