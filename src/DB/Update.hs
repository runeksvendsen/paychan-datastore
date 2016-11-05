{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Update
(
  withDBState
)
where

import           DB.Types
import           DB.Tx
import           DB.Fetch
import           Model.PayState (parseLookupRes, mkEntity)

import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import           Network.Google as Google
import           Network.Google.Datastore

import           Control.Monad.IO.Class     (liftIO)
-- import qualified Control.Monad.Catch as      Except
import           Control.Exception      as Except
import           Data.Maybe                 (fromMaybe)
import           Control.Lens


-- |Provide a function which can do anything it wants with a RecvPayChan from the DB,
--  and return either a Left to indicate that no update should take place, or Right to
--  indicate that the DB entity should be udated to the provided value.
--  Throws an exception if the channel doesn't exist.
withDBState projectId sendPK f = do
    tx <- txBeginUnsafe projectId

    (eitherRes,oldVer) <- fmap errorOnNothing .
        (liftIO . (`Except.onException` txRollback projectId tx)) $ do
                maybeRes <- txLookup projectId sendPK tx
                case parseLookupRes maybeRes of
                    Just (chan,ver) ->
                        liftIO (f chan) >>= \res -> return $ Just (res,ver)
                    Nothing         -> return   Nothing

    updStatus <- case eitherRes of
        Left _        -> txRollback projectId tx >> return NotUpdated
        Right newChan -> dbStateUpdate projectId tx oldVer newChan

    return eitherRes
  where
    errorOnNothing = fromMaybe (Except.throw NoSuchChannel)

-- |Commit. Finish transaction by updating DB item.
dbStateUpdate projectId tx prevVer recvChan = do
    commRes <- txCommit projectId tx recvChan
    case commRes ^. crMutationResults of
        [r]       -> return $ if getMRVersion r > prevVer then Updated else NotUpdated
        []        -> return NotUpdated
        n@(_:_:_) -> Except.throw . InternalError $
            "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
  where
    getMRVersion r = fromMaybe
        (throw . InternalError $ "MutationResult: empty version field")
        (r ^. mrVersion)

txCommit projectId tx chan =
    Google.send (projectsCommit commReq projectId)
  where
    commReq = commitRequest
        & crMutations .~ [mutation & mUpdate ?~ mkEntity projectId chan]
        & crMode ?~ Transactional
        & crTransaction ?~ tx

