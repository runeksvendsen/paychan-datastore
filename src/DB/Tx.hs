{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Tx where


import           Types
import           DB.Mutate
import           DB.Fetch
import           Model.PayState
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import qualified Control.Monad as M
import qualified Control.Exception as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Catch    (MonadCatch)
import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy (Proxy)
import           Control.Lens
import           System.IO              (stderr)
import           Test.QuickCheck (Gen, sample', vectorOf, choose, generate)
import           Data.String.Conversions          (cs)

import qualified Network.HTTP.Conduit as HTTP




data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show


-- |Provide a function which can do anything it wants with a RecvPayChan from the DB,
--  and return either a Left to indicate that no update should take place, or Right to
--  indicate that the DB entity should be udated to the provided value.
--  Throws an exception if the channel doesn't exist.
withDBState sendPK f = do
    tx <- txBeginUnsafe
    (eitherRes,oldVer) <- errorOnNothing <$>
        (`Except.onException` rollback tx) $ lookupEval tx
    updStatus <- case eitherRes of
        Left _        -> txRollback tx >> return NotUpdated
        Right newChan -> dbStateUpdate tx oldVer newChan
    return (eitherRes, updStatus)
  where
    lookupEval tx = do
        maybeRes <- txLookup sendPK tx
        case maybeRes of
            Just (chan,ver) -> return Just (f chan,ver)
            Nothing         -> return Nothing
    rollback tx = putStrLn "withDBState: Exception. Running cleanup" >> txRollback env tx
    errorOnNothing = maybe (Except.throw NoSuchChannel)

-- |Finish transaction by updating DB item (commit)
dbStateUpdate tx prevVer recvChan = do
    commRes <- txCommit tx recvChan
    case commRes ^. crMutationResults of
        [r]       -> return $ if getMRVersion r > prevVer then Updated else NotUpdated
        []        -> return NotUpdated
        n@(_:_:_) -> throw . InternalError $
            "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
  where
    getMRVersion r = fromMaybe (throw . InternalError $ "MutationResult: empty version field") $
        r ^. mrVersion

-- |Finish with update
txCommit tx chan =
    Google.send (projectsCommit comm projectId)
  where
    comm = commitRequest
        & crMutations .~ [mutation & mUpdate ?~ mkEntity projectId chan]
        & crMode ?~ Transactional
        & crTransaction ?~ tx

-- |Finish transaction without doing anything (rollback)
txRollback tx =
    Google.send (projectsRollback rollbackReq projectId)
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx

-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
txBeginUnsafe = do
    txBeginRes <- Google.send (projectsBeginTransaction beginTransactionRequest projectId)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> throw . InternalError $
                "CloudStore API BUG: Transaction identifier should always be present"
