{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Update
(
  withDBState
)
where

import           Util
import           DB.Tx
import           DB.Lookup
import           Model.PayState (parseLookupRes, mkEntity)

import           Network.Google as Google

import qualified Control.Exception        as Except
import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)


-- |Provide a function which can do anything it wants with a RecvPayChan from the DB,
--  and return either a Left to indicate that no update should take place, or Right to
--  indicate that the DB entity should be udated to the provided value.
--  Throws an exception if the channel doesn't exist.
withDBState :: ( MonadCatch m
               , MonadGoogle '[AuthDatastore] m
               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => ProjectId
            -> SendPubKey
            -> (RecvPayChan -> IO (Either PayChanError RecvPayChan))
            -> m (Either PayChanError RecvPayChan)
withDBState pid sendPK f = do
    (eitherRes,_) <- withTx pid $ \tx -> do
        (chan,_) <- (errorOnNothing . parseLookupRes) <$> txLookup pid sendPK tx
        eitherRes <- liftIO (f chan)
        case eitherRes of
            Left _        -> return (eitherRes, Nothing)
            Right newChan -> return (eitherRes, Just $ mkCommitReq newChan)
    return eitherRes
  where
    errorOnNothing = fromMaybe (Except.throw NoSuchChannel)
    mkCommitReq chan = commitRequest
            & crMutations .~ [mutation & mUpdate ?~ mkEntity pid chan]

-- |Check entity version.
checkCommResponse :: EntityVersion -> CommitResponse -> UpdateResult
checkCommResponse prevVer commResp =
    case commResp ^. crMutationResults of
        [r]       -> if getMRVersion r > prevVer then Updated else NotUpdated
        []        -> NotUpdated
        n@(_:_:_) -> throw . InternalError $
            "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
  where
    getMRVersion r = fromMaybe
        (throw . InternalError $ "MutationResult: empty version field")
        (r ^. mrVersion)



