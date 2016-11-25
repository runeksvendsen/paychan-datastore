{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Update
(
  withDBState
)
where

import           Util
import           DB.Types
import           DB.Tx.Safe
import           DB.Tx.Lookup   (txLookup, parseLookupRes)
import           DB.Model.Convert

import           Network.Google as Google
import qualified Control.Exception        as Except
import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)

import Debug.Trace


-- |Provide a function which can do anything it wants with a RecvPayChan from the DB,
--  and return either a Left to indicate that no update should take place, or Right to
--  indicate that the DB entity should be udated to the provided value.
--  Throws an exception if the channel doesn't exist.
withDBState :: ( MonadCatch m
               , MonadGoogle '[AuthDatastore] m
               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
            => ProjectId
            -> SendPubKey
            -> (RecvPayChan -> IO (Either PayChanError (RecvPayChan,Note)))
            -> m (Either PayChanError RecvPayChan)
withDBState pid sendPK f = do
    (eitherRes,_) <- withTx pid $ \tx -> do
        resE <- txLookup pid tx root (getIdentifier sendPK)
        ((chan,_),_) <- case resE of
            Right resL -> return $ head resL
            Left e     -> error e
        eitherRes <- liftIO (f chan)
        case eitherRes of
            Left  _        -> return (eitherRes, Nothing)
            Right (newState,note) ->
                return ( eitherRes
                       , Just $ unTagged $ traceCommit $
                            mkUpdate root newState </>
                            mkInsert (getIdent newState) note
                       )
    return $ case eitherRes of
        Right (state,_) -> Right state
        Left e -> Left e


traceCommit :: forall a. Show a => a -> a
traceCommit c = show c `trace` c

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



