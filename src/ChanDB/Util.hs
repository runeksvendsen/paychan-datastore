module ChanDB.Util where

import LibPrelude
import           DB.Types
import           DB.Tx.Safe
import           DB.Request                 (txLookup, getFirstResult)
import           DB.Model.Convert

import           Control.Exception          (throw)
import           Data.Maybe                 (fromMaybe)




traceCommit :: forall a. Show a => a -> a
traceCommit c = show c `trace` c

-- Check entity version
-- NB: Versioning disabled.
-- When updating to >= gogol-datastore-0.1.1, return actual version using 'mrVersion'

{-
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

        (Just 1)-- (r ^. mrVersion)
-}
