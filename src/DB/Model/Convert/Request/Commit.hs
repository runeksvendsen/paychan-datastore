{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Commit where

import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Util

import qualified Network.Google.Datastore as DS
import Network.Google.Datastore hiding (Entity, key)



mkInsert :: (HasAncestors a, IsEntity a) => a -> DS.CommitRequest
mkInsert a = mutationReq [ mutation & mInsert ?~ encodeEntity a ]

mkUpsert :: (HasAncestors a, IsEntity a) => a -> DS.CommitRequest
mkUpsert a = mutationReq [ mutation & mUpsert ?~ encodeEntity a ]

mkUpdate :: (HasAncestors a, IsEntity a) => a -> DS.CommitRequest
mkUpdate a = mutationReq [ mutation & mUpdate ?~ encodeEntity a ]

mkDelete :: HasAncestors a => a -> DS.CommitRequest
mkDelete a = mutationReq [ mutation & mDelete ?~ encodeKey a ]




instance Monoid CommitRequest where
    mempty = mutationReq []
    mutReq1 `mappend` mutReq2 = mutationReq $
        (mutReq1 ^. crMutations) ++
        (mutReq2 ^. crMutations)

mutationReq :: [Mutation] -> CommitRequest
mutationReq mutL = commitRequest
    & crMutations .~ mutL

-- -- |Check entity version.
-- checkCommResponse :: EntityVersion -> CommitResponse -> UpdateResult
-- checkCommResponse prevVer commResp =
--     case commResp ^. crMutationResults of
--         [r]       -> if getMRVersion r > prevVer then Updated else NotUpdated
--         []        -> NotUpdated
--         n@(_:_:_) -> throw . InternalError $
--             "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
--   where
--     getMRVersion r = fromMaybe
--         (throw . InternalError $ "MutationResult: empty version field")
--         (r ^. mrVersion)

