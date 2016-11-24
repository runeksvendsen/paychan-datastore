{-# OPTIONS_GHC -fno-warn-orphan-instances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Request.Commit where

import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import Types
import Util

import qualified Network.Google.Datastore as DS
import Network.Google.Datastore hiding (Entity, key)



mkInsert :: forall a anc. HasAncestor a anc => Ident anc -> a -> Tagged a DS.CommitRequest
mkInsert anc a = Tagged $ mutationReq
    [ mutation & mInsert ?~ unTagged (encodeEntity anc a :: Tagged a DS.Entity) ]

mkUpsert :: forall a anc. HasAncestor a anc => Ident anc -> a -> Tagged a DS.CommitRequest
mkUpsert anc a = Tagged $ mutationReq
    [ mutation & mUpsert ?~ unTagged (encodeEntity anc a :: Tagged a DS.Entity) ]

mkUpdate :: forall a anc. HasAncestor a anc => Ident anc -> a -> Tagged a DS.CommitRequest
mkUpdate anc a = Tagged $ mutationReq
    [ mutation & mUpdate ?~ unTagged (encodeEntity anc a :: Tagged a DS.Entity) ]


mkDelete :: forall a anc. HasAncestor a anc => Ident anc -> Ident a -> Tagged a DS.CommitRequest
mkDelete anc a = Tagged $ mutationReq
    [ mutation & mDelete ?~ unTagged (encodeKey anc a :: Tagged a DS.Key)]




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

