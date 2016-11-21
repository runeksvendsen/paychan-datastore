{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Request where

import DB.Model.Storable
import Util

import qualified Network.Google.Datastore as DS
import Network.Google.Datastore hiding (Entity, key)



insert :: IsStorable a => a -> DS.CommitRequest
insert a = mutationReq [ mutation & mInsert ?~ encodeEntity a ]

upsert :: IsStorable a => a -> DS.CommitRequest
upsert a = mutationReq [ mutation & mUpsert ?~ encodeEntity a ]

update :: IsStorable a => a -> DS.CommitRequest
update a = mutationReq [ mutation & mUpdate ?~ encodeEntity a ]

delete :: IsStorable a => a -> DS.CommitRequest
delete a = mutationReq [ mutation & mDelete ?~ encodeKey a ]

lookup :: IsStorable a => a -> DS.LookupRequest
lookup a = lookupRequest & lrKeys .~ [ encodeKey a ]



instance Monoid CommitRequest where
    mempty = mutationReq []
    mutReq1 `mappend` mutReq2 = mutationReq $
        (mutReq1 ^. crMutations) ++
        (mutReq2 ^. crMutations)

mutationReq :: [Mutation] -> CommitRequest
mutationReq mutL = commitRequest
    & crMutations .~ mutL
