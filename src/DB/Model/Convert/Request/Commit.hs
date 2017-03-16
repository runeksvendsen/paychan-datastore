{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module DB.Model.Convert.Request.Commit where

import DB.Model.Types.Entity
import DB.Model.Convert.Entity
import DB.Types
import LibPrelude

import qualified Network.Google.Datastore     as DS
import qualified Control.Monad.Writer.Strict  as W

data Insert a = Insert a
data Upsert a = Upsert a
data Update a = Update a


class HasMutLens mut where
    getLens :: mut -> Lens' Mutation (Maybe Entity)

instance HasMutLens (Insert a) where
    getLens = const mInsert

instance HasMutLens (Upsert a) where
    getLens = const mUpsert

instance HasMutLens (Update a) where
    getLens = const mUpdate


class (HasMutLens mut, IsEntity b) => IsMutation mut b | mut -> b where
    getEnt :: mut -> b

instance IsEntity a => IsMutation (Insert a) a where
    getEnt (Insert a) = a

instance IsEntity a => IsMutation (Upsert a) a where
    getEnt (Upsert a) = a

instance IsEntity a => IsMutation (Update a) a where
    getEnt (Update a) = a


mkMutation :: (DatastoreM m, IsMutation mut b)
           => NamespaceId
           -> mut
           -> m DS.CommitRequest
mkMutation ns m = do
    partId <- mkPartitionId ns
    return $ mutationReq
        [ mutation & getLens m ?~ mkEntity (Just partId) (getEnt m) ]

mkDelete :: HasKeyPath k => Maybe PartitionId -> k -> Tagged a DS.CommitRequest
mkDelete partM k = Tagged $ mutationReq [mkDeleteMut partM k]

mkDeleteMut :: HasKeyPath k => Maybe PartitionId -> k -> DS.Mutation
mkDeleteMut partM k = mutation &
    mDelete ?~ unTagged (encodeKeyPath partM k :: Tagged a DS.Key)

mkDeleteM :: (DatastoreM m, HasKeyPath k) => NamespaceId -> k -> m DS.Mutation
mkDeleteM ns key = do
    partId <- mkPartitionId ns
    return $ mkDeleteMut (Just partId) key

commitAdd :: (DatastoreTxM m, IsMutation mut e) => mut -> m ()
commitAdd mut = do
    ns  <- getNSId
    mut <- mkMutation ns mut
    W.tell mut




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

