module DB.Request.Commit where

import LibPrelude
import DB.Types
import DB.Model.Convert
import DB.Request.Send

import qualified Network.Google.Datastore     as DS


-- | Commit an arbitrary number of mutations,
--    respecting the 500-entities-per-request limit.
--   https://cloud.google.com/datastore/docs/concepts/limits
multiCommit :: ( DatastoreM m
               , HasScope '[AuthDatastore] ProjectsCommit
               )
            => [DS.Mutation]
            -> m ()
multiCommit mutL =
    doChunksM 500 mutL doReq
  where
    doReq = void . sendReq . projectsCommit . mutationReq
    doChunksM _   []  _ = return ()
    doChunksM len lst f =
        case splitAt len lst of
            (l,remL) -> f l >> doChunksM len remL f

