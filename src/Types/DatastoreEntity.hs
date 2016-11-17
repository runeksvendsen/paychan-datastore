module DatastoreEntity where

import qualified Network.Google.Datastore as DS

class DatastoreEntity a where
    deKey :: DS.Key