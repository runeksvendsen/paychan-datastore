module DB.Model.Types.Key where

import Util
import DB.Types


class IsKey a where
    getKey :: a -> Key

instance IsKey Key where
    getKey = id

data WithAncestor k anc = WithAncestor k anc

instance (IsKey k, IsKey anc) => IsKey (WithAncestor k anc) where
    getKey (WithAncestor k anc) =
        getKey k & kPath .~
          ( getKey anc ^. kPath ++ getKey k ^. kPath )
