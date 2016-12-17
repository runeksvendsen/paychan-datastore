module DB.Model.Types.Ancestor where

import Util
import DB.Model.Types
import DB.Model.Convert.Identifier


class Typeable a => HasKeyPath a where
    pathElems :: a -> [PathElement]

instance HasKeyPath PathElement where
    pathElems = (: []) . id

instance Typeable a => HasKeyPath (Ident a) where
    pathElems = pathElems . unTagged . toPathElem


-- data TheEntity a = TheEntity a
--     deriving (Eq, Show, Typeable)

-- instance Identifier a => HasKeyPath (TheEntity a) where
--     pathElems (TheEntity i) = pathElems $ identPathElem i


data WithAncestor anc a = WithAncestor anc a
    deriving (Eq, Show, Typeable)

instance Identifier a => HasKeyPath (WithAncestor Void a) where
    pathElems (WithAncestor _ a) =
        [ identPathElem a ]

-- instance (Identifier anc, Typeable a) => HasKeyPath (WithAncestor anc (Ident a)) where
--     pathElems (WithAncestor anc i) =
--         [ identPathElem anc , unTagged $ toPathElem i ]

instance (Identifier anc, HasKeyPath a) => HasKeyPath (WithAncestor anc a) where
    pathElems (WithAncestor anc i) =
        identPathElem anc : pathElems i

