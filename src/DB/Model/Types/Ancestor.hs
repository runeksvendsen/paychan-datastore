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


data WithAncestor anc a = WithAncestor (Ident anc) a
    deriving (Eq, Show, Typeable)


instance (Identifier anc, HasKeyPath a) => HasKeyPath (WithAncestor anc a) where
    pathElems (WithAncestor anc i) =
        unTagged (toPathElem anc) : pathElems i

(<//>) :: Identifier ak
       => ak
       -> a
       -> WithAncestor anc a
anc <//> a = WithAncestor (ident anc) a


-- class ConcatIdent anc a where
--     (<//>) :: anc -> a -> [PathElement]

-- instance (Identifier anc, HasKeyPath a) => ConcatIdent anc a where
--     anc <//> a = identPathElem anc : pathElems a

-- instance (Identifier anc, Typeable a) => ConcatIdent anc (Ident a) where
--     anc <//> a = [ identPathElem anc, unTagged (toPathElem a) ]



-- instance Identifier a => HasKeyPath (WithAncestor Void a) where
--     pathElems (WithAncestor _ a) =
--         [ identPathElem a ]