module DB.Model.Types.KeyPath
(
  HasKeyPath(..)
, WithAncestor
, EntityKey(..)
, RootKey, VoidKey
, (<//>)
, module Idn
)
where

import Util
import DB.Model.Types
import DB.Model.Convert.Identifier as Idn
import qualified Network.Google.Datastore.Types as DS


class (Show a, Typeable a) => HasKeyPath a where
    pathElems :: a -> [PathElement]
    -- | Parse element(s) from the list, return leftovers
    parseElems :: [PathElement] -> Either String (a, [PathElement])

instance HasKeyPath PathElement where
    pathElems = (: [])
    parseElems (pe:peL) = Right (pe, peL)
    parseElems [] = Left "Parse fail: end-of-input."

instance Typeable a => HasKeyPath (Ident a) where
    pathElems = pathElems . unTagged . toPathElem
    parseElems (pe:peL) = parseIdent pe >>= \ident -> Right (ident, peL)
    parseElems [] = Left $ "Parse fail: end-of-input." ++
        " Expected element of kind " ++ show (typeOf (undefined :: a))
--
instance HasKeyPath Void where
    pathElems = const []
    parseElems peL = Right (error "Void parse", peL)


-- | An identifier with an ancestor, which itself can be a 'WithAncestor'
data WithAncestor a anc = WithAncestor (Ident a) anc
    deriving (Eq, Show, Typeable)

instance (Typeable a, HasKeyPath anc) => HasKeyPath (WithAncestor a anc) where
    pathElems (WithAncestor a anc) =
        pathElems a ++ pathElems anc
    parseElems (pe:peL) = parseIdent pe >>= \ident ->
        parseElems peL >>= \(anc, peL') -> Right (WithAncestor ident anc, peL')
    parseElems [] = Left $ "Parse fail: end-of-input." ++
        " Expected element of kind " ++ show (typeOf (undefined :: a))

-- instance IsKind a => HasKeyPath (WithAncestor a (Ident Void)) where
--     pathElems (WithAncestor a _) =
--         pathElems a
--     parseElems (idnt:lo) = parseIdent idnt >>= \ident ->
--         Right (WithAncestor ident root, lo)
--     parseElems [] = Left $ "Parse fail: end-of-input." ++
--         " Expected element of kind " ++ show (typeOf (undefined :: a))


-- | Same as 'WithAncestor', but only knows the kind/type of the entity (not the key kinds).
--  Allows us to return many different key paths pointing to the same
--   entity type.
data EntityKey a = EntityKey [PathElement]
    deriving (Eq, Show)

instance Typeable a => HasKeyPath (EntityKey a) where
    pathElems (EntityKey peL) = peL
    parseElems [] = Left "Empty PathElement list"
    parseElems peL = checkKind peL >>=
        \l -> Right (EntityKey l, [])
        where
            typeStr = cs $ show (typeOf (undefined :: a))
            checkKind l = maybe
                (Left $ "Missing kind. Expected: " ++ show typeStr)
                Right (last l ^. DS.peKind) >>= \k ->
                    if k == typeStr then
                        Right l
                    else
                        Left $ "Type mismatch. Found " ++ cs k ++ " expected " ++ cs typeStr




(<//>) :: Identifier a
       => a
       -> anc
       -> WithAncestor b anc
anc <//> a = WithAncestor (ident anc) a




type RootKey a = WithAncestor a Void     -- ^ Key for root entity without an ancestor
type VoidKey = WithAncestor Void (Ident Void)    -- ^ The uninhabited key (doesn't exist)
