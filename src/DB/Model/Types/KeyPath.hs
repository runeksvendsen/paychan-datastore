module DB.Model.Types.KeyPath
(
  HasKeyPath(..)
, KeyWithAnc
, EntityKey(..)
, RootKey, VoidKey
, rootIdent
, (<//>)
, module Idn
)
where

import LibPrelude
import DB.Model.Types
import DB.Error.Util
import DB.Model.Convert.Identifier as Idn
import qualified Network.Google.Datastore.Types as DS


class (Show a, Typeable a) => HasKeyPath a where
    -- | Root key is right-most list element: @/a/b/c -> [c,b,a]@
    pathElems :: a -> [PathElement]
    -- | Parse element(s) from the list, return leftovers
    parseElems :: [PathElement] -> Either DBException (a, [PathElement])

instance HasKeyPath PathElement where
    pathElems = (: [])
    parseElems (pe:peL) = Right (pe, peL)
    parseElems [] = Left $ UserError $ ParseFail "end-of-input."

instance Typeable a => HasKeyPath (Ident a) where
    pathElems = pathElems . unTagged . toPathElem
    parseElems (pe:peL) = parseIdent pe >>= \ident -> Right (ident, peL)
    parseElems [] = Left $ UserError $ ParseFail $ "end-of-input." ++
        " Expected element of kind " ++ show (typeOf (undefined :: a))
--
instance HasKeyPath Void where
    pathElems = const []
    parseElems peL = Right (error "HasKeyPath Void parse", peL)


-- | An identifier with an ancestor, which itself can be a 'KeyWithAnc'
data KeyWithAnc a anc = KeyWithAnc (Ident a) anc
    deriving (Eq, Show, Typeable)

instance (Typeable a, HasKeyPath anc) => HasKeyPath (KeyWithAnc a anc) where
    pathElems (KeyWithAnc a anc) =
        pathElems a ++ pathElems anc
    parseElems [] = Left $ UserError $ ParseFail $ "end-of-input." ++
        " Expected element of kind " ++ show (typeOf (undefined :: a))
    parseElems (pe:peL) = parseIdent pe >>= \ident ->
            parseElems peL >>= \(anc, peL') -> Right (KeyWithAnc ident anc, peL')

-- | Same as 'KeyWithAnc', but only knows the kind/type of the entity.
data EntityKey a = EntityKey [PathElement]
    deriving (Eq, Show)

instance Typeable a => HasKeyPath (EntityKey a) where
    pathElems (EntityKey peL) = peL
    parseElems [] = Left $ InternalError $ ParseError "HasKeyPath EntityKey: Empty PathElement list"
    parseElems peL = checkKind peL >>=
        \l -> Right (EntityKey l, [])
        where
            typeStr = cs $ show (typeOf (undefined :: a))
            checkKind l = maybe
                (Left $ InternalError $ ParseError $ "Missing kind. Expected: " ++ show typeStr)
                Right (last l ^. DS.peKind) >>= \k ->
                    if k == typeStr then
                        Right l
                    else
                        Left $ InternalError $ ParseError $ "Type mismatch. Found " ++ cs k ++ " expected " ++ cs typeStr

rootIdent :: forall a. Typeable a => Ident a -> EntityKey a
rootIdent = EntityKey . mkList . unTagged . toPathElem
    where mkList a = [a]

(<//>) :: Identifier a
       => anc
       -> a
       -> KeyWithAnc b anc
anc <//> a = KeyWithAnc (ident a) anc




type RootKey a = KeyWithAnc a Void     -- ^ Key for root entity without an ancestor
type VoidKey = KeyWithAnc Void (Ident Void)    -- ^ The uninhabited key (doesn't exist)
