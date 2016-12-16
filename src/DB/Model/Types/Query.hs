module DB.Model.Types.Query where

import Util
import DB.Types
import DB.Model.Convert.Value.Native
import DB.Model.Convert.Identifier


class IsQuery a where
    mkQuery :: a -> Query

instance IsQuery Query where
    mkQuery = id


-- | A query of specified kind ('Ident')
data OfKind k q = OfKind (Ident k) q

instance (IsQuery q, Identifier k) => IsQuery (OfKind k q) where
    mkQuery (OfKind k q) =
        mkQuery q & qKind .~ ( gqlKind k : mkQuery q ^. qKind )


-- | Turns any 'Query' into a keys-only query
data KeysOnlyQuery q = KeysOnly q

instance IsQuery q => IsQuery (KeysOnlyQuery q) where
    mkQuery (KeysOnly q) = mkQuery q &
        -- Keys-only query:
        -- https://cloud.google.com/datastore/docs/concepts/queries#datastore_keys_only_query_gql
        qProjection .~ [ projection & pProperty ?~ (propertyReference & prName ?~ "__key__") ]


data AncestorQuery anc q = AncestorQuery (Ident anc) q

instance (IsQuery q, Identifier anc) => IsQuery (AncestorQuery anc q) where
    mkQuery (AncestorQuery anc q) =
        addFilter (mkQuery q) propFilter
      where
        propFilter = propertyFilter
            & pfProperty ?~ (propertyReference & prName ?~ "__key__")
            & pfOp ?~ PFOHasAncestor
            & pfValue ?~ encode (unTagged $ identKey anc)


data FilterProperty v q = FilterProperty Text PropertyFilterOp v q

instance (IsQuery q, NativeValue v) => IsQuery (FilterProperty v q) where
    mkQuery (FilterProperty prop op v q) =
        addFilter (mkQuery q) propFilter
      where
        propFilter = propertyFilter
            & pfProperty ?~ (propertyReference & prName ?~ prop)
            & pfOp ?~ op
            & pfValue ?~ encode v

addFilter :: Query -> PropertyFilter -> Query
addFilter q pf =
    q & qFilter ?~ (fromMaybe filter' currentFilterM & fCompositeFilter ?~ newCompFilter )
  where
    newCompFilter = compositeFilter & cfOp ?~ And & cfFilters .~ (newFilter : oldCompFilters)
    newFilter = filter' & fPropertyFilter ?~ pf
    currentFilterM = mkQuery q ^. qFilter
    oldCompFilters = fromMaybe [] $
      currentFilterM >>=
      (^. fCompositeFilter) >>=
      (Just . (^. cfFilters))

data OrderBy q = OrderBy Text PropertyOrderDirection q

instance IsQuery q => IsQuery (OrderBy q) where
    mkQuery (OrderBy prop ord q) =
        mkQuery q & qOrder .~ ( propOrd : mkQuery q ^. qOrder )
      where
        propOrd = propertyOrder
            & poProperty ?~ (propertyReference & prName ?~ prop)
            & poDirection ?~ ord


data StartAtCursor q = StartAtCursor Cursor q

instance IsQuery a => IsQuery (StartAtCursor a) where
    mkQuery (StartAtCursor c q) = mkQuery q & qStartCursor ?~ c

