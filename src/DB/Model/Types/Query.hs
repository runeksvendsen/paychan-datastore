module DB.Model.Types.Query where

import LibPrelude
import DB.Types
import DB.Model.Convert.Value
import DB.Model.Convert.Identifier


-- | Something that represents a Datastore query
class IsQuery a where
    mkQuery :: PartitionId  -- ^ The 'PartitionId' in which the query will execute
            -> a            -- ^ The query
            -> Query        -- ^ Datastore 'Query'

instance IsQuery Query where
    mkQuery _ = id


-- | A query of specified kind ('Ident')
data OfKind k q = OfKind k q

instance (IsQuery q, Typeable k) => IsQuery (OfKind k q) where
    mkQuery p (OfKind k q) =
        mkQuery p q & qKind .~ ( gqlKind k : mkQuery p q ^. qKind )


-- | Turns any 'Query' into a keys-only query
data KeysOnly q = KeysOnly q

instance IsQuery q => IsQuery (KeysOnly q) where
    mkQuery p (KeysOnly q) = mkQuery p q &
        -- Keys-only query:
        -- https://cloud.google.com/datastore/docs/concepts/queries#datastore_keys_only_query_gql
        qProjection .~ [ projection & pProperty ?~ (propertyReference & prName ?~ "__key__") ]


-- | Ancestor queries are strongly consistent,
--  whereas global queries are eventually consistent.
data AncestorQuery anc q = AncestorQuery (Ident anc) q

instance (IsQuery q, Typeable anc) => IsQuery (AncestorQuery anc q) where
    mkQuery p (AncestorQuery anc q) =
        addFilter p (mkQuery p q) propFilter
      where
        ancKey = unTagged (identKey anc) & kPartitionId ?~ p
        propFilter = propertyFilter
            & pfProperty ?~ (propertyReference & prName ?~ "__key__")
            & pfOp ?~ PFOHasAncestor
            & pfValue ?~ encode ancKey


data FilterProperty v q = FilterProperty Text PropertyFilterOp v q

instance (IsQuery q, ToValue v) => IsQuery (FilterProperty v q) where
    mkQuery p (FilterProperty prop op v q) =
        addFilter p (mkQuery p q) propFilter
      where
        propFilter = propertyFilter
            & pfProperty ?~ (propertyReference & prName ?~ prop)
            & pfOp ?~ op
            & pfValue ?~ toValue v

addFilter :: PartitionId -> Query -> PropertyFilter -> Query
addFilter p q pf =
    q & qFilter ?~ (fromMaybe filter' currentFilterM & fCompositeFilter ?~ newCompFilter )
  where
    newCompFilter = compositeFilter & cfOp ?~ And & cfFilters .~ (newFilter : oldCompFilters)
    newFilter = filter' & fPropertyFilter ?~ pf
    currentFilterM = mkQuery p q ^. qFilter
    oldCompFilters = fromMaybe [] $
      currentFilterM >>=
      (^. fCompositeFilter) >>=
      (Just . (^. cfFilters))

data OrderBy q = OrderBy Text PropertyOrderDirection q

instance IsQuery q => IsQuery (OrderBy q) where
    mkQuery p (OrderBy prop ord q) =
        mkQuery p q & qOrder .~ ( propOrd : mkQuery p q ^. qOrder )
      where
        propOrd = propertyOrder
            & poProperty ?~ (propertyReference & prName ?~ prop)
            & poDirection ?~ ord


data StartAtCursor q = StartAtCursor Cursor q

instance IsQuery a => IsQuery (StartAtCursor a) where
    mkQuery p (StartAtCursor c q) = mkQuery p q & qStartCursor ?~ c

