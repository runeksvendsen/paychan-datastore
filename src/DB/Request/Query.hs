{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert
import           Util


runQueryReq ::
            Maybe TxId
         -> Tagged a RunQueryRequest
         -> Datastore ( Tagged (a,anc) RunQueryResponse )
runQueryReq txM req = Tagged <$> sendReq (projectsRunQuery txReq)
    where
        txReq = maybe (unTagged req) applyTxId txM
        applyTxId tx = unTagged req & rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)



-- | Ancestor queries are strongly consistent,
--  whereas global queries are eventually consistent.
txAncestorQuery :: forall a anc.
           HasAncestor a anc
          => Maybe NamespaceId
          -> TxId
          -> Ident anc
          -> Text
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
txAncestorQuery nsM tx anc query = do
    partIdM <- maybe (return Nothing) (fmap Just . mkPartitionId) nsM
    parseQueryRes <$> runQueryReq (Just tx) (mkReq partIdM)
        where
            mkReq pM = mkAncQueryReq pM anc query :: Tagged a RunQueryRequest


globalQuery :: forall a anc.
            HasAncestor a anc
          => Maybe NamespaceId
          -> Text
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
globalQuery nsM query = do
    partIdM <- maybe (return Nothing) (fmap Just . mkPartitionId) nsM
    parseQueryRes <$> runQueryReq Nothing (mkQueryReq partIdM query)



keysOnlyQuery :: forall a anc.
                HasAncestor a anc
              => Maybe NamespaceId
              -> Maybe Text
              -> Datastore ( Either String [ (Ident a, Ident anc) ] )
keysOnlyQuery nsM filterM = do
    partIdM <- maybe (return Nothing) (fmap Just . mkPartitionId) nsM
    let maybeWhere = maybe "" (" WHERE " <>) filterM
    let completeQuery = gqlSelectString "__key__" (undefined :: Ident a) <> maybeWhere
    identE <- parseQueryResKeys <$> runQueryReq Nothing (mkQueryReq partIdM completeQuery)
    let rmVer (i,a,_) = (i,a)
    return $ fmap (map rmVer) identE


--         ancestorSelect n tx' anc w = txAncestorQuery n tx' anc (selectQuery w)
--         payChanId = castIdent $ getIdent k :: Ident RecvPayChan
--     getFirstResult <$> ancestorSelect (Just ns) tx payChanId "most_recent_note = TRUE"


