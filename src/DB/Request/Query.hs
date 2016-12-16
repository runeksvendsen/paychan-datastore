{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import DB.Model.Types.Query
import DB.Request.Util
import DB.Types
import DB.Model.Convert
import Util


heyHo :: DatastoreM m => RunQueryRequest -> m RunQueryResponse
heyHo req = sendReq' (projectsRunQuery req)

runQueryReq :: DatastoreM m
         => Maybe TxId
         -> Tagged a RunQueryRequest
         -> m ( Tagged (a,anc) RunQueryResponse )
runQueryReq txM req = Tagged <$> sendReq' (projectsRunQuery txReq)
    where
        txReq = maybe (unTagged req) applyTxId txM
        applyTxId tx = unTagged req & rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)


txQuery :: forall q a anc.
           ( IsQuery q
           , HasAncestor a anc )
          => Maybe NamespaceId
          -> TxId
          -> q
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
txQuery nsM tx q = do
    req <- mkQueryReq nsM q
    parseQueryRes <$> runQueryReq (Just tx) req

entityQuery :: forall q a anc.
           ( IsQuery q
           , HasAncestor a anc )
          => Maybe NamespaceId
          -> q
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
entityQuery nsM q = do
    req <- mkQueryReq nsM q
    parseQueryRes <$> runQueryReq Nothing req


keysOnlyQuery :: forall q a anc.
               ( IsQuery q
               , HasAncestor a anc )
              => Maybe NamespaceId
              -> q
              -> Datastore ( Either String [ (Ident a, Ident anc) ] )
keysOnlyQuery nsM q = do
    req <- mkQueryReq nsM q
    identE <- parseQueryResKeys <$> runQueryReq Nothing req
    let rmVer (i,a,_) = (i,a)
    return $ fmap (map rmVer) identE



