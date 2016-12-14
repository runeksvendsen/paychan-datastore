{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import           DB.Types
import           DB.Model.Convert
import           Util
import           Network.Google as Google


txAncestorQuery :: forall a anc m.
           ( MonadGoogle '[AuthDatastore] m
           , HasScope    '[AuthDatastore] ProjectsRunQuery
           , HasAncestor a anc)
          => NamespaceId
          -> TxId
          -> Ident anc
          -> Text
          -> m ( Either String [ ((a, Ident anc), EntityVersion) ] )
txAncestorQuery nsId tx anc query =
    parseQueryRes <$> reqRes
        where
            reqRes :: m (Tagged a RunQueryResponse)
            reqRes = Tagged <$> Google.send (projectsRunQuery reqWithTx (nsProjectId nsId))
            reqWithTx = unTagged (mkQueryReq nsId (Just anc) query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)
