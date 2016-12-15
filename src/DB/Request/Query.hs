{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert
import           Util


txAncestorQuery :: forall a anc m.
           ( DatastoreM m
           , HasScope    '[AuthDatastore] ProjectsRunQuery
           , HasAncestor a anc)
          => Maybe PartitionId
          -> TxId
          -> Ident anc
          -> Text
          -> m ( Either String [ ((a, Ident anc), EntityVersion) ] )
txAncestorQuery partM tx anc query =
    parseQueryRes <$> reqRes
        where
            reqRes :: m (Tagged a RunQueryResponse)
            reqRes = Tagged <$> sendReq (projectsRunQuery reqWithTx)
            reqWithTx = unTagged (mkQueryReq partM (Just anc) query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)
