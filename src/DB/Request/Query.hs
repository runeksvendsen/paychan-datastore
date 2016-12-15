{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert
import           Util


txAncestorQuery :: forall a anc.
           HasAncestor a anc
          => Maybe PartitionId
          -> TxId
          -> Ident anc
          -> Text
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
txAncestorQuery partM tx anc query =
    parseQueryRes <$> reqRes
        where
            reqRes = Tagged <$> sendReq (projectsRunQuery reqWithTx)
            reqWithTx = unTagged (mkQueryReq partM (Just anc) query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)
