{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Query where

import           DB.Request.Util
import           DB.Types
import           DB.Model.Convert
import           Util


-- | Ancestor queries are strongly consistent,
--  whereas global queries are eventually consistent.
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
            reqWithTx = unTagged (mkAncQueryReq partM anc query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)

globalQuery :: forall a anc.
            HasAncestor a anc
          => Maybe PartitionId
          -> TxId
          -> Text
          -> Datastore ( Either String [ ((a, Ident anc), EntityVersion) ] )
globalQuery partM tx query =
    parseQueryRes <$> reqRes
        where
            reqRes = Tagged <$> sendReq (projectsRunQuery reqWithTx)
            reqWithTx = unTagged (mkQueryReq partM query :: Tagged a RunQueryRequest) &
                rqrReadOptions ?~ (readOptions & roTransaction ?~ tx)

