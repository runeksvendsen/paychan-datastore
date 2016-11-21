{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module DB.Creation where

import DB.Types
import Util
import DB.Model.Entity


--
-- dbInsert :: IsEntity a k => a -> m ()
-- dbInsert = error "STUB"
--
-- dbUpdate :: IsEntity a k => a -> m ()
-- dbUpdate = error "STUB"
--
-- dbRemove :: IsKey k  => k -> m ()
-- dbRemove = error "STUB"

--
-- runReqWithTx :: ( MonadGoogle '[AuthDatastore] m
--               ,    HasScope '[AuthDatastore] ProjectsBeginTransaction )
--              => ProjectId -> CommitRequest -> m CommitResponse
-- runReqWithTx pid commitReq =
--     withTx pid ( const $ return ((), Just commitReq) ) >>=
--         \(_,maybeResp) -> return $ fromMaybe
--            (throw $ InternalError "runReqWithTx: 'withTx' did not return CommitResponse")
--            maybeResp
