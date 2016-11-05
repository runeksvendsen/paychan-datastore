{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, PolyKinds #-}
module DB.Tx where

import           Types
import           DB.Types
import           Model.PayState
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import qualified Control.Monad as M
import qualified Control.Exception as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Catch    (MonadCatch)
import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy (Proxy)
import           Control.Lens
import           System.IO              (stderr)
import           Test.QuickCheck (Gen, sample', vectorOf, choose, generate)
import           Data.String.Conversions          (cs)

import qualified Network.HTTP.Conduit as HTTP



-- |Finish transaction without doing anything (rollback)
-- txRollback :: ( HasScope s '["https://www.googleapis.com/auth/cloud-platform"]
--               , MonadGoogle s m )
--            => ProjectId -> TxId -> m CommitResponse
txRollback projectId tx =
    Google.send (projectsRollback rollbackReq projectId)
  where
    rollbackReq = rollbackRequest & rrTransaction ?~ tx

-- |Begin transaction. The returned handle must be released safely after use,
--   by doing either a commit or a rollback.
-- txBeginUnsafe :: ( MonadGoogle s m
--                  , HasScope s TxId    )
--               => ProjectId
--               -> m TxId
txBeginUnsafe projectId = do
    txBeginRes <- Google.send (projectsBeginTransaction beginTransactionRequest projectId)
    case txBeginRes ^. btrTransaction of
            Just tid -> return tid
            Nothing  -> Except.throw . InternalError $
                "CloudStore API BUG: Transaction identifier should always be present"


--    :: ( MonadGoogle s m
--       , HasScope s '["https://www.googleapis.com/auth/cloud-platform",
--                      "https://www.googleapis.com/auth/datastore"]    )
--       => ProjectId
--       -> m TxId