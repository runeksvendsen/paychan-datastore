{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DB.Types
(
  module DB.Types
, module Types
, module DB.Model.Types.Entity
, module Datastore
, module Google
, DBException(..)
)
where

import           Types
import           DB.Model.Types.Entity


import qualified Control.Exception as Except
import qualified Data.ByteString as BS
import Network.Google.Datastore as Datastore hiding (Entity, key, query)
import Network.Google           as Google


data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException

type EntityVersion = Int64
type TxId = BS.ByteString   -- ^ Transaction handle
type Cursor = BS.ByteString -- ^ Query batch result handle
