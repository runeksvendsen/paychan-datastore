{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds #-}
module DB.Types
(
  module DB.Types
, module DB.Model.Types.Entity
, module DB.Model.Types.Namespace
, module Datastore
, module Google
, DBException(..)
)
where

import           DB.Model.Types.Entity
import           DB.Model.Types.Namespace

import           Data.Int                         (Int64)
import qualified Control.Exception as Except
import qualified Data.ByteString as BS
import Network.Google.Datastore as Datastore hiding (Entity, key, query)
import Network.Google           as Google
import qualified Data.Text as T


type ProjectId = T.Text

type AuthCloudPlatform = "https://www.googleapis.com/auth/cloud-platform"
type AuthDatastore = "https://www.googleapis.com/auth/datastore"

data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException

type EntityVersion = Int64
type TxId = BS.ByteString   -- ^ Transaction handle
type Cursor = BS.ByteString -- ^ Query batch result handle
