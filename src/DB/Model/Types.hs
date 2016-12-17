module DB.Model.Types
(
  module DB.Model.Types
, module Typeable
, module Datastore
, Void
)
where

import           Data.Int                         (Int64)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Typeable as Typeable
import Network.Google.Datastore as Datastore hiding (key)
import           Data.Void                        (Void)

type ProjectId = T.Text
type NamespaceId = T.Text

type AuthCloudPlatform = "https://www.googleapis.com/auth/cloud-platform"
type AuthDatastore = "https://www.googleapis.com/auth/datastore"

type EntityVersion = Int64
type TxId = BS.ByteString   -- ^ Transaction handle
type Cursor = BS.ByteString -- ^ Query batch result handle
