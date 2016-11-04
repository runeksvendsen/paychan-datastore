module Types
(
  module Types
, cs
)
where

import qualified Data.Text                      as T
import           Data.String.Conversions          (cs)
import           Data.Int                         (Int64)
import qualified Data.ByteString as BS



data UpdateResult = Updated | NotUpdated deriving Show

type ProjectId = T.Text
type Version = Int64
type TxId = BS.ByteString