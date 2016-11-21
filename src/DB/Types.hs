{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DB.Types
(
  module DB.Types
, module Types
, module Datastore
, module Google
, DBException(..)
)
where

import           Types
-- import           DB.Model.Entity ()
import qualified Control.Exception as Except
import qualified Data.ByteString as BS
import Network.Google.Datastore as Datastore hiding (Entity, key)
import Network.Google           as Google
-- import Refined


-- type ProperFraction =
--   Refined (GreaterThan 0) Int64

-- newtype ObjectId = DSID Int64 deriving (Eq, Show, Enum, Integral, Num, Ord, Real)

-- instance Bounded ObjectId where
--     minBound = 1
--     maxBound = fromIntegral (maxBound :: Int64)


data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException

type EntityVersion = Int64
type TxId = BS.ByteString   -- ^ Transaction handle
type Cursor = BS.ByteString -- ^ Query batch result handle
