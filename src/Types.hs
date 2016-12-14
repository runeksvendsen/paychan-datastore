{-# LANGUAGE DataKinds, RecordWildCards #-}
module Types
(
  Catch.MonadCatch
, MonadIO
, BS.ByteString
, T.Text
, Int64
, Tagged(..)
-- , setMostRecentNote
)
where


import           Data.Tagged (Tagged(..))
import           Control.Monad.IO.Class     (MonadIO)
import qualified Control.Monad.Catch as      Catch

import           Data.ByteString as BS
import           Data.Int                         (Int64)
import qualified Data.Text                      as T
