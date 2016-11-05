module Util
(
  module Util
, module Types
, module Control.Lens
, Except.throw
, fromMaybe
, liftIO
, cs
)
where

import           Types
import           Control.Monad.IO.Class     (liftIO)

import qualified Control.Exception as Except
import           Control.Lens
import           Data.Maybe                    (fromMaybe)
import           Data.String.Conversions          (cs)

internalError  = Except.throw . InternalError
-- internalErrorM = Catch.throwM . InternalError

