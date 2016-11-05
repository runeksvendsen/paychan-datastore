module Util
(
  module Util
, module Types
, module Control.Lens
, Except.throw
, fromMaybe
)
where

import           Types
import qualified Control.Exception as Except
import qualified Control.Monad.Catch as      Catch
import           Control.Lens
import           Data.Maybe                    (fromMaybe)

internalError  = Except.throw . InternalError
-- internalErrorM = Catch.throwM . InternalError

