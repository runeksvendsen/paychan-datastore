module Util
(
  module Util
, module Types
, module Control.Lens
, Except.throw
, fromMaybe
, liftIO
, cs
, fmapL
, (<=<), (>=>)
, (<>)
)
where

import           Types
import           Control.Monad                  ((<=<), (>=>))
import           Control.Monad.IO.Class         (liftIO)
import           Data.Monoid                    ((<>))

import qualified Control.Exception as Except
import qualified Control.Monad.Catch as      Catch
import           Control.Lens
import           Data.Maybe                     (fromMaybe)
import           Data.String.Conversions        (cs)
import           Data.EitherR                   (fmapL)

internalError  = Except.throw . InternalError

internalErrorM :: (Catch.MonadThrow m) => String -> m a
internalErrorM = Catch.throwM . InternalError

