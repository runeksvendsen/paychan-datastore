module Util
(
  module Control.Lens
, Except.throw
, fromMaybe, isJust, isNothing, fromJust
, lefts, rights
, liftIO
, cs
, fmapL
, (<=<), (>=>)
, (<>), mempty, (</>)
, trace
)
where

import           Control.Monad                  ((<=<), (>=>))
import           Control.Monad.IO.Class         (liftIO)
import           Data.Monoid                    ((<>), mempty)

import qualified Control.Exception as Except

import           Control.Lens
import           Data.Maybe                     (fromMaybe, isJust, isNothing, fromJust)
import           Data.Either                    (lefts, rights)
import           Data.String.Conversions        (cs)
import           Data.EitherR                   (fmapL)
import           Data.Tagged (Tagged(..))


import Debug.Trace (trace)

(</>) :: Monoid a
      => Tagged b a
      -> Tagged c a
      -> Tagged c a
tg1 </> tg2 = Tagged $ unTagged tg1 <> unTagged tg2

