module Util
(
  module Control.Lens
, Except.throw
, fromMaybe, isJust, fromJust
, lefts, rights
, liftIO
, cs
, fmapL
, (<=<), (>=>)
, (<>), (</>)
)
where

import           Control.Monad                  ((<=<), (>=>))
import           Control.Monad.IO.Class         (liftIO)
import           Data.Monoid                    ((<>))

import qualified Control.Exception as Except

import           Control.Lens
import           Data.Maybe                     (fromMaybe, isJust, fromJust)
import           Data.Either                    (lefts, rights)
import           Data.String.Conversions        (cs)
import           Data.EitherR                   (fmapL)
import           Data.Tagged (Tagged(..))

(</>) :: Monoid a
      => Tagged b a
      -> Tagged c a
      -> Tagged c a
tg1 </> tg2 = Tagged $ unTagged tg1 <> unTagged tg2

