{-# LANGUAGE RecordWildCards #-}
module Util
(
  module Types
, module Control.Lens
, module Data.Maybe
, module Ctrl
, Except.throw
-- , fromMaybe, isJust, isNothing, fromJust, listToMaybe
, lefts, rights
, liftIO
, cs
, fmapL
, (<=<), (>=>), forM
, (<>), mempty, (</>)
, trace
, void
, Ctrl.liftBaseOp
, printf
)
where

import           Types
import           Control.Monad                  ((<=<), (>=>), void, forM)
import           Control.Monad.IO.Class         (liftIO)
import           Data.Monoid                    as Ctrl ((<>), mempty)
import           Control.Monad.Trans.Control    as Ctrl

import qualified Control.Exception as Except

import           Control.Lens                   hiding (op)
import           Data.Maybe                     -- (listToMaybe, fromMaybe, isJust, isNothing, fromJust)
import           Data.Either                    (lefts, rights)
import           Data.String.Conversions        (cs)
import           Data.EitherR                   (fmapL)
import           Data.Tagged (Tagged(..))
import           Text.Printf (printf)

import Debug.Trace (trace)



(</>) :: Monoid a
      => Tagged b a
      -> Tagged c a
      -> Tagged c a
tg1 </> tg2 = Tagged $ unTagged tg1 <> unTagged tg2

