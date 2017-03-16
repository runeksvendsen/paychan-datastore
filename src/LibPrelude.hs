{-# LANGUAGE RecordWildCards #-}
module LibPrelude
( module X
, lefts, rights
, liftIO
, cs, cshow
, fmapL
, (<=<), (>=>), forM, mapM
, (<>), mempty
, trace
, void
, Ctrl.MonadBaseControl
, Ctrl.liftBaseOp
, printf
, decodeJsonObject
, Typeable
)
where

import LibPrelude.Types               as X
import Control.Monad.Catch            as X
import Control.Monad.Logger           as X
import Data.Maybe                     as X
import Control.Lens                   as X hiding (op)

import Control.Monad                  ((<=<), (>=>), void, forM, mapM)
import Control.Monad.IO.Class         (liftIO)
import Data.Monoid                    as Ctrl ((<>), mempty)
import Control.Monad.Trans.Control    as Ctrl

import Data.Either                    (lefts, rights)
import Data.String.Conversions        (cs)
import Data.EitherR                   (fmapL)
import Data.Tagged                    (Tagged(..))
import Text.Printf                    (printf)
import Data.Typeable                  (Typeable)

import qualified Data.Text as T

import Debug.Trace (trace)
import qualified Data.Aeson             as JSON


cshow :: Show a => a -> T.Text
cshow = cs . show

decodeJsonObject :: forall a. JSON.FromJSON a => JSON.Object -> Either String a
decodeJsonObject props =
    case JSON.fromJSON $ JSON.Object props of
        JSON.Success a -> Right a
        JSON.Error e   -> Left e
