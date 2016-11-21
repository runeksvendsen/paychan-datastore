module DB.Util.Error where

import           DB.Types
import qualified Control.Monad.Catch as      Catch
import qualified Control.Exception as Except


internalError  = Except.throw . InternalError

internalErrorM :: (Catch.MonadThrow m) => String -> m a
internalErrorM = Catch.throwM . InternalError
