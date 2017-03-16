module DB.Error.Util
( module DB.Error.Util
, module DB.Error.Types
, Catch.throwM
) where

import DB.Error.Types
import qualified Control.Monad.Catch as      Catch
import qualified Control.Exception as Except


throwLeft :: (Catch.MonadThrow m, Except.Exception e)
            => Either e b -> m b
throwLeft = either Catch.throwM return

throwOnNothing :: Catch.MonadThrow m
               => Maybe a -> m a
throwOnNothing = maybe (Catch.throwM NoSuchEntity) return

internalError  = Except.throw . InternalError

internalErrorM :: (Catch.MonadThrow m) => String -> m a
internalErrorM s = Catch.throwM $ InternalError $ Bug s

catErr :: String -> DBException -> DBException
catErr s (UserError     (ParseFail e))  = UserError     $ ParseFail  $ s ++ e
catErr s (InternalError (ParseError e)) = InternalError $ ParseError $ s ++ e
catErr _ x = x
