module DB.Error.Types where

import qualified Control.Exception as Except
import qualified Network.Haskoin.Crypto as HC

data DBException =
    NoSuchEntity
  | UserError UserError
  | InternalError InternalError
      deriving (Eq, Show)

data InternalError =
    ParseError String
  | Bug String
      deriving (Eq, Show)

data UserError =
    ParseFail String
      deriving (Eq, Show)

instance Except.Exception DBException
