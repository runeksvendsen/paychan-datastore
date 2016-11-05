module DB.Types where

import qualified Control.Exception as Except

data UpdateResult = Updated | NotUpdated deriving Show
data DBException  =
    NoSuchChannel
  | InternalError String
        deriving Show

instance Except.Exception DBException
