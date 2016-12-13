module ChanDB
(
  module DB.Types
, module Update
, insertChan, removeChan
)
where

import DB.Types

import ChanDB.Creation      (insertChan, removeChan)
import ChanDB.Update    as Update


