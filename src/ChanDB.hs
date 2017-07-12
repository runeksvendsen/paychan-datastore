module ChanDB
( module Ex
, Impl, TxImpl, ConfImpl
, insertChan
, removeChan
)
where

import ChanDB.Interface as Ex
import ChanDB.Types as Ex
import ChanDB.Types.StoredNote as Ex

import DB.Error.Util as Ex

import ChanDB.Creation      (insertChan, removeChan)
import ChanDB.Update    as Ex
import           PromissoryNote                   (PromissoryNote, UUID)

type Impl = Datastore
type TxImpl = DatastoreTx
type ConfImpl = DatastoreConf
