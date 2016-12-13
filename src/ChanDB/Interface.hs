module ChanDB.Interface where

import Data.Bitcoin.PaymentChannel.Types (RecvPayChanX, SendPubKey, BitcoinAmount)
import           Data.Time.Clock (UTCTime)


type Key = SendPubKey

class Monad m => ChanDB m where
    create          :: RecvPayChanX     -> m ()
    payBegin        :: Key              -> m RecvPayChanX
    payAbort        ::                     m ()
    payFinish       :: RecvPayChanX     -> m ()
    selectChannels  :: ChanQuery        -> m [Key]
    settleBegin     :: [Key]            -> m [RecvPayChanX]
    settleFin       :: [RecvPayChanX]   -> m ()



data ChanQuery =
    All
  | ExpiresBefore UTCTime
  | CoveringValue BitcoinAmount

-- settleBegin: byExpiration byValue


