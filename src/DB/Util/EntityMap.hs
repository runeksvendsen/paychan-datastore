module DB.Util.EntityMap where

import           Util
import           DB.Index.Keys
import qualified STMContainers.Map as STM
import           Data.Hashable
import qualified Data.Serialize as Bin


type ChanMapCopy = STM.Map SendPubKey RecvPayChan

-- fromDB = undefined
--     where keys = openChannelKeys






instance Hashable SendPubKey where
    hashWithSalt salt sendPK =
        salt `hashWithSalt` Bin.encode sendPK


