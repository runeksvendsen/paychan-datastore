{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}
module Model.ChanIndex where

import           Util
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import           Data.Maybe                       (listToMaybe, fromMaybe)


kindName :: T.Text
kindName = "OpenChanIndex"

mkPartitionId :: ProjectId -> PartitionId
mkPartitionId projectId = partitionId &
    piProjectId ?~ projectId &
    piNamespaceId ?~ "open_states"

mkKey :: ProjectId -> Pay.SendPubKey -> Key
mkKey projectId sendPK = key &
    kPartitionId ?~ mkPartitionId projectId &
    kPath .~ [pathElement & peKind ?~ kindName & peName ?~ encodeHex sendPK]
  where
    encodeHex = cs . B16.encode . Bin.encode


keyFromEntity :: Entity -> Either String Pay.SendPubKey
keyFromEntity ent = fmapL ("ChanIndex: " ++) $
    case ent ^. eKey of
        Nothing -> Left "No Key in Entity"
        Just key ->
            case key ^. kPath of
                [] -> Left "No PathElement in Entity Key"
                n@(_:_:_)  -> Left $ "Multiple PathElements in Entity Key: " ++ show n
                [pe] ->
                    case pe ^. peName of
                        Nothing   -> Left "No Name in PathElement"
                        Just name -> decodeHex name
  where
    decodeHex = fmapL ("Error parsing SendPubKey from hex: " ++) .
            Bin.decode . fst . B16.decode . cs

mkEntity :: ProjectId -> Pay.RecvPayChan -> Entity
mkEntity projectId recvChan = entity
    & eKey ?~ mkKey projectId (Pay.getSenderPubKey recvChan)
    & eProperties ?~ encodeAsProperty projectId recvChan

-- Property conversion {
encodeAsProperty :: ProjectId -> Pay.RecvPayChan -> EntityProperties
encodeAsProperty _ recvChan = entityProperties $
    Map.fromList [
       ( "funding_value"
       , value &
         vExcludeFromIndexes ?~ False &
           vIntegerValue ?~ fromIntegral (Pay.getFundingAmount recvChan)
       ),
       ( "expiration_date"
       , value &
         vExcludeFromIndexes ?~ False &
            vIntegerValue ?~ fromIntegral (Pay.toWord32 $ Pay.getExpirationDate recvChan)
       )
       ]

-- decodeFromPropertyOrFail :: EntityProperties -> Pay.SendPubKey
-- decodeFromPropertyOrFail = either error id . decodeFromProperty

-- } Property conversion


-- parseLookupRes :: LookupResponse -> Maybe (Pay.SendPubKey, EntityVersion)
-- parseLookupRes lookupRes =
--     listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
--         case res ^. erEntity of
--             Nothing  -> error "LookupResponse: Empty entityResult"
--             Just ent -> Just
--                 ( decodeFromPropertyOrFail $ fromMaybe (error "LookupResponse: No properties in entity")
--                     (ent ^. eProperties)
--                 , fromMaybe (error "EntityResult: Entity version should be present for ResultType.FULL")
--                     (res ^. erVersion)
--                 )
--
-- showJsonStr :: JSON.ToJSON a => a -> String
-- showJsonStr = cs . JSON.encode
