{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}
module Model.ChanIndex where

import           Util
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                       (listToMaybe, fromMaybe)


mkPartitionId :: ProjectId -> PartitionId
mkPartitionId projectId = partitionId &
    piProjectId ?~ projectId &
    piNamespaceId ?~ "open_states"

mkKey :: ProjectId -> Pay.SendPubKey -> Key
mkKey projectId sendPK = key &
    kPartitionId ?~ mkPartitionId projectId &
    kPath .~ [pathElement & peKind ?~ "OpenChanIndex" & peName ?~ encodeHex sendPK]
  where
    encodeHex = cs . B16.encode . Bin.encode

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

-- decodeFromProperty :: EntityProperties -> Either String Pay.SendPubKey
-- decodeFromProperty props =
--     case Map.lookup "key" map of
--         Just val ->
--             case val ^. vKeyValue of
--                 Just key ->
--                     maybe
--                         (Left "No PathList in OpenChanIndex EntityProperties")
--                         (Right . (^. peName))
--                         (listToMaybe $ key ^. kPath) >>=
--                             maybe
--                             (Left "Missing 'name' field in OpenChanIndex EnityProperties")
--                             decodeHex
--                 Nothing -> Left "Expected Key value in 'key'."
--         Nothing -> Left "no 'key' key in OpenChanIndex EntityProperties"
--   where
--     map = props ^. epAddtional
--     decodeHex = either (\e -> Left $ "Failed to decode SendPubKey from hex: " ++ e) Right .
--             Bin.decode . fst . B16.decode . cs
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
