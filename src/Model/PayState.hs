{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Model.PayState where

import           Types
import           Network.Google.Datastore
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import           Control.Lens
import qualified Data.Aeson                     as JSON
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                       (listToMaybe, fromMaybe)



mkPartitionId :: ProjectId -> Pay.SendPubKey -> PartitionId
mkPartitionId projectId sendPK = partitionId &
    piProjectId ?~ projectId &
    piNamespaceId ?~ (cs . B16.encode . Bin.encode $ sendPK)

mkKey :: ProjectId -> Pay.SendPubKey -> Key
mkKey projectId sendPK = key &
    kPartitionId ?~ mkPartitionId projectId sendPK &
    kPath .~ [pathElement & peKind ?~ "RecvPayChan" & peName ?~ "state"]

mkEntity :: ProjectId -> Pay.RecvPayChan -> Entity
mkEntity projectId recvChan = entity
    & eKey ?~ mkKey projectId (Pay.getSenderPubKey recvChan)
    & eProperties ?~ encodeAsProperty recvChan

-- Property conversion {
encodeAsProperty :: Pay.RecvPayChan -> EntityProperties
encodeAsProperty recvChan = entityProperties $
    Map.fromList [
       ( "data"
       , value &
           vExcludeFromIndexes ?~ True &
           -- Put RecvState in Property as JSON string type
           vStringValue ?~ cs (JSON.encode recvChan)
       ),
       ( "value_received"
       , value &
           vExcludeFromIndexes ?~ False &
           -- Put RecvState in Property as JSON string type
           vIntegerValue ?~ fromIntegral (Pay.valueToMe recvChan)
       )]

decodeFromPropertyOrFail :: EntityProperties -> Pay.RecvPayChan
decodeFromPropertyOrFail = either error id . decodeFromProperty

decodeFromProperty :: EntityProperties -> Either String Pay.RecvPayChan
decodeFromProperty props =
    case Map.lookup "data" map of
        Just val ->
            case val ^. vStringValue of
                Just str -> maybe
                    (Left "Failed to decode RecvPayChan from EntityProperties")
                    Right
                    (JSON.decode $ cs str)
                Nothing -> Left "Expected String value."
        Nothing -> Left "no 'data' key in EntityProperties"
  where
    map = props ^. epAddtional
-- } Property conversion


parseLookupRes :: LookupResponse -> Maybe (Pay.RecvPayChan, Version)
parseLookupRes lookupRes =
    listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
        case res ^. erEntity of
            Nothing  -> error "LookupResponse: Empty entityResult"
            Just ent -> Just
                ( decodeFromPropertyOrFail $ fromMaybe (error "LookupResponse: No properties in entity")
                    (ent ^. eProperties)
                , fromMaybe (error "EntityResult: Entity version should be present for ResultType.FULL")
                    (res ^. erVersion)
                )

showJsonStr :: JSON.ToJSON a => a -> String
showJsonStr = cs . JSON.encode
