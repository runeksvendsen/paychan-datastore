{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Model.Class where

import qualified Data.Text                      as T

import           Util
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified Data.Aeson                     as JSON
import qualified Data.ByteString.Base16         as B16
import qualified Data.Serialize                 as Bin
import           Data.String.Conversions          (cs)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                       (listToMaybe, fromMaybe)
import qualified Data.ByteString.Base64         as B64


type KeyString = T.Text

class Bin.Serialize k => KindKey k where
    projectId   :: k -> ProjectId
    kindName    :: k -> T.Text
    namespace   :: k -> T.Text
    -- DB
    dbRemove    :: k -> m ()
    dbRemove = undefined

class KindKey k => DatastoreEntity k a | a -> k where
    properties  :: a -> [(KeyString, Value)]
    getKey      :: a -> k
    -- DB
    dbInsert    :: a -> m ()
    dbLookup    :: k -> m (Maybe a)
    dbUpdate    :: a -> m ()
    dbInsert    = undefined
    dbLookup    = undefined
    dbUpdate    = undefined
    getKey      = undefined

data IndexEntity k a = IndexEntity k [(KeyString, Value)]

class DatastoreEntity k a => IndexedEntity k a where
    indexProperties    :: a -> [(KeyString, Value)]
    entityList         :: m [IndexEntity k a]


-- |Wrap any type a (where instance IndexedEntity a) in this newtype,
--  to write the type to the database in its index form.
data EntityIndexProxy k a = EntityIndexProxy k a deriving Bin.Serialize

instance IndexedEntity k a => DatastoreEntity (EntityIndexProxy k a) where
    properties = idxProperties

instance IndexedEntity k a => DatastoreEntity (EntityIndexProxy k a) where
--     kindName (EntityIndexProxy k a) = "Index" <> kindName k
--     namespace = kindName

--

instance KindKey SendPubKey where
    projectId _ = "paychan"
    kindName _ = "RecvPayChan"
    namespace = cs . B16.encode . Bin.encode

instance DatastoreEntity SendPubKey Pay.RecvPayChan where
    properties recvChan =
           [( "value_received"
           , value &
               vExcludeFromIndexes ?~ False &
               -- Put RecvState in Property as JSON string type
               vIntegerValue ?~ fromIntegral (Pay.valueToMe recvChan)
           )]
--
-- instance IndexedEntity Pay.RecvPayChan where
--     idxProperties recvChan = [
--         ( "funding_value"
--         , value &
--           vExcludeFromIndexes ?~ False &
--            vIntegerValue ?~ fromIntegral (Pay.getFundingAmount recvChan)
--         ),
--         ( "expiration_date"
--         , value &
--           vExcludeFromIndexes ?~ False &
--             vIntegerValue ?~ fromIntegral (Pay.toWord32 $ Pay.getExpirationDate recvChan)
--         ),
--         ( "expiration_date_pretty"
--         , value &
--           vExcludeFromIndexes ?~ True &
--             vStringValue ?~ cs (show $ Pay.getExpirationDate recvChan)
--         )]

mkKey :: KindKey k => k -> Key
mkKey a = key &
    kPartitionId ?~ dePartitionId a &
    kPath .~ [pathElement & peKind ?~ kindName a] -- & peName ?~ "state"

dePartitionId :: KindKey k => k -> PartitionId
dePartitionId k = partitionId &
    piProjectId ?~ projectId k &
    piNamespaceId ?~ namespace k

deDataProp :: KindProperties a => a -> Value
deDataProp a = value &
    vExcludeFromIndexes ?~ True &
    -- Serialize to base64 and put in EntityProperty under "data"
    vStringValue ?~ cs (B64.encode . Bin.encode $ a)

deEntityProps :: KindProperties a => a -> EntityProperties
deEntityProps a = entityProperties $
    Map.fromList $
        ("data", deDataProp a) : properties a

deMkEntity :: DatastoreEntity k a => a -> k -> Entity
deMkEntity a _ = entity
    & eKey ?~ mkKey (getKey a)
    & eProperties ?~ deEntityProps a

deDecode :: KindProperties a => EntityProperties -> Either String a
deDecode props =
    case Map.lookup "data" map of
        Nothing  -> Left "no 'data' key in EntityProperties"
        Just val -> case val ^. vStringValue of
            Nothing  -> Left "Expected String value."
            Just str -> decodeFromB64 str >>= decodeFromBin
  where
    map = props ^. epAddtional
    decodeFromB64 = fmapL ("Failed to decode base64: " ++) . B64.decode . cs
    decodeFromBin = fmapL ("Failed to decode data: " ++) . Bin.decode


decodeFromEntity :: KindProperties a => Entity -> Either String a
decodeFromEntity ent = deDecode $
    fromMaybe (internalError "No properties in entity") $
    ent ^. eProperties


decodeFromPropertyOrFail :: EntityProperties -> Pay.RecvPayChan
decodeFromPropertyOrFail = either internalError id . deDecode


parseLookupRes :: LookupResponse -> Maybe (Pay.RecvPayChan, EntityVersion)
parseLookupRes lookupRes =
    listToMaybe (lookupRes ^. lrFound) >>= \res ->  -- lrFound: Entities found as `ResultType.FULL` entities.
        case res ^. erEntity of
            Nothing  -> internalError "LookupResponse: Empty entityResult"
            Just ent -> Just
                ( decodeFromPropertyOrFail $
                    fromMaybe (internalError "LookupResponse: No properties in entity")
                    (ent ^. eProperties)
                , fromMaybe (internalError $
                         "CloudStore API BUG. LookupResponse: " ++
                         "Entity version not present")
                    (res ^. erVersion)
                )

showJsonStr :: JSON.ToJSON a => a -> String
showJsonStr = cs . JSON.encode


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

