module ChanDB.PubKey
( getCurrent
, lookupKey
, markAsUsed
-- *Init/setup
, getOrInitialize
-- *Cleanup
, deleteEverything
)
where

import LibPrelude
import DB.Error
import DB.Request
import DB.Query
import ChanDB.Types

import qualified Network.Haskoin.Crypto   as HC
import qualified Data.Serialize           as Bin
import qualified Control.Monad.Catch      as Except
import qualified Control.Concurrent.Cache as Cache



-- | Get the current pubkey, or initialize the DB if it isn't present.
getOrInitialize :: ( DatastoreTxM m
                   , LookupM m (KeyWithAnc CurrentKey (Ident XPubKey)) (JustEntity CurrentKey)
                   )
                => NamespaceId
                -> XPubKey
                -> m KeyAtIndex
getOrInitialize ns xpub = do
    kaiE <- Except.try $ ckKey <$> currKeyGet ns xpub
    case kaiE of
        Right kai ->
            logInfoN "Pubkey read on startup" >> return kai
        Left (MissingXPub _) ->
            logWarnN "DB uninitialized. Initializing..." >> initialize ns xpub
        Left err ->
            logErrorN ("getOrInitialize: Unexpected exception: " <> cshow err) >> throwM err


-- | Initialize DB with first pubkey. Will fail if already initialized.
initialize :: DatastoreTxM m
           => NamespaceId
           -> XPubKey
           -> m KeyAtIndex
initialize ns xpub = do
    let kai = mkFirstPubKey xpub
    updInsertPubKey ns xpub kai
    -- Make some noise
    logInfoN $ cs (printf "Initialized DB for key %s" pubKeyId :: String)
    return kai
  where
    pubKeyId :: String
    pubKeyId = either (const $ error "Bad XPub Ident") cs (objectId xpub)

cachedGetCurrent ::
       DatastoreConf
    -> NamespaceId
    -> XPubKey
    -> Word     -- ^ Cache timeout in seconds. After this timeout, new fetches will refresh the cache.
    -> IO KeyAtIndex
cachedGetCurrent cfg ns xpub refreshSecs =
    Cache.createTimedCache (fromIntegral refreshSecs * round 1e6) False fetcher
        >>= Cache.fetch
  where
    fetcher = throwLeft =<< runDatastore cfg (getCurrent ns xpub)


-- | Return newest (unused) pubkey
getCurrent :: ( DatastoreM m
              , LookupM m (KeyWithAnc CurrentKey (Ident XPubKey)) (JustEntity CurrentKey)
              )
           => NamespaceId
           -> XPubKey
           -> m KeyAtIndex
getCurrent ns xpub = ckKey <$> currKeyGet ns xpub

-- | A channel has been opened with this 'RecvPubKey'.
--   If this pubkey is the newest key then advance to next pubkey.
--   Return newest pubkey.
markAsUsed :: ( DatastoreTxM m
              , LookupM m (KeyWithAnc CurrentKey (Ident XPubKey)) (JustEntity CurrentKey)
              )
           => NamespaceId
           -> XPubKey
           -> RecvPubKey
           -> m KeyAtIndex
markAsUsed ns xpub usedKey = do
    currKey <- currKeyGet ns xpub
    if usedKey == kaiPubKey (ckKey currKey)
        then advancePubKey currKey
        else return $ ckKey currKey
  where
    advancePubKey currKey' = do
        -- Increment index and derive new pubkey
        let newKai = nextPubKey xpub currKey'
        updInsertPubKey ns xpub newKai
        return newKai

-- | Look up index of a pubkey
lookupKey :: DatastoreM m
          => NamespaceId
          -> XPubKey
          -> RecvPubKey
          -> m (Maybe KeyAtIndex)
lookupKey ns xpub findKey =
    firstEntRes <$> lookup' ns lookupKey
  where
    lookupKey :: KeyWithAnc KeyAtIndex (Ident HC.XPubKey)
    lookupKey = ident xpub <//> findKey

currKeyGet :: LookupM m (KeyWithAnc CurrentKey (Ident XPubKey)) (JustEntity CurrentKey)
           => NamespaceId
           -> XPubKey
           -> m CurrentKey
currKeyGet ns xpub = do
    let currDbKey = error "currKeyGet: CurrentKey eval" :: CurrentKey
    lookupRes <- dbLookup ns (pkDBKey xpub currDbKey)
    let missingErr = MissingXPub xpub
    maybe (throwM missingErr) return . firstEntRes $ lookupRes

updInsertPubKey :: DatastoreTxM m
                => NamespaceId
                -> XPubKey
                -> KeyAtIndex
                -> m ()
updInsertPubKey ns xpub kai = do
    -- Store current key & new pubkey
    let newCurr = CurrentKey kai
        upsertCurrent = Upsert $ EntityWithAnc newCurr (ident xpub :: Ident XPubKey)
        insertNewPub  = Insert $ EntityWithAnc kai  (ident xpub :: Ident XPubKey)
    commitAdd upsertCurrent >> commitAdd insertNewPub

deleteEverything :: ( DatastoreM m
                    , LookupM m (KeyWithAnc CurrentKey (Ident XPubKey)) (JustEntity CurrentKey)
                    )
                 => NamespaceId -> HC.XPubKey -> m ()
deleteEverything ns xpub = do
    currKey <- currKeyGet ns xpub
    let kaiLst = keysTillCurrent xpub currKey
    delCurr <- xPubChildDel ns xpub currKey
    delKaiL <- mapM (xPubChildDel ns xpub) kaiLst
    multiCommit $ delCurr : delKaiL


-- Util
firstEntRes = fmap justEntity . getFirstResult

-- | Get the database key for something stored under an 'HC.XPubKey'
pkDBKey :: forall ent.
    ( Identifier ent
    , HasKeyPath (KeyWithAnc ent (Ident HC.XPubKey))
    ) =>
    HC.XPubKey -> ent -> KeyWithAnc ent (Ident HC.XPubKey)
pkDBKey xpub e = ident xpub <//> e :: KeyWithAnc ent (Ident HC.XPubKey)

-- | Create Delete-mutation for something stored under an 'HC.XPubKey'
xPubChildDel :: forall ent m.
    ( Identifier ent
    , HasKeyPath (KeyWithAnc ent (Ident HC.XPubKey))
    , DatastoreM m
    ) =>
    NamespaceId -> HC.XPubKey -> ent -> m Mutation
xPubChildDel ns xpub e = mkDeleteM ns (pkDBKey xpub e)

keysTillCurrent :: HC.XPubKey -> CurrentKey -> [KeyAtIndex]
keysTillCurrent xpub (CurrentKey kaiEnd) =
    take numKeys $ iterate (increment xpub) (mkFirstPubKey xpub)
  where
    numKeys = 1 + fromIntegral (kaiIndex kaiEnd)

mkFirstPubKey :: HC.XPubKey -> KeyAtIndex
mkFirstPubKey xpub =
    KeyAtIndex 0 (MkRecvPubKey pub)
  where
    (_, pub) = HC.deriveAddr xpub 0

-- | Given a root pubkey and the current 'KeyAtIndex', return the next 'KeyAtIndex'
increment :: HC.XPubKey -> KeyAtIndex -> KeyAtIndex
increment xpub (KeyAtIndex idx _) =
    KeyAtIndex newIdx (MkRecvPubKey newPub)
  where
    newIdx      = idx+1
    (_, newPub) = HC.deriveAddr xpub (fromIntegral newIdx)

-- | Advance to next pubkey
nextPubKey :: HC.XPubKey -> CurrentKey -> KeyAtIndex
nextPubKey xPub (CurrentKey currKai) = increment xPub currKai
