module PubKey where

import TestPrelude
import Test.Hspec
import qualified Network.Haskoin.Test   as HTT
import qualified Network.Haskoin.Crypto as HC
import qualified ChanDB as DB
import qualified ChanDB as PK
    ( pubKeySetup
    , pubKeyCurrent
    , pubKeyLookup
    , pubKeyMarkUsed
    , pubKeyDELETE
    , KeyAtIndex(..)
    , CurrentKey(..)
    )


pubKeySpec :: Spec
pubKeySpec =
  around withArbXPub $
    describe "current key" $ do
      it "has derive_index=0 when just initialized" $ \(h,xpub) ->
        PK.kaiIndex <$> dbRun h (PK.pubKeyCurrent xpub) `shouldReturn` 0

      it "has derive_index=<n> after marking <n> keys as used" $ \(h,xpub) -> do
        markCount <- fromIntegral <$> generate (choose (10,100) :: Gen Int)
        forM_ (take (fromIntegral markCount) [0..]) (const $ getAndMarkUsed h xpub)
        PK.kaiIndex <$> dbRun h (PK.pubKeyCurrent xpub) `shouldReturn` markCount


getAndMarkUsed :: DB.ChanDB DB.Impl h => h -> HC.XPubKey -> IO ()
getAndMarkUsed h xpub = do
    kai <- dbRun h $ PK.pubKeyCurrent xpub
    void $ dbRun h $ PK.pubKeyMarkUsed xpub (PK.kaiPubKey kai)


dbRun :: DB.ChanDB DB.Impl h => h -> DB.Impl a -> IO a
dbRun h m = failLeft <$> DB.runDB h m

main :: IO ()
main = hspec pubKeySpec


pkAlloc :: DB.ChanDB DB.Impl h => h -> HC.XPubKey -> IO HC.XPubKey
pkAlloc h xpub = dbRun h (PK.pubKeySetup xpub) >> return xpub

pkCleanup :: DB.ChanDB DB.Impl h => h -> HC.XPubKey -> IO ()
pkCleanup h xpub = dbRun h (PK.pubKeyDELETE xpub)

withArbXPub :: DB.ChanDB DB.Impl h => ((h,HC.XPubKey) -> IO ()) -> IO ()
withArbXPub f = do
    dbHandle <- DB.getHandle stderr DB.LevelInfo
    HTT.ArbitraryXPubKey _ xpub <- generate arbitrary
    bracket
        (pkAlloc dbHandle xpub)
        (pkCleanup dbHandle)
        (\pk -> f (dbHandle, pk))


failLeft :: Show e => Either e a -> a
failLeft = either (error . show) id
