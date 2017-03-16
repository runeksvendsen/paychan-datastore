module PubKey where

import Test.Hspec
import Test.QuickCheck        (Gen, sample', vectorOf, choose, generate, arbitrary)
import Control.Exception      (bracket)
import qualified Network.Haskoin.Test   as HTT
import qualified Network.Haskoin.Crypto as HC
import qualified ChanDB as DB
import qualified ChanDB as PK
    ( pubKeySetup
    , pubKeyCurrent
    , pubKeyLookup
    , pubKeyMarkUsed
    , pubKeyDELETE
    )


pubKeySpec :: Spec
pubKeySpec =
  around withArbXPub $
    describe "ChanDB PubKey" $
      it "current key has derive_index=0 when just initialized" $ \(h,xpub) ->
        DB.kaiIndex <$> dbRun h (PK.pubKeyCurrent xpub) `shouldReturn` 0


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
    dbHandle <- DB.getHandle DB.Debug
    HTT.ArbitraryXPubKey _ xpub <- generate arbitrary
    bracket
        (pkAlloc dbHandle xpub)
        (pkCleanup dbHandle)
        (\pk -> f (dbHandle, pk))


failLeft :: Show e => Either e a -> a
failLeft = either (error . show) id
