module Test.Main where

import DB.Model.EntityProps          -- (jsonFromDS, jsonToDS)
import Test.ArbitraryJSON   ()
import Test.Framework       (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.String.Conversions        (cs)

import Debug.Trace
import qualified Data.Aeson as JSON
-- import qualified Data.Bitcoin.PaymentChannel.Test as Pay



main :: IO ()
main = -- quickCheck $ \jsonVal -> show (jsonVal :: JSON.Value) `trace` True -- (jsonFromDS . jsonToDSTrace $ jsonVal) == jsonVal
    defaultMain tests

tests :: [Test]
tests =
    [ testGroup "JSON Conversion"
        [ testProperty "jsonFromDS . jsonToDS = id" $
            \json -> -- cs (JSON.encode json) `trace`
                if convertVal json == json then True else
                    error $ unlines $
                        [ "### ERROR ###" ]
                        ++ [ "Original: " ++ show json ]
                        ++ [ "\nParsed:   " ++ show (convertVal json) ]
--                         ++ [ "\nJSON:     " ++ cs (JSON.encode json) ]
        ]
    ]
  where
    convertVal :: JSON.Value -> JSON.Value
    convertVal = jsonFromDS . jsonToDS []
