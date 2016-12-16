module Conversion where

import DB.Model.Convert.Properties
import ArbitraryJSON   ()
import Test.Framework       (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import           Data.String.Conversions        (cs)
import qualified Data.Aeson as JSON
{-# ANN module ("HLint: ignore Redundant if"::String) #-}


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "Conversion"
        [ testProperty "Properties" $ propertiesConvert
        ]
    ]

propertiesConvert :: JSON.Value -> Bool
propertiesConvert json = if convertVal json == json then True else
    error $ unlines $
        [ "### ERROR ###" ]
        ++ [ "Original: " ++ show json ]
        ++ [ "\nParsed:   " ++ show (convertVal json) ]
        ++ [ "\nJSON:     " ++ cs (JSON.encode json) ]
     where
       convertVal =  jsonFromDS . jsonToDS []

