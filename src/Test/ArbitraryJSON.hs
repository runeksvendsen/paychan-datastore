module Test.ArbitraryJSON where

import Test.QuickCheck
import Data.Aeson
import Data.Scientific         (scientific)
import Data.Text.Arbitrary     ()
import qualified Data.Vector as V


instance Arbitrary Value where
  arbitrary = oneof [obj, arr]
    where
    json = oneof [obj, arr, str, num, bl, nullg]
    obj = object <$> vectorOf 2 ((.=) <$> arbitrary <*> json)
    arr = Array . V.fromList <$> vectorOf 4 json
    str = String <$> arbitrary
    num = fmap Number $ scientific <$> arbitrary <*> arbitrary
    bl = Bool <$> arbitrary
    nullg = pure Null
