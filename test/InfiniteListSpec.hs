module InfiniteListSpec (spec) where

import qualified Control.Exception.Base as B
import InfiniteList
import Test.Hspec
import Prelude hiding (cycle)

spec :: Spec
spec = do
  describe "Infinite list" $ do
    it "cycles finite list" $
      take 7 (cycle [1 .. 3]) `shouldBe` [1 :: Int, 2, 3, 1, 2, 3, 1]

    it "cycles infinite list" $
      take 7 (cycle [1 ..]) `shouldBe` [1 :: Int .. 7]

    it "throws error on empty list" $
      B.evaluate (cycle []) `shouldThrow` anyErrorCall
