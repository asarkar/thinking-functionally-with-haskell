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

    -- shouldThrow expects a first argument of type `IO a`,
    -- whereas `cycle` returns `[a], so, we use evaluate to
    -- convert it to `IO [a]`.
    it "throws error on empty list" $
      B.evaluate (cycle []) `shouldThrow` anyErrorCall

    it "produces the infinite list of Fibonacci numbers" $
      and (zipWith (==) fibs [0, 1, 1, 2, 3, 5, 8]) `shouldBe` True

    it "produces the infinite list of Hamming numbers" $
      and (zipWith (==) hamming [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16]) `shouldBe` True
