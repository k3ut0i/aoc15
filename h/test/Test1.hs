module Test1 (test) where

import Test.Hspec
import Day1

test :: IO()
test = hspec $ do
  describe "test 1: examples" $ do
    it "zero example 1" $ do
      countFloors "(())" `shouldBe` 0
    it "zero example 2" $ do
      countFloors "()()" `shouldBe` 0
