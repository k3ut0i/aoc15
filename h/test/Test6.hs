module Test6 (test) where

import Test.Hspec
import Day6

eg :: [Instr]
eg = map read ["turn on 0,0 through 999,999",
               "toggle 0,0 through 999,0",
               "turn off 499,499 through 500,500"]

test :: IO ()
test = hspec $ do
  describe "example instructions" $ do
    it "i1" $ do
      lit (update (eg!!0) l) `shouldBe` 1000000
    it "i2" $ do
      lit (update (eg!!1) l) `shouldBe` 1000
    it "i3" $ do
      lit (update (eg!!2) l) `shouldBe` 0
    it "all" $ do
      lit (foldl (flip update) l eg) `shouldBe` 998996
  where
    l = emptyL 1000

