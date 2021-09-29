module Test7 (test) where

import Test.Hspec
import Day7
import Data.Word (Word16)
eg :: [Con]
eg = map read ["123 -> x",
               "456 -> y",
               "x AND y -> d",
               "x OR y -> e",
               "x LSHIFT 2 -> f",
               "y RSHIFT 2 -> g",
               "NOT x -> h",
               "NOT y -> i"
              ]
egVals :: [(String, Word16)]
egVals = [("d", 72),
          ("e", 507),
          ("f", 492),
          ("g", 114),
          ("h", 65412),
          ("i", 65079),
          ("x", 123),
          ("y", 456)]

test :: IO ()
test = hspec $ do
  describe "test 7:" $ do
    it "123 -> x parsing" $ do
      read "123 -> x" `shouldBe` Con "x" (RAW (Val 123))
    it "evaluations for the example" $ do
      all (\(s, i) -> Just i == evalS s eg)  egVals `shouldBe` True
      
