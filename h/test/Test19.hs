module Test19 (test) where

import Test.Hspec
import Day19 (allReplacements, Chemistry)

sample :: Chemistry
sample = [ ("H", "HO")
         , ("H", "OH")
         , ("O", "HH")]
 
test :: IO ()
test = hspec $ do
  describe "Test19: " $ do
    it "example test" $ do
      allReplacements sample "HOH" `shouldBe` [ "HOOH", "HOHO", "HOH"
                                              , "OHOH", "HOOH", "HOH"
                                              , "HHHH", "HOH"]
