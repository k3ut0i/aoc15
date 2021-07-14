module Test5 (test) where

import Test.Hspec
import Day5 (nice, pairRep, skipRep)

test :: IO ()
test = hspec $ do
  describe "positive examples" $ do
    it "ugknbfddgicrmopn" $ do
      nice "ugknbfddgicrmopn" `shouldBe` True
    it "aaa" $ do
      nice "aaa" `shouldBe` True
  describe "negative examples" $ do
    it "jchzalrnumimnmhp" $ do
      nice "jchzalrnumimnmhp" `shouldBe` False
    it "haegwjzuvuyypxyu" $ do
      nice "haegwjzuvuyypxyu" `shouldBe` False
    it "dvszwmarrgswjxmb" $ do
      nice "dvszwmarrgswjxmb" `shouldBe` False
  describe "pairRep tests" $ do
    it "xyxy" $ do
      pairRep "xyxy"  `shouldBe` True
    it "aabcdefgaa" $ do
      pairRep "aabcdefgaa" `shouldBe` True
    it "aaa" $ do
      pairRep "aaa" `shouldBe` False
  describe "skipRep tests" $ do
    it "all skip rep tests: xyx, abcdefeghi, and aaa" $ do
      all skipRep ["xyx", "abcdefeghi", "aaa"] `shouldBe` True
