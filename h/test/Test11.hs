module Test11 (test) where
import Day11 (goodPass, findNext)
import Test.Hspec

test :: IO ()
test = hspec $ do
  describe "test 11: " $ do
    it "example hijklmmn" $ do
      goodPass "hijklmmn" `shouldBe` False
    it "example abbceffg" $ do
      goodPass "abbceffg" `shouldBe` False
    it "example abbcegjk" $ do
      goodPass "abbcegjk" `shouldBe` False
    it "example abcdffaa" $ do
      goodPass "abcdffaa" `shouldBe` True
    it "example ghjaabcc" $ do
      goodPass "ghjaabcc" `shouldBe` True
    it "nextpass abcdefgh" $ do
      findNext "abcdefgh" `shouldBe` "abcdffaa"
    it "nextpass ghijklmn" $ do
      findNext "ghijklmn" `shouldBe` "ghjaabcc"
