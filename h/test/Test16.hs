module Test16 (test) where

import Test.Hspec
import Day16
import Data.HashMap.Strict (fromList)

someTrivialSubs :: [Poss]
someTrivialSubs = map fromList [ [(Children, 3)]
                               , [(Cats, 7)]
                               , [(Trees, 3), (Perfumes, 1)]]
someTrivialNonSubs :: [Poss]
someTrivialNonSubs = map fromList [ [(Children, 0)]
                                  , [(Cats, 7), (Cars, 1)]
                                  , [(Perfumes, 10)]
                                  , [(Akitas, 1)]]
test :: IO ()
test = hspec $ do
  describe "Day16 tests:" $ do
    it "some trivial subsets of the scanner data" $ do
      all (`isSubset` scannerData) someTrivialSubs `shouldBe` True
    it "some trivial non-subsets of the scanner data" $do
      any (`isSubset` scannerData) someTrivialNonSubs `shouldBe` False
