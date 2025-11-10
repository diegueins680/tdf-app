module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $
  describe "Sanity" $
    it "keeps the test harness wired" $
      True `shouldBe` True
