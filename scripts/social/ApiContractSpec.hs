{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson (eitherDecode, encode)
import Test.Hspec
import TDF.Social.API

main :: IO ()
main = hspec $ do
  describe "social command trust boundary" $ do
    it "round-trips operation, expected revision and request identity" $
      eitherDecode (encode (Command "request" 2 "retry")) `shouldBe` Right (Command "request" 2 "retry")
    it "rejects caller-supplied acting identity" $
      (eitherDecode "{\"operation\":\"request\",\"expectedRevision\":0,\"requestKey\":\"x\",\"actor\":99}" :: Either String Command)
        `shouldSatisfy` either (const True) (const False)
    it "rejects a fractional revision" $
      (eitherDecode "{\"operation\":\"request\",\"expectedRevision\":0.5,\"requestKey\":\"x\"}" :: Either String Command)
        `shouldSatisfy` either (const True) (const False)
    it "preserves independent discoverability and personalization settings" $
      eitherDecode (encode (Preferences True False 1)) `shouldBe` Right (Preferences True False 1)
