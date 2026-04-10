{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import Data.Aeson (eitherDecode)
import Data.Proxy (Proxy (..))
import Servant.API (MimeUnrender (mimeUnrender))
import Test.Hspec

import TDF.API.Types (LooseJSON, RolePayload (..))

spec :: Spec
spec = do
    describe "RolePayload FromJSON" $ do
        it "parses raw string payloads" $
            decodeRole "\"Engineer\"" `shouldBe` Right (RolePayload "Engineer")

        it "parses object payloads that provide the role field" $
            decodeRole "{\"role\":\"Teacher\"}" `shouldBe` Right (RolePayload "Teacher")

        it "parses object payloads that provide a value field" $
            decodeRole "{\"value\":\"Artist\"}" `shouldBe` Right (RolePayload "Artist")

        it "fails when neither role nor value is present" $
            decodeRole "{}" `shouldSatisfy` isLeft

        it "fails when both role and value are present to avoid ambiguous role assignment bodies" $
            decodeRole "{\"role\":\"Teacher\",\"value\":\"Artist\"}" `shouldSatisfy` isLeft

    describe "RolePayload LooseJSON MimeUnrender" $ do
        it "still accepts plain text role bodies sent as application/json" $
            decodeLooseRole "Teacher" `shouldBe` Right (RolePayload "Teacher")

        it "rejects malformed or ambiguous JSON-like bodies instead of treating them as raw role text" $ do
            decodeLooseRole "{\"role\":\"Teacher\",\"value\":\"Artist\"}" `shouldSatisfy` isLeft
            decodeLooseRole "{}" `shouldSatisfy` isLeft
  where
    decodeRole = eitherDecode
    decodeLooseRole = mimeUnrender (Proxy :: Proxy LooseJSON)
    isLeft (Left _) = True
    isLeft (Right _) = False
