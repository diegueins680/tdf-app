{-# LANGUAGE OverloadedStrings #-}

module TDF.APITypesSpec (spec) where

import           Test.Hspec
import           Data.Aeson (eitherDecode)

import           TDF.API.Types (RolePayload(..))

spec :: Spec
spec = describe "RolePayload FromJSON" $ do
  it "parses raw string payloads" $
    decodeRole "\"Engineer\"" `shouldBe` Right (RolePayload "Engineer")

  it "parses object payloads that provide the role field" $
    decodeRole "{\"role\":\"Teacher\"}" `shouldBe` Right (RolePayload "Teacher")

  it "parses object payloads that provide a value field" $
    decodeRole "{\"value\":\"Artist\"}" `shouldBe` Right (RolePayload "Artist")

  it "fails when neither role nor value is present" $
    decodeRole "{}" `shouldSatisfy` isLeft
  where
    decodeRole = eitherDecode
    isLeft (Left _)  = True
    isLeft (Right _) = False
