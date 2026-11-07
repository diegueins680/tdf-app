{-# LANGUAGE OverloadedStrings #-}

module TDF.Auth.AccountNameSpec (spec) where

import Test.Hspec
import TDF.Auth.AccountName (initialAccountName)

spec :: Spec
spec = describe "initial account public names" $ do
  it "uses a neutral name when the provider supplies no name" $
    initialAccountName Nothing `shouldBe` "Cuenta TDF"
  it "uses a neutral name for an empty provider name" $
    initialAccountName (Just "") `shouldBe` "Cuenta TDF"
  it "uses a neutral name for a whitespace-only provider name" $
    initialAccountName (Just "  \t ") `shouldBe` "Cuenta TDF"
  it "preserves an explicitly supplied name while trimming whitespace" $
    initialAccountName (Just "  Música Independiente  ") `shouldBe` "Música Independiente"
