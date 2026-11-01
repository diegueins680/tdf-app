{-# LANGUAGE OverloadedStrings #-}

module TDF.Auth.GoogleProfileNameSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Time (UTCTime(..), fromGregorian)
import Test.Hspec
import TDF.Auth.AccountName (initialAccountName)
import TDF.ServerAuth
  ( GoogleProfile(..), validateGoogleIdTokenInfo, validateGoogleTokenExpiry )

spec :: Spec
spec = describe "Google public-name privacy through token decoding" $ do
  forM_ [ ("missing", [], Nothing)
        , ("null", ["name" A..= A.Null], Nothing)
        , ("empty", ["name" A..= ("" :: T.Text)], Nothing)
        , ("whitespace", ["name" A..= ("  \t " :: T.Text)], Nothing)
        , ("punctuation", ["name" A..= (" ... --- " :: T.Text)], Nothing)
        , ("hidden format", ["name" A..= ("Private\x200dUser" :: T.Text)], Nothing)
        , ("control character", ["name" A..= ("Private\nUser" :: T.Text)], Nothing)
        , ("overlong", ["name" A..= T.replicate 161 "a"], Nothing)
        , ("explicit", ["name" A..= ("  Public Name  " :: T.Text)], Just "Public Name")
        ] $ \(label, fields, expectedName) ->
    it ("keeps private contact data out of the public name for a " <> label <> " name") $
      case decoded (A.object (baseFields <> fields)) of
        Left problem -> expectationFailure (T.unpack problem)
        Right profile -> do
          -- Exercise the actual selector after JSON decoding and validation.
          initialAccountName (gpName profile) `shouldBe` maybe "Cuenta TDF" id expectedName
          gpName profile `shouldBe` expectedName
          gpEmail profile `shouldBe` "private.tag+contact@example.invalid"
          gpSubject profile `shouldBe` "google-sub-1"
          gpIssuer profile `shouldBe` "https://accounts.google.com"
  forM_ [ ("unverified email", "email_verified", A.Bool False)
        , ("wrong audience", "aud", A.String "another-client")
        , ("wrong issuer", "iss", A.String "https://issuer.example.invalid")
        , ("blank subject", "sub", A.String " ")
        , ("invalid email", "email", A.String "not-an-email")
        , ("expired token", "exp", A.Number 99)
        ] $ \(label, key, value) ->
    it ("retains authentication rejection for " <> label) $
      case A.object baseFields of
        A.Object fields -> decoded (A.Object (KM.insert key value fields)) `shouldSatisfy` isLeft
        _ -> expectationFailure "Fixture must be an object"
  where
    baseFields =
      [ "aud" A..= ("client-id" :: T.Text)
      , "email" A..= (" PRIVATE.TAG+contact@Example.Invalid " :: T.Text)
      , "email_verified" A..= True
      , "sub" A..= ("google-sub-1" :: T.Text)
      , "iss" A..= ("https://accounts.google.com" :: T.Text)
      , "exp" A..= (4102444800 :: Integer)
      ]
    decoded value = case A.fromJSON value of
      A.Error problem -> Left (T.pack problem)
      A.Success info ->
        validateGoogleTokenExpiry (UTCTime (fromGregorian 1970 1 1) 100) info
          >> validateGoogleIdTokenInfo (Just "client-id") info
