{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerAuthSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.ServerAuth (normalizeAuthEmailAddress, validatePasswordResetToken)

spec :: Spec
spec = do
  authEmailSpec
  passwordResetTokenSpec

authEmailSpec :: Spec
authEmailSpec = describe "normalizeAuthEmailAddress" $ do
  it "normalizes a maximum-sized auth email before signup or reset flows use it" $ do
    let localPart = T.replicate 64 "a"
        domain =
          T.intercalate
            "."
            [ T.replicate 63 "b"
            , T.replicate 63 "c"
            , T.replicate 61 "d"
            ]
        email = localPart <> "@" <> domain
    T.length email `shouldBe` 254
    normalizeAuthEmailAddress (" " <> T.toUpper email <> " ") `shouldBe` Just email

  it "rejects oversized auth email parts before database or email fallbacks run" $ do
    normalizeAuthEmailAddress (T.replicate 65 "a" <> "@example.com")
      `shouldBe` Nothing
    normalizeAuthEmailAddress ("user@" <> T.replicate 64 "b" <> ".com")
      `shouldBe` Nothing
    normalizeAuthEmailAddress
      ( T.replicate 64 "a"
          <> "@"
          <> T.intercalate
            "."
            [ T.replicate 63 "b"
            , T.replicate 63 "c"
            , T.replicate 62 "d"
            ]
      )
      `shouldBe` Nothing

passwordResetTokenSpec :: Spec
passwordResetTokenSpec = describe "validatePasswordResetToken" $ do
  it "trims surrounding whitespace on canonical reset tokens before confirmation" $
    validatePasswordResetToken "  550e8400-e29b-41d4-a716-446655440000  "
      `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

  it "rejects blank reset tokens instead of querying the database with empty input" $
    case validatePasswordResetToken "   " of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err) `shouldContain` "Token is required"
      Right value ->
        expectationFailure ("Expected blank reset token to be rejected, got " <> show value)

  it "rejects oversized reset tokens before the confirm path performs a token lookup" $
    case validatePasswordResetToken (T.replicate 513 "a") of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err) `shouldContain` "Token must be 512 characters or fewer"
      Right value ->
        expectationFailure ("Expected oversized reset token to be rejected, got " <> show value)

  it "rejects whitespace or control characters inside reset tokens" $ do
    let assertRejected rawToken =
          case validatePasswordResetToken rawToken of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain`
                "Token must not contain whitespace or control characters"
            Right value ->
              expectationFailure ("Expected malformed reset token to be rejected, got " <> show value)
    assertRejected "550e8400 e29b-41d4-a716-446655440000"
    assertRejected "550e8400-e29b-41d4-a716-\n446655440000"
