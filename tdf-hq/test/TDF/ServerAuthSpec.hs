{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerAuthSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (chr)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist (Entity (..), Key)
import Database.Persist.Sql (toSqlKey)
import Servant (ServerError (errBody, errHTTPCode))
import Test.Hspec

import TDF.Models (UserCredential (..))
import TDF.ServerAuth
  ( GoogleIdTokenInfo (..)
  , GoogleProfile (..)
  , normalizeAuthEmailAddress
  , selectUniqueGoogleLoginCredential
  , selectUniqueLoginEmailCredential
  , selectUniquePasswordResetCredential
  , validateGoogleIdTokenInfo
  , validatePasswordResetToken
  , validateSignupDisplayName
  )

spec :: Spec
spec = do
  authEmailSpec
  signupDisplayNameSpec
  passwordResetTokenSpec
  googleTokenInfoSpec
  loginEmailFallbackSpec
  googleLoginEmailFallbackSpec
  passwordResetDeliverySpec

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

signupDisplayNameSpec :: Spec
signupDisplayNameSpec = describe "validateSignupDisplayName" $ do
  it "trims signup name parts into the stored display name" $
    validateSignupDisplayName "  Ada  " "  Lovelace  "
      `shouldBe` Right "Ada Lovelace"

  it "rejects hidden formatting characters before signup stores ambiguous display names" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
        assertRejected label result =
          case result of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` label
              BL8.unpack (errBody err) `shouldContain` "hidden formatting"
            Right value ->
              expectationFailure
                ("Expected hidden formatting character to be rejected, got " <> show value)
    assertRejected "firstName" $
      validateSignupDisplayName ("Ada" <> hiddenFormat) "Lovelace"
    assertRejected "lastName" $
      validateSignupDisplayName "Ada" ("Love" <> hiddenFormat <> "lace")

passwordResetTokenSpec :: Spec
passwordResetTokenSpec = describe "validatePasswordResetToken" $ do
  it "trims surrounding whitespace on canonical reset tokens before confirmation" $
    validatePasswordResetToken "  550e8400-e29b-41d4-a716-446655440000  "
      `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

  it "canonicalizes uppercase UUID reset tokens before confirmation" $
    validatePasswordResetToken "550E8400-E29B-41D4-A716-446655440000"
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
              BL8.unpack (errBody err) `shouldContain` "Token must not contain whitespace or control characters"
            Right value ->
              expectationFailure ("Expected malformed reset token to be rejected, got " <> show value)
    assertRejected "550e8400 e29b-41d4-a716-446655440000"
    assertRejected "550e8400-e29b-41d4-a716-\n446655440000"

  it "rejects non-UUID reset tokens before the confirm flow performs a database lookup" $
    case validatePasswordResetToken "not-a-real-reset-token" of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err) `shouldContain` "Token format is invalid"
      Right value ->
        expectationFailure ("Expected malformed reset token to be rejected, got " <> show value)

googleTokenInfoSpec :: Spec
googleTokenInfoSpec = describe "validateGoogleIdTokenInfo" $ do
  it "normalizes Google emails only after rejecting invalid token email shapes" $ do
    case validateGoogleIdTokenInfo (Just "client-id") googleTokenInfo of
      Right profile -> do
        gpEmail profile `shouldBe` "ada@example.com"
        gpName profile `shouldBe` Just "Ada Lovelace"
      Left err ->
        expectationFailure ("Expected valid Google token info, got " <> T.unpack err)

    case validateGoogleIdTokenInfo
      (Just "client-id")
      googleTokenInfo { gitEmail = "ada@localhost" } of
      Left err ->
        T.unpack err `shouldContain` "correo válido"
      Right profile ->
        expectationFailure
          ("Expected malformed Google token email to be rejected, got " <> show profile)

  it "rejects blank or unsafe Google subjects before account fallback creation" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
        assertRejected rawSubject =
          case validateGoogleIdTokenInfo
            (Just "client-id")
            googleTokenInfo { gitSub = rawSubject } of
            Left err ->
              T.unpack err `shouldContain` "identificador válido"
            Right profile ->
              expectationFailure
                ("Expected malformed Google subject to be rejected, got " <> show profile)
    assertRejected "   "
    assertRejected "google sub 1"
    assertRejected ("google-sub-" <> hiddenFormat <> "1")

loginEmailFallbackSpec :: Spec
loginEmailFallbackSpec = describe "selectUniqueLoginEmailCredential" $ do
  it "allows exactly one login email fallback credential candidate" $
    selectedLoginCredentialKey [credentialEntity 21 201]
      `shouldBe` Just (toSqlKey 21)

  it "rejects ambiguous login email fallback candidates instead of picking the first" $ do
    selectedLoginCredentialKey []
      `shouldBe` Nothing
    selectedLoginCredentialKey
      [ credentialEntity 21 201
      , credentialEntity 22 202
      ]
      `shouldBe` Nothing

googleLoginEmailFallbackSpec :: Spec
googleLoginEmailFallbackSpec = describe "selectUniqueGoogleLoginCredential" $ do
  it "allows absent or exactly one Google email credential candidate" $ do
    selectedGoogleCredentialKey []
      `shouldBe` Right Nothing
    selectedGoogleCredentialKey [credentialEntity 31 301]
      `shouldBe` Right (Just (toSqlKey 31))

  it "rejects ambiguous Google email credentials before account fallback creation" $
    case selectedGoogleCredentialKey
      [ credentialEntity 31 301
      , credentialEntity 32 302
      ] of
      Left err ->
        T.unpack err `shouldContain` "varias cuentas"
      Right value ->
        expectationFailure
          ("Expected ambiguous Google email credentials to fail, got " <> show value)

passwordResetDeliverySpec :: Spec
passwordResetDeliverySpec = describe "selectUniquePasswordResetCredential" $ do
  it "allows exactly one password reset credential candidate" $
    selectedCredentialKey [credentialEntity 11 101]
      `shouldBe` Just (toSqlKey 11)

  it "rejects missing or ambiguous password reset credential candidates instead of picking the first match" $ do
    selectedCredentialKey []
      `shouldBe` Nothing
    selectedCredentialKey
      [ credentialEntity 11 101
      , credentialEntity 12 102
      ]
      `shouldBe` Nothing

credentialEntity :: Int64 -> Int64 -> Entity UserCredential
credentialEntity credentialId partyId =
  Entity
    (toSqlKey credentialId)
    UserCredential
      { userCredentialPartyId = toSqlKey partyId
      , userCredentialUsername = "reset@example.com"
      , userCredentialPasswordHash = "hash"
      , userCredentialActive = True
      }

googleTokenInfo :: GoogleIdTokenInfo
googleTokenInfo =
  GoogleIdTokenInfo
    { gitAud = "client-id"
    , gitEmail = " ADA@Example.COM "
    , gitEmailVerified = True
    , gitName = Just " Ada Lovelace "
    , gitPicture = Nothing
    , gitSub = "google-sub-1"
    , gitIss = Just "https://accounts.google.com"
    }

credentialEntityKey :: Entity UserCredential -> Key UserCredential
credentialEntityKey (Entity key _) = key

selectedCredentialKey :: [Entity UserCredential] -> Maybe (Key UserCredential)
selectedCredentialKey = fmap credentialEntityKey . selectUniquePasswordResetCredential

selectedLoginCredentialKey :: [Entity UserCredential] -> Maybe (Key UserCredential)
selectedLoginCredentialKey = fmap credentialEntityKey . selectUniqueLoginEmailCredential

selectedGoogleCredentialKey
  :: [Entity UserCredential]
  -> Either T.Text (Maybe (Key UserCredential))
selectedGoogleCredentialKey =
  fmap (fmap credentialEntityKey) . selectUniqueGoogleLoginCredential
