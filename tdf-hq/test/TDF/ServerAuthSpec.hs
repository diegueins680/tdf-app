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

import TDF.Auth
  ( AuthedUser (..)
  , ModuleAccess (..)
  , hasModuleAccess
  , modulesForRoles
  , resolveUsernameFromLabel
  , validateModuleAccess
  )
import TDF.DTO (LoginRequest (..))
import TDF.Models (RoleEnum (..), UserCredential (..))
import TDF.ServerAuth
  ( GoogleIdTokenInfo (..)
  , GoogleProfile (..)
  , normalizeAuthEmailAddress
  , parsePasswordChangeAuthToken
  , selectUniqueGoogleLoginCredential
  , selectUniqueLoginEmailCredential
  , selectUniquePasswordResetCredential
  , validateCurrentPasswordInput
  , validateGoogleIdTokenInput
  , validateLoginRequest
  , validateGoogleIdTokenInfo
  , validatePasswordResetToken
  , validateRequestedSignupRoles
  , validateSignupArtistClaimEmail
  , validateSignupDisplayName
  , validateSignupGoogleIdToken
  , validateOptionalSignupPhone
  )

spec :: Spec
spec = do
  authEmailSpec
  moduleAccessSpec
  loginRequestSpec
  currentPasswordInputSpec
  passwordChangeAuthHeaderSpec
  tokenLabelUsernameSpec
  signupRoleSpec
  signupDisplayNameSpec
  signupGoogleIdTokenSpec
  signupPhoneSpec
  signupArtistClaimEmailSpec
  passwordResetTokenSpec
  googleIdTokenInputSpec
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

  it "rejects ambiguous final domain labels before auth fallback lookups run" $ do
    normalizeAuthEmailAddress "ada@example.123" `shouldBe` Nothing
    normalizeAuthEmailAddress "ada@example.c" `shouldBe` Nothing
    normalizeAuthEmailAddress "ada@example.co" `shouldBe` Just "ada@example.co"

moduleAccessSpec :: Spec
moduleAccessSpec = describe "validateModuleAccess" $ do
  let mkUser roles =
        AuthedUser
          { auPartyId = toSqlKey 1
          , auRoles = roles
          , auModules = modulesForRoles roles
          }
      assertRejected expectedMessage result =
        case result of
          Left err -> do
            errHTTPCode err `shouldBe` 403
            BL8.unpack (errBody err) `shouldContain` expectedMessage
          Right value ->
            expectationFailure ("Expected module access to be rejected, got " <> show value)

  it "allows coherent users with the required module grant" $
    validateModuleAccess ModuleAdmin (mkUser [Admin]) `shouldBe` Right ()

  it "denies direct module checks for impossible party ids, stale grants, or duplicates" $ do
    hasModuleAccess ModuleAdmin (mkUser [Admin]) `shouldBe` True
    hasModuleAccess
      ModuleAdmin
      ((mkUser [Admin]) { auPartyId = toSqlKey 0 })
      `shouldBe` False
    hasModuleAccess ModuleAdmin (mkUser [Admin, Admin]) `shouldBe` False
    hasModuleAccess
      ModuleAdmin
      ((mkUser [Admin]) { auModules = modulesForRoles [Webmaster] })
      `shouldBe` False

  it "rejects impossible party ids or incoherent grants before handler authorization" $ do
    assertRejected "Valid authenticated party required" $
      validateModuleAccess
        ModuleAdmin
        ((mkUser [Admin]) { auPartyId = toSqlKey 0 })
    assertRejected "Missing access to module: Admin" $
      validateModuleAccess ModuleAdmin (mkUser [Fan])
    assertRejected "Role grants must be unique" $
      validateModuleAccess ModuleAdmin (mkUser [Admin, Admin])
    assertRejected "Module grants must match roles" $
      validateModuleAccess
        ModuleAdmin
        ((mkUser [Admin]) { auModules = modulesForRoles [Webmaster] })

loginRequestSpec :: Spec
loginRequestSpec = describe "validateLoginRequest" $ do
  it "trims canonical login inputs before credential lookup" $
    case validateLoginRequest (LoginRequest " ada@example.com " " TempPass123! ") of
      Right (LoginRequest usernameVal passwordVal) -> do
        usernameVal `shouldBe` "ada@example.com"
        passwordVal `shouldBe` "TempPass123!"
      Left err ->
        expectationFailure ("Expected valid login request, got " <> show err)

  it "rejects malformed login inputs before database fallback lookup" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
        assertRejected label request =
          case validateLoginRequest request of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` label
            Right value ->
              expectationFailure ("Expected invalid login request, got " <> show value)
    assertRejected "Username is required" (LoginRequest "   " "TempPass123!")
    assertRejected
      "Username must not contain whitespace"
      (LoginRequest "ada example" "TempPass123!")
    assertRejected
      "hidden formatting"
      (LoginRequest ("ada" <> hiddenFormat <> "@example.com") "TempPass123!")
    assertRejected "Password is required" (LoginRequest "ada@example.com" "   ")
    assertRejected
      "Password must not contain control characters"
      (LoginRequest "ada@example.com" "Temp\nPass123!")
    assertRejected
      "Password must not contain hidden formatting characters"
      (LoginRequest "ada@example.com" ("Temp" <> hiddenFormat <> "Pass123!"))

currentPasswordInputSpec :: Spec
currentPasswordInputSpec = describe "validateCurrentPasswordInput" $ do
  it "trims current passwords before the password-change credential check" $
    validateCurrentPasswordInput "  old-pass  " `shouldBe` Right "old-pass"

  it "rejects current passwords that bcrypt would truncate before password-change lookup" $ do
    let assertRejected label rawPassword =
          case validateCurrentPasswordInput rawPassword of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` label
            Right value ->
              expectationFailure ("Expected invalid current password, got " <> show value)
    assertRejected "Current password is required" "   "
    assertRejected "72 bytes or fewer" (T.replicate 73 "a")
    assertRejected "control characters" "old\npass"
    assertRejected
      "hidden formatting"
      ("old" <> T.singleton (chr 0x200D) <> "pass")

passwordChangeAuthHeaderSpec :: Spec
passwordChangeAuthHeaderSpec = describe "parsePasswordChangeAuthToken" $ do
  it "requires literal spaces before password-change token fallback lookup" $ do
    parsePasswordChangeAuthToken " Bearer session-token "
      `shouldBe` Right "session-token"

    let assertRejected rawHeader =
          case parsePasswordChangeAuthToken rawHeader of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err)
                `shouldContain` "Authorization header must be Bearer <token>"
            Right value ->
              expectationFailure
                ("Expected malformed Authorization header to be rejected, got " <> show value)

    assertRejected "Bearer\tsession-token"
    assertRejected ("Bearer" <> T.singleton (chr 0x00A0) <> "session-token")

tokenLabelUsernameSpec :: Spec
tokenLabelUsernameSpec = describe "resolveUsernameFromLabel" $ do
  it "keeps generated auth labels usable as current-session username fallbacks" $ do
    resolveUsernameFromLabel " password-login: ada@example.com "
      `shouldBe` Just "ada@example.com"
    resolveUsernameFromLabel "google-login:ada@example.com"
      `shouldBe` Just "ada@example.com"

  it "rejects malformed stored label usernames before session fallback rendering" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
    resolveUsernameFromLabel ("password-login:ada" <> hiddenFormat <> "@example.com")
      `shouldBe` Nothing
    resolveUsernameFromLabel "password-login:ada example.com"
      `shouldBe` Nothing
    resolveUsernameFromLabel ("password-login:" <> T.replicate 255 "a")
      `shouldBe` Nothing

signupRoleSpec :: Spec
signupRoleSpec = describe "validateRequestedSignupRoles" $ do
  it "keeps default signup roles and accepted requested roles" $
    validateRequestedSignupRoles (Just [Artist])
      `shouldBe` Right [Customer, Fan, Artist]

  it "rejects duplicate requested roles instead of silently collapsing signup intent" $
    case validateRequestedSignupRoles (Just [Artist, Fan, Artist]) of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err)
          `shouldContain` "Requested signup roles must not contain duplicates: Artist"
      Right value ->
        expectationFailure ("Expected duplicate signup roles to be rejected, got " <> show value)

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

signupGoogleIdTokenSpec :: Spec
signupGoogleIdTokenSpec = describe "validateSignupGoogleIdToken" $ do
  it "rejects Google id tokens on password signup instead of silently ignoring them" $ do
    validateSignupGoogleIdToken Nothing `shouldBe` Right ()
    validateSignupGoogleIdToken (Just "   ") `shouldBe` Right ()
    case validateSignupGoogleIdToken (Just "header.payload.signature") of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err)
          `shouldContain` "googleIdToken is not supported on password signup"
      Right value ->
        expectationFailure
          ("Expected password signup googleIdToken to be rejected, got " <> show value)

signupPhoneSpec :: Spec
signupPhoneSpec = describe "validateOptionalSignupPhone" $ do
  it "normalizes explicit signup phone separators before persistence" $
    validateOptionalSignupPhone (Just " +593 99 123 4567 ")
      `shouldBe` Right (Just "+593991234567")

  it "rejects Unicode line or hidden phone separators before signup persistence" $ do
    let assertRejected rawPhone =
          case validateOptionalSignupPhone (Just rawPhone) of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` "phone must be a valid phone number"
            Right value ->
              expectationFailure
                ("Expected unsafe signup phone to be rejected, got " <> show value)
    assertRejected ("099" <> T.singleton (chr 0x2028) <> "1234567")
    assertRejected ("099" <> T.singleton (chr 0x2029) <> "1234567")
    assertRejected ("099" <> T.singleton (chr 0x200D) <> "1234567")

signupArtistClaimEmailSpec :: Spec
signupArtistClaimEmailSpec = describe "validateSignupArtistClaimEmail" $ do
  it "allows unclaimed artist profiles with no stored email or the same normalized email" $ do
    validateSignupArtistClaimEmail "ada@example.com" Nothing `shouldBe` Right ()
    validateSignupArtistClaimEmail " ada@example.com " (Just "ADA@Example.com")
      `shouldBe` Right ()
    validateSignupArtistClaimEmail "ada@example.com" (Just "   ")
      `shouldBe` Right ()

  it "rejects mismatched or malformed stored emails before binding a signup to an artist profile" $ do
    validateSignupArtistClaimEmail "ada@example.com" (Just "other@example.com")
      `shouldBe` Left "Artist profile email does not match signup email"
    validateSignupArtistClaimEmail "ada@example.com" (Just "not-an-email")
      `shouldBe` Left "Artist profile email does not match signup email"

passwordResetTokenSpec :: Spec
passwordResetTokenSpec = describe "validatePasswordResetToken" $ do
  it "trims surrounding whitespace on canonical reset tokens before confirmation" $
    validatePasswordResetToken "  550e8400-e29b-41d4-a716-446655440000  "
      `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

  it "canonicalizes uppercase UUID reset tokens before confirmation" $
    validatePasswordResetToken "550E8400-E29B-41D4-A716-446655440000"
      `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

  it "rejects nil UUID reset tokens before the confirm flow performs a database lookup" $
    case validatePasswordResetToken "00000000-0000-0000-0000-000000000000" of
      Left err -> do
        errHTTPCode err `shouldBe` 400
        BL8.unpack (errBody err) `shouldContain` "Token format is invalid"
      Right value ->
        expectationFailure ("Expected nil reset token to be rejected, got " <> show value)

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

googleIdTokenInputSpec :: Spec
googleIdTokenInputSpec = describe "validateGoogleIdTokenInput" $ do
  it "trims canonical Google id tokens before tokeninfo verification" $
    validateGoogleIdTokenInput "  header.payload.signature  "
      `shouldBe` Right "header.payload.signature"

  it "rejects malformed Google id tokens before tokeninfo fallback verification" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
        assertRejected expectedMessage rawToken =
          case validateGoogleIdTokenInput rawToken of
            Left err -> do
              errHTTPCode err `shouldBe` 400
              BL8.unpack (errBody err) `shouldContain` expectedMessage
            Right value ->
              expectationFailure
                ("Expected invalid Google idToken to be rejected, got " <> show value)
    assertRejected "Google idToken is required" "   "
    assertRejected "Google idToken must not contain whitespace" "header.payload signature"
    assertRejected
      "Google idToken must not contain control characters"
      "header.payload\NULsignature"
    assertRejected
      "Google idToken must not contain hidden formatting"
      ("header" <> hiddenFormat <> ".payload")
    assertRejected "Google idToken must contain only ASCII characters" "header.páyload.signature"
    assertRejected
      "Google idToken must be a JWT with three non-empty segments"
      "header.payload"
    assertRejected
      "Google idToken must be a JWT with three non-empty segments"
      "header..signature"
    assertRejected
      "Google idToken must be 4096 characters or fewer"
      (T.replicate 4097 "a")

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
    assertRejected "gøogle-sub-1"
    assertRejected (T.replicate 256 "a")
    assertRejected ("google-sub-" <> hiddenFormat <> "1")

  it "falls back to verified email when Google profile names are unsafe" $ do
    let hiddenFormat = T.singleton (chr 0x200D)
        assertFallsBack rawName =
          case validateGoogleIdTokenInfo
            (Just "client-id")
            googleTokenInfo { gitName = Just rawName } of
            Right profile ->
              gpName profile `shouldBe` Just "ada@example.com"
            Left err ->
              expectationFailure
                ("Expected unsafe Google profile name to fall back, got " <> T.unpack err)
    assertFallsBack ("Ada" <> hiddenFormat <> "Lovelace")
    assertFallsBack "Ada\nLovelace"
    assertFallsBack (T.replicate 161 "a")

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
