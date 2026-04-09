{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist (Entity(..), get, insert)
import Database.Persist.Sql (SqlPersistT, rawExecute, toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import TDF.Auth (AuthedUser (..), hasAiToolingAccess, hasOperationsAccess, hasSocialInboxAccess, hasSocialSyncAccess, hasStrictAdminAccess, modulesForRoles)
import Servant (ServerError (errBody, errHTTPCode))
import TDF.Models (ApiToken (..), BookingStatus (..), Party (..), PaymentMethod (..), PricingModel (..), RoleEnum (..), ServiceCatalog (..), ServiceKind (..), UserCredential (..))
import TDF.Server
    ( normalizeOptionalInput
    , parseBookingStatus
    , parseCourseFollowUpType
    , parseCourseRegistrationStatus
    , parsePaymentMethodText
    , validateBookingListFilters
    , validateRolePayload
    , validateServiceAdCurrency
    , validateCmsContentStatus
    , validateCourseNonNegativeField
    , validateCourseRegistrationContactChannels
    , validateCourseRegistrationEmail
    , validateCourseRegistrationPhoneE164
    , validateOptionalCourseNonNegativeField
    , validateOptionalPositiveIdField
    , validatePublicBookingContactDetails
    , validateServiceMarketplaceCatalog
    , validateWhatsAppPhoneInput
    )
import TDF.ServerAuth
    ( normalizeAuthEmailAddress
    , resolvePasswordResetDelivery
    , runPasswordResetConfirm
    , signupEmailExists
    )
import Test.Hspec

mkUser :: [RoleEnum] -> AuthedUser
mkUser roles =
    AuthedUser
        { auPartyId = toSqlKey 1
        , auRoles = roles
        , auModules = modulesForRoles roles
        }

mkCatalog :: ServiceKind -> Bool -> ServiceCatalog
mkCatalog kind active =
    ServiceCatalog
        { serviceCatalogName = "Marketplace catalog"
        , serviceCatalogKind = kind
        , serviceCatalogPricingModel = Hourly
        , serviceCatalogDefaultRateCents = Just 9000
        , serviceCatalogTaxBps = Nothing
        , serviceCatalogCurrency = "USD"
        , serviceCatalogBillingUnit = Just "session"
        , serviceCatalogActive = active
        }

expectCatalogError :: Either ServerError ServiceKind -> (ServerError -> Expectation) -> Expectation
expectCatalogError result assertErr =
    case result of
        Left serverErr -> assertErr serverErr
        Right kind -> expectationFailure ("Expected catalog validation error, got kind: " <> show kind)

spec :: Spec
spec = describe "TDF.Server helpers" $ do
    describe "normalizeOptionalInput" $ do
        it "returns Nothing when input is Nothing" $
            normalizeOptionalInput Nothing `shouldBe` Nothing

        it "trims whitespace and preserves meaningful content" $
            normalizeOptionalInput (Just "   Live Room  ") `shouldBe` Just "Live Room"

        it "drops strings that only contain whitespace" $
            normalizeOptionalInput (Just "   ") `shouldBe` Nothing

    describe "validateOptionalPositiveIdField" $ do
        it "preserves omitted ids and accepts positive identifiers" $ do
            validateOptionalPositiveIdField "engineerPartyId" Nothing `shouldBe` Right Nothing
            validateOptionalPositiveIdField "engineerPartyId" (Just 42) `shouldBe` Right (Just 42)

        it "rejects zero or negative ids instead of accepting invalid booking references" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "engineerPartyId must be a positive integer"
                    Right value ->
                        expectationFailure
                            ("Expected invalid optional id input to be rejected, got: " <> show value)
            assertInvalid (validateOptionalPositiveIdField "engineerPartyId" (Just 0))
            assertInvalid (validateOptionalPositiveIdField "engineerPartyId" (Just (-7)))

    describe "validateBookingListFilters" $ do
        it "preserves omitted filters and accepts positive booking list identifiers" $
            validateBookingListFilters (Just 7) (Just 11) (Just 13)
                `shouldBe` Right (Just 7, Just 11, Just 13)

        it "rejects invalid booking list filters instead of silently broadening the query" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right filtersVal ->
                        expectationFailure
                            ("Expected invalid booking list filters to be rejected, got: " <> show filtersVal)
            assertInvalid "bookingId must be a positive integer" (validateBookingListFilters (Just 0) Nothing Nothing)
            assertInvalid "partyId must be a positive integer" (validateBookingListFilters Nothing (Just (-1)) Nothing)
            assertInvalid "engineerPartyId must be a positive integer" (validateBookingListFilters Nothing Nothing (Just 0))

    describe "validateCmsContentStatus" $ do
        it "defaults omitted status to draft and normalizes supported explicit values" $ do
            validateCmsContentStatus Nothing `shouldBe` Right "draft"
            validateCmsContentStatus (Just " Published ") `shouldBe` Right "published"
            validateCmsContentStatus (Just "ARCHIVED") `shouldBe` Right "archived"

        it "rejects blank or unknown explicit statuses instead of persisting ambiguous CMS versions" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "draft, published, archived"
                    Right statusVal ->
                        expectationFailure ("Expected invalid CMS status to be rejected, got: " <> show statusVal)
            assertInvalid (validateCmsContentStatus (Just "   "))
            assertInvalid (validateCmsContentStatus (Just "scheduled"))

    describe "normalizeAuthEmailAddress" $ do
        it "trims and lowercases valid auth emails before signup or reset flows use them" $ do
            normalizeAuthEmailAddress " User@Example.com " `shouldBe` Just "user@example.com"

        it "rejects blank or malformed auth emails instead of sending ambiguous auth responses" $ do
            normalizeAuthEmailAddress "   " `shouldBe` Nothing
            normalizeAuthEmailAddress "not-an-email" `shouldBe` Nothing
            normalizeAuthEmailAddress "user@ example.com" `shouldBe` Nothing

    describe "signupEmailExists" $ do
        it "treats mixed-case stored usernames or party emails as the same signup identity" $ do
            exists <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Signup User"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "User@Example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                _ <- insert UserCredential
                    { userCredentialPartyId = partyId
                    , userCredentialUsername = "User@Example.com"
                    , userCredentialPasswordHash = "hashed"
                    , userCredentialActive = True
                    }
                signupEmailExists "user@example.com"
            exists `shouldBe` True

    describe "resolvePasswordResetDelivery" $ do
        it "resolves active accounts by stored primary email even when the username differs" $ do
            resolved <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Reset User"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "User@Example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                _ <- insert UserCredential
                    { userCredentialPartyId = partyId
                    , userCredentialUsername = "custom-handle"
                    , userCredentialPasswordHash = "hashed"
                    , userCredentialActive = True
                    }
                resolvePasswordResetDelivery "  user@example.com  "
            case resolved of
                Just (Entity _ cred, recipientEmail, displayName) -> do
                    userCredentialUsername cred `shouldBe` "custom-handle"
                    recipientEmail `shouldBe` "User@Example.com"
                    displayName `shouldBe` "Reset User"
                Nothing ->
                    expectationFailure "Expected password reset delivery to resolve by primary email"

        it "ignores inactive credentials for password reset delivery" $ do
            resolved <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Inactive User"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "inactive@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                _ <- insert UserCredential
                    { userCredentialPartyId = partyId
                    , userCredentialUsername = "inactive-user"
                    , userCredentialPasswordHash = "hashed"
                    , userCredentialActive = False
                    }
                resolvePasswordResetDelivery "inactive@example.com"
            case resolved of
                Nothing -> pure ()
                Just _ ->
                    expectationFailure "Expected inactive credential to be ignored for password reset delivery"

    describe "runPasswordResetConfirm" $ do
        it "accepts reset tokens labeled with the account email even when the username differs" $ do
            (result, updatedHash, tokenActive) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Reset User"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "user@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                credId <- insert UserCredential
                    { userCredentialPartyId = partyId
                    , userCredentialUsername = "custom-handle"
                    , userCredentialPasswordHash = "old-hash"
                    , userCredentialActive = True
                    }
                tokenId <- insert ApiToken
                    { apiTokenToken = "reset-token"
                    , apiTokenPartyId = partyId
                    , apiTokenLabel = Just "password-reset:user@example.com"
                    , apiTokenActive = True
                    }
                result <- runPasswordResetConfirm "reset-token" "new-password-123"
                updatedCred <- get credId
                updatedToken <- get tokenId
                pure
                    ( result
                    , fmap userCredentialPasswordHash updatedCred
                    , fmap apiTokenActive updatedToken
                    )

            case result of
                Left _ ->
                    expectationFailure "Expected password reset confirmation to succeed for email-labeled reset tokens"
                Right _ -> pure ()
            updatedHash `shouldNotBe` Just "old-hash"
            tokenActive `shouldBe` Just False

    describe "parseBookingStatus" $ do
        it "parses a known status and ignores surrounding whitespace" $
            parseBookingStatus "  InProgress   " `shouldBe` Right InProgress

        it "accepts separator and casing variants that normalize to a known status" $ do
            parseBookingStatus "confirmed" `shouldBe` Right Confirmed
            parseBookingStatus "no_show" `shouldBe` Right NoShow
            parseBookingStatus "in progress" `shouldBe` Right InProgress

        it "rejects unknown statuses with the allowed values in the error" $
            case parseBookingStatus "not-a-status" of
                Left msg -> do
                    T.isInfixOf "Tentative" msg `shouldBe` True
                    T.isInfixOf "Cancelled" msg `shouldBe` True
                Right statusVal ->
                    expectationFailure ("Expected an invalid-status error, got: " <> show statusVal)

    describe "validateServiceMarketplaceCatalog" $ do
        it "returns the active catalog kind so marketplace bookings inherit the real service kind" $
            validateServiceMarketplaceCatalog (Just (mkCatalog Mixing True)) `shouldBe` Right Mixing

        it "rejects missing catalogs with a 404" $
            expectCatalogError (validateServiceMarketplaceCatalog Nothing) $ \serverErr -> do
                errHTTPCode serverErr `shouldBe` 404
                BL8.unpack (errBody serverErr) `shouldContain` "Service catalog not found"

        it "rejects inactive catalogs before creating ads or bookings" $
            expectCatalogError (validateServiceMarketplaceCatalog (Just (mkCatalog Rehearsal False))) $ \serverErr -> do
                errHTTPCode serverErr `shouldBe` 409
                BL8.unpack (errBody serverErr) `shouldContain` "Service catalog is inactive"

    describe "validateServiceAdCurrency" $ do
        it "defaults omitted values to USD and normalizes explicit ISO codes" $ do
            validateServiceAdCurrency Nothing `shouldBe` Right "USD"
            validateServiceAdCurrency (Just " usd ") `shouldBe` Right "USD"
            validateServiceAdCurrency (Just "eur") `shouldBe` Right "EUR"

        it "rejects blank or malformed ad currencies instead of storing ambiguous pricing data" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "3-letter ISO code"
                    Right currencyVal ->
                        expectationFailure ("Expected invalid ad currency error, got: " <> show currencyVal)
            assertInvalid (validateServiceAdCurrency (Just "   "))
            assertInvalid (validateServiceAdCurrency (Just "usdollars"))
            assertInvalid (validateServiceAdCurrency (Just "12$"))

    describe "validateRolePayload" $ do
        it "normalizes known role labels before party-role assignment" $ do
            validateRolePayload " teacher " `shouldBe` Right Teacher
            validateRolePayload "studio-manager" `shouldBe` Right StudioManager
            validateRolePayload "readonly" `shouldBe` Right ReadOnly

        it "rejects unknown roles instead of silently downgrading them to ReadOnly" $
            case validateRolePayload "not-a-role" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "role must be one of:"
                    BL8.unpack (errBody serverErr) `shouldContain` "ReadOnly"
                    BL8.unpack (errBody serverErr) `shouldContain` "Teacher"
                Right roleVal ->
                    expectationFailure ("Expected invalid role payload to be rejected, got: " <> show roleVal)

    describe "parsePaymentMethodText" $ do
        it "defaults missing or blank payment methods to OtherM while normalizing supported values" $ do
            parsePaymentMethodText Nothing `shouldBe` Right OtherM
            parsePaymentMethodText (Just "   ") `shouldBe` Right OtherM
            parsePaymentMethodText (Just " PayPal ") `shouldBe` Right PayPalM
            parsePaymentMethodText (Just "bank") `shouldBe` Right BankTransferM
            parsePaymentMethodText (Just "other") `shouldBe` Right OtherM

        it "rejects unsupported explicit payment methods instead of silently storing OtherM" $
            case parsePaymentMethodText (Just "paypol") of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "paymentMethod must be one of"
                Right paymentMethodVal ->
                    expectationFailure ("Expected invalid payment method to be rejected, got: " <> show paymentMethodVal)

    describe "parseCourseRegistrationStatus" $ do
        it "normalizes supported course registration statuses and canonicalizes canceled" $ do
            parseCourseRegistrationStatus " Pending Payment " `shouldBe` Right "pending_payment"
            parseCourseRegistrationStatus "PAID" `shouldBe` Right "paid"
            parseCourseRegistrationStatus "canceled" `shouldBe` Right "cancelled"

        it "rejects unsupported course registration statuses with the allowed values" $
            case parseCourseRegistrationStatus "refunded" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "pending_payment, paid, cancelled"
                Right statusVal ->
                    expectationFailure ("Expected an invalid course-registration status error, got: " <> show statusVal)

    describe "validateCourseRegistrationPhoneE164" $ do
        it "preserves omitted and blank phones while normalizing meaningful values" $ do
            validateCourseRegistrationPhoneE164 Nothing `shouldBe` Right Nothing
            validateCourseRegistrationPhoneE164 (Just "   ") `shouldBe` Right Nothing
            validateCourseRegistrationPhoneE164 (Just " +593 99 123 4567 ") `shouldBe` Right (Just "+593991234567")

        it "rejects explicitly invalid or implausible phones instead of silently discarding them" $ do
            let assertInvalid rawPhone = case validateCourseRegistrationPhoneE164 (Just rawPhone) of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "phoneE164"
                    Right phoneVal ->
                        expectationFailure
                            ("Expected invalid course-registration phone to be rejected, got: " <> show phoneVal)
            assertInvalid "call-me-maybe"
            assertInvalid "12345"
            assertInvalid "+1234567890123456"

        it "rejects free-form text that merely contains digits instead of storing a misleading partial phone" $
            case validateCourseRegistrationPhoneE164 (Just "call me at 099 123 4567") of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "phoneE164"
                Right phoneVal ->
                    expectationFailure ("Expected mixed text phone input to be rejected, got: " <> show phoneVal)

    describe "validateWhatsAppPhoneInput" $ do
        it "normalizes meaningful WhatsApp phone inputs before they reach transport handlers" $
            validateWhatsAppPhoneInput " +593 99 123 4567 " `shouldBe` Right "+593991234567"

        it "rejects blank, mixed-text, or implausible WhatsApp phone inputs" $ do
            let assertInvalid rawPhone = case validateWhatsAppPhoneInput rawPhone of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "WhatsApp"
                    Right phoneVal ->
                        expectationFailure ("Expected invalid WhatsApp phone input to be rejected, got: " <> show phoneVal)
            assertInvalid "   "
            assertInvalid "call me at 099 123 4567"
            assertInvalid "12345"
            assertInvalid "+1234567890123456"

    describe "validateCourseRegistrationEmail" $ do
        it "preserves omitted and blank emails while normalizing meaningful values" $ do
            validateCourseRegistrationEmail Nothing `shouldBe` Right Nothing
            validateCourseRegistrationEmail (Just "   ") `shouldBe` Right Nothing
            validateCourseRegistrationEmail (Just " User@Example.com ") `shouldBe` Right (Just "user@example.com")

        it "rejects explicitly invalid email shapes instead of storing unusable addresses" $
            case validateCourseRegistrationEmail (Just "not-an-email") of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "email inválido"
                Right emailVal ->
                    expectationFailure ("Expected invalid course-registration email to be rejected, got: " <> show emailVal)

    describe "validatePublicBookingContactDetails" $ do
        it "normalizes the public-booking email and optional phone before party creation" $
            validatePublicBookingContactDetails
                " User@Example.com "
                (Just " +593 99 123 4567 ")
                `shouldBe` Right ("user@example.com", Just "+593991234567")

        it "rejects invalid explicit public-booking contact data instead of silently degrading the booking" $ do
            let assertInvalid rawEmail rawPhone expected = case validatePublicBookingContactDetails rawEmail rawPhone of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expected
                    Right contactVal ->
                        expectationFailure ("Expected invalid public-booking contact data to be rejected, got: " <> show contactVal)
            assertInvalid "   " Nothing "email requerido"
            assertInvalid "not-an-email" Nothing "email inválido"
            assertInvalid "user@example.com" (Just "call me at 099 123 4567") "phoneE164 inválido"

    describe "validateCourseRegistrationContactChannels" $ do
        it "accepts registrations with at least one contact channel" $ do
            validateCourseRegistrationContactChannels (Just "user@example.com") Nothing `shouldBe` Right ()
            validateCourseRegistrationContactChannels Nothing (Just "+593991234567") `shouldBe` Right ()
            validateCourseRegistrationContactChannels (Just "user@example.com") (Just "+593991234567") `shouldBe` Right ()

        it "rejects registrations that omit both email and phone" $
            case validateCourseRegistrationContactChannels Nothing Nothing of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "email o phoneE164 requerido"
                Right result ->
                    expectationFailure ("Expected missing course-registration contact channels to be rejected, got: " <> show result)

    describe "parseCourseFollowUpType" $ do
        it "defaults missing values to note and canonicalizes supported variants" $ do
            parseCourseFollowUpType Nothing `shouldBe` Right "note"
            parseCourseFollowUpType (Just " Status Change ") `shouldBe` Right "status_change"
            parseCourseFollowUpType (Just "payment-receipt") `shouldBe` Right "payment_receipt"

        it "rejects blank or unsupported follow-up types with the allowed values" $
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "note, call, whatsapp, email, payment_receipt, status_change, registration"
                    Right entryType ->
                        expectationFailure ("Expected an invalid follow-up type error, got: " <> show entryType)
             in do
                    assertInvalid (parseCourseFollowUpType (Just "   "))
                    assertInvalid (parseCourseFollowUpType (Just "telegram"))

    describe "course upsert numeric validation" $ do
        it "accepts non-negative required and optional values" $ do
            validateCourseNonNegativeField "priceCents" 0 `shouldBe` Right 0
            validateCourseNonNegativeField "capacity" 25 `shouldBe` Right 25
            validateOptionalCourseNonNegativeField "sessionStartHour" Nothing `shouldBe` Right Nothing
            validateOptionalCourseNonNegativeField "sessionDurationHours" (Just 3) `shouldBe` Right (Just 3)

        it "rejects negative values instead of silently clamping them to zero" $ do
            let assertInvalid result fieldName = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` (fieldName <> " must be greater than or equal to 0")
                    Right value ->
                        expectationFailure ("Expected a negative " <> fieldName <> " error, got: " <> show value)
            assertInvalid (validateCourseNonNegativeField "priceCents" (-1)) "priceCents"
            assertInvalid (validateCourseNonNegativeField "capacity" (-5)) "capacity"
            assertInvalid (validateOptionalCourseNonNegativeField "sessionStartHour" (Just (-1))) "sessionStartHour"
            assertInvalid (validateOptionalCourseNonNegativeField "sessionDurationHours" (Just (-2))) "sessionDurationHours"

    describe "hasOperationsAccess" $ do
        it "denies baseline customer sessions even though they carry package access" $
            hasOperationsAccess (mkUser [Fan, Customer]) `shouldBe` False

        it "does not treat package-only roles as operations staff" $
            forM_ [[Artist], [Artista], [Vendor], [Customer]] $ \roles ->
                hasOperationsAccess (mkUser roles) `shouldBe` False

        it "matches the intended single-role operations matrix" $
            forM_ [minBound .. maxBound] $ \role ->
                hasOperationsAccess (mkUser [role]) `shouldBe` (role `elem` [Admin, Manager, StudioManager, Webmaster, Maintenance])

    describe "hasAiToolingAccess" $ do
        it "denies baseline customer sessions" $
            hasAiToolingAccess (mkUser [Fan, Customer]) `shouldBe` False

        it "tracks the operations-access matrix for paid tooling" $
            forM_ [minBound .. maxBound] $ \role ->
                hasAiToolingAccess (mkUser [role]) `shouldBe` hasOperationsAccess (mkUser [role])

    describe "hasStrictAdminAccess" $ do
        it "requires the literal Admin role instead of broad admin-module membership" $ do
            hasStrictAdminAccess (mkUser [Fan, Customer]) `shouldBe` False
            hasStrictAdminAccess (mkUser [Webmaster]) `shouldBe` False
            hasStrictAdminAccess (mkUser [StudioManager]) `shouldBe` False
            hasStrictAdminAccess (mkUser [Admin]) `shouldBe` True

        it "matches the intended single-role strict-admin matrix" $
            forM_ [minBound .. maxBound] $ \role ->
                hasStrictAdminAccess (mkUser [role]) `shouldBe` (role == Admin)

    describe "hasSocialInboxAccess" $ do
        it "denies baseline and read-only CRM sessions" $ do
            hasSocialInboxAccess (mkUser [Fan, Customer]) `shouldBe` False
            hasSocialInboxAccess (mkUser [ReadOnly]) `shouldBe` False

        it "matches the intended single-role inbox matrix" $
            forM_ [minBound .. maxBound] $ \role ->
                hasSocialInboxAccess (mkUser [role]) `shouldBe` (role `elem` [Admin, Manager, StudioManager, Reception, LiveSessionsProducer, Producer, AandR, Webmaster])

    describe "hasSocialSyncAccess" $ do
        it "denies baseline and non-admin staff sessions" $ do
            hasSocialSyncAccess (mkUser [Fan, Customer]) `shouldBe` False
            hasSocialSyncAccess (mkUser [Webmaster]) `shouldBe` False
            hasSocialSyncAccess (mkUser [StudioManager]) `shouldBe` False

        it "matches the strict-admin matrix for global sync data" $
            forM_ [minBound .. maxBound] $ \role ->
                hasSocialSyncAccess (mkUser [role]) `shouldBe` hasStrictAdminAccess (mkUser [role])

runAuthSqlite :: SqlPersistT IO a -> IO a
runAuthSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeAuthSchema backend
        liftIO $ runReaderT action backend

initializeAuthSchema :: SqlPersistT IO ()
initializeAuthSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"legal_name\" VARCHAR NULL,\
        \\"display_name\" VARCHAR NOT NULL,\
        \\"is_org\" BOOLEAN NOT NULL,\
        \\"tax_id\" VARCHAR NULL,\
        \\"primary_email\" VARCHAR NULL,\
        \\"primary_phone\" VARCHAR NULL,\
        \\"whatsapp\" VARCHAR NULL,\
        \\"instagram\" VARCHAR NULL,\
        \\"emergency_contact\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"user_credential\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"username\" VARCHAR NOT NULL,\
        \\"password_hash\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_credential_username\" UNIQUE (\"username\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"api_token\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"token\" VARCHAR NOT NULL,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"label\" VARCHAR NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_api_token\" UNIQUE (\"token\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party_role\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"role\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_party_role\" UNIQUE (\"party_id\", \"role\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []
