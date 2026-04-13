{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerSpec (spec) where

import Control.Monad (forM_)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Aeson (eitherDecode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import Data.Time (fromGregorian)
import Data.Time.Clock (UTCTime (..), getCurrentTime, secondsToDiffTime)
import Database.Persist (Entity(..), get, insert)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, toSqlKey)
import Database.Persist.Sqlite (runSqlite)
import TDF.Auth (AuthedUser (..), hasAiToolingAccess, hasOperationsAccess, hasSocialInboxAccess, hasSocialSyncAccess, hasStrictAdminAccess, modulesForRoles)
import TDF.Routes.Courses (CourseSessionIn (..), CourseSyllabusIn (..))
import Servant (ServerError (errBody, errHTTPCode))
import TDF.Models
    ( ApiToken (..)
    , BookingStatus (..)
    , PackageProduct (..)
    , Party (..)
    , PaymentMethod (..)
    , PricingModel (..)
    , RefundPolicy (..)
    , RoleEnum (..)
    , ServiceAd (..)
    , ServiceCatalog (..)
    , ServiceKind (..)
    , UnitsKind (..)
    , UserCredential (..)
    )
import qualified TDF.DTO as DTO
import TDF.Server
    ( MetaBackfillOptions(..)
    , normalizeOptionalInput
    , parseBookingStatus
    , parseBoolParam
    , parseCourseFollowUpType
    , parseCourseRegistrationStatus
    , parseDirectionParam
    , resolveOptionalBookingPartyReference
    , resolveServiceAdEntity
    , validateMetaBackfillOptions
    , parsePaymentMethodText
    , validateBookingTimeRange
    , validateWhatsAppMessagesLimit
    , validateBookingListFilters
    , validatePublicBookingDurationMinutes
    , validateRolePayload
    , validateServiceAdCurrency
    , validateServiceAdSlotMinutes
    , validateCmsContentStatus
    , normalizeOptionalCmsFilter
    , validateCourseNonNegativeField
    , validateCourseRegistrationContactChannels
    , validateCourseRegistrationEmail
    , validateCourseRegistrationEmailEventListLimit
    , validateCourseRegistrationListLimit
    , validateOptionalCourseRegistrationStatusFilter
    , validateCourseSessionInputs
    , validateCourseSyllabusInputs
    , validateMarketplaceOrderListLimit
    , validateMarketplaceOrderListOffset
    , validateChatMessageListLookup
    , validateOptionalMarketplaceOrderStatus
    , validateMarketplaceOrderUpdateStatus
    , validateCourseRegistrationPhoneE164
    , validateCourseRegistrationReceiptDeletion
    , validateCourseRegistrationUrlField
    , validateMarketplaceBuyerEmail
    , parsePayPalCaptureOrderStatus
    , validateOptionalCourseNonNegativeField
    , validatePositiveIdField
    , validateOptionalPositiveIdField
    , validateServiceMarketplaceBookingRefs
    , validatePublicBookingContactDetails
    , validateRequiredCmsField
    , validateServiceMarketplaceCatalog
    , validateWhatsAppPhoneInput
    , validatePublicBookingStartAt
    , validateCourseRegistrationId
    , validateCourseRegistrationReceiptId
    , validateCourseRegistrationFollowUpId
    , resolvePackagePurchaseRefs
    , resolvePartyRoleAssignmentTarget
    )
import TDF.ServerAuth
    ( normalizeAuthEmailAddress
    , parsePasswordChangeAuthToken
    , resolvePasswordResetDelivery
    , runPasswordResetConfirm
    , signupEmailExists
    , validateOptionalSignupClaimArtistId
    , validateOptionalSignupPhone
    , validateRequestedSignupRoles
    , validateSignupFanArtistIds
    )
import TDF.ServerProposals
    ( resolveOptionalProposalClientPartyReference
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

decodeSignup :: BL8.ByteString -> Either String DTO.SignupRequest
decodeSignup = eitherDecode

decodeLoginRequest :: BL8.ByteString -> Either String DTO.LoginRequest
decodeLoginRequest = eitherDecode

decodeGoogleLoginRequest :: BL8.ByteString -> Either String DTO.GoogleLoginRequest
decodeGoogleLoginRequest = eitherDecode

decodeChangePasswordRequest :: BL8.ByteString -> Either String DTO.ChangePasswordRequest
decodeChangePasswordRequest = eitherDecode

decodePasswordResetRequest :: BL8.ByteString -> Either String DTO.PasswordResetRequest
decodePasswordResetRequest = eitherDecode

decodePasswordResetConfirmRequest :: BL8.ByteString -> Either String DTO.PasswordResetConfirmRequest
decodePasswordResetConfirmRequest = eitherDecode

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "TDF.Server helpers" $ do
    describe "normalizeOptionalInput" $ do
        it "returns Nothing when input is Nothing" $
            normalizeOptionalInput Nothing `shouldBe` Nothing

        it "trims whitespace and preserves meaningful content" $
            normalizeOptionalInput (Just "   Live Room  ") `shouldBe` Just "Live Room"

        it "drops strings that only contain whitespace" $
            normalizeOptionalInput (Just "   ") `shouldBe` Nothing

    describe "validatePositiveIdField" $ do
        it "accepts positive identifiers for public resource lookups" $
            validatePositiveIdField "artistId" 42 `shouldBe` Right 42

        it "rejects zero or negative identifiers instead of issuing ambiguous lookups" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "artistId must be a positive integer"
                    Right value ->
                        expectationFailure
                            ("Expected invalid positive id input to be rejected, got: " <> show value)
            assertInvalid (validatePositiveIdField "artistId" 0)
            assertInvalid (validatePositiveIdField "artistId" (-7))

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

    describe "course registration lookup ids" $ do
        it "rejects non-positive registration ids before course admin handlers can treat malformed lookups as missing rows" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` "registrationId must be a positive integer"
                    Right value ->
                        expectationFailure
                            ("Expected invalid registration lookup id to be rejected, got: " <> show value)
            assertInvalid (validateCourseRegistrationId 0)
            assertInvalid (validateCourseRegistrationId (-7))

        it "rejects non-positive receipt and follow-up ids before nested course admin lookups hit the database" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid nested course registration lookup id to be rejected, got: " <> show value)
            assertInvalid "receiptId must be a positive integer"
                (validateCourseRegistrationReceiptId 0)
            assertInvalid "followUpId must be a positive integer"
                (validateCourseRegistrationFollowUpId (-3))

    describe "resolvePartyRoleAssignmentTarget" $ do
        it "rejects non-positive party ids before attempting any role assignment" $ do
            result <- runAuthSqlite $
                resolvePartyRoleAssignmentTarget 0
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "partyId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid party id to be rejected, got: " <> show value)

        it "returns 404 for unknown parties instead of surfacing a database foreign-key failure" $ do
            result <- runAuthSqlite $
                resolvePartyRoleAssignmentTarget 999999
            case result of
                Left serverErr ->
                    errHTTPCode serverErr `shouldBe` 404
                Right value ->
                    expectationFailure
                        ("Expected unknown party role assignment to be rejected, got: " <> show value)

        it "resolves existing parties before the role upsert runs" $ do
            (expectedPartyId, result) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Role Assignment Target"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "roles@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                resolved <- resolvePartyRoleAssignmentTarget (fromSqlKey partyId)
                pure (partyId, resolved)
            case result of
                Left serverErr ->
                    expectationFailure
                        ("Expected existing party role assignment target to resolve, got: " <> show serverErr)
                Right resolvedKey ->
                    resolvedKey `shouldBe` expectedPartyId

    describe "validateServiceMarketplaceBookingRefs" $ do
        it "accepts positive ad and slot identifiers before marketplace booking lookups" $
            validateServiceMarketplaceBookingRefs 42 99 `shouldBe` Right (42, 99)

        it "rejects zero or negative booking refs instead of turning malformed marketplace requests into 404s" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid marketplace booking refs to be rejected, got: " <> show value)
            assertInvalid "adId must be a positive integer"
                (validateServiceMarketplaceBookingRefs 0 99)
            assertInvalid "slotId must be a positive integer"
                (validateServiceMarketplaceBookingRefs 42 (-3))

    describe "resolveServiceAdEntity" $ do
        it "rejects non-positive ad ids before service slot handlers can treat them as missing ads or empty slot lists" $ do
            let assertInvalid rawAdId = do
                    result <- try $ runServiceAdSqlite $
                        resolveServiceAdEntity rawAdId
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "adId must be a positive integer"
                        Right value ->
                            expectationFailure
                                ("Expected invalid service ad id to be rejected, got: " <> show value)
            assertInvalid 0
            assertInvalid (-7)

        it "returns 404 for unknown ads instead of collapsing them into an empty slot listing" $ do
            result <- try $ runServiceAdSqlite $
                resolveServiceAdEntity 999999
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 404
                    BL8.unpack (errBody serverErr) `shouldContain` "Service ad not found"
                Right value ->
                    expectationFailure
                        ("Expected unknown service ad lookup to be rejected, got: " <> show value)

        it "resolves existing service ads before downstream slot logic runs" $ do
            (expectedAdId, result) <- runServiceAdSqlite $ do
                now <- liftIO getCurrentTime
                adId <- insert ServiceAd
                    { serviceAdProviderPartyId = toSqlKey 1
                    , serviceAdServiceCatalogId = Nothing
                    , serviceAdRoleTag = "Mixing"
                    , serviceAdHeadline = "Analog mix"
                    , serviceAdDescription = Nothing
                    , serviceAdFeeCents = 12000
                    , serviceAdCurrency = "USD"
                    , serviceAdSlotMinutes = 60
                    , serviceAdActive = True
                    , serviceAdCreatedAt = now
                    }
                resolved <- resolveServiceAdEntity (fromSqlKey adId)
                pure (adId, resolved)
            case result of
                Entity resolvedKey resolvedAd -> do
                    resolvedKey `shouldBe` expectedAdId
                    serviceAdHeadline resolvedAd `shouldBe` "Analog mix"

    describe "validateChatMessageListLookup" $ do
        it "accepts a positive thread id plus at most one positive pagination cursor" $ do
            validateChatMessageListLookup 12 Nothing Nothing
                `shouldBe` Right (12, Nothing, Nothing)
            validateChatMessageListLookup 12 (Just 33) Nothing
                `shouldBe` Right (12, Just 33, Nothing)
            validateChatMessageListLookup 12 Nothing (Just 44)
                `shouldBe` Right (12, Nothing, Just 44)

        it "rejects invalid thread or cursor ids instead of turning chat lookups into empty or missing results" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure
                            ("Expected invalid chat lookup input to be rejected, got: " <> show value)
            assertInvalid "threadId must be a positive integer"
                (validateChatMessageListLookup 0 Nothing Nothing)
            assertInvalid "beforeId must be a positive integer"
                (validateChatMessageListLookup 12 (Just 0) Nothing)
            assertInvalid "afterId must be a positive integer"
                (validateChatMessageListLookup 12 Nothing (Just (-4)))

        it "rejects contradictory beforeId and afterId cursors instead of silently picking one" $
            case validateChatMessageListLookup 12 (Just 33) (Just 44) of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "Use either beforeId or afterId"
                Right value ->
                    expectationFailure
                        ("Expected contradictory chat cursors to be rejected, got: " <> show value)

    describe "resolveOptionalBookingPartyReference" $ do
        it "preserves omitted refs and resolves existing booking parties" $ do
            (expectedPartyId, omittedResult, resolvedResult) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Booked Engineer"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "engineer@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                omitted <- resolveOptionalBookingPartyReference "engineerPartyId" Nothing
                resolved <- resolveOptionalBookingPartyReference "engineerPartyId" (Just (fromSqlKey partyId))
                pure (fromSqlKey partyId, omitted, resolved)

            case omittedResult of
                Right Nothing -> pure ()
                Right other ->
                    expectationFailure ("Expected omitted booking party reference to stay empty, got: " <> show other)
                Left serverErr ->
                    expectationFailure ("Expected omitted booking party reference to succeed, got: " <> show serverErr)
            case resolvedResult of
                Right (Just (Entity resolvedKey resolvedParty)) -> do
                    fromSqlKey resolvedKey `shouldBe` expectedPartyId
                    partyDisplayName resolvedParty `shouldBe` "Booked Engineer"
                Right other ->
                    expectationFailure ("Expected booking party reference to resolve, got: " <> show other)
                Left serverErr ->
                    expectationFailure ("Expected booking party reference to resolve, got: " <> show serverErr)

        it "rejects unknown booking party ids instead of deferring to a foreign-key failure" $ do
            result <- runAuthSqlite $
                resolveOptionalBookingPartyReference "engineerPartyId" (Just 999999)
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 422
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "engineerPartyId references an unknown party"
                Right value ->
                    expectationFailure
                        ("Expected unknown booking party id to be rejected, got: " <> show value)

    describe "resolveOptionalProposalClientPartyReference" $ do
        it "preserves omitted refs and resolves existing proposal client parties" $ do
            (expectedPartyId, omittedResult, resolvedResult) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Proposal Client"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "client@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                omitted <- resolveOptionalProposalClientPartyReference Nothing
                resolved <- resolveOptionalProposalClientPartyReference (Just (fromSqlKey partyId))
                pure (fromSqlKey partyId, omitted, resolved)

            omittedResult `shouldBe` Right Nothing
            resolvedResult `shouldBe` Right (Just (toSqlKey expectedPartyId))

        it "rejects unknown proposal client party ids instead of persisting dangling CRM references" $ do
            result <- runAuthSqlite $
                resolveOptionalProposalClientPartyReference (Just 999999)
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 422
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "clientPartyId references an unknown party"
                Right value ->
                    expectationFailure
                        ("Expected unknown proposal client party id to be rejected, got: " <> show value)

    describe "resolvePackagePurchaseRefs" $ do
        it "resolves existing active package products for known buyers" $ do
            (expectedBuyerId, expectedProductId, result) <- runPackageSqlite $ do
                now <- liftIO getCurrentTime
                buyerKey <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Package Buyer"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "buyer@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                productKey <- insert PackageProduct
                    { packageProductName = "10 mixing hours"
                    , packageProductServiceKind = Mixing
                    , packageProductUnitsKind = Hours
                    , packageProductUnitsQty = 10
                    , packageProductPriceCents = 90000
                    , packageProductExpiresDays = Nothing
                    , packageProductTransferable = False
                    , packageProductRefundPolicy = CreditOnly
                    , packageProductActive = True
                    }
                resolved <- resolvePackagePurchaseRefs
                    (DTO.PackagePurchaseReq (fromSqlKey buyerKey) (fromSqlKey productKey))
                pure (buyerKey, productKey, resolved)

            case result of
                Left serverErr ->
                    expectationFailure
                        ("Expected package purchase refs to resolve, got: " <> show serverErr)
                Right (buyerKey, productKey, productRecord) -> do
                    buyerKey `shouldBe` expectedBuyerId
                    productKey `shouldBe` expectedProductId
                    packageProductPriceCents productRecord `shouldBe` 90000

        it "rejects malformed, missing, or inactive package purchase references instead of silently no-oping purchases" $ do
            now <- getCurrentTime

            invalidBuyerId <- runPackageSqlite $
                resolvePackagePurchaseRefs (DTO.PackagePurchaseReq 0 1)
            case invalidBuyerId of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "buyerId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid buyer id to be rejected, got: " <> show value)

            missingBuyer <- runPackageSqlite $ do
                productKey <- insert PackageProduct
                    { packageProductName = "Starter credits"
                    , packageProductServiceKind = Classes
                    , packageProductUnitsKind = Credits
                    , packageProductUnitsQty = 5
                    , packageProductPriceCents = 25000
                    , packageProductExpiresDays = Nothing
                    , packageProductTransferable = False
                    , packageProductRefundPolicy = CreditOnly
                    , packageProductActive = True
                    }
                resolvePackagePurchaseRefs
                    (DTO.PackagePurchaseReq 999999 (fromSqlKey productKey))
            case missingBuyer of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 422
                    BL8.unpack (errBody serverErr) `shouldContain`
                        "buyerId references an unknown party"
                Right value ->
                    expectationFailure
                        ("Expected unknown buyer to be rejected, got: " <> show value)

            missingProduct <- runPackageSqlite $ do
                buyerKey <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Missing Product Buyer"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "missing-product@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                resolvePackagePurchaseRefs
                    (DTO.PackagePurchaseReq (fromSqlKey buyerKey) 999999)
            case missingProduct of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 422
                    BL8.unpack (errBody serverErr) `shouldContain`
                        "productId references an unknown package product"
                Right value ->
                    expectationFailure
                        ("Expected unknown package product to be rejected, got: " <> show value)

            inactiveProduct <- runPackageSqlite $ do
                buyerKey <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Inactive Product Buyer"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "inactive-product@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyCreatedAt = now
                    }
                productKey <- insert PackageProduct
                    { packageProductName = "Hidden package"
                    , packageProductServiceKind = Recording
                    , packageProductUnitsKind = Sessions
                    , packageProductUnitsQty = 1
                    , packageProductPriceCents = 45000
                    , packageProductExpiresDays = Nothing
                    , packageProductTransferable = False
                    , packageProductRefundPolicy = None
                    , packageProductActive = False
                    }
                resolvePackagePurchaseRefs
                    (DTO.PackagePurchaseReq (fromSqlKey buyerKey) (fromSqlKey productKey))
            case inactiveProduct of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 409
                    BL8.unpack (errBody serverErr) `shouldContain` "Package product is inactive"
                Right value ->
                    expectationFailure
                        ("Expected inactive package product to be rejected, got: " <> show value)

    describe "validateBookingListFilters" $ do
        it "preserves omitted filters and accepts either a unique booking id or broader party filters" $ do
            validateBookingListFilters (Just 7) Nothing Nothing
                `shouldBe` Right (Just 7, Nothing, Nothing)
            validateBookingListFilters Nothing (Just 11) (Just 13)
                `shouldBe` Right (Nothing, Just 11, Just 13)

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

        it "rejects bookingId combined with broader party filters instead of silently ignoring them" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` "bookingId cannot be combined with partyId or engineerPartyId"
                    Right filtersVal ->
                        expectationFailure
                            ("Expected conflicting booking list filters to be rejected, got: " <> show filtersVal)
            assertInvalid (validateBookingListFilters (Just 7) (Just 11) Nothing)
            assertInvalid (validateBookingListFilters (Just 7) Nothing (Just 13))

    describe "validateWhatsAppMessagesLimit" $ do
        it "defaults omitted limits and preserves explicit values inside the supported page window" $ do
            validateWhatsAppMessagesLimit Nothing `shouldBe` Right 100
            validateWhatsAppMessagesLimit (Just 1) `shouldBe` Right 1
            validateWhatsAppMessagesLimit (Just 200) `shouldBe` Right 200

        it "rejects out-of-range limits instead of silently clamping WhatsApp inbox queries" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "limit must be between 1 and 200"
                    Right limitVal ->
                        expectationFailure ("Expected invalid WhatsApp messages limit to be rejected, got: " <> show limitVal)
            assertInvalid (validateWhatsAppMessagesLimit (Just 0))
            assertInvalid (validateWhatsAppMessagesLimit (Just 201))
            assertInvalid (validateWhatsAppMessagesLimit (Just (-5)))

    describe "WhatsApp inbox filter parsing" $ do
        it "preserves omitted filters and normalizes supported explicit direction and repliedOnly values" $ do
            parseDirectionParam Nothing `shouldBe` Right Nothing
            parseDirectionParam (Just " ALL ") `shouldBe` Right Nothing
            parseDirectionParam (Just " Incoming ") `shouldBe` Right (Just "incoming")
            parseBoolParam Nothing `shouldBe` Right False
            parseBoolParam (Just " YES ") `shouldBe` Right True
            parseBoolParam (Just "0") `shouldBe` Right False

        it "rejects blank or unknown filters instead of silently broadening WhatsApp inbox queries" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure ("Expected invalid WhatsApp inbox filter to be rejected, got: " <> show value)
            assertInvalid
                "direction must be omitted or one of: all, incoming, outgoing"
                (parseDirectionParam (Just "   "))
            assertInvalid
                "direction must be omitted or one of: all, incoming, outgoing"
                (parseDirectionParam (Just "sideways"))
            assertInvalid
                "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no"
                (parseBoolParam (Just "   "))
            assertInvalid
                "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no"
                (parseBoolParam (Just "maybe"))

    describe "validateMetaBackfillOptions" $ do
        it "keeps defaults only when omitted and normalizes supported explicit values" $ do
            validateMetaBackfillOptions (object [])
                `shouldBe` Right
                    MetaBackfillOptions
                        { mboPlatform = "all"
                        , mboConversationLimit = 50
                        , mboMessagesPerConversation = 50
                        , mboOnlyUnread = True
                        , mboDryRun = False
                        }
            validateMetaBackfillOptions
                (object
                    [ "platform" .= (" Instagram " :: T.Text)
                    , "conversationLimit" .= (12 :: Int)
                    , "messagesPerConversation" .= (7 :: Int)
                    , "onlyUnread" .= False
                    , "dryRun" .= True
                    ])
                `shouldBe` Right
                    MetaBackfillOptions
                        { mboPlatform = "instagram"
                        , mboConversationLimit = 12
                        , mboMessagesPerConversation = 7
                        , mboOnlyUnread = False
                        , mboDryRun = True
                        }

        it "rejects invalid explicit platform or limit values instead of broadening the backfill request" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right optionsVal ->
                        expectationFailure ("Expected invalid meta backfill options to be rejected, got: " <> show optionsVal)
            assertInvalid
                "all, instagram, facebook"
                (validateMetaBackfillOptions (object ["platform" .= ("threads" :: T.Text)]))
            assertInvalid
                "conversationLimit must be between 1 and 200"
                (validateMetaBackfillOptions (object ["conversationLimit" .= (0 :: Int)]))
            assertInvalid
                "messagesPerConversation must be between 1 and 200"
                (validateMetaBackfillOptions (object ["messagesPerConversation" .= (201 :: Int)]))

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

    describe "normalizeOptionalCmsFilter" $ do
        it "trims meaningful CMS filter values and drops blank ones" $ do
            normalizeOptionalCmsFilter Nothing `shouldBe` Nothing
            normalizeOptionalCmsFilter (Just "  homepage ") `shouldBe` Just "homepage"
            normalizeOptionalCmsFilter (Just "   ") `shouldBe` Nothing

    describe "validateRequiredCmsField" $ do
        it "trims required CMS identifiers before create and lookup handlers use them" $ do
            validateRequiredCmsField "slug" "  homepage hero " `shouldBe` Right "homepage hero"
            validateRequiredCmsField "locale" " es-EC " `shouldBe` Right "es-EC"

        it "rejects blank CMS identifiers instead of persisting or querying ambiguous keys" $ do
            let assertInvalid fieldName rawValue =
                    case validateRequiredCmsField fieldName rawValue of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` T.unpack fieldName
                            BL8.unpack (errBody serverErr) `shouldContain` "requerido"
                        Right value ->
                            expectationFailure ("Expected blank CMS field to be rejected, got: " <> show value)
            assertInvalid "slug" "   "
            assertInvalid "locale" "\n\t"

    describe "normalizeAuthEmailAddress" $ do
        it "trims and lowercases valid auth emails before signup or reset flows use them" $ do
            normalizeAuthEmailAddress " User@Example.com " `shouldBe` Just "user@example.com"
            normalizeAuthEmailAddress " User.Name+Artist@Example.com "
                `shouldBe` Just "user.name+artist@example.com"

        it "rejects blank or malformed auth emails instead of sending ambiguous auth responses" $ do
            normalizeAuthEmailAddress "   " `shouldBe` Nothing
            normalizeAuthEmailAddress "not-an-email" `shouldBe` Nothing
            normalizeAuthEmailAddress "user@ example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress ".user@example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user.@example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user..name@example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user()@example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user@example..com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user@-example.com" `shouldBe` Nothing
            normalizeAuthEmailAddress "user@example-.com" `shouldBe` Nothing

    describe "validateRequestedSignupRoles" $ do
        it "preserves allowed self-signup roles while still enforcing baseline customer/fan access" $ do
            validateRequestedSignupRoles (Just [Student, Fan, Customer, Vendor, Student])
                `shouldBe` Right [Customer, Fan, Student, Vendor]
            validateRequestedSignupRoles Nothing
                `shouldBe` Right [Customer, Fan]

        it "rejects forbidden self-signup roles instead of silently dropping them" $
            case validateRequestedSignupRoles (Just [Student, Admin, Manager]) of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Requested signup roles are not allowed for self-signup: Admin, Manager"
                Right rolesVal ->
                    expectationFailure
                        ("Expected forbidden signup roles to be rejected, got: " <> show rolesVal)

    describe "validateOptionalSignupPhone" $ do
        it "treats omitted or blank signup phones as absent and canonicalizes valid numbers" $ do
            validateOptionalSignupPhone Nothing `shouldBe` Right Nothing
            validateOptionalSignupPhone (Just "   ") `shouldBe` Right Nothing
            validateOptionalSignupPhone (Just " +593 99 123 4567 ")
                `shouldBe` Right (Just "+593991234567")

        it "rejects malformed or implausible signup phones instead of storing free-form contact text" $ do
            let assertInvalid rawPhone = case validateOptionalSignupPhone (Just rawPhone) of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "phone must be a valid phone number"
                    Right value ->
                        expectationFailure
                            ("Expected invalid signup phone to be rejected, got: " <> show value)
            assertInvalid "---"
            assertInvalid "call me at 099 123 4567"
            assertInvalid "12345"
            assertInvalid "+1234567890123456"

    describe "SignupRequest FromJSON" $ do
        it "accepts canonical public signup fields" $
            case decodeSignup
                "{\"firstName\":\"Ada\",\"lastName\":\"Lovelace\",\"email\":\"ada@example.com\",\"phone\":\"+593991234567\",\"password\":\"supersecret\",\"roles\":[\"Student\"],\"fanArtistIds\":[7,11],\"claimArtistId\":42}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical signup payload to decode, got: " <> decodeErr)
                Right (DTO.SignupRequest firstNameValue lastNameValue emailValue phoneValue _ _ _ _ _ _ _ _ rolesValue fanArtistIdsValue claimArtistIdValue) -> do
                    firstNameValue `shouldBe` "Ada"
                    lastNameValue `shouldBe` "Lovelace"
                    emailValue `shouldBe` "ada@example.com"
                    phoneValue `shouldBe` Just "+593991234567"
                    rolesValue `shouldBe` Just [Student]
                    fanArtistIdsValue `shouldBe` Just [7, 11]
                    claimArtistIdValue `shouldBe` Just 42

        it "rejects unexpected signup keys instead of silently ignoring typoed client payloads" $ do
            decodeSignup
                "{\"firstName\":\"Ada\",\"lastName\":\"Lovelace\",\"email\":\"ada@example.com\",\"password\":\"supersecret\",\"claimArtistID\":42}"
                `shouldSatisfy` isLeft
            decodeSignup
                "{\"firstName\":\"Ada\",\"lastName\":\"Lovelace\",\"email\":\"ada@example.com\",\"password\":\"supersecret\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "auth request FromJSON" $ do
        it "accepts canonical login and password-reset payloads" $ do
            case decodeLoginRequest "{\"username\":\"ada@example.com\",\"password\":\"supersecret\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical login payload to decode, got: " <> decodeErr)
                Right (DTO.LoginRequest usernameValue passwordValue) -> do
                    usernameValue `shouldBe` "ada@example.com"
                    passwordValue `shouldBe` "supersecret"

            case decodeGoogleLoginRequest "{\"idToken\":\"google-id-token\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical Google login payload to decode, got: " <> decodeErr)
                Right (DTO.GoogleLoginRequest idTokenValue) ->
                    idTokenValue `shouldBe` "google-id-token"

            case decodeChangePasswordRequest
                "{\"username\":\"ada@example.com\",\"currentPassword\":\"old-secret\",\"newPassword\":\"new-secret\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical change-password payload to decode, got: " <> decodeErr)
                Right (DTO.ChangePasswordRequest usernameValue currentPasswordValue newPasswordValue) -> do
                    usernameValue `shouldBe` Just "ada@example.com"
                    currentPasswordValue `shouldBe` "old-secret"
                    newPasswordValue `shouldBe` "new-secret"

            case decodePasswordResetRequest "{\"email\":\"ada@example.com\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical password-reset payload to decode, got: " <> decodeErr)
                Right (DTO.PasswordResetRequest emailValue) ->
                    emailValue `shouldBe` "ada@example.com"

            case decodePasswordResetConfirmRequest
                "{\"token\":\"reset-token\",\"newPassword\":\"supersecret\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical password-reset confirm payload to decode, got: " <> decodeErr)
                Right (DTO.PasswordResetConfirmRequest tokenValue newPasswordValue) -> do
                    tokenValue `shouldBe` "reset-token"
                    newPasswordValue `shouldBe` "supersecret"

        it "rejects unexpected auth keys instead of silently accepting typoed or over-posted payloads" $ do
            decodeLoginRequest
                "{\"username\":\"ada@example.com\",\"password\":\"supersecret\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeGoogleLoginRequest
                "{\"idToken\":\"google-id-token\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodeChangePasswordRequest
                "{\"username\":\"ada@example.com\",\"currentPassword\":\"old-secret\",\"newPassword\":\"new-secret\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodePasswordResetRequest
                "{\"email\":\"ada@example.com\",\"unexpected\":true}"
                `shouldSatisfy` isLeft
            decodePasswordResetConfirmRequest
                "{\"token\":\"reset-token\",\"newPassword\":\"supersecret\",\"unexpected\":true}"
                `shouldSatisfy` isLeft

    describe "validateOptionalSignupClaimArtistId" $ do
        it "preserves omission and accepts positive artist ids for explicit profile claims" $ do
            validateOptionalSignupClaimArtistId Nothing `shouldBe` Right Nothing
            validateOptionalSignupClaimArtistId (Just 42) `shouldBe` Right (Just 42)

        it "rejects zero or negative artist ids instead of silently dropping the requested claim" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "claimArtistId must be a positive integer"
                    Right value ->
                        expectationFailure
                            ("Expected invalid claimArtistId to be rejected, got: " <> show value)
            assertInvalid (validateOptionalSignupClaimArtistId (Just 0))
            assertInvalid (validateOptionalSignupClaimArtistId (Just (-7)))

    describe "validateSignupFanArtistIds" $ do
        it "preserves omission and deduplicates positive artist ids before signup follows are created" $ do
            validateSignupFanArtistIds Nothing `shouldBe` Right []
            validateSignupFanArtistIds (Just [7, 11, 7, 13]) `shouldBe` Right [7, 11, 13]

        it "rejects zero or negative artist ids instead of silently dropping requested follows" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "fanArtistIds must contain only positive integers"
                    Right value ->
                        expectationFailure
                            ("Expected invalid fanArtistIds to be rejected, got: " <> show value)
            assertInvalid (validateSignupFanArtistIds (Just [7, 0, 13]))
            assertInvalid (validateSignupFanArtistIds (Just [-5]))

    describe "parsePasswordChangeAuthToken" $ do
        it "accepts standard bearer headers and preserves the raw-token fallback" $ do
            parsePasswordChangeAuthToken " Bearer session-token " `shouldBe` Right "session-token"
            parsePasswordChangeAuthToken "raw-session-token" `shouldBe` Right "raw-session-token"

        it "rejects malformed authorization headers instead of misreporting them as invalid tokens" $ do
            let assertInvalid rawHeader = case parsePasswordChangeAuthToken rawHeader of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "Authorization header must be Bearer <token>"
                    Right tokenVal ->
                        expectationFailure ("Expected malformed authorization header to be rejected, got: " <> show tokenVal)
            assertInvalid "Bearer"
            assertInvalid "Basic session-token"
            assertInvalid "Bearer too many parts"

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

    describe "validateServiceAdSlotMinutes" $ do
        it "defaults omitted values and preserves explicit slot lengths at or above the minimum" $ do
            validateServiceAdSlotMinutes Nothing `shouldBe` Right 60
            validateServiceAdSlotMinutes (Just 15) `shouldBe` Right 15
            validateServiceAdSlotMinutes (Just 45) `shouldBe` Right 45

        it "rejects explicit slot lengths below the marketplace minimum instead of silently rewriting them" $ do
            let assertInvalid rawMinutes =
                    case validateServiceAdSlotMinutes (Just rawMinutes) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "slotMinutes must be at least 15"
                        Right slotMinutesVal ->
                            expectationFailure ("Expected invalid slotMinutes error, got: " <> show slotMinutesVal)
            assertInvalid (-5)
            assertInvalid 0
            assertInvalid 14

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

    describe "validateOptionalCourseRegistrationStatusFilter" $ do
        it "keeps omitted filters absent and canonicalizes supported explicit values" $ do
            validateOptionalCourseRegistrationStatusFilter Nothing `shouldBe` Right Nothing
            validateOptionalCourseRegistrationStatusFilter (Just " Pending Payment ")
                `shouldBe` Right (Just "pending_payment")
            validateOptionalCourseRegistrationStatusFilter (Just "canceled")
                `shouldBe` Right (Just "cancelled")

        it "rejects blank or unknown filters instead of silently widening course-registration listings" $ do
            let assertInvalid expectedMessage result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                    Right statusVal ->
                        expectationFailure
                            ("Expected invalid optional course-registration status filter, got: " <> show statusVal)
            assertInvalid
                "status must be omitted or one of: pending_payment, paid, cancelled"
                (validateOptionalCourseRegistrationStatusFilter (Just "   "))
            assertInvalid
                "pending_payment, paid, cancelled"
                (validateOptionalCourseRegistrationStatusFilter (Just "refunded"))

    describe "validateCourseRegistrationListLimit" $ do
        it "defaults omitted limits and preserves explicit values inside the supported page window" $ do
            validateCourseRegistrationListLimit 200 Nothing `shouldBe` Right 200
            validateCourseRegistrationListLimit 200 (Just 1) `shouldBe` Right 1
            validateCourseRegistrationListLimit 200 (Just 500) `shouldBe` Right 500

        it "rejects out-of-range limits instead of silently clamping course-registration listings" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "limit must be between 1 and 500"
                    Right limitVal ->
                        expectationFailure ("Expected invalid course-registration list limit to be rejected, got: " <> show limitVal)
            assertInvalid (validateCourseRegistrationListLimit 200 (Just 0))
            assertInvalid (validateCourseRegistrationListLimit 200 (Just 501))
            assertInvalid (validateCourseRegistrationListLimit 200 (Just (-3)))

    describe "validateCourseRegistrationEmailEventListLimit" $ do
        it "defaults omitted limits and preserves explicit values inside the supported page window" $ do
            validateCourseRegistrationEmailEventListLimit Nothing `shouldBe` Right 100
            validateCourseRegistrationEmailEventListLimit (Just 1) `shouldBe` Right 1
            validateCourseRegistrationEmailEventListLimit (Just 500) `shouldBe` Right 500

        it "rejects out-of-range limits instead of silently clamping course-registration email history" $ do
            let assertInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "limit must be between 1 and 500"
                    Right limitVal ->
                        expectationFailure ("Expected invalid course-registration email-event limit to be rejected, got: " <> show limitVal)
            assertInvalid (validateCourseRegistrationEmailEventListLimit (Just 0))
            assertInvalid (validateCourseRegistrationEmailEventListLimit (Just 501))
            assertInvalid (validateCourseRegistrationEmailEventListLimit (Just (-3)))

    describe "marketplace order list pagination validation" $ do
        it "keeps marketplace order defaults only when the caller omits pagination" $ do
            validateMarketplaceOrderListLimit Nothing `shouldBe` Right 50
            validateMarketplaceOrderListLimit (Just 1) `shouldBe` Right 1
            validateMarketplaceOrderListLimit (Just 200) `shouldBe` Right 200
            validateMarketplaceOrderListOffset Nothing `shouldBe` Right 0
            validateMarketplaceOrderListOffset (Just 0) `shouldBe` Right 0
            validateMarketplaceOrderListOffset (Just 25) `shouldBe` Right 25

        it "rejects explicit out-of-range pagination instead of silently clamping admin order queries" $ do
            let assertLimitInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "limit must be between 1 and 200"
                    Right limitVal ->
                        expectationFailure ("Expected invalid marketplace order limit to be rejected, got: " <> show limitVal)
                assertOffsetInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr) `shouldContain` "offset must be greater than or equal to 0"
                    Right offsetVal ->
                        expectationFailure ("Expected invalid marketplace order offset to be rejected, got: " <> show offsetVal)
            assertLimitInvalid (validateMarketplaceOrderListLimit (Just 0))
            assertLimitInvalid (validateMarketplaceOrderListLimit (Just 201))
            assertOffsetInvalid (validateMarketplaceOrderListOffset (Just (-1)))

    describe "validateOptionalMarketplaceOrderStatus" $ do
        it "keeps omitted filters absent and canonicalizes supported statuses" $ do
            validateOptionalMarketplaceOrderStatus Nothing `shouldBe` Right Nothing
            validateOptionalMarketplaceOrderStatus (Just " PayPal Pending ")
                `shouldBe` Right (Just "paypal_pending")
            validateOptionalMarketplaceOrderStatus (Just "canceled")
                `shouldBe` Right (Just "cancelled")
            validateOptionalMarketplaceOrderStatus (Just "datafast failed")
                `shouldBe` Right (Just "datafast_failed")

        it "rejects blank or unknown marketplace statuses instead of silently broadening the list query" $ do
            let assertInvalid rawStatus =
                    case validateOptionalMarketplaceOrderStatus (Just rawStatus) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "status must be omitted or one of"
                        Right statusVal ->
                            expectationFailure
                                ( "Expected invalid marketplace order status to be rejected, got: "
                                    <> show statusVal
                                )
            assertInvalid "   "
            assertInvalid "refunded"

    describe "validateMarketplaceOrderUpdateStatus" $ do
        it "keeps omitted update statuses untouched and canonicalizes supported explicit values" $ do
            validateMarketplaceOrderUpdateStatus Nothing `shouldBe` Right Nothing
            validateMarketplaceOrderUpdateStatus (Just " PayPal Pending ")
                `shouldBe` Right (Just "paypal_pending")
            validateMarketplaceOrderUpdateStatus (Just "canceled")
                `shouldBe` Right (Just "cancelled")

        it "rejects blank or unknown explicit statuses instead of turning admin updates into silent no-ops" $ do
            let assertInvalid rawStatus expectedMessage =
                    case validateMarketplaceOrderUpdateStatus (Just rawStatus) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right statusVal ->
                            expectationFailure
                                ( "Expected invalid marketplace order update status to be rejected, got: "
                                    <> show statusVal
                                )
            assertInvalid "   " "status cannot be blank"
            assertInvalid "refunded" "pending, contact, paid, cancelled"

    describe "parsePayPalCaptureOrderStatus" $ do
        it "maps known PayPal capture statuses onto canonical marketplace order states" $ do
            parsePayPalCaptureOrderStatus "COMPLETED" `shouldBe` Right "paid"
            parsePayPalCaptureOrderStatus " approved " `shouldBe` Right "paypal_pending"
            parsePayPalCaptureOrderStatus "VOIDED" `shouldBe` Right "cancelled"
            parsePayPalCaptureOrderStatus "DENIED" `shouldBe` Right "paypal_failed"

        it "rejects unsupported PayPal capture statuses instead of persisting raw gateway labels" $
            case parsePayPalCaptureOrderStatus "mystery_status" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 502
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Unsupported PayPal capture status: mystery_status"
                Right statusVal ->
                    expectationFailure
                        ( "Expected unsupported PayPal capture status to be rejected, got: "
                            <> show statusVal
                        )

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
            validateCourseRegistrationEmail (Just " User.Name+Promo@Example.com ")
                `shouldBe` Right (Just "user.name+promo@example.com")

        it "rejects explicitly invalid email shapes instead of storing unusable addresses" $ do
            let assertInvalid rawEmail =
                    case validateCourseRegistrationEmail (Just rawEmail) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "email inválido"
                        Right emailVal ->
                            expectationFailure ("Expected invalid course-registration email to be rejected, got: " <> show emailVal)
            assertInvalid "not-an-email"
            assertInvalid "user@example..com"
            assertInvalid "user@-example.com"
            assertInvalid "user@example-.com"
            assertInvalid ".user@example.com"
            assertInvalid "user.@example.com"
            assertInvalid "user..name@example.com"
            assertInvalid "user()@example.com"
            assertInvalid "usér@example.com"

    describe "validateMarketplaceBuyerEmail" $ do
        it "trims and lowercases valid buyer emails before checkout creates marketplace orders" $
            validateMarketplaceBuyerEmail " Buyer@Example.com " `shouldBe` Right "buyer@example.com"

        it "rejects blank or malformed buyer emails instead of creating unusable checkout attempts" $ do
            let assertInvalid rawEmail expectedMessage =
                    case validateMarketplaceBuyerEmail rawEmail of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right emailVal ->
                            expectationFailure ("Expected invalid marketplace buyer email to be rejected, got: " <> show emailVal)
            assertInvalid "   " "buyerEmail requerido"
            assertInvalid "not-an-email" "buyerEmail inválido"
            assertInvalid "buyer@example..com" "buyerEmail inválido"
            assertInvalid "buyer@-example.com" "buyerEmail inválido"
            assertInvalid "buyer@example-.com" "buyerEmail inválido"
            assertInvalid "buyer..name@example.com" "buyerEmail inválido"
            assertInvalid "buyer()@example.com" "buyerEmail inválido"

    describe "validateCourseRegistrationUrlField" $ do
        it "trims valid absolute http(s) URLs and still lets optional attachment fields clear to Nothing" $ do
            validateCourseRegistrationUrlField "attachmentUrl" Nothing `shouldBe` Right Nothing
            validateCourseRegistrationUrlField "attachmentUrl" (Just "   ") `shouldBe` Right Nothing
            validateCourseRegistrationUrlField "fileUrl" (Just " https://files.example.com/proof.pdf ")
                `shouldBe` Right (Just "https://files.example.com/proof.pdf")

        it "rejects malformed or non-http course registration asset URLs instead of storing opaque strings" $ do
            let assertInvalid fieldName rawUrl = case validateCourseRegistrationUrlField fieldName (Just rawUrl) of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` (T.unpack fieldName <> " must be an absolute http(s) URL")
                    Right urlVal ->
                        expectationFailure ("Expected invalid course registration URL to be rejected, got: " <> show urlVal)
            assertInvalid "fileUrl" "receipt.pdf"
            assertInvalid "fileUrl" "ftp://files.example.com/proof.pdf"
            assertInvalid "attachmentUrl" "https://files.example.com/proof copy.pdf"
            assertInvalid "attachmentUrl" "https://files..example.com/proof.pdf"
            assertInvalid "fileUrl" "https://files_example.com/proof.pdf"

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
            assertInvalid "user()@example.com" Nothing "email inválido"
            assertInvalid "user@example.com" (Just "call me at 099 123 4567") "phoneE164 inválido"

    describe "validatePublicBookingDurationMinutes" $ do
        it "defaults omitted durations to one hour and preserves explicit durations >= 30 minutes" $ do
            validatePublicBookingDurationMinutes Nothing `shouldBe` Right 60
            validatePublicBookingDurationMinutes (Just 30) `shouldBe` Right 30
            validatePublicBookingDurationMinutes (Just 90) `shouldBe` Right 90

        it "rejects explicit durations below the booking minimum instead of silently rewriting them to 30 minutes" $ do
            let assertInvalid rawDuration =
                    case validatePublicBookingDurationMinutes (Just rawDuration) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "durationMinutes must be at least 30"
                        Right durationVal ->
                            expectationFailure ("Expected invalid booking duration to be rejected, got: " <> show durationVal)
            assertInvalid (-15)
            assertInvalid 0
            assertInvalid 29

    describe "validatePublicBookingStartAt" $ do
        it "accepts public bookings whose requested start time is still in the future" $ do
            let now = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 36000)
                startsAt = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 39600)
            validatePublicBookingStartAt now startsAt `shouldBe` Right startsAt

        it "rejects past or current public booking starts instead of creating already-expired bookings" $ do
            let now = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 36000)
                pastStart = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 32400)
                assertInvalid startsAtCandidate =
                    case validatePublicBookingStartAt now startsAtCandidate of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "startsAt must be in the future"
                        Right startsAtValue ->
                            expectationFailure ("Expected invalid public booking start time to be rejected, got: " <> show startsAtValue)
            assertInvalid pastStart
            assertInvalid now

    describe "validateBookingTimeRange" $ do
        it "accepts booking ranges whose end is strictly after the start" $ do
            let startsAt = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 36000)
                endsAt = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 39600)
            validateBookingTimeRange startsAt endsAt `shouldBe` Right ()

        it "rejects zero-length or reversed booking ranges before persistence" $ do
            let startsAt = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 36000)
                sameEndsAt = startsAt
                reversedEndsAt = UTCTime (fromGregorian 2026 4 10) (secondsToDiffTime 32400)
                assertInvalid endsAtCandidate =
                    case validateBookingTimeRange startsAt endsAtCandidate of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` "endsAt must be after startsAt"
                        Right result ->
                            expectationFailure ("Expected invalid booking time range to be rejected, got: " <> show result)
            assertInvalid sameEndsAt
            assertInvalid reversedEndsAt

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

    describe "validateCourseRegistrationReceiptDeletion" $ do
        it "allows receipt deletion for non-paid registrations or when paid registrations still have other receipts" $ do
            validateCourseRegistrationReceiptDeletion "pending_payment" 1 `shouldBe` Right ()
            validateCourseRegistrationReceiptDeletion "paid" 2 `shouldBe` Right ()

        it "rejects deleting the last receipt from a paid registration instead of leaving paid rows without proof" $
            case validateCourseRegistrationReceiptDeletion "paid" 1 of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 409
                    BL8.unpack (errBody serverErr) `shouldContain` "inscripcion pagada"
                Right result ->
                    expectationFailure ("Expected paid receipt deletion to be rejected, got: " <> show result)

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

    describe "course upsert nested text validation" $ do
        it "trims meaningful session labels, syllabus titles, and syllabus topics before persistence" $ do
            let sessionDay = fromGregorian 2026 4 20
            case validateCourseSessionInputs [CourseSessionIn "  Kickoff session  " sessionDay (Just 2)] of
                Right [CourseSessionIn labelVal dateVal orderVal] -> do
                    labelVal `shouldBe` "Kickoff session"
                    dateVal `shouldBe` sessionDay
                    orderVal `shouldBe` Just 2
                Right value ->
                    expectationFailure ("Expected a single normalized course session, got: " <> show value)
                Left serverErr ->
                    expectationFailure ("Expected course session validation to succeed, got: " <> show serverErr)
            case validateCourseSyllabusInputs [CourseSyllabusIn "  Intro module  " ["  Ableton  ", " ", " Mixing"] (Just 3)] of
                Right [CourseSyllabusIn titleVal topicsVal orderVal] -> do
                    titleVal `shouldBe` "Intro module"
                    topicsVal `shouldBe` ["Ableton", "Mixing"]
                    orderVal `shouldBe` Just 3
                Right value ->
                    expectationFailure ("Expected a single normalized syllabus item, got: " <> show value)
                Left serverErr ->
                    expectationFailure ("Expected syllabus validation to succeed, got: " <> show serverErr)

        it "rejects blank session labels or syllabus titles instead of storing empty course rows" $ do
            let sessionDay = fromGregorian 2026 4 20
                assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid nested course text to be rejected, got: " <> show value)
            assertInvalid
                "sessions[1].label is required"
                (validateCourseSessionInputs [CourseSessionIn "   " sessionDay Nothing])
            assertInvalid
                "syllabus[1].title is required"
                (validateCourseSyllabusInputs [CourseSyllabusIn "   " ["Ableton"] Nothing])

        it "rejects negative nested ordering values instead of persisting ambiguous sort positions" $ do
            let sessionDay = fromGregorian 2026 4 20
                assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid nested course ordering to be rejected, got: " <> show value)
            assertInvalid
                "sessions[1].order must be greater than or equal to 0"
                (validateCourseSessionInputs [CourseSessionIn "Kickoff session" sessionDay (Just (-1))])
            assertInvalid
                "syllabus[1].order must be greater than or equal to 0"
                (validateCourseSyllabusInputs [CourseSyllabusIn "Intro module" ["Ableton"] (Just (-2))])

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

runServiceAdSqlite :: SqlPersistT IO a -> IO a
runServiceAdSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeServiceAdSchema backend
        liftIO $ runReaderT action backend

initializeServiceAdSchema :: SqlPersistT IO ()
initializeServiceAdSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_ad\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"provider_party_id\" INTEGER NOT NULL,\
        \\"service_catalog_id\" INTEGER NULL,\
        \\"role_tag\" VARCHAR NOT NULL,\
        \\"headline\" VARCHAR NOT NULL,\
        \\"description\" VARCHAR NULL,\
        \\"fee_cents\" INTEGER NOT NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"slot_minutes\" INTEGER NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []

runAuthSqlite :: SqlPersistT IO a -> IO a
runAuthSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeAuthSchema backend
        liftIO $ runReaderT action backend

runPackageSqlite :: SqlPersistT IO a -> IO a
runPackageSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeAuthSchema backend
        liftIO $ runReaderT initializePackageSchema backend
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
        "CREATE TABLE IF NOT EXISTS \"party_role\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"role\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_party_role\" UNIQUE (\"party_id\", \"role\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
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

initializePackageSchema :: SqlPersistT IO ()
initializePackageSchema = do
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"package_product\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"service_kind\" VARCHAR NOT NULL,\
        \\"units_kind\" VARCHAR NOT NULL,\
        \\"units_qty\" INTEGER NOT NULL,\
        \\"price_cents\" INTEGER NOT NULL,\
        \\"expires_days\" INTEGER NULL,\
        \\"transferable\" BOOLEAN NOT NULL,\
        \\"refund_policy\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL\
        \)"
        []
