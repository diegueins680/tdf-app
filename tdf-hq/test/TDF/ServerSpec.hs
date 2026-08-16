Warning: truncated output (original token count: 195897)
Total output lines: 15079

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.ServerSpec (spec) where

import Control.Monad (forM_)
import Control.Exception (bracket, toException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Aeson (eitherDecode, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime, secondsToDiffTime)
import Database.Persist (Entity(..), Key, PersistValue(PersistText), count, get, insert, insert_, insertKey, toPersistValue, (==.))
import Database.Persist.Sql
    ( SqlPersistT
    , fromSqlKey
    , rawExecute
    , runMigration
    , runSqlPool
    , toSqlKey
    )
import Database.Persist.Sqlite (createSqlitePool, runSqlite)
import TDF.API
    ( AdsInquiry (..)
    , CreateBookingReq (..)
    , CmsContentDTO (..)
    , PublicBookingReq (..)
    , UpdateBookingReq (..)
    , WhatsAppConsentStatus (..)
    )
import TDF.API.Future (StubResponse (..))
import qualified TDF.API.Future as Future
import TDF.API.Drive (DriveUploadForm (..))
import qualified TDF.API.Facebook as FB
import qualified TDF.API.Instagram as IG
import TDF.API.Types
    ( DriveTokenExchangeRequest (..)
    , DriveTokenRefreshRequest (..)
    , LabelTrackCreate (..)
    , LabelTrackUpdate (..)
    , MarketplaceOrderDTO (..)
    , MarketplaceOrderItemDTO (..)
    , maxMarketplaceCartItemQuantity
    )
import TDF.Auth
    ( AuthedUser (..)
    , extractTokenFromHeaders
    , hasAiToolingAccess
    , hasOperationsAccess
    , hasSocialInboxAccess
    , hasSocialSyncAccess
    , hasStrictAdminAccess
    , loadAuthedUser
    , lookupUsernameFromToken
    , ModuleAccess (..)
    , moduleName
    , modulesForRoles
    )
import TDF.Routes.Courses (CourseSessionIn (..), CourseSyllabusIn (..), UTMTags (..))
import qualified TDF.Routes.Academy as Academy
import Servant (ServerError (errBody, errHTTPCode), err500, (:<|>) (..))
import Servant.Multipart
    ( FileData (..)
    , FromMultipart (fromMultipart)
    , Input (..)
    , MultipartData (..)
    , Tmp
    )
import Servant.Server.Internal.Handler (runHandler)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import TDF.Config
    ( AppConfig (..)
    , llmProvider
    , llmProviderApiBase
    , llmProviderDefaultChatModel
    )
import qualified TDF.Courses.Production as ProductionCourse
import qualified TDF.Calendar.Models as Cal
import qualified TDF.CMS.Models as CMS
import qualified TDF.Catalog.Models as Catalog
import TDF.DB (Env (..))
import TDF.DTO.SocialEventsDTO (ArtistDTO (..))
import TDF.Handlers.InputList
    ( AssetField (..)
    , renderInputListLatex
    , renderInputListLatexWithAssets
    )
import TDF.Models
    ( ApiToken (..)
    , ArtistProfile (..)
    , Booking (..)
    , BookingResource (..)
    , BookingStatus (..)
    , ChatMessage (..)
    , ChatThread (..)
    , PackageProduct (..)
    , Party (..)
    , PaymentMethod (..)
    , PricingModel (..)
    , Resource (..)
    , ResourceType (..)
    , RefundPolicy (..)
    , RoleEnum (..)
    , ServiceAd (..)
    , ServiceAdSlot (..)
    , ServiceCatalog (..)
    , ServiceEscrow (..)
    , ServiceKind (..)
    , UnitsKind (..)
    , UserCredential (..)
    , roleToText
    )
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import TDF.DTO
    ( AdCreativeUpsert (..)
    , AdsAssistRequest (..)
    , CampaignUpsert (..)
    , CreateInvoiceLineReq (..)
    )
import qualified TDF.DTO as DTO
import TDF.Server
    ( MarketplaceCartTotalsState(..)
    , DriveApiResp(..)
    , DriveMetaResp(..)
    , GoogleToken(..)
    , PayPalLink(..)
    , PayPalToken(..)
    , MetaBackfillOptions(..)
    , PreparedLine(..)
    , SessionInputLookup(..)
    , WAInbound(..)
    , extractWhatsAppInbound
    , normalizeOptionalInput
    , normalizeRequestedResourceIds
    , parseMcpRequest
    , parseToolCallParams
    , validateMcpToolArguments
    , parseBookingStatus
    , parseBoolParam
    , parseCourseFollowUpType
    , parseCourseRegistrationStatus
    , parseDirectionParam
    , resolveBookingEngineerName
    , resolveOptionalBookingEngineerReference
    , resolveOptionalBookingPartyReference
    , resolveInstagramBackfillTarget
    , resolveServiceAdEntity
    , resolveServiceAdSlotEntity
    , resolveServiceMarketplaceBookingEntity
    , validateMetaBackfillOptions
    , validateMetaBackfillConversationId
    , validateMetaBackfillConversationIdField
    , validateMetaBackfillMessageCreatedAt
    , parsePaymentMethodText
    , validateBookingTimeRange
    , validateEngineer
    , validateWhatsAppMessagesLimit
    , validateBookingListFilters
    , validateUpdateBookingRequestHasChanges
    , validatePartyDisplayName
    , validatePartyDisplayNameUpdate
    , validatePartyListPagination
    , validatePartyPrimaryEmail
    , validatePartyPrimaryEmailUpdate
    , validatePublicBookingDurationMinutes
    , validateStrictAdminAccess
    , validateServiceAdCatalogId
    , validateServiceAdCurrency
    , validateReceiptCurrency
    , validateReceiptBuyerName
    , validateReceiptBuyerEmail
    , validateServiceAdSlotMinutes
    , validateServiceAdSlotWindow
    , validateCmsContentStatus
    , normalizeOptionalCmsFilter
    , validateCmsLocaleFilter
    , validateOptionalCmsLocaleFilter
    , validateCourseCurrency
    , validateCourseNonNegativeField
    , validateRequiredCourseTextField
    , validateCoursePositiveField
    , validateCourseSlug
    , loadCourseMetadata
    , validateCourseRegistrationContactChannels
    , validateCourseRegistrationEmail
    , validateCourseRegistrationEmailEventListLimit
    , validateCourseRegistrationListLimit
    , validateCourseRegistrationSeatAvailability
    , validateCourseRegistrationSource
    , validateCourseRegistrationReceiptMimeType
    , validateOptionalCourseRegistrationTextField
    , validateCourseRegistrationUtm
    , validateOptionalCourseRegistrationStatusFilter
    , validateOptionalCourseSessionStartHour
    , validateOptionalCourseSessionDurationHours
    , validateCourseSessionScheduleWindow
    , validateOptionalCourseSlugFilter
    , validateCourseTextListField
    , validateCourseSessionInputs
    , validateCourseSyllabusInputs
    , validateMarketplaceOrderListLimit
    , validateMarketplaceOrderListOffset
    , validateChatMessageListLookup
    , validateChatSendMessageBody
    , validateOptionalMarketplaceOrderStatus
    , validateMarketplaceOrderUpdateStatus
    , validateMarketplaceOrderPaidAtUpdate
    , resolveMarketplaceOrderPaidAtForStatus
    , validateOptionalMarketplacePaymentProviderUpdate
    , validateMarketplaceStripeAdminUpdate
    , validateCourseRegistrationPhoneE164
    , validateCourseRegistrationStoredName
    , resolveCourseRegistrationAttachmentName
    , validateCourseRegistrationReceiptDeletion
    , validateCourseRegistrationUrlField
    , validateCoursePublicUrlField
    , validateMarketplaceBuyerName
    , validateMarketplaceBuyerEmail
    , validateMarketplaceBuyerPhone
    , validateMarketplacePathId
    , validateMarketplacePublicListingActive
    , redactMarketplaceOrderForPublicLookup
    , requireMarketplaceOrderLookupResult
    , requireLoadedMarketplaceWriteResult
    , requireLoadedMarketplacePublicOrderResponse
    , requireMarketplaceCartTotals
    , resolveMarketplaceCartCurrency
    , validateMarketplaceCartLineQuantity
    , validateDatafastEntityId
    , validateDatafastResourcePath
    , validateDatafastOrderResourcePath
    , validateDatafastResultCodeField
    , validateDatafastSuccessfulPaymentAmountAndCurrency
    , validateOptionalDatafastCredential
    , validateOptionalDatafastVersionDf
    , resolvePaypalBaseUrl
    , validatePayPalCredential
    , validatePayPalAccessTokenField
    , validatePayPalTokenResponse
    , resolvePayPalApprovalUrl
    , resolvePayPalApprovalUrlForBase
    , validatePayPalApprovalUrlOrderToken
    , extractPayPalCaptureStatus
    , extractPayPalPayerEmail
    , parsePayPalCaptureOrderStatus
    , validatePayPalCaptureOrderId
    , validatePayPalCaptureOrderReference
    , prepareLine
    , validateMarketplaceOnlinePaymentTotal
    , createLabelTrack
    , validateLabelTrackTitle
    , validateOptionalLabelTrackNote
    , validateLabelTrackOwnerIdFilter
    , validateLabelTrackPathId
    , validateLabelTrackUpdateHasChanges
    , validateOptionalLabelTrackStatus
    , validateOptionalCourseNonNegativeField
    , validatePositiveIdField
    , validateOptionalPositiveIdField
    , validateSessionPathId
    , validateSessionInputLookup
    , validateInputListInventoryFilters
    , listInventory
    , resolveSocialTargetPartyId
    , validateSocialProfilePartyIds
    , resolveFanProfileDisplayName
    , validateFanProfileUpdate
    , validateServiceMarketplaceBookingRefs
    , validateServiceMarketplaceBookingTitle
    , validateServiceMarketplaceBookingNotes
    , validateServiceMarketplaceBookingSlot
    , validateServiceMarketplaceCompletion
    , requireServiceEscrowForBooking
    , requirePersistedBookingDTO
    , selectUniquePartyByPrimaryEmail
    , selectUniquePartyByPrimaryPhone
    , ensurePartyForInquiry
    , ensurePartyForCourseRegistrationDb
    , findExistingRegistration
    , courseRegistrationFollowUpCounts
    , validatePublicBookingContactDetails
    , validatePublicBookingFullName
    , validateBookingNotes
    , validatePublicBookingNotes
    , validateRequiredBookingTitle
    , validateOptionalBookingTitleUpdate
    , validateRequiredCmsField
    , validateRequiredCmsLocale
    , validateRequiredCmsSlug
    , validateOptionalCmsTitle
    , validateOptionalCmsPayload
    , validateCmsContentPathId
    , validateOptionalCmsSlugFilter
    , validateOptionalCmsSlugPrefix
    , validateServiceMarketplaceCatalog
    , validateWhatsAppPhoneInput
    , validateWhatsAppReplyBody
    , validateWhatsAppReplyExternalId
    , validateWhatsAppReplyTarget
    , validateOperatorQuestionChannel
    , validateOperatorQuestionRequiredIdentifier
    , validateOperatorQuestionIdentifier
    , validateOperatorQuestionTextField
    , normalizeOperatorWhatsAppPhone
    , buildOperatorQuestionMessage
    , validateWhatsAppConsentDisplayName
    , validateWhatsAppConsentSource
    , validateWhatsAppOptOutReason
    , whatsappWebhookServer
    , validatePublicBookingStartAt
    , validateCourseRegistrationId
    , validateCourseRegistrationReceiptId
    , validateCourseRegistrationFollowUpId
    , whatsAppConsentStatusFromRow
    , validateDriveAccess
    , resolveResourcesForBooking
    , runDb
    , resolvePackagePurchaseRefs
    , resolveInvoiceCustomerId
    , createInvoice
    , getInvoiceById
    , getInvoicesBySession
    , createReceipt
    , updateBooking
    , createParty
    , getParty
    , updateParty
    , getReceipt
    , resolvePartyRelatedTarget
    , resolveFanFollowArtistTarget
    , fanListFollows
    , fanUnfollowArtist
    , artistGetOwnProfile
    , chatListMessages
    , adsGetCampaign
    , adsUpsertCampaign
    , adsUpsertAd
    , adsListAdsForCampaign
    , adsListExamples
    , validateAdsInquiry
    , validateAdsAssistRequest
    , resolveAdsAssistExampleScope
    , shouldUseAdsAssistNoAiFallback
    , resolveAdsAssistFinalReply
    , validateAdCreativeLandingUrl
    , validateAdCreativeExternalId
    , validateAdsAdminName
    , validateCampaignBudgetCents
    , validateCampaignDateRange
    , validateCampaignStatus
    , validateAdCreativeStatus
    , validateCalendarAuthorizationCode
    , resolveCalendarClientCreds
    , validateCalendarEventListQuery
    , validateCalendarSyncWindow
    , validateCalendarRedirectUri
    , validateConfiguredCalendarRedirectUri
    , validateGoogleCalendarSyncCursor
    , validateGoogleCalendarEventId
    , validateGoogleCalendarEventStatus
    , selectUniqueCalendarConfigFallback
    , googleCalendarEventsEndpoint
    , validateConfiguredDriveAccessToken
    , resolveDriveClientCreds
    , validateDriveTokenExchangeRequest
    , validateDriveTokenRefreshRequest
    , extractApiErrorMessage
    , extractModelReplyText
    , extractChatKitSession
    , validateChatKitSessionPayload
    , resolveDriveUploadFolderId
    , resolveDriveUploadName
    , resolveDriveUploadMimeType
    , validateDriveUploadFileSize
    , formatDriveUploadFailure
    , formatDriveUploadException
    , formatGoogleOAuthFailure
    , decodeDriveMetaResourceKeyIfSuccessful
    , resolveDrivePublicUrl
    , resolveDrivePublicUrlAfterPermission
    , resolveWorkflowId
    , openAIChatRequestErrorMessage
    , shouldRetryWithFallbackModel
    , listMarketplace
    , resolveMarketplacePhotoUrl
    , calendarServer
    , cmsAdminServer
    )
import qualified TDF.ServerRadio as Radio
import qualified TDF.Server.SocialSync as SocialSync
import qualified TDF.WhatsApp.Types as WA
import TDF.ServerAuth
    ( findReusableActiveToken
    , normalizeAuthEmailAddress
    , parsePasswordChangeAuthToken
    , resolvePasswordResetDelivery
    , runPasswordResetConfirm
    , sessionServer
    , signupEmailExists
    , validateAuthPassword
    , validateSignupDisplayName
    , validateOptionalSignupClaimArtistId
    , validateOptionalSignupPhone
    , validateSignupFanArtistIds
    , validateSignupFanArtistTargets
    )
import TDF.Services.FacebookMessaging (formatFacebookGraphHttpError, sendFacebookText)
import TDF.Services.InstagramMessaging (sendInstagramTextWithContext)
import TDF.ServerProposals
    ( resolveOptionalProposalClientPartyReference
    , resolveOptionalProposalPipelineCardReference
    , resolveOptionalProposalPipelineCardReferenceUpdate
    )
import TDF.ServerFuture
    ( allowedFutureAdminConsoleCardIds
    , allowedFutureStubReservedSiblingRoutes
    , allowedFutureStubReservedTopLevelEndpointRoutes
    , allowedFutureStubMetadata
    , allowedFutureStubAreas
    , canonicalFutureStubMetadata
    , deriveFutureStubAreas
    , futureAdminConsoleStatus
    , futureStubId
    , futureStubMethod
    , futureStubResponseFor
    , futureStubRequiredModule
    , futureStubRequiredRoles
    , futureStubStatus
    , futureServer
    , futureAdminConsoleView
    , invalidCardText
    , mountedFutureStubAreas
    , reservedFutureStubRoutes
    , validateFutureAdminAccess
    , validateFutureAdminAccessWithBaselineRoles
    , validateFutureAdminBaselineRoles
    , validateAllowedFutureStubReservedSiblingRoutes
    , validateAllowedFutureStubReservedTopLevelEndpointRoutes
    , validateFutureAdminConsoleCard
    , validateFutureAdminConsoleCardIds
    , validateFutureAdminConsoleCardWithIds
    , validateFutureAdminConsoleMethod
    , validateFutureAdminConsolePublishedId
    , validateFutureAdminConsolePublishedPath
    , validateFutureAdminConsoleRequiredModule
    , validateFutureAdminConsoleRouteIn
    , validateFutureAdminConsoleView
    , validateFutureAdminConsoleViewWithCatalog
    , validateReservedFutureStubRoutes
    , validateReservedFutureStubTopLevelAreas
    , validateFutureStubArea
    , validateFutureStubAreaRegistry
    , validateFutureStubCatalog
    , validateFutureStubCatalogAreaOrder
    , validateFutureStubCatalogEndpointLeaves
    , validateFutureStubCatalogEndpointLeavesWithCardIds
    , validateFutureStubCatalogEntry
    , validateFutureStubCatalogResponseWithConsole
    , validateFutureStubCatalogResponses
    , validateFutureStubCatalogRouteBoundaries
    , validateFutureStubCatalogTopLevelBoundaries
    , validateFutureStubEndpoint
    , validateFutureStubMetadata
    , validateFutureStubMetadataIn
    , validateFutureStubPublishedId
    , validateFutureStubPublishedPath
    , validateFutureStubRequiredModule
    , validateFutureStubAuthMetadata
    , validateFutureStubMethod
    , validateFutureStubStatus
    , validateFutureStubResponse
    , validateAllowedFutureStubMetadata
    , validateFutureAdminConsoleStatus
    , validateFutureMethodMetadataWith
    , validateFutureStatusMetadataWith
    , futureStubResponseForWithConsole
    )
import TDF.ServerFanClub
    ( validateFanClubPostMutationTarget
    , validateFanClubPostPathId
    )
import TDF.Server.SocialEventsHandlers (validateEventArtistIds)
import TDF.ServerExtra
    ( validateFacebookReplyTarget
    , validateInstagramReplyTarget
    , validateSocialReplyBody
    )
import TDF.Services.InstagramSync
    ( InstagramMedia(..)
    , InstagramMediaList(..)
    , buildUserMediaRequestUrl
    )
import Test.Hspec
import Web.PathPieces (PathPiece, fromPathPiece, toPathPiece)

mkUser :: [RoleEnum] -> AuthedUser
mkUser roles =
    AuthedUser
        { auPartyId = toSqlKey 1
        , auRoles = roles
        , auModules = modulesForRoles roles
        }

futureAdminUser :: AuthedUser
futureAdminUser =
    mkUser [Admin, Fan, Customer]

firstFutureStub :: AuthedUser -> Either ServerError StubResponse
firstFutureStub user =
    let _catalog :<|> accessStubs :<|> _ = futureServer user
        loginOptions :<|> _ = accessStubs
    in loginOptions

futureCatalog :: AuthedUser -> Either ServerError [StubResponse]
futureCatalog user =
    let catalog :<|> _ = futureServer user
    in catalog

firstFutureAdminConsole :: AuthedUser -> Either ServerError Future.AdminConsoleView
firstFutureAdminConsole user =
    let _catalog
            :<|> _access
            :<|> _crm
            :<|> _scheduling
            :<|> _packages
            :<|> _invoicing
            :<|> _inventory
            :<|> adminStubs
            :<|> _experience = futureServer user
        _seedPolicy :<|> adminConsole = adminStubs
    in adminConsole

allFutureStubs :: AuthedUser -> [Either ServerError StubResponse]
allFutureStubs user =
    let _catalog
            :<|> accessStubs
            :<|> crmStubs
            :<|> schedulingStubs
            :<|> packagesStubs
            :<|> invoicingStubs
            :<|> inventoryStubs
            :<|> adminStubs
            :<|> experienceStubs = futureServer user
        accessLoginOptions
            :<|> accessModuleBehaviour
            :<|> accessSessionPolicy = accessStubs
        crmPartiesListColumns
            :<|> crmPartiesFilters
            :<|> crmPartiesDetailTabs = crmStubs
        schedulingBookingsViews
            :<|> schedulingSessionsCreation
            :<|> schedulingRoomsFeatures = schedulingStubs
        packagesCatalog
            :<|> packagesPurchaseFlow = packagesStubs
        invoicingComposer
            :<|> invoicingStatusFlow = invoicingStubs
        inventoryAssetsMetadata
            :<|> inventoryAssetsWorkflow
            :<|> inventoryStock = inventoryStubs
        adminSeedPolicy :<|> _adminConsole = adminStubs
        experienceNavigation
            :<|> experienceFeedback
            :<|> experienceOffline
            :<|> experienceDesign
            :<|> experienceAuditing = experienceStubs
    in [ accessLoginOptions
       , accessModuleBehaviour
       , accessSessionPolicy
       , crmPartiesListColumns
       , crmPartiesFilters
       , crmPartiesDetailTabs
       , schedulingBookingsViews
       , schedulingSessionsCreation
       , schedulingRoomsFeatures
       , packagesCatalog
       , packagesPurchaseFlow
       , invoicingComposer
       , invoicingStatusFlow
       , inventoryAssetsMetadata
       , inventoryAssetsWorkflow
       , inventoryStock
       , adminSeedPolicy
       , experienceNavigation
       , experienceFeedback
       , experienceOffline
       , experienceDesign
       , experienceAuditing
       ]

inputListSessionKey :: ME.SessionId
inputListSessionKey =
    case fromPathPiece ("00000000-0000-0000-0000-000000000084" :: Text) of
        Just keyVal -> keyVal
        Nothing -> error "Expected fixture input-list session id to parse"

mkDriveMultipart :: [(Text, Text)] -> [FileData Tmp] -> MultipartData Tmp
mkDriveMultipart fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkDriveUploadFile :: Text -> FileData Tmp
mkDriveUploadFile fileName =
    FileData
        { fdInputName = "file"
        , fdFileName = fileName
        , fdFileCType = "application/pdf"
        , fdPayload = "/tmp/mock-drive-upload"
        }

withEnvOverrides :: [(String, Maybe String)] -> IO a -> IO a
withEnvOverrides overrides action =
    bracket setup restore (const action)
  where
    setup = do
        previous <- mapM capture overrides
        apply overrides
        pure previous
    restore previous = apply previous
    capture (key, _) = do
        value <- lookupEnv key
        pure (key, value)
    apply = mapM_ assign
    assign (key, value) =
        case value of
            Just raw -> setEnv key raw
            Nothing -> unsetEnv key

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

decodeChatSendMessageRequest :: BL8.ByteString -> Either String DTO.ChatSendMessageRequest
decodeChatSendMessageRequest = eitherDecode

decodeVCardExchangeRequest :: BL8.ByteString -> Either String DTO.VCardExchangeRequest
decodeVCardExchangeRequest = eitherDecode

decodePublicBookingRequest :: BL8.ByteString -> Either String PublicBookingReq
decodePublicBookingRequest = eitherDecode

decodeAcademyEnrollReq :: BL8.ByteString -> Either String Academy.EnrollReq
decodeAcademyEnrollReq = eitherDecode

decodeCreateBookingRequest :: BL8.ByteString -> Either String CreateBookingReq
decodeCreateBookingRequest = eitherDecode

decodeUpdateBookingRequest :: BL8.ByteString -> Either String UpdateBookingReq
decodeUpdateBookingRequest = eitherDecode

decodePackagePurchaseReq :: BL8.ByteString -> Either String DTO.PackagePurchaseReq
decodePackagePurchaseReq = eitherDecode

decodeCreateReceiptReq :: BL8.ByteString -> Either String DTO.CreateReceiptReq
decodeCreateReceiptReq = eitherDecode

decodePartyCreate :: BL8.ByteString -> Either String DTO.PartyCreate
decodePartyCreate = eitherDecode

decodePartyUpdate :: BL8.ByteString -> Either String DTO.PartyUpdate
decodePartyUpdate = eitherDecode

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = describe "TDF.Server helpers" $ do
    describe "Academy enrollment request contract" $ do
        it "normalizes allowed academy roles before persistence" $ do
            Academy.validateAcademyRole " Artist " `shouldBe` Right "artist"
            Academy.validateAcademyRole "manager" `shouldBe` Right "manager"

            case decodeAcademyEnrollReq
                "{\"email\":\" Learner@Example.com \",\"role\":\" MANAGER \",\"platform\":\" web \",\"referralCode\":\" abc123 \"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical academy enrollment payload to decode, got: " <> decodeErr)
                Right (Academy.EnrollReq emailValue roleValue platformValue referralCodeValue) -> do
                    emailValue `shouldBe` "learner@example.com"
                    roleValue `shouldBe` "manager"
                    platformValue `shouldBe` Just "web"
                    referralCodeValue `shouldBe` Just "ABC123"

        it "rejects unsupported academy roles before the database role check can fail ambiguously" $ do
            case Academy.validateAcademyRole "student" of
                Left msg ->
                    msg `shouldBe` "role must be one of: artist, manager"
                Right roleValue ->
                    expectationFailure ("Expected unsupported academy role to be rejected, got: " <> show roleValue)

            decodeAcademyEnrollReq
                "{\"email\":\"learner@example.com\",\"role\":\"student\"}"
                `shouldSatisfy` isLeft

    describe "radio metadata validation" $ do
        it "rejects hidden formatting markers in upstream now-playing titles" $
            case Radio.resolveRadioNowPlayingFetchResult
                (Right (Just ("Artist" <> "\x202E" <> " - Track")))
             of
                Left err -> do
                    errHTTPCode err `shouldBe` 502
                    BL8.unpack (errBody err) `shouldContain` "hidden formatting"
                Right value ->
                    expectationFailure
                        ("Expected unsafe now-playing metadata to be rejected, got: " <> show value)

        it "rejects hidden formatting markers in stored radio metadata and filters" $ do
            case Radio.validateRadioOptionalMetadataField
                "rsuName"
                160
                (Just ("Station" <> "\x200B"))
             of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "hidden formatting"
                Right value ->
                    expectationFailure
                        ("Expected unsafe station metadata to be rejected, got: " <> show value)

            case Radio.validateRadioSearchFilter "genre" 120 (Just ("jazz" <> "\x2028")) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "hidden formatting"
                Right value ->
                    expectationFailure
                        ("Expected unsafe radio search filter to be rejected, got: " <> show value)

        it "rejects blank explicit import sources instead of silently dropping them" $ do
            case Radio.validateRadioImportSources
                (Just ["https://stations.example.com/streams.csv", "   "])
             of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "sources must not include blank entries"
                Right value ->
                    expectationFailure
                        ("Expected blank radio import source to be rejected, got: " <> show value)

            case Radio.validateRadioImportSources
                (Just [" https://stations.example.com/streams.csv "])
             of
                Left err ->
                    expectationFailure
                        ("Expected radio import source to normalize, got: " <> show err)
                Right sources ->
                    sources `shouldBe` ["https://stations.example.com/streams.csv"]

        it "de-duplicates duplicate explicit import sources after canonicalization" $
            case Radio.validateRadioImportSources
                (Just
                    [ "https://github.com/mikepierce/internet-radio-streams"
                    , "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
                    ])
             of
                Left err ->
                    expectationFailure
                        ("Expected duplicate radio import sources to be de-duplicated, got: " <> show err)
                Right value ->
                    value `shouldBe` ["https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"]

        it "requires HTTPS for public radio transmission listen bases" $
            case Radio.validateRadioTransmissionPublicBase "http://radio.example.com/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "RADIO_PUBLIC_BASE must be https"
                Right value ->
                    expectationFailure
                        ("Expected insecure radio public base to be rejected, got: " <> show value)

        it "rejects encoded dot segments in radio stream paths before persisting stream URLs" $
            case Radio.validateRadioStreamUrl "https://radio.example.com/streams/%2e%2e/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL8.unpack (errBody err) `shouldContain` "path must not contain empty, dot, or dot-dot segments"
                Right value ->
                    expectationFailure
                        ("Expected encoded dot segment stream URL to be rejected, got: " <> show value)

    describe "Party request FromJSON" $ do
        it "accepts canonical CRM party create and update bodies" $ do
            case decodePartyCreate
                "{\"cDisplayName\":\"Ada Lovelace\",\"cIsOrg\":false,\"cLegalName\":\"Ada Byron\",\"cPrimaryEmail\":\"ada@example.com\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical party create payload to decode, got: " <> decodeErr)
                Right (DTO.PartyCreate legalNameValue displayNameValue isOrgValue _ primaryEmailValue _ _ _ _ _) -> do
                    legalNameValue `shouldBe` Just "Ada Byron"
                    displayNameValue `shouldBe` "Ada Lovelace"
                    isOrgValue `shouldBe` False
                    primaryEmailValue `shouldBe` Just "ada@example.com"

            case decodePartyUpdate
                "{\"uDisplayName\":\"Ada Updated\",\"uPrimaryEmail\":\"ada.updated@example.com\",\"uNotes\":\"VIP\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected canonical party update payload to decode, got: " <> decodeErr)
                Right (DTO.PartyUpdate _ displayNameValue _ _ primaryEmailValue _ _ _ _ notesValue) -> do
                    displayNameValue `shouldBe` Just "Ada Updated"
                    primaryEmailValue `shouldBe` Just "ada.updated@example.com"
                    notesValue `shouldBe` Just "VIP"

        it "rejects typoed or response-shaped CRM party keys instead of silently dropping them" $ do
            decodePartyCreate
                "{\"cDisplayName\":\"Ada Lovelace\",\"cIsOrg\":false,\"displayName\":\"ignored\"}"
                `shouldSatisfy` isLeft
            decodePartyUpdate
                "{\"uDisplayName\":\"Ada Updated\",\"primaryEmail\":\"ignored@example.com\"}"
                `shouldSatisfy` isLeft

        it "rejects empty CRM party updates instead of returning a silent no-op success" $
            case decodePartyUpdate "{}" of
                Left decodeErr ->
                    decodeErr `shouldContain` "PartyUpdate must include at least one field"
                Right payload ->
                    expectationFailure
                        ("Expected empty party update to fail, got: " <> show payload)

        it "accepts nullable optional fields in mixed CRM party updates" $
            case decodePartyUpdate
                "{\"uDisplayName\":\"Blue Records\",\"uPrimaryEmail\":null,\"uPrimaryPhone\":null,\"uInstagram\":\"blue_records333\"}" of
                Left decodeErr ->
                    expectationFailure ("Expected nullable update payload to decode, got: " <> decodeErr)
                Right (DTO.PartyUpdate _ displayNameValue _ _ primaryEmailValue primaryPhoneValue _ instagramValue _ _) -> do
                    displayNameValue `shouldBe` Just "Blue Records"
                    primaryEmailValue `shouldBe` Nothing
                    primaryPhoneValue `shouldBe` Nothing
                    instagramValue `shouldBe` Just "blue_records333"

        it "normalizes valid CRM display names before persistence" $ do
            validatePartyDisplayName "  Ada Lovelace  "
                `shouldBe` Right "Ada Lovelace"
            validatePartyDisplayNameUpdate Nothing `shouldBe` Right Nothing
            validatePartyDisplayNameUpdate (Just "  Ada Updated  ")
                `shouldBe` Right (Just "Ada Updated")

        it "normalizes CRM party emails and treats blank updates as explicit clears" $ do
            validatePartyPrimaryEmail Nothing `shouldBe` Right Nothing
            validatePartyPrimaryEmail (Just "  Ada@Example.COM  ")
                `shouldBe` Right (Just "ada@example.com")
            validatePartyPrimaryEmail (Just "   ") `shouldBe` Right Nothing
            validatePartyPrimaryEmailUpdate Nothing `shouldBe` Right Nothing
            validatePartyPrimaryEmailUpdate (Just "   ")
                `shouldBe` Right (Just Nothing)

        it "rejects malformed CRM party emails before party storage" $ do
            let assertInvalid result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "primaryEmail inválido"
                        Right value ->
                            expectationFailure
                                ("Expected invalid party primaryEmail to be rejected, got: " <> show value)
            assertInvalid (validatePartyPrimaryEmail (Just "not-an-email"))
            assertInvalid (validatePartyPrimaryEmail (Just "ada@example..com"))
            assertInvalid (validatePartyPrimaryEmailUpdate (Just "ada @example.com"))

        it "rejects blank or unsafe CRM display names before party creation reaches storage" $ do
            let assertInvalid rawDisplayName expectedMessage = do
                    result <-
                        runHandler $
                            runReaderT
                                ( createParty
                                    (mkUser [Admin])
                                    ( DTO.PartyCreate
                                        Nothing
                                        rawDisplayName
                                        False
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                        Nothing
                                    )
                                )
                                (error "createParty should reject invalid displayName before reading Env")
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid party displayName to be rejected, got: " <> show value)
            assertInvalid "   " "displayName must not be blank"
            assertInvalid "Ada\nLovelace" "displayName must not contain control characters"
            assertInvalid
                ("Ada" <> T.singleton '\x202E' <> "Lovelace")
                "displayName must not contain control characters"

        it "rejects non-positive CRM party path ids before database lookup" $ do
            let emptyUpdate =
                    DTO.PartyUpdate
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                assertInvalidPath action = do
                    result <-
                        runHandler $
                            runReaderT
                                action
                                (error "party path id should reject before reading Env")
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "partyId must be a positive integer"
                        Right value ->
                            expectationFailure
                                ("Expected invalid party path id to be rejected, got: " <> show value)
            assertInvalidPath (getParty (mkUser [Admin]) 0)
            assertInvalidPath (updateParty (mkUser [Admin]) (-10) emptyUpdate)

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

    describe "validateEventArtistIds" $ do
        it "requires explicit artist ids instead of dropping nested artist-shaped objects" $ do
            let eventArtistRef mArtistId =
                    ArtistDTO
                        { artistId = mArtistId
                        , artistPartyId = Nothing
                        , artistName = "Ada Lovelace"
                        , artistGenres = []
                        , artistGenreIds = []
                        , artistBio = Nothing
                        , artistAvatarUrl = Nothing
                        , artistSocialLinks = Nothing
                        , artistCreatedAt = Nothing
                        , artistUpdatedAt = Nothing
                        }
                assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid event artist references to be rejected, got: "
                                    <> show value
                                )

            validateEventArtistIds [] `shouldBe` Right []
            validateEventArtistIds [eventArtistRef (Just "42")]
                `shouldBe` Right [toSqlKey 42]

            assertInvalid
                "eventArtists[].artistId is required"
                (validateEventArtistIds [eventArtistRef Nothing])
            assertInvalid
                "eventArtists[].artistId must be a positive integer"
                (validateEventArtistIds [eventArtistRef (Just "0")])
            assertInvalid
                "eventArtists[].artistId must be unique"
                ( validateEventArtistIds
                    [ eventArtistRef (Just "42")
                    , eventArtistRef (Just "42")
                    ]
                )
            assertInvalid
                "eventArtists supports at most 50 artists"
                ( validateEventArtistIds
                    [ eventArtistRef (Just (T.pack (show (n :: Int))))
                    | n <- [1 .. 51]
                    ]
                )

    describe "validatePartyListPagination" $ do
        it "keeps CRM party list defaults only when pagination is omitted" $ do
            validatePartyListPagination Nothing Nothing `shouldBe` Right (200, 0)
            validatePartyListPagination (Just 1) (Just 0) `shouldBe` Right (1, 0)
            validatePartyListPagination (Just 500) (Just 10000)
                `shouldBe` Right (500, 10000)

        it "rejects explicit out-of-range pagination instead of silently clamping CRM party queries" $ do
            let assertLimitInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` "limit must be between 1 and 500"
                    Right value ->
                        expectationFailure
                            ("Expected invalid party list limit to be rejected, got: " <> show value)
                assertOffsetInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` "offset must be greater than or equal to 0"
                    Right value ->
                        expectationFailure
                            ("Expected invalid party list offset to be rejected, got: " <> show value)
                assertDeepOffsetInvalid result = case result of
                    Left serverErr -> do
                        errHTTPCode serverErr `shouldBe` 400
                        BL8.unpack (errBody serverErr)
                            `shouldContain` "offset must be 10000 or fewer"
                    Right value ->
                        expectationFailure
                            ("Expected deep party list offset to be rejected, got: " <> show value)
            assertLimitInvalid (validatePartyListPagination (Just 0) Nothing)
            assertLimitInvalid (validatePartyListPagination (Just 501) Nothing)
            assertOffsetInvalid (validatePartyListPagination Nothing (Just (-1)))
            assertDeepOffsetInvalid (validatePartyListPagination Nothing (Just 10001))

    describe "validateSessionInputLookup" $ do
        it "accepts exactly one public input-list session selector" $ do
            let validSessionId = "00000000-0000-0000-0000-000000000084"
            validateSessionInputLookup Nothing Nothing `shouldBe` Right (SessionInputByIndex 1)
            validateSessionInputLookup (Just 2) Nothing `shouldBe` Right (SessionInputByIndex 2)
            case validateSessionInputLookup Nothing (Just validSessionId) of
                Right (SessionInputByKey keyVal) ->
                    toPathPiece keyVal `shouldBe` validSessionId
                Right other ->
                    expectationFailure ("Expected sessionId lookup, got: " <> show other)
                Left serverErr ->
                    expectationFailure ("Expected valid sessionId lookup, got: " <> show serverErr)

        it "rejects ambiguous or malformed public input-list session selectors" $ do
            let validSessionId = "00000000-0000-0000-0000-000000000084"
            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid input-list selector to be rejected, got: " <> show value)
            assertInvalid
                "Provide either index or sessionId, not both"
                (validateSessionInputLookup (Just 1) (Just validSessionId))
            assertInvalid
                "index must be greater than or equal to 1"
                (validateSessionInputLookup (Just 0) Nothing)
            assertInvalid
                "Invalid sessionId"
                (validateSessionInputLookup Nothing (Just "not-a-session-id"))
            assertInvalid
                "Invalid sessionId"
                (validateSessionInputLookup Nothing (Just "AAAAAAAA-0000-0000-0000-000000000084"))

    describe "listInventory" $
        it "rejects non-canonical public session ids before inventory fallback lookup" $ do
            result <-
                runHandler $
                    runReaderT
                        ( listInventory
                            (Just "mic")
                            (Just "AAAAAAAA-0000-0000-0000-000000000084")
                            Nothing
                        )
                        (error "listInventory should reject invalid sessionId before reading Env")
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr) `shouldContain` "Invalid sessionId"
                Right value ->
                    expectationFailure
                        ("Expected non-canonical inventory sessionId to be rejected, got: " <> show value)

    describe "validateSessionPathId" $ do
        it "accepts canonical session UUID path identifiers" $ do
            let validSessionId = "00000000-0000-0000-0000-000000000084"
            case validateSessionPathId validSessionId of
                Right keyVal ->
                    toPathPiece keyVal `shouldBe` validSessionId
                Left serverErr ->
                    expectationFailure
                        ("Expected valid session path id, got: " <> show serverErr)

        it "rejects malformed or non-canonical session invoice ids before lookup fallback" $ do
            let assertInvalid rawId =
                    case validateSessionPathId rawId of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid session identifier"
                        Right keyVal ->
                            expectationFailure
                                ( "Expected invalid session path id, got: "
                                    <> T.unpack (toPathPiece keyVal)
                                )
            assertInvalid "not-a-session-id"
            assertInvalid " 00000000-0000-0000-0000-000000000084"
            assertInvalid "AAAAAAAA-0000-0000-0000-000000000084"
            assertInvalid "00000000000000000000000000000084"

    describe "validateInputListInventoryFilters" $ do
        it "accepts broad inventory browsing and scoped field availability lookups" $ do
            validateInputListInventoryFilters Nothing Nothing Nothing `shouldBe` Right ()
            validateInputListInventoryFilters (Just AssetFieldMic) Nothing Nothing
                `shouldBe` Right ()
            validateInputListInventoryFilters
                (Just AssetFieldMic)
                (Just inputListSessionKey)
                (Just 3)
                `shouldBe` Right ()

        it "rejects ignored availability context before inventory queries run" $ do
            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid inventory filters to be rejected, got: "
                                    <> show value
                                )
            assertInvalid
                "channel must be greater than or equal to 1"
                ( validateInputListInventoryFilters
                    (Just AssetFieldMic)
                    (Just inputListSessionKey)
                    (Just 0)
                )
            assertInvalid
                "channel requires field"
                (validateInputListInventoryFilters Nothing Nothing (Just 1))
            assertInvalid
                "sessionId requires field"
                (validateInputListInventoryFilters Nothing (Just inputListSessionKey) Nothing)
            assertInvalid
                "channel requires sessionId"
                (validateInputListInventoryFilters (Just AssetFieldMic) Nothing (Just 1))

    describe "renderInputListLatex" $ do
        it "keeps generated headings single-line by neutralizing control and formatting characters" $ do
            let latex =
                    renderInputListLatex
                        ("Session\n\\input{secret}" <> T.singleton '\x202E' <> "x")
                        []
                titleLines =
                    filter ("\\section*" `T.isPrefixOf`) (T.lines latex)
            titleLines
                `shouldBe`
                    [ "\\section*{Input List --- Session \\textbackslash{}input\\{secret\\} x}"
                    ]

        it "renders the canonical microphone asset name instead of the legacy copied instrument text" $ do
            let parseUuidKey label raw =
                    case fromPathPiece raw of
                        Just key -> key
                        Nothing -> error ("invalid UUID fixture for " <> label)
                rowKey = parseUuidKey "input row" "00000000-0000-4000-8000-000000000041" :: ME.InputRowId
                versionKey = parseUuidKey "input-list version" "00000000-0000-4000-8000-000000000042" :: ME.InputListVersionId
                micKey = parseUuidKey "microphone asset" "00000000-0000-4000-8000-000000000043" :: ME.AssetId
                row = ME.InputRow
                    { ME.inputRowVersionId = versionKey
                    , ME.inputRowChannelNumber = 7
                    , ME.inputRowTrackName = Just "OH L"
                    , ME.inputRowInstrument = Just "AKG C414 (HC)"
                    , ME.inputRowInstrumentId = Nothing
                    , ME.inputRowMicId = Just micKey
                    , ME.inputRowStandId = Nothing
                    , ME.inputRowCableId = Nothing
                    , ME.inputRowPreampId = Nothing
                    , ME.inputRowInsertOutboardId = Nothing
                    , ME.inputRowConverterChannel = Nothing
                    , ME.inputRowPhantom = Just True
                    , ME.inputRowPolarity = Nothing
                    , ME.inputRowHpf = Nothing
                    , ME.inputRowPad = Nothing
                    , ME.inputRowNotes = Nothing
                    }
                latex = renderInputListLatexWithAssets
                    "Session"
                    (Map.singleton micKey "AKG C414")
                    [Entity rowKey row]
            T.unpack latex `shouldContain` "OH L & AKG C414"
            T.unpack latex `shouldNotContain` "AKG C414 (HC)"

    describe "parseMcpRequest" $ do
        it "accepts canonical JSON-RPC 2.0 MCP requests" $ do
            case parseMcpRequest
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= (1 :: Int)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                ) of
                Just _ -> pure ()
                Nothing -> expectationFailure "Expected canonical MCP request to parse"
            case parseMcpRequest
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("initialized" :: T.Text)
                    ]
                ) of
                Just _ -> pure ()
                Nothing -> expectationFailure "Expected initialized MCP notification to parse"

        it "rejects malformed JSON-RPC envelopes before MCP method fallback handling" $ do
            let assertInvalid payload =
                    case parseMcpRequest payload of
                        Nothing -> pure ()
                        Just value ->
                            expectationFailure
                                ("Expected malformed MCP request to be rejected, got: " <> show value)
            assertInvalid (object ["method" .= ("tools/list" :: T.Text)])
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("1.0" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("   " :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= (" tools/list " :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list?debug=true" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("/tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools//list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list/" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= object ["nested" .= True]
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= A.Null
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= (1.5 :: Double)
                    , "method" .= ("tools/list" :: T.Text)
                ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= (9007199254740992 :: Integer)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= ("   " :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= ("request\n1" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= ("request" <> T.singleton '\x202E' <> "1" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "id" .= T.replicate 129 "a"
                    , "method" .= ("tools/list" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    , "params" .= ("ignored" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    , "params" .= ([1, 2] :: [Int])
                    ]
                )
            assertInvalid
                ( object
                    [ "jsonrpc" .= ("2.0" :: T.Text)
                    , "method" .= ("tools/list" :: T.Text)
                    , "paramsTypo" .= object []
                    ]
                )

    describe "parseToolCallParams" $ do
        it "defaults omitted MCP tool arguments to an object" $
            parseToolCallParams
                (object ["name" .= ("tdf_health_check" :: T.Text)])
                `shouldBe` Just ("tdf_health_check", object [])

        it "rejects malformed MCP tool calls before handler fallback can mask bad params" $ do
            let assertInvalid payload =
                    case parseToolCallParams payload of
                        Nothing -> pure ()
                        Just value ->
                            expectationFailure
                                ("Expected malformed MCP tool params to be rejected, got: " <> show value)
            assertInvalid (object ["name" .= ("   " :: T.Text)])
            assertInvalid (object ["name" .= (" tdf_health_check " :: T.Text)])
            assertInvalid (object ["name" .= ("tdf_health_check?verbose=true" :: T.Text)])
            assertInvalid (object ["name" .= ("/tdf_health_check" :: T.Text)])
            assertInvalid (object ["name" .= ("tdf//health_check" :: T.Text)])
            assertInvalid (object ["name" .= ("tdf_health_check/" :: T.Text)])
            assertInvalid
                ( object
                    [ "name" .= ("tdf_health_check" :: T.Text)
                    , "arguments" .= A.Null
                    ]
                )
            assertInvalid
                ( object
                    [ "name" .= ("tdf_health_check" :: T.Text)
                    , "arguments" .= ([1, 2] :: [Int])
                    ]
                )
            assertInvalid
                ( object
                    [ "name" .= ("tdf_health_check" :: T.Text)
                    , "arguments" .= ("not-an-object" :: T.Text)
                    ]
                )
            assertInvalid
                ( object
                    [ "name" .= ("tdf_health_check" :: T.Text)
                    , "argument" .= object []
                    ]
                )

    describe "validateMcpToolArguments" $ do
        it "accepts empty arguments for the no-input health check tool" $
            validateMcpToolArguments "tdf_health_check" (object [])
                `shouldBe` Right ()

        it "rejects unsupported health check arguments instead of ignoring caller intent" $
            validateMcpToolArguments
                "tdf_health_check"
                (object ["verbose" .= True])
                `shouldBe` Left "tdf_health_check does not accept arguments"

    describe "resolveInvoiceCustomerId" $ do
        it "rejects non-positive customer ids before invoice creation can hit persistence" $ do
            result <- runAuthSqlite $
                resolveInvoiceCustomerId 0
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "customerId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid invoice customer id to be rejected, got: " <> show value)

        it "returns 422 for unknown customers instead of surfacing a database foreign-key failure" $ do
            result <- runAuthSqlite $
                resolveInvoiceCustomerId 999999
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 422
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "customerId references an unknown party"
                Right value ->
                    expectationFailure
                        ("Expected unknown invoice customer to be rejected, got: " <> show value)

        it "resolves existing customers before invoice creation proceeds" $ do
            (expectedPartyId, result) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Invoice Customer"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "invoice-customer@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyStripeCustomerId = Nothing
                    , partyCountryCode = Nothing
                    , partyCountryId = Nothing
                    , partyCreatedAt = now
                    }
                resolved <- resolveInvoiceCustomerId (fromSqlKey partyId)
                pure (partyId, resolved)
            case result of
                Left serverErr ->
                    expectationFailure
                        ("Expected existing invoice customer to resolve, got: " <> show serverErr)
                Right resolvedKey ->
                    resolvedKey `shouldBe` expectedPartyId

    describe "createInvoice" $ do
        it "rejects malformed explicit currencies before invoice creation can persist ambiguous totals" $ do
            let validLine =
                    CreateInvoiceLineReq
                        { cilDescription = "Studio session"
                        , cilQuantity = 1
                        , cilUnitCents = 9000
                        , cilTaxBps = Nothing
                        , cilServiceOrderId = Nothing
                        , cilPackagePurchaseId = Nothing
                        }
                assertInvalid rawCurrency = do
                    result <-
                        runHandler $
                            runReaderT
                                ( createInvoice
                                    (mkUser [Accounting])
                                    (DTO.CreateInvoiceReq 42 (Just rawCurrency) Nothing Nothing [validLine] Nothing)
                                )
                                (error "createInvoice should reject invalid ciCurrency before reading Env")

                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "currency must be a 3-letter ISO code"
                        Right invoice ->
                            expectationFailure
                                ("Expected invalid invoice currency to be rejected, got: " <> show invoice)

            assertInvalid "usdollars"
            assertInvalid "12$"
            assertInvalid "   "

        it "rejects malformed invoice numbers before invoice creation can persist them" $ do
            let validLine =
                    CreateInvoiceLineReq
                        { cilDescription = "Studio session"
                        , cilQuantity = 1
                        , cilUnitCents = 9000
                        , cilTaxBps = Nothing
                        , cilServiceOrderId = Nothing
                        , cilPackagePurchaseId = Nothing
                        }
                assertInvalid rawNumber expectedMessage = do
                    result <-
                        runHandler $
                            runReaderT
                                ( createInvoice
                                    (mkUser [Accounting])
                                    (DTO.CreateInvoiceReq 42 Nothing (Just rawNumber) Nothing [validLine] Nothing)
                                )
                                (error "createInvoice should reject invalid ciNumber before reading Env")

                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right invoice ->
                            expectationFailure
                                ("Expected invalid invoice number to be rejected, got: " <> show invoice)

            assertInvalid "INV-2026\n001" "Invoice number must not contain control characters"
            assertInvalid
                ("INV-2026" <> T.singleton '\x202E' <> "001")
                "Invoice number must not contain control characters or Unicode formatting marks"
            assertInvalid
                (T.replicate 65 "A")
                "Invoice number must be 64 characters or fewer"

        it "rejects oversized line item lists before invoice creation can fan out database writes" $ do
            let validLine =
                    CreateInvoiceLineReq
                        { cilDescription = "Studio session"
                        , cilQuantity = 1
                        , cilUnitCents = 9000
                        , cilTaxBps = Nothing
                        , cilServiceOrderId = Nothing
                        , cilPackagePurchaseId = Nothing
                        }
                oversizedInvoice =
                    DTO.CreateInvoiceReq
                        42
                        Nothing
                        Nothing
                        Nothing
                        (replicate 101 validLine)
                        Nothing

            result <-
                runHandler $
                    runReaderT
                        (createInvoice (mkUser [Accounting]) oversizedInvoice)
                        (error "createInvoice should reject oversized ciLineItems before reading Env")

            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invoice supports at most 100 line items"
                Right invoice ->
                    expectationFailure
                        ("Expected oversized invoice to be rejected, got: " <> show invoice)

        it "rejects aggregate invoice totals that exceed the backend amount range before persistence" $ do
            let largeLine =
                    CreateInvoiceLineReq
                        { cilDescription = "Large license installment"
                        , cilQuantity = 1
                        , cilUnitCents = (maxBound :: Int) `div` 2 + 1
                        , cilTaxBps = Nothing
                        , cilServiceOrderId = Nothing
                        , cilPackagePurchaseId = Nothing
                        }
                oversizedInvoice =
                    DTO.CreateInvoiceReq
                        42
                        Nothing
                        Nothing
                        Nothing
                        [largeLine, largeLine]
                        Nothing

            result <-
                runHandler $
                    runReaderT
                        (createInvoice (mkUser [Accounting]) oversizedInvoice)
                        (error "createInvoice should reject oversized totals before reading Env")

            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invoice subtotal exceeds supported invoice amount"
                Right invoice ->
                    expectationFailure
                        ("Expected oversized invoice total to be rejected, got: " <> show invoice)

    describe "invoice and receipt lookup ids" $
        it "rejects non-positive lookup ids before treating them as missing rows" $ do
            let assertInvalid expectedMessage action = do
                    result <- runHandler action
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid lookup id to be rejected, got: " <> show value)

            assertInvalid
                "invoiceId must be a positive integer"
                ( runReaderT
                    (getInvoiceById (mkUser [Accounting]) 0)
                    (error "getInvoiceById should reject invalid invoiceId before reading Env")
                )
            assertInvalid
                "receiptId must be a positive integer"
                ( runReaderT
                    (getReceipt (mkUser [Accounting]) (-1))
                    (error "getReceipt should reject invalid receiptId before reading Env")
                )

    describe "getInvoicesBySession" $
        it "distinguishes unknown sessions from known sessions with no invoices" $ do
            let missingSessionId = "00000000-0000-0000-0000-000000000901"
                existingSessionId = "00000000-0000-0000-0000-000000000902"
                existingSessionKey =
                    case fromPathPiece existingSessionId of
                        Just keyVal -> keyVal
                        Nothing -> error "Expected fixture session id to parse"
                now = UTCTime (fromGregorian 2026 4 22) (secondsToDiffTime 0)
                envFor pool =
                    Env
                        { envPool = pool
                        , envConfig = error "envConfig should be unused by invoice session lookup tests"
                        }
                runLookup pool sessionId =
                    runHandler $
                        runReaderT
                            (getInvoicesBySession (mkUser [Accounting]) sessionId)
                            (envFor pool)

            pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
            runSqlPool initializeSessionInvoiceLookupSchema pool

            missingResult <- runLookup pool missingSessionId
            case missingResult of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 404
                    BL8.unpack (errBody serverErr) `shouldContain` "Session not found"
                Right value ->
                    expectationFailure
                        ("Expected unknown session invoice lookup to fail, got: " <> show value)

            runSqlPool
                ( insertKey existingSessionKey
                    ME.Session
                        { ME.sessionBookingRef = Nothing
                        , ME.sessionBandId = Nothing
                        , ME.sessionClientPartyRef = Nothing
                        , ME.sessionService = "Tracking"
                        , ME.sessionStartAt = now
                        , ME.sessionEndAt = addUTCTime 3600 now
                        , ME.sessionEngineerRef = "Engineer"
                        , ME.sessionAssistantRef = Nothing
                        , ME.sessionStatus = ME.InPrep
                        , ME.sessionSampleRate = Nothing
                        , ME.sessionBitDepth = Nothing
                        , ME.sessionDaw = Nothing
                        , ME.sessionSessionFolderDriveId = Nothing
                        , ME.sessionNotes = Nothing
                        }
                )
                pool

            existingResult <- runLookup pool existingSessionId
            case existingResult of
                Left serverErr ->
                    expectationFailure
                        ("Expected known empty session invoice lookup to succeed, got: " <> show serverErr)
                Right value ->
                    BL8.unpack (A.encode value) `shouldBe` "[]"

    describe "prepareLine" $ do
        it "accepts a single positive provenance reference and preserves it on the prepared invoice line" $
            case prepareLine
                CreateInvoiceLineReq
                    { cilDescription = "  Mixing session  "
                    , cilQuantity = 1
                    , cilUnitCents = 12000
                    , cilTaxBps = Just 1200
                    , cilServiceOrderId = Just 42
                    , cilPackagePurchaseId = Nothing
                    } of
                Left errMsg ->
                    expectationFailure ("Expected valid invoice line to prepare, got: " <> T.unpack errMsg)
                Right preparedLine -> do
                    plDescription preparedLine `shouldBe` "Mixing session"
                    fmap fromSqlKey (plServiceOrderId preparedLine) `shouldBe` Just 42
                    plPackagePurchaseId preparedLine `shouldBe` Nothing
                    plTotal preparedLine `shouldBe` 13440

        it "rejects unsafe invoice line description characters before persistence" $ do
            let assertInvalid rawDescription =
                    case prepareLine
                        CreateInvoiceLineReq
                            { cilDescription = rawDescription
                            , cilQuantity = 1
                            , cilUnitCents = 1000
                            , cilTaxBps = Nothing
                            , cilServiceOrderId = Nothing
                            , cilPackagePurchaseId = Nothing
                            } of
                        Left errMsg ->
                            errMsg
                                `shouldBe` "Line item description must not contain control characters or Unicode formatting marks"
                        Right preparedLine ->
                            expectationFailure
                                ("Expected unsafe invoice line description to be rejected, got: " <> T.unpack (plDescription preparedLine))
            assertInvalid "Session\nInjection"
            assertInvalid ("Session" <> T.singleton '\x202E' <> "001")

        it "rejects non-positive provenance references before invoice creation can hit ambiguous foreign-key errors" $ do
            let assertInvalid expectedMessage request =
                    case prepareLine request of
                        Left errMsg ->
                            errMsg `shouldBe` expectedMessage
                        Right preparedLine ->
                            expectationFailure
                                ("Expected invalid invoice line reference to be rejected, got: " <> show (plServiceOrderId preparedLine, plPackagePurchaseId preparedLine))
            assertInvalid
                "serviceOrderId must be a positive integer"
                CreateInvoiceLineReq
                    { cilDescription = "Session"
                    , cilQuantity = 1
                    , cilUnitCents = 1000
                    , cilTaxBps = Nothing
                    , cilServiceOrderId = Just 0
                    , cilPackagePurchaseId = Nothing
                    }
            assertInvalid
                "packagePurchaseId must be a positive integer"
                CreateInvoiceLineReq
                    { cilDescription = "Package"
                    , cilQuantity = 1
                    , cilUnitCents = 1000
                    , cilTaxBps = Nothing
                    , cilServiceOrderId = Nothing
                    , cilPackagePurchaseId = Just (-3)
                    }

        it "rejects line items that try to point at both a service order and a package purchase" $
            case prepareLine
                CreateInvoiceLineReq
                    { cilDescription = "Bundle"
                    , cilQuantity = 1
                    , cilUnitCents = 1000
                    , cilTaxBps = Nothing
                    , cilServiceOrderId = Just 11
                    , cilPackagePurchaseId = Just 22
                    } of
                Left errMsg ->
                    errMsg `shouldBe` "Line item may reference either serviceOrderId or packagePurchaseId, not both"
                Right preparedLine ->
                    expectationFailure
                        ("Expected contradictory invoice line provenance to be rejected, got: " <> show (plServiceOrderId preparedLine, plPackagePurchaseId preparedLine))

        it "rejects tax basis points above 100 percent before invoice totals are calculated" $
            case prepareLine
                CreateInvoiceLineReq
                    { cilDescription = "Session"
                    , cilQuantity = 1
                    , cilUnitCents = 1000
                    , cilTaxBps = Just 10001
                    , cilServiceOrderId = Nothing
                    , cilPackagePurchaseId = Nothing
                    } of
                Left errMsg ->
                    errMsg `shouldBe` "Line item tax basis points must be 10000 or less"
                Right preparedLine ->
                    expectationFailure
                        ("Expected excessive tax basis points to be rejected, got: " <> show (plTaxBps preparedLine))

        it "rejects line totals that exceed the backend amount range before persistence" $
            case prepareLine
                CreateInvoiceLineReq
                    { cilDescription = "Large annual license"
                    , cilQuantity = maxBound
                    , cilUnitCents = 1
                    , cilTaxBps = Just 10000
                    , cilServiceOrderId = Nothing
                    , cilPackagePurchaseId = Nothing
                    } of
                Left errMsg ->
                    errMsg `shouldBe` "Line item total exceeds supported invoice amount"
                Right preparedLine ->
                    expectationFailure
                        ("Expected oversized invoice line total to be rejected, got: " <> show (plTotal preparedLine))

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

    describe "resolvePartyRelatedTarget" $ do
        it "rejects non-positive party ids before related lookups can return empty fallback data" $ do
            result <- runAuthSqlite $
                resolvePartyRelatedTarget 0
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "partyId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid related party id to be rejected, got: " <> show value)

        it "returns 404 for unknown parties instead of publishing an empty related fallback" $ do
            result <- runAuthSqlite $
                resolvePartyRelatedTarget 999999
            case result of
                Left serverErr ->
                    errHTTPCode serverErr `shouldBe` 404
                Right value ->
                    expectationFailure
                        ("Expected unknown related party lookup to be rejected, got: " <> show value)

    describe "resolveSocialTargetPartyId" $ do
        it "rejects non-positive party ids before social follow creation attempts any lookup" $ do
            result <- runAuthSqlite $
                resolveSocialTargetPartyId 0
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 400
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "partyId must be a positive integer"
                Right value ->
                    expectationFailure
                        ("Expected invalid social target party id to be rejected, got: " <> show value)

        it "returns 404 for unknown social targets instead of pretending friend or vCard follow creation succeeded" $ do
            result <- runAuthSqlite $
                resolveSocialTargetPartyId 999999
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 404
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Party not found"
                Right value ->
                    expectationFailure
                        ("Expected unknown social target party to be rejected, got: " <> show value)

        it "resolves existing social targets before the follow upsert runs" $ do
            (expectedPartyId, result) <- runAuthSqlite $ do
                now <- liftIO getCurrentTime
                partyId <- insert Party
                    { partyLegalName = Nothing
                    , partyDisplayName = "Social Target"
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just "social-target@example.com"
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyStripeCustomerId = Nothing
                    , partyCountryCode = Nothing
                    , partyCountryId = Nothing
                    , partyCreatedAt = now
                    }
                resolved <- resolveSocialTargetPartyId (fromSqlKey partyId)
                pure (partyId, resolved)
            case result of
                Left serverErr ->
                    expectationFailure
                        ("Expected existing social target party to resolve, got: " <> show serverErr)
                Right resolvedKey ->
                    resolvedKey `shouldBe` expectedPartyId

    describe "validateSocialProfilePartyIds" $ do
        it "keeps social profile batch lookups positive, unique, and bounded" $ do
            validateSocialProfilePartyIds [] `shouldBe` Right []
            validateSocialProfilePartyIds [12, 33, 44] `shouldBe` Right [12, 33, 44]

            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid social profile party ids, got: " <> show value)
            assertInvalid
                "partyId query must contain only positive integers"
                (validateSocialProfilePartyIds [12, 0, 44])
            assertInvalid
                "partyId query must not contain duplicate ids"
                (validateSocialProfilePartyIds [12, 33, 12])
            assertInvalid
                "partyId query supports at most 100 ids"
                (validateSocialProfilePartyIds [1..101])

    describe "validateFanProfileUpdate" $ do
        let profileUpdate displayName =
                DTO.FanProfileUpdate
                    { DTO.fpuDisplayName = displayName
                    , DTO.fpuAvatarUrl = Nothing
                    , DTO.fpuFavoriteGenreIds = []
                    , DTO.fpuBio = Nothing
                    , DTO.fpuCity = Nothing
                    }

        it "normalizes fan display names before profile fallback rendering" $ do
            case validateFanProfileUpdate (profileUpdate (Just "  Ada Fan  ")) of
                Right validated ->
                    DTO.fpuDisplayName validated `shouldBe` Just "Ada Fan"
                Left serverErr ->
                    expectationFailure
                        ("Expected valid fan profile update, got: " <> show serverErr)

            case validateFanProfileUpdate (profileUpdate (Just "   ")) of
                Right validated ->
                    DTO.fpuDisplayName validated `shouldBe` Nothing
                Left serverErr ->
                    expectationFailure
                        ("Expected blank fan display name to clear, got: " <> show serverErr)

            resolveFanProfileDisplayName (Just "   ") (Just "  Party Name  ")
                `shouldBe` Just "Party Name"
            resolveFanProfileDisplayName (Just "  Ada Fan  ") (Just "Party Name")
                `shouldBe` Just "Ada Fan"

        it "rejects unsafe or oversized fan display names before persistence" $ do
            let assertInvalid rawDisplayName expectedMessage =
                    case validateFanProfileUpdate (profileUpdate (Just rawDisplayName)) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid fan profile update, got: " <> show value)
            assertInvalid "Ada\nFan" "displayName must not contain control characters"
            assertInvalid
                ("Ada" <> T.singleton '\x202E' <> "Fan")
                "hidden formatting characters"
            assertInvalid (T.replicate 161 "A") "displayName must be 160 characters or fewer"

    describe "resolveFanFollowArtistTarget" $ do
        it "requires fan follow targets to be published artist profiles" $ do
            (artistPartyId, nonArtistResult, missingResult, invalidResult, validResult) <-
                runAuthSqlite $ do
                    now <- liftIO getCurrentTime
                    let insertParty displayName emailAddress =
                            insert
                                Party
                                    { partyLegalName = Nothing
                                    , partyDisplayName = displayName
                                    , partyIsOrg = False
                                    , partyTaxId = Nothing
                                    , partyPrimaryEmail = Just emailAddress
                                    , partyPrimaryPhone = Nothing
                                    , partyWhatsapp = Nothing
                                    , partyInstagram = Nothing
                                    , partyEmergencyContact = Nothing
                                    , partyNotes = Nothing
                                    , partyStripeCustomerId = Nothing
                                    , partyCountryCode = Nothing
                                    , partyCountryId = Nothing
                                    , partyCreatedAt = now
                                    }
                        insertArtistProfile artistKey =
                            insert_
                                ArtistProfile
                                    { artistProfileArtistPartyId = artistKey
                                    , artistProfileSlug = Just "fan-follow-target"
                                    , artistProfileBio = Nothing
                                    , artistProfileCity = Nothing
                                    , artistProfileHeroImageUrl = Nothing
                                    , artistProfileSpotifyArtistId = Nothing
                                    , artistProfileSpotifyUrl = Nothing
                                    , artistProfileYoutubeChannelId = Nothing
                                    , artistProfileYoutubeUrl = Nothing
                                    , artistProfileWebsiteUrl = Nothing
                                    , artistProfileFeaturedVideoUrl = Nothing
                                    , artistProfileGenres = Nothing
                                    , artistProfileHighlights = Nothing
                                    , artistProfileStripeAccountId = Nothing
                                    , artistProfileCountryCode = Nothing
                                    , artistProfileCountryId = Nothing
                                    , artistProfileCreatedAt = now
                                    , artistProfileUpdatedAt = Nothing
                                    }
                    artistPartyId <-
                        insertParty "Fan Follow Artist" "follow-artist@example.com"
                    insertArtistProfile artistPartyId
                    nonArtistPartyId <- insertParty "Plain Party" "plain-party@example.com"
                    nonArtistResult <-
                        resolveFanFollowArtistTarget (fromSqlKey nonArtistPartyId)
                    missingResult <- resolveFanFollowArtistTarget 999999
                    invalidResult <- resolveFanFollowArtistTarget 0
                    validResult <- resolveFanFollowArtistTarget (fromSqlKey artistPartyId)
                    pure (artistPartyId, nonArtistResult, missingResult, invalidResult, validResult)

            validResult `shouldBe` Right artistPartyId
            let assertRejected :: Int -> String -> Either ServerError (Key Party) -> IO ()
                assertRejected expectedCode expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` expectedCode
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right target ->
                            expectationFailure
                                ( "Expected invalid fan follow artist target, got: "
                                    <> show (fromSqlKey target)
                                )
            assertRejected 404 "Artist profile not found" nonArtistResult
            assertRejected 404 "Artist profile not found" missingResult
            assertRejected 400 "artistId must be a positive integer" invalidResult

    describe "fanUnfollowArtist" $ do
        it "rejects invalid or duplicated fan grants before loading follow fallback data" $ do
            let duplicatedFan =
                    mkUser [Fan, Fan]
                invalidPartyFan =
                    (mkUser [Fan]) { auPartyId = toSqlKey 0 }
                assertRejected user = do
                    result <-
                        runHandler $
                            runReaderT
                                (fanListFollows user)
                                (error "fanListFollows should reject malformed auth before reading Env")
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 403
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Fan access requires coherent role grants"
                        Right _ ->
                            expectationFailure
                                "Expected malformed fan auth scope to be rejected"
            assertRejected duplicatedFan
            assertRejected invalidPartyFan

        it "rejects invalid fan follow targets before deleting can return a misleading no-op" $ do
            let user = mkUser [Fan]
                assertInvalid rawArtistId expectedMessage = do
                    result <-
                        runHandler $
                            runReaderT
                                (fanUnfollowArtist user rawArtistId)
                                (error "fanUnfollowArtist should reject invalid ids before reading Env")
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid fan unfollow target to be rejected, got: " <> show value)
            assertInvalid 0 "Invalid artist id"
            assertInvalid 1 "No puedes dejar de seguirte a ti mismo"

    describe "artistGetOwnProfile" $
        it "rejects invalid or duplicated artist grants before loading profile fallback data" $ do
            let duplicatedArtist =
                    mkUser [Artist, Artist]
                invalidPartyArtist =
                    (mkUser [Artist]) { auPartyId = toSqlKey 0 }
                assertRejected user = do
                    result <-
                        runHandler $
                            runReaderT
                                (artistGetOwnProfile user)
                                (error "artistGetOwnProfile should reject malformed auth before reading Env")
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 403
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Artist access requires coherent role grants"
                        Right _ ->
                            expectationFailure
                                "Expected malformed artist auth scope to be rejected"
            assertRejected duplicatedArtist
            assertRejected invalidPartyArtist

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

    describe "validateServiceMarketplaceBookingTitle" $ do
        it "keeps omitted titles as the service-ad headline fallback and trims explicit titles" $ do
            validateServiceMarketplaceBookingTitle Nothing `shouldBe` Right Nothing
            validateServiceMarketplaceBookingTitle (Just "  Mezcla analogica  ")
                `shouldBe` Right (Just "Mezcla analogica")

        it "rejects blank, oversized, or unsafe titles before service marketplace booking writes" $ do
            let assertInvalid rawTitle expectedMessage =
                    case validateServiceMarketplaceBookingTitle (Just rawTitle) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid service marketplace title to be rejected, got: "
                                    <> show value
                                )
  …145897 tokens truncated… $ do
            case futureStubResponseFor "crm" "parties/list-columns" of
                Right response -> do
                    stubArea response `shouldBe` "crm"
                    stubEndpoint response `shouldBe` "parties/list-columns"
                    stubId response `shouldBe` "crm.parties.list-columns"
                    stubPath response `shouldBe` "/stubs/crm/parties/list-columns"
                    stubMethod response `shouldBe` "GET"
                    stubStatus response `shouldBe` "planned"
                    stubRequiredRole response `shouldBe` roleToText Admin
                    stubRequiredRoles response `shouldBe` futureStubRequiredRoles
                    stubRequiredModule response `shouldBe` moduleName ModuleAdmin
                    stubImplemented response `shouldBe` False
                Left serverErr ->
                    expectationFailure
                        ("Expected canonical future stub response, got: " <> show serverErr)

            case futureStubResponseFor "crm" "parties/export" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invalid future stub metadata"
                    BL8.unpack (errBody serverErr)
                        `shouldNotContain` "Invalid future stub response"
                Right value ->
                    expectationFailure
                        ("Expected invalid future stub metadata, got: " <> show value)

        it "blocks mounted generic stubs when the admin console fallback metadata drifts" $ do
            case futureStubResponseForWithConsole
                futureAdminConsoleView
                "crm"
                "parties/list-columns" of
                Right response ->
                    stubId response `shouldBe` "crm.parties.list-columns"
                Left serverErr ->
                    expectationFailure
                        ( "Expected canonical fallback discovery surface to serve, got: "
                            <> show serverErr
                        )

            case futureStubResponseForWithConsole
                (futureAdminConsoleView { Future.viewStatus = "planned" })
                "crm"
                "parties/list-columns" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invalid future admin console metadata"
                Right response ->
                    expectationFailure
                        ( "Expected drifted admin console fallback metadata to block "
                            <> "generic stub serving, got: "
                            <> show response
                        )

            case futureStubResponseForWithConsole
                (futureAdminConsoleView { Future.viewStatus = "planned" })
                "crm"
                "parties/export" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invalid future admin console metadata"
                    BL8.unpack (errBody serverErr)
                        `shouldNotContain` "Invalid future stub metadata"
                Right response ->
                    expectationFailure
                        ( "Expected admin console drift to be reported before "
                            <> "route metadata drift, got: "
                            <> show response
                        )

    describe "validateFutureStubPublishedId" $
        it "keeps fallback discovery ids tied to canonical route segments" $ do
            validateFutureStubPublishedId
                "crm"
                "parties/list-columns"
                "crm.parties.list-columns"
                `shouldBe` Right "crm.parties.list-columns"

            let assertInvalid rawId =
                    case validateFutureStubPublishedId "crm" "parties/list-columns" rawId of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub response"
                        Right value ->
                            expectationFailure
                                ("Expected invalid published future stub id, got: " <> show value)

            assertInvalid "crm.parties.filters"
            assertInvalid "crm.parties/list-columns"
            assertInvalid "crm.parties..list-columns"
            assertInvalid "CRM.parties.list-columns"
            assertInvalid "crm.parties.list-columns."

            case validateFutureStubPublishedId
                "crm"
                "parties/export"
                "crm.parties.export" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invalid future stub response"
                Right value ->
                    expectationFailure
                        ("Expected unregistered future stub id to fail, got: " <> show value)

    describe "validateFutureStubPublishedPath" $
        it "keeps fallback discovery paths rooted under canonical protected stubs" $ do
            validateFutureStubPublishedPath
                "crm"
                "parties/list-columns"
                "/stubs/crm/parties/list-columns"
                `shouldBe` Right "/stubs/crm/parties/list-columns"

            let assertInvalid path =
                    case validateFutureStubPublishedPath "crm" "parties/list-columns" path of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub response"
                        Right value ->
                            expectationFailure
                                ("Expected invalid published future stub path, got: " <> show value)

            assertInvalid "/crm/parties/list-columns"
            assertInvalid "/stubs/crm/../parties/list-columns"
            assertInvalid "/stubs/crm/parties//list-columns"
            assertInvalid "/stubs/crm/parties/list-columns/"
            assertInvalid "/stubs/crm/parties/list-columns?draft=true"

            case validateFutureStubPublishedPath
                "crm"
                "parties/export"
                "/stubs/crm/parties/export" of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    BL8.unpack (errBody serverErr)
                        `shouldContain` "Invalid future stub response"
                Right value ->
                    expectationFailure
                        ("Expected unregistered future stub path to fail, got: " <> show value)

    describe "validateFutureAdminConsolePublishedId" $
        it "keeps the special admin console preview id separate from generic stubs" $ do
            validateFutureAdminConsolePublishedId "admin.console"
                `shouldBe` Right "admin.console"

            let assertInvalid rawId =
                    case validateFutureAdminConsolePublishedId rawId of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid admin console preview id, got: "
                                    <> show value
                                )

            assertInvalid "admin.seed"
            assertInvalid "admin/console"
            assertInvalid "admin..console"
            assertInvalid "Admin.console"
            assertInvalid "admin.console."

    describe "validateFutureAdminConsolePublishedPath" $
        it "keeps the special admin console preview rooted under protected stubs" $ do
            validateFutureAdminConsolePublishedPath "/stubs/admin/console"
                `shouldBe` Right "/stubs/admin/console"

            let assertInvalid path =
                    case validateFutureAdminConsolePublishedPath path of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid admin console preview path, got: "
                                    <> show value
                                )

            assertInvalid "/admin/console"
            assertInvalid "/stubs/admin/../console"
            assertInvalid "/stubs/admin//console"
            assertInvalid "/stubs/admin/console/"
            assertInvalid "/stubs/admin/console?preview=true"

    describe "validateFutureStubAuthMetadata" $
        it "keeps fallback discovery auth metadata canonical and duplicate-free" $ do
            futureStubRequiredRoles `shouldBe` ["Admin", "Fan", "Customer"]
            validateFutureStubAuthMetadata "Admin" futureStubRequiredRoles
                `shouldBe` Right ("Admin", ["Admin", "Fan", "Customer"])

            let assertInvalid requiredRole requiredRoles =
                    case validateFutureStubAuthMetadata requiredRole requiredRoles of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub response"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid future stub auth metadata, got: "
                                    <> show value
                                )

            assertInvalid "Manager" ["Admin", "Fan", "Customer"]
            assertInvalid "Admin" ["Admin", "Customer", "Fan"]
            assertInvalid "Admin" ["Admin", "Fan", "Fan"]
            assertInvalid "Admin" ["Admin", "Fan", "Customer", "Manager"]

    describe "validateFutureStubStatus" $
        it "pins fallback discovery statuses to their canonical response envelopes" $ do
            futureStubStatus `shouldBe` "planned"
            futureAdminConsoleStatus `shouldBe` "preview"
            validateFutureStubStatus "planned" `shouldBe` Right "planned"
            validateFutureAdminConsoleStatus "preview" `shouldBe` Right "preview"

            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid fallback discovery status metadata, got: "
                                    <> show value
                                )

            assertInvalid
                "Invalid future stub response"
                (validateFutureStubStatus "preview")
            assertInvalid
                "Invalid future stub response"
                (validateFutureStubStatus "planned ")
            assertInvalid
                "Invalid future admin console metadata"
                (validateFutureAdminConsoleStatus "planned")
            assertInvalid
                "Invalid future stub response"
                (validateFutureStatusMetadataWith
                    (Left err500 { errBody = "Invalid future stub response" })
                    "preview"
                    "planned"
                    "planned")

    describe "validateFutureStubMethod" $
        it "pins fallback discovery method metadata to mounted GET routes" $ do
            futureStubMethod `shouldBe` "GET"
            validateFutureStubMethod "GET" `shouldBe` Right "GET"
            validateFutureAdminConsoleMethod "GET" `shouldBe` Right "GET"

            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid fallback discovery method metadata, got: "
                                    <> show value
                                )

            assertInvalid
                "Invalid future stub response"
                (validateFutureStubMethod "POST")
            assertInvalid
                "Invalid future admin console metadata"
                (validateFutureAdminConsoleMethod "POST")
            assertInvalid
                "Invalid future stub response"
                (validateFutureMethodMetadataWith
                    (Left err500 { errBody = "Invalid future stub response" })
                    "POST"
                    "POST")

    describe "validateFutureStubRequiredModule" $
        it "keeps fallback discovery module metadata pinned to canonical Admin" $ do
            futureStubRequiredModule `shouldBe` "Admin"
            validateFutureStubRequiredModule "Admin" `shouldBe` Right "Admin"
            validateFutureAdminConsoleRequiredModule "Admin" `shouldBe` Right "Admin"

            let assertInvalidStub requiredModule =
                    case validateFutureStubRequiredModule requiredModule of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub response"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid future stub module metadata, got: "
                                    <> show value
                                )
                assertInvalidConsole requiredModule =
                    case validateFutureAdminConsoleRequiredModule requiredModule of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid admin console module metadata, got: "
                                    <> show value
                                )

            assertInvalidStub "admin"
            assertInvalidStub "ModuleAdmin"
            assertInvalidStub "Admin "
            assertInvalidConsole "CRM"

    describe "validateFutureStubResponse" $ do
        it "rejects malformed fallback discovery response envelopes before serving them" $ do
            let mkResponseWithId
                    stubIdValue
                    area
                    endpoint
                    path
                    method
                    status
                    requiredRole
                    requiredModule
                    implemented =
                    StubResponse
                        { stubArea = area
                        , stubEndpoint = endpoint
                        , stubId = stubIdValue
                        , stubPath = path
                        , stubMethod = method
                        , stubStatus = status
                        , stubRequiredRole = requiredRole
                        , stubRequiredRoles = futureStubRequiredRoles
                        , stubRequiredModule = requiredModule
                        , stubImplemented = implemented
                        }
                mkResponse area endpoint =
                    mkResponseWithId (futureStubId area endpoint) area endpoint
                validResponse =
                    mkResponse
                        "crm"
                        "parties/list-columns"
                        "/stubs/crm/parties/list-columns"
                        "GET"
                        "planned"
                        "Admin"
                        "Admin"
                        False
                assertInvalid response =
                    case validateFutureStubResponse response of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub response"
                        Right value ->
                            expectationFailure
                                ("Expected invalid future stub response, got: " <> show value)

            case validateFutureStubResponse validResponse of
                Right response -> do
                    stubArea response `shouldBe` "crm"
                    stubEndpoint response `shouldBe` "parties/list-columns"
                    stubId response `shouldBe` "crm.parties.list-columns"
                    stubPath response `shouldBe` "/stubs/crm/parties/list-columns"
                    stubMethod response `shouldBe` "GET"
                    stubStatus response `shouldBe` "planned"
                    stubRequiredRole response `shouldBe` roleToText Admin
                    stubRequiredRoles response `shouldBe` futureStubRequiredRoles
                    stubRequiredModule response `shouldBe` moduleName ModuleAdmin
                    stubImplemented response `shouldBe` False
                Left serverErr ->
                    expectationFailure
                        ("Expected valid future stub response, got: " <> show serverErr)

            assertInvalid
                (mkResponseWithId
                    "crm.parties.filters"
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "GET"
                    "planned"
                    "Admin"
                    "Admin"
                    False)
            assertInvalid
                (validResponse { stubRequiredRoles = ["Admin"] })
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "POST"
                    "planned"
                    "Admin"
                    "Admin"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "get"
                    "planned"
                    "Admin"
                    "Admin"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "GET"
                    "ready"
                    "Admin"
                    "Admin"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "GET"
                    "planned"
                    "Manager"
                    "Admin"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "GET"
                    "planned"
                    "Admin"
                    "CRM"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/list-columns"
                    "GET"
                    "planned"
                    "Admin"
                    "Admin"
                    True)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/export"
                    "/stubs/crm/parties/export"
                    "GET"
                    "planned"
                    "Admin"
                    "Admin"
                    False)
            assertInvalid
                (mkResponse
                    "crm"
                    "parties/list-columns"
                    "/stubs/crm/parties/filters"
                    "GET"
                    "planned"
                    "Admin"
                    "Admin"
                    False)

    describe "invalidCardText" $
        it "rejects ambiguous Unicode in admin console fallback copy" $ do
            invalidCardText 120 "Tokens API" `shouldBe` False
            invalidCardText 120 ("Tokens" <> T.singleton '\x00A0' <> "API")
                `shouldBe` True
            invalidCardText 120 ("Tokens" <> T.singleton '\x2007' <> "API")
                `shouldBe` True
            invalidCardText 120 ("Gestio" <> T.singleton '\x0301' <> "n de usuarios")
                `shouldBe` True
            invalidCardText 120 ("T" <> T.singleton '\x043E' <> "kens API")
                `shouldBe` True
            invalidCardText 120 ("Tokens API " <> T.singleton '\x1F511')
                `shouldBe` True
            invalidCardText 120 ("Tokens" <> T.singleton '\xE000' <> "API")
                `shouldBe` True
            invalidCardText 120 ("Tokens API " <> T.singleton '\x00A9')
                `shouldBe` True
            invalidCardText 120 ("Roles " <> T.singleton '\x00B1' <> " permisos")
                `shouldBe` True

    describe "validateFutureAdminConsoleCardIds" $
        it "rejects drifted admin console card registries before serving fallback discovery metadata" $ do
            validateFutureAdminConsoleCardIds allowedFutureAdminConsoleCardIds
                `shouldBe` Right ["user-management", "api-tokens"]

            let assertInvalid cardIds =
                    case validateFutureAdminConsoleCardIds cardIds of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected drifted admin console card registry to fail, got: "
                                    <> show value
                                )

            assertInvalid []
            assertInvalid ["api-tokens", "user-management"]
            assertInvalid ["user-management", "api-tokens", "api-tokens"]
            assertInvalid ["user-management", "api tokens"]
            assertInvalid ["user-management", "unknown-card"]

    describe "validateFutureAdminConsoleCard" $ do
        it "rejects malformed or mislabeled admin console cards before serving fallback discovery metadata" $ do
            let mkCardWith implementedValue cardIdValue titleValue bodyValue =
                    Future.AdminConsoleCard
                        { Future.cardId = cardIdValue
                        , Future.title = titleValue
                        , Future.body = bodyValue
                        , Future.implemented = implementedValue
                        }
                mkCard = mkCardWith False
                validUserManagementBody =
                    [ "La asignación de roles se administra desde la pantalla de Parties."
                    , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
                    ]
                validCard =
                    mkCard "user-management" "Gestión de usuarios" validUserManagementBody
                assertInvalid card =
                    case validateFutureAdminConsoleCard card of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ("Expected invalid admin console card, got: " <> show value)
                assertInvalidWithIds cardIds card =
                    case validateFutureAdminConsoleCardWithIds cardIds card of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid admin console card registry, got: "
                                    <> show value
                                )

            case validateFutureAdminConsoleCard validCard of
                Right card ->
                    Future.cardId card `shouldBe` "user-management"
                Left serverErr ->
                    expectationFailure
                        ("Expected valid admin console card, got: " <> show serverErr)

            assertInvalidWithIds ["user-management"] validCard
            assertInvalidWithIds ["api-tokens", "user-management"] validCard
            assertInvalidWithIds
                ["user-management", "api-tokens", "api-tokens"]
                validCard
            assertInvalid (mkCard "User Management" "Gestión de usuarios" ["Roles"])
            assertInvalid (mkCard "unknown-card" "Gestión de usuarios" ["Roles"])
            assertInvalid (mkCard "api-tokens" "Gestión de usuarios" ["Roles"])
            assertInvalid (mkCardWith True "user-management" "Gestión de usuarios" validUserManagementBody)
            assertInvalid (mkCard "user-management" " Gestión de usuarios" ["Roles"])
            assertInvalid (mkCard "user-management" "Gestión\nusuarios" ["Roles"])
            assertInvalid (mkCard "user-management" "Gestión\x2028usuarios" ["Roles"])
            assertInvalid (mkCard "user-management" "Gestión\x200B de usuarios" ["Roles"])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" ["Roles\x202E"])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" ["Roles\x2029seguros"])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" ["Roles y permisos"])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" ["Roles", "roles"])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" [])
            assertInvalid (mkCard "user-management" "Gestión de usuarios" ["Roles", " "])

    describe "validateFutureAdminConsoleView" $ do
        it "rejects duplicate card ids or malformed status before serving fallback discovery" $ do
            let mkCard cardIdValue titleValue bodyValue =
                    Future.AdminConsoleCard
                        { Future.cardId = cardIdValue
                        , Future.title = titleValue
                        , Future.body = bodyValue
                        , Future.implemented = False
                        }
                userManagementBody =
                    [ "La asignación de roles se administra desde la pantalla de Parties."
                    , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
                    ]
                apiTokensBody =
                    [ "Los tokens de servicio deben administrarse desde un flujo dedicado."
                    , "El acceso quedará separado de usuarios humanos para integraciones internas."
                    ]
                validUserManagementCard =
                    mkCard "user-management" "Gestión de usuarios" userManagementBody
                validApiTokensCard =
                    mkCard "api-tokens" "Tokens API" apiTokensBody
                mkViewWithRoute
                    areaValue
                    endpointValue
                    pathValue
                    methodValue
                    statusValue
                    roleValue
                    moduleValue
                    implementedValue
                    cardsValue =
                    Future.AdminConsoleView
                        { Future.viewArea = areaValue
                        , Future.viewEndpoint = endpointValue
                        , Future.viewId = futureStubId areaValue endpointValue
                        , Future.viewPath = pathValue
                        , Future.viewMethod = methodValue
                        , Future.viewStatus = statusValue
                        , Future.viewRequiredRole = roleValue
                        , Future.viewRequiredRoles = futureStubRequiredRoles
                        , Future.viewRequiredModule = moduleValue
                        , Future.viewImplemented = implementedValue
                        , Future.cards = cardsValue
                        }
                mkViewWith statusValue roleValue moduleValue implementedValue cardsValue =
                    mkViewWithRoute
                        "admin"
                        "console"
                        "/stubs/admin/console"
                        "GET"
                        statusValue
                        roleValue
                        moduleValue
                        implementedValue
                        cardsValue
                mkView statusValue =
                    mkViewWith statusValue "Admin" "Admin" False
                validCards =
                    [ validUserManagementCard
                    , validApiTokensCard
                    ]
                validView =
                    mkView "preview" validCards
                assertInvalid view =
                    case validateFutureAdminConsoleView view of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ("Expected invalid admin console view, got: " <> show value)

            case validateFutureAdminConsoleView validView of
                Right view -> do
                    Future.viewArea view `shouldBe` "admin"
                    Future.viewEndpoint view `shouldBe` "console"
                    Future.viewId view `shouldBe` "admin.console"
                    Future.viewPath view `shouldBe` "/stubs/admin/console"
                    Future.viewMethod view `shouldBe` "GET"
                    Future.viewStatus view `shouldBe` "preview"
                    Future.viewRequiredRole view `shouldBe` "Admin"
                    Future.viewRequiredRoles view `shouldBe` futureStubRequiredRoles
                    Future.viewRequiredModule view `shouldBe` "Admin"
                    Future.viewImplemented view `shouldBe` False
                    map Future.cardId (Future.cards view)
                        `shouldBe` ["user-management", "api-tokens"]
                Left serverErr ->
                    expectationFailure
                        ("Expected valid admin console view, got: " <> show serverErr)

            assertInvalid (mkView "planned" [validUserManagementCard])
            assertInvalid (validView { Future.viewId = "admin.seed" })
            assertInvalid
                (mkViewWithRoute
                    "crm"
                    "console"
                    "/stubs/admin/console"
                    "GET"
                    "preview"
                    "Admin"
                    "Admin"
                    False
                    validCards)
            assertInvalid
                (mkViewWithRoute
                    "admin"
                    "seed"
                    "/stubs/admin/console"
                    "GET"
                    "preview"
                    "Admin"
                    "Admin"
                    False
                    validCards)
            assertInvalid
                (mkViewWithRoute
                    "admin"
                    "console"
                    "/stubs/admin/seed"
                    "GET"
                    "preview"
                    "Admin"
                    "Admin"
                    False
                    validCards)
            assertInvalid
                (mkViewWithRoute
                    "admin"
                    "console"
                    "/stubs/admin/console"
                    "POST"
                    "preview"
                    "Admin"
                    "Admin"
                    False
                    validCards)
            assertInvalid (mkViewWith "preview" "Manager" "Admin" False validCards)
            assertInvalid (validView { Future.viewRequiredRoles = ["Admin"] })
            assertInvalid (mkViewWith "preview" "Admin" "CRM" False validCards)
            assertInvalid (mkViewWith "preview" "Admin" "Admin" True validCards)
            assertInvalid (mkView "preview" [])
            assertInvalid (mkView "preview" [validUserManagementCard])
            assertInvalid
                (mkView
                    "preview"
                    [ validUserManagementCard
                    , mkCard "user-management" "Tokens API" apiTokensBody
                    ])
            assertInvalid
                (mkView
                    "preview"
                    [ validApiTokensCard
                    , validUserManagementCard
                    ])
            assertInvalid
                (mkView
                    "preview"
                    [mkCard "User Management" "Gestión de usuarios" userManagementBody])
            assertInvalid
                (mkView
                    "preview"
                    [ validUserManagementCard
                    , mkCard "api-tokens" "gestión de usuarios" apiTokensBody
                    ])
            assertInvalid
                (mkView
                    "preview"
                    [ validUserManagementCard
                    , mkCard
                        "api-tokens"
                        "Tokens API"
                        [ "La asignación de roles se administra desde la pantalla de Parties."
                        , "El acceso quedará separado de usuarios humanos para integraciones internas."
                        ]
                    ])

        it "rejects admin console fallback discovery when the canonical stub catalog drifts" $
            case firstFutureAdminConsole futureAdminUser of
                Left serverErr ->
                    expectationFailure
                        ("Expected canonical admin console preview, got: " <> show serverErr)
                Right consoleView -> do
                    case validateFutureAdminConsoleViewWithCatalog
                            allowedFutureStubMetadata
                            consoleView of
                        Right validated ->
                            Future.viewId validated `shouldBe` "admin.console"
                        Left serverErr ->
                            expectationFailure
                                ( "Expected canonical admin console catalog dependency, got: "
                                    <> show serverErr
                                )

                    case validateFutureAdminConsoleViewWithCatalog
                            [("crm", "parties/list-columns")]
                            consoleView of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future stub catalog"
                            BL8.unpack (errBody serverErr)
                                `shouldNotContain` "Invalid future admin console metadata"
                        Right value ->
                            expectationFailure
                                ( "Expected drifted fallback discovery catalog to fail, got: "
                                    <> show value
                                )

    describe "validateFutureStubCatalogResponseWithConsole" $
        it "rejects a drifted mounted admin console before serving the discovery catalog" $
            case firstFutureAdminConsole futureAdminUser of
                Left serverErr ->
                    expectationFailure
                        ("Expected canonical admin console preview, got: " <> show serverErr)
                Right consoleView -> do
                    case validateFutureStubCatalogResponseWithConsole consoleView of
                        Right responses ->
                            map (\response -> (stubArea response, stubEndpoint response)) responses
                                `shouldBe` allowedFutureStubMetadata
                        Left serverErr ->
                            expectationFailure
                                ( "Expected canonical discovery surface, got: "
                                    <> show serverErr
                                )

                    case validateFutureStubCatalogResponseWithConsole
                            consoleView { Future.viewStatus = "planned" } of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 500
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "Invalid future admin console metadata"
                        Right responses ->
                            expectationFailure
                                ( "Expected drifted admin console to block catalog serving, got: "
                                    <> show responses
                                )

    describe "futureServer" $ do
        it "serves a validated canonical fallback discovery catalog" $ do
            case futureCatalog (mkUser [StudioManager]) of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 403
                    BL8.unpack (errBody serverErr) `shouldContain` "Admin role required"
                Right value ->
                    expectationFailure
                        ("Expected fallback discovery catalog access to be rejected, got: " <> show value)

            case futureCatalog futureAdminUser of
                Right catalog -> do
                    map (\response -> (stubArea response, stubEndpoint response)) catalog
                        `shouldBe` allowedFutureStubMetadata
                    map stubId catalog
                        `shouldBe` map (uncurry futureStubId) allowedFutureStubMetadata
                    map stubPath catalog
                        `shouldBe` map
                            (\(area, endpoint) -> "/stubs/" <> area <> "/" <> endpoint)
                            allowedFutureStubMetadata
                    catalog `shouldSatisfy` all ((== "GET") . stubMethod)
                    catalog `shouldSatisfy` all ((== "planned") . stubStatus)
                    catalog `shouldSatisfy` all ((== roleToText Admin) . stubRequiredRole)
                    catalog `shouldSatisfy` all ((== futureStubRequiredRoles) . stubRequiredRoles)
                    catalog `shouldSatisfy` all ((== moduleName ModuleAdmin) . stubRequiredModule)
                    catalog `shouldSatisfy` all (not . stubImplemented)
                Left serverErr ->
                    expectationFailure
                        ("Expected Admin fallback discovery catalog, got: " <> show serverErr)

        it "keeps every mounted fallback discovery stub aligned with the canonical catalog" $ do
            let assertRejected response =
                    case response of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 403
                            BL8.unpack (errBody serverErr) `shouldContain` "Admin role required"
                        Right value ->
                            expectationFailure
                                ( "Expected mounted fallback discovery stub access to be "
                                    <> "rejected, got: "
                                    <> show value
                                )
            mapM_ assertRejected (allFutureStubs (mkUser [StudioManager]))

            case sequence (allFutureStubs futureAdminUser) of
                Right routeResponses -> do
                    map (\response -> (stubArea response, stubEndpoint response)) routeResponses
                        `shouldBe` allowedFutureStubMetadata
                    map stubId routeResponses
                        `shouldBe` map (uncurry futureStubId) allowedFutureStubMetadata
                    map stubPath routeResponses
                        `shouldBe` map
                            (\(area, endpoint) -> "/stubs/" <> area <> "/" <> endpoint)
                            allowedFutureStubMetadata
                    routeResponses `shouldSatisfy` all ((== "GET") . stubMethod)
                    routeResponses `shouldSatisfy` all ((== "planned") . stubStatus)
                    routeResponses `shouldSatisfy` all ((== roleToText Admin) . stubRequiredRole)
                    routeResponses
                        `shouldSatisfy` all ((== futureStubRequiredRoles) . stubRequiredRoles)
                    routeResponses `shouldSatisfy` all ((== moduleName ModuleAdmin) . stubRequiredModule)
                    routeResponses `shouldSatisfy` all (not . stubImplemented)
                Left serverErr ->
                    expectationFailure
                        ("Expected every mounted fallback discovery stub to validate, got: "
                            <> show serverErr)

        it "requires literal Admin before serving fallback discovery stubs" $ do
            case firstFutureStub (mkUser [StudioManager]) of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 403
                    BL8.unpack (errBody serverErr) `shouldContain` "Admin role required"
                Right value ->
                    expectationFailure
                        ("Expected fallback discovery access to be rejected, got: " <> show value)

            case firstFutureStub futureAdminUser of
                Right stubResponse -> do
                    stubArea stubResponse `shouldBe` "access"
                    stubEndpoint stubResponse `shouldBe` "login-options"
                    stubId stubResponse `shouldBe` "access.login-options"
                    stubMethod stubResponse `shouldBe` "GET"
                    stubStatus stubResponse `shouldBe` "planned"
                    stubRequiredRole stubResponse `shouldBe` roleToText Admin
                    stubRequiredRoles stubResponse `shouldBe` futureStubRequiredRoles
                    stubRequiredModule stubResponse `shouldBe` moduleName ModuleAdmin
                Left serverErr ->
                    expectationFailure
                        ("Expected Admin fallback discovery access, got: " <> show serverErr)

        it "serves admin console preview cards only after metadata validation" $
            case firstFutureAdminConsole futureAdminUser of
                Right consoleView -> do
                    Future.viewArea consoleView `shouldBe` "admin"
                    Future.viewEndpoint consoleView `shouldBe` "console"
                    Future.viewId consoleView `shouldBe` "admin.console"
                    Future.viewPath consoleView `shouldBe` "/stubs/admin/console"
                    Future.viewMethod consoleView `shouldBe` "GET"
                    Future.viewStatus consoleView `shouldBe` "preview"
                    Future.viewRequiredRole consoleView `shouldBe` "Admin"
                    Future.viewRequiredRoles consoleView `shouldBe` futureStubRequiredRoles
                    Future.viewRequiredModule consoleView `shouldBe` "Admin"
                    Future.viewImplemented consoleView `shouldBe` False
                    map Future.cardId (Future.cards consoleView)
                        `shouldBe` ["user-management", "api-tokens"]
                    Future.cards consoleView `shouldSatisfy` (not . null)
                    A.toJSON consoleView
                        `shouldBe` A.object
                            [ "stubArea" .= ("admin" :: Text)
                            , "stubEndpoint" .= ("console" :: Text)
                            , "stubId" .= ("admin.console" :: Text)
                            , "stubPath" .= ("/stubs/admin/console" :: Text)
                            , "stubMethod" .= ("GET" :: Text)
                            , "stubStatus" .= ("preview" :: Text)
                            , "stubRequiredRole" .= ("Admin" :: Text)
                            , "stubRequiredRoles" .= futureStubRequiredRoles
                            , "stubRequiredModule" .= ("Admin" :: Text)
                            , "stubImplemented" .= False
                            , "cards" .=
                                [ A.object
                                    [ "cardId" .= ("user-management" :: Text)
                                    , "title" .= ("Gestión de usuarios" :: Text)
                                    , "body" .=
                                        ( [ "La asignación de roles se administra desde la pantalla de Parties."
                                          , "Próximamente aquí se podrá crear usuarios de servicio y tokens API."
                                          ] :: [Text]
                                        )
                                    , "implemented" .= False
                                    ]
                                , A.object
                                    [ "cardId" .= ("api-tokens" :: Text)
                                    , "title" .= ("Tokens API" :: Text)
                                    , "body" .=
                                        ( [ "Los tokens de servicio deben administrarse desde un flujo dedicado."
                                          , "El acceso quedará separado de usuarios humanos para integraciones internas."
                                          ] :: [Text]
                                        )
                                    , "implemented" .= False
                                    ]
                                ]
                            ]
                Left serverErr ->
                    expectationFailure
                        ("Expected Admin fallback console access, got: " <> show serverErr)

        it "marks fallback discovery stubs as non-implemented placeholders" $
            case firstFutureStub futureAdminUser of
                Right stubResponse -> do
                    stubImplemented stubResponse `shouldBe` False
                    A.toJSON stubResponse
                        `shouldBe` A.object
                            [ "stubArea" .= ("access" :: Text)
                            , "stubEndpoint" .= ("login-options" :: Text)
                            , "stubId" .= ("access.login-options" :: Text)
                            , "stubPath" .= ("/stubs/access/login-options" :: Text)
                            , "stubMethod" .= ("GET" :: Text)
                            , "stubStatus" .= ("planned" :: Text)
                            , "stubRequiredRole" .= ("Admin" :: Text)
                            , "stubRequiredRoles" .= futureStubRequiredRoles
                            , "stubRequiredModule" .= ("Admin" :: Text)
                            , "stubImplemented" .= False
                            ]
                Left serverErr ->
                    expectationFailure
                        ("Expected Admin fallback discovery access, got: " <> show serverErr)

    describe "hasSocialInboxAccess" $ do
        it "denies baseline and read-only CRM sessions" $ do
            hasSocialInboxAccess (mkUser [Fan, Customer]) `shouldBe` False
            hasSocialInboxAccess (mkUser [ReadOnly]) `shouldBe` False

        it "honors persisted CRM grants while rejecting revoked or duplicated access" $ do
            let independentlyGrantedManager =
                    (mkUser [Manager]) { auModules = modulesForRoles [Webmaster] }
                revokedManager = (mkUser [Manager]) { auModules = Set.empty }
                duplicatedManager =
                    mkUser [Manager, Manager]
            hasSocialInboxAccess independentlyGrantedManager `shouldBe` True
            hasSocialInboxAccess revokedManager `shouldBe` False
            hasSocialInboxAccess duplicatedManager `shouldBe` False

        it "matches the intended single-role inbox matrix" $
            forM_ [minBound .. maxBound] $ \role ->
                hasSocialInboxAccess (mkUser [role]) `shouldBe` (role `elem` [Admin, Manager, StudioManager, Reception, LiveSessionsProducer, Producer, AandR, Webmaster])

    describe "social sync URL validation" $ do
        it "keeps social sync external post identities to visible ASCII before upsert matching" $ do
            SocialSync.validateSocialSyncExternalPostId " ig-post_42 "
                `shouldBe` Right "ig-post_42"

            let assertInvalid rawId =
                    case SocialSync.validateSocialSyncExternalPostId rawId of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "externalPostId must contain visible ASCII"
                        Right value ->
                            expectationFailure
                                ( "Expected non-ASCII social sync externalPostId to be rejected, got: "
                                    <> show value
                                )

            assertInvalid ("ig-post-" <> T.singleton '\x00E9')
            assertInvalid ("ig-post-" <> T.singleton '\x0661')

        it "requires HTTPS permalinks and media URLs before persisting synced posts" $ do
            SocialSync.validateSocialSyncPermalink
                (Just " https://www.instagram.com/p/post42/ ")
                `shouldBe` Right (Just "https://www.instagram.com/p/post42/")
            SocialSync.validateSocialSyncMediaUrls
                (Just [" https://cdn.example.com/post.jpg?sig=1 "])
                `shouldBe` Right (Just "https://cdn.example.com/post.jpg?sig=1")

            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected unsafe social sync URL to be rejected, got: "
                                    <> show value
                                )
            assertInvalid
                "permalink must be an absolute public https URL"
                ( SocialSync.validateSocialSyncPermalink
                    (Just "http://www.instagram.com/p/post42/")
                )
            assertInvalid
                "mediaUrls entries must be absolute public https URLs"
                ( SocialSync.validateSocialSyncMediaUrls
                    (Just ["http://cdn.example.com/post.jpg"])
                )

        it "rejects URL fragments before storing ambiguous social sync links" $ do
            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected fragmented social sync URL to be rejected, got: "
                                    <> show value
                                )

            assertInvalid
                "permalink must not contain URL fragments"
                ( SocialSync.validateSocialSyncPermalink
                    (Just "https://www.instagram.com/p/post42/#comments")
                )
            assertInvalid
                "mediaUrls entries must not contain URL fragments"
                ( SocialSync.validateSocialSyncMediaUrls
                    (Just ["https://cdn.example.com/post.jpg#preview"])
                )

        it "rejects ambiguous social sync URL path segments before persistence" $ do
            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected ambiguous social sync URL path to be rejected, got: "
                                    <> show value
                                )

            assertInvalid
                "permalink path must not contain empty, dot, or dot-dot segments"
                ( SocialSync.validateSocialSyncPermalink
                    (Just "https://www.instagram.com/p/%2e%2e/post42")
                )
            assertInvalid
                "mediaUrls entries path must not contain empty, dot, or dot-dot segments"
                ( SocialSync.validateSocialSyncMediaUrls
                    (Just ["https://cdn.example.com/posts/../post.jpg"])
                )
            assertInvalid
                "mediaUrls entries path must not contain empty, dot, or dot-dot segments"
                ( SocialSync.validateSocialSyncMediaUrls
                    (Just ["https://cdn.example.com/posts//post.jpg"])
                )

        it "keeps social sync permalinks tied to the declared platform domain" $ do
            SocialSync.validateSocialSyncPermalinkForPlatform
                "instagram"
                (Just " https://www.instagram.com/p/post42/ ")
                `shouldBe` Right (Just "https://www.instagram.com/p/post42/")
            SocialSync.validateSocialSyncPermalinkForPlatform
                "instagram"
                (Just "https://www.instagram.com:443/p/post42/")
                `shouldBe` Right (Just "https://www.instagram.com:443/p/post42/")
            SocialSync.validateSocialSyncPermalinkForPlatform
                "facebook"
                (Just "https://fb.watch/post42/")
                `shouldBe` Right (Just "https://fb.watch/post42/")

            let assertInvalid platform rawUrl =
                    case SocialSync.validateSocialSyncPermalinkForPlatform platform (Just rawUrl) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "permalink must match the declared platform domain"
                        Right value ->
                            expectationFailure
                                ( "Expected cross-platform social sync permalink to be rejected, got: "
                                    <> show value
                                )
            assertInvalid "instagram" "https://www.facebook.com/tdf/posts/42"
            assertInvalid "facebook" "https://www.instagram.com/p/post42/"
            assertInvalid "instagram" "https://instagram.com.evil.example/p/post42/"
            assertInvalid "instagram" "https://www.instagram.com:444/p/post42/"

        it "rejects root social sync permalinks before storing ambiguous fallback links" $ do
            let assertInvalid platform rawUrl =
                    case SocialSync.validateSocialSyncPermalinkForPlatform platform (Just rawUrl) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL8.unpack (errBody serverErr)
                                `shouldContain` "permalink must include a post path"
                        Right value ->
                            expectationFailure
                                ( "Expected root social sync permalink to be rejected, got: "
                                    <> show value
                                )
            assertInvalid "instagram" "https://www.instagram.com/"
            assertInvalid "facebook" "https://facebook.com?story_fbid=42"

    describe "hasSocialSyncAccess" $ do
        it "denies baseline and non-admin staff sessions" $ do
            hasSocialSyncAccess (mkUser [Fan, Customer]) `shouldBe` False
            hasSocialSyncAccess (mkUser [Webmaster]) `shouldBe` False
            hasSocialSyncAccess (mkUser [StudioManager]) `shouldBe` False

        it "honors persisted Admin modules while rejecting revoked or duplicated sync access" $ do
            let independentlyGrantedAdmin =
                    (mkUser [Admin]) { auModules = modulesForRoles [Webmaster] }
                revokedAdmin = (mkUser [Admin]) { auModules = Set.empty }
                duplicatedAdmin =
                    mkUser [Admin, Admin]
            hasSocialSyncAccess independentlyGrantedAdmin `shouldBe` True
            hasSocialSyncAccess revokedAdmin `shouldBe` False
            hasSocialSyncAccess duplicatedAdmin `shouldBe` False
            hasSocialSyncAccess (mkUser [Fan, Customer, Admin]) `shouldBe` True

        it "matches the strict-admin matrix for global sync data" $
            forM_ [minBound .. maxBound] $ \role ->
                hasSocialSyncAccess (mkUser [role]) `shouldBe` hasStrictAdminAccess (mkUser [role])

runServiceAdSqlite :: SqlPersistT IO a -> IO a
runServiceAdSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeServiceAdSchema backend
        liftIO $ runReaderT action backend

marketplaceTestConfig :: Bool -> AppConfig
marketplaceTestConfig seedFlag =
    AppConfig
        { dbHost = "127.0.0.1"
        , dbPort = "5432"
        , dbUser = "postgres"
        , dbPass = "postgres"
        , dbName = "tdf_hq_test"
        , dbConnUrl = Nothing
        , dbSslMode = Nothing
        , appPort = 8080
        , resetDb = False
        , seedDatabase = seedFlag
        , runMigrations = False
        , seedTriggerToken = Nothing
        , appBaseUrl = Nothing
        , assetsBaseUrl = Nothing
        , assetsRootDir = "assets"
        , courseDefaultSlug = "produccion-musical"
        , courseDefaultMapUrl = Nothing
        , courseDefaultInstructorAvatar = Nothing
        , openAiApiKey = Nothing
        , openAiModel = "gpt-5-chat-latest"
        , openAiEmbedModel = "text-embedding-3-small"
        , chatKitWorkflowId = Nothing
        , chatKitApiBase = llmProviderApiBase llmProvider
        , ragTopK = 8
        , ragChunkWords = 220
        , ragChunkOverlap = 40
        , ragAvailabilityDays = 14
        , ragAvailabilityPerResource = 6
        , ragRefreshHours = 24
        , ragEmbedBatchSize = 64
        , emailConfig = Nothing
        , googleClientId = Nothing
        , facebookAppId = Nothing
        , facebookAppSecret = Nothing
        , facebookGraphBase = "https://graph.facebook.com/v20.0"
        , facebookMessagingToken = Nothing
        , facebookMessagingPageId = Nothing
        , facebookMessagingApiBase = "https://graph.facebook.com/v20.0"
        , instagramAppToken = Nothing
        , instagramGraphBase = "https://graph.instagram.com"
        , instagramMessagingToken = Nothing
        , instagramMessagingAccountId = Nothing
        , instagramMessagingApiBase = "https://graph.facebook.com/v20.0"
        , instagramVerifyToken = Nothing
        , sessionCookieName = "tdf_session"
        , sessionCookieDomain = Nothing
        , sessionCookiePath = "/"
        , sessionCookieSecure = False
        , sessionCookieSameSite = "Lax"
        , sessionCookieMaxAgeSeconds = Nothing
        , stripeSecretKey = Nothing
        , stripePublishableKey = Nothing
        , stripeWebhookSecret = Nothing
        , eventDiscoveryEnabled = False
        , eventDiscoveryAutoPublish = False
        , eventDiscoveryPilotLimit = 20
        , ticketmasterApiKey = Nothing
        , ticketmasterApiBase = "https://app.ticketmaster.com/discovery/v2"
        , eventDiscoveryLookaheadDays = 90
        , eventDiscoveryMaxPagesPerCity = 5
        , eventDiscoveryHourLocal = 3
        , eventDiscoveryCountryCode = Nothing
        , googleRoutesApiKey = Nothing
        , googleRoutesApiBase = "https://routes.googleapis.com"
        , eventLogisticsRecheckEnabled = False
        , artistEnrichmentEnabled = False
        , artistEnrichmentAutoPublish = False
        , artistEnrichmentHourLocal = 4
        , artistEnrichmentBatchSize = 500
        , artistEnrichmentStaleDays = 90
        , defaultCurrency = "USD"
        , supportedCurrencies = ["USD", "EUR", "GBP", "CAD", "AUD", "JPY", "BRL"]
        , defaultTimezone = "UTC"
        , supportedLocales = ["en", "es", "fr", "de", "pt"]
        , defaultLocale = "en"
        , enableGdprCompliance = True
        }

initializeMarketplaceListingSchema :: SqlPersistT IO ()
initializeMarketplaceListingSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"marketplace_listing\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"asset_id\" INTEGER NOT NULL,\
        \\"title\" VARCHAR NOT NULL,\
        \\"purpose\" VARCHAR NOT NULL,\
        \\"price_usd_cents\" INTEGER NOT NULL,\
        \\"markup_pct\" INTEGER NOT NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []

initializeSessionInvoiceLookupSchema :: SqlPersistT IO ()
initializeSessionInvoiceLookupSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"session\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"booking_ref\" VARCHAR NULL,\
        \\"band_id\" uuid NULL,\
        \\"client_party_ref\" VARCHAR NULL,\
        \\"service\" VARCHAR NOT NULL,\
        \\"start_at\" TIMESTAMP NOT NULL,\
        \\"end_at\" TIMESTAMP NOT NULL,\
        \\"engineer_ref\" VARCHAR NOT NULL,\
        \\"assistant_ref\" VARCHAR NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"sample_rate\" INTEGER NULL,\
        \\"bit_depth\" INTEGER NULL,\
        \\"daw\" VARCHAR NULL,\
        \\"session_folder_drive_id\" VARCHAR NULL,\
        \\"notes\" VARCHAR NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"session_invoice\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"session_id\" uuid NOT NULL,\
        \\"invoice_id\" INTEGER NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_session_invoice\" UNIQUE (\"session_id\", \"invoice_id\")\
        \)"
        []

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
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_ad_slot\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"ad_id\" INTEGER NOT NULL REFERENCES \"service_ad\"(\"id\") ON DELETE RESTRICT ON UPDATE RESTRICT,\
        \\"starts_at\" TIMESTAMP NOT NULL,\
        \\"ends_at\" TIMESTAMP NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \UNIQUE(\"ad_id\", \"starts_at\", \"ends_at\")\
        \)"
        []

runAuthSqlite :: SqlPersistT IO a -> IO a
runAuthSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeAuthSchema backend
        liftIO $ runReaderT action backend

seedSessionUsernameFallbackRows :: SqlPersistT IO (Key Party, Key Party)
seedSessionUsernameFallbackRows = do
    now <- liftIO getCurrentTime
    let insertParty displayName emailAddress =
            insert
                Party
                    { partyLegalName = Nothing
                    , partyDisplayName = displayName
                    , partyIsOrg = False
                    , partyTaxId = Nothing
                    , partyPrimaryEmail = Just emailAddress
                    , partyPrimaryPhone = Nothing
                    , partyWhatsapp = Nothing
                    , partyInstagram = Nothing
                    , partyEmergencyContact = Nothing
                    , partyNotes = Nothing
                    , partyStripeCustomerId = Nothing
                    , partyCountryCode = Nothing
                    , partyCountryId = Nothing
                    , partyCreatedAt = now
                    }
        insertCredential partyId username =
            insert_
                UserCredential
                    { userCredentialPartyId = partyId
                    , userCredentialUsername = username
                    , userCredentialPasswordHash = "hash"
                    , userCredentialActive = True
                    }
        insertToken partyId tokenValue labelValue =
            insert_
                ApiToken
                    { apiTokenToken = tokenValue
                    , apiTokenPartyId = partyId
                    , apiTokenLabel = labelValue
                    , apiTokenActive = True
                    }
    ambiguousPartyId <- insertParty "Ambiguous Session User" "ambiguous-session@example.com"
    insertCredential ambiguousPartyId "first-session@example.com"
    insertCredential ambiguousPartyId "second-session@example.com"
    insertToken ambiguousPartyId "ambiguous-token" Nothing

    googlePartyId <- insertParty "Google Session User" "google@example.com"
    insertCredential googlePartyId "first-google-session@example.com"
    insertCredential googlePartyId "second-google-session@example.com"
    insertToken googlePartyId "google-token" (Just "google-login:google@example.com")
    pure (ambiguousPartyId, googlePartyId)

runPackageSqlite :: SqlPersistT IO a -> IO a
runPackageSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeAuthSchema backend
        liftIO $ runReaderT initializePackageSchema backend
        liftIO $ runReaderT action backend

runResourceSqlite :: SqlPersistT IO a -> IO a
runResourceSqlite action =
    runSqlite ":memory:" $ do
        backend <- ask
        liftIO $ runReaderT initializeResourceSchema backend
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
        \\"stripe_customer_id\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []

    rawExecute
        "CREATE TABLE IF NOT EXISTS \"user_locale_preferences\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"user_id\" INTEGER NOT NULL UNIQUE,\
        \\"locale\" VARCHAR NOT NULL,\
        \\"currency\" VARCHAR NOT NULL,\
        \\"timezone\" VARCHAR NOT NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"locale_id\" VARCHAR NULL,\
        \\"currency_id\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \FOREIGN KEY(\"user_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []

    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_role\" (\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL UNIQUE, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL, \"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"sort_order\" INTEGER NOT NULL, \"system_role\" BOOLEAN NOT NULL, \"emergency_administrator\" BOOLEAN NOT NULL, \"self_assignable\" BOOLEAN NOT NULL, \"automatic_assignable\" BOOLEAN NOT NULL, \"active\" BOOLEAN NOT NULL, \"workflow_state_id\" VARCHAR NOT NULL, \"created_by\" INTEGER NULL, \"updated_by\" INTEGER NULL, \"approved_by\" INTEGER NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"published_revision\" INTEGER NOT NULL, \"version\" INTEGER NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_module\" (\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL UNIQUE, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL, \"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"sort_order\" INTEGER NOT NULL, \"active\" BOOLEAN NOT NULL, \"internal_only\" BOOLEAN NOT NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_action\" (\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL UNIQUE, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL, \"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"sensitive\" BOOLEAN NOT NULL, \"grantable\" BOOLEAN NOT NULL, \"active\" BOOLEAN NOT NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_permission\" (\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL UNIQUE, \"module_id\" VARCHAR NOT NULL, \"action_id\" VARCHAR NOT NULL, \"resource_scope\" VARCHAR NOT NULL, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL, \"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"sensitive\" BOOLEAN NOT NULL, \"public_metadata\" BOOLEAN NOT NULL, \"active\" BOOLEAN NOT NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"role_permission\" (\"id\" VARCHAR PRIMARY KEY, \"role_id\" VARCHAR NOT NULL, \"permission_id\" VARCHAR NOT NULL, \"granted_by\" INTEGER NULL, \"approved_by\" INTEGER NULL, \"active\" BOOLEAN NOT NULL, \"created_at\" TIMESTAMP NOT NULL, \"revoked_at\" TIMESTAMP NULL, \"version\" INTEGER NOT NULL, UNIQUE(\"role_id\",\"permission_id\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_role_assignment_policy\" (\"id\" VARCHAR PRIMARY KEY, \"code\" VARCHAR NOT NULL UNIQUE, \"trigger_code\" VARCHAR NOT NULL, \"role_id\" VARCHAR NOT NULL, \"name_es\" VARCHAR NOT NULL, \"name_en\" VARCHAR NOT NULL, \"description_es\" VARCHAR NULL, \"description_en\" VARCHAR NULL, \"requires_verified_email\" BOOLEAN NOT NULL, \"active\" BOOLEAN NOT NULL, \"effective_from\" TIMESTAMP NULL, \"effective_to\" TIMESTAMP NULL, \"created_by\" INTEGER NULL, \"updated_by\" INTEGER NULL, \"approved_by\" INTEGER NULL, \"created_at\" TIMESTAMP NOT NULL, \"updated_at\" TIMESTAMP NOT NULL, \"version\" INTEGER NOT NULL, UNIQUE(\"trigger_code\",\"role_id\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"party_security_role\" (\"id\" VARCHAR PRIMARY KEY DEFAULT (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-8' || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6)))), \"party_id\" INTEGER NOT NULL, \"role_id\" VARCHAR NOT NULL, \"granted_by\" INTEGER NULL, \"approved_by\" INTEGER NULL, \"approval_mode\" VARCHAR NOT NULL, \"emergency_reason\" VARCHAR NULL, \"source_revision_id\" VARCHAR NULL, \"source_policy_id\" VARCHAR NULL, \"active\" BOOLEAN NOT NULL, \"created_at\" TIMESTAMP NOT NULL, \"revoked_at\" TIMESTAMP NULL, \"version\" INTEGER NOT NULL, UNIQUE(\"party_id\",\"role_id\"))"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"security_audit_event\" (\"id\" VARCHAR PRIMARY KEY DEFAULT (lower(hex(randomblob(4))) || '-' || lower(hex(randomblob(2))) || '-4' || substr(lower(hex(randomblob(2))),2) || '-8' || substr(lower(hex(randomblob(2))),2) || '-' || lower(hex(randomblob(6)))), \"revision_id\" VARCHAR NULL, \"source_policy_id\" VARCHAR NULL, \"entity_kind\" VARCHAR NOT NULL, \"party_id\" INTEGER NULL, \"role_id\" VARCHAR NOT NULL, \"permission_id\" VARCHAR NULL, \"operation\" VARCHAR NOT NULL, \"previous_active\" BOOLEAN NULL, \"new_active\" BOOLEAN NULL, \"actor_id\" INTEGER NULL, \"reviewer_id\" INTEGER NULL, \"approver_id\" INTEGER NULL, \"occurred_at\" TIMESTAMP NOT NULL, \"source_platform\" VARCHAR NOT NULL, \"reason\" VARCHAR NULL, \"correlation_id\" VARCHAR NOT NULL, \"approval_mode\" VARCHAR NOT NULL, \"result\" VARCHAR NOT NULL)"
        []
    rawExecute
        "INSERT INTO security_role (id,code,name_es,name_en,sort_order,system_role,emergency_administrator,self_assignable,automatic_assignable,active,workflow_state_id,created_at,updated_at,published_revision,version) VALUES ('00000000-0000-4000-8000-000000000001','fan','Fan','Fan',1,1,0,0,0,1,'00000000-0000-4000-8000-000000000099',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1,1), ('00000000-0000-4000-8000-000000000002','customer','Cliente','Customer',2,1,0,0,1,1,'00000000-0000-4000-8000-000000000099',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1,1), ('00000000-0000-4000-8000-000000000007','student','Estudiante','Student',3,1,0,0,1,1,'00000000-0000-4000-8000-000000000099',CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1,1)"
        []
    rawExecute
        "INSERT INTO security_role_assignment_policy (id,code,trigger_code,role_id,name_es,name_en,requires_verified_email,active,created_at,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000305','course.registration.student','course-registration','00000000-0000-4000-8000-000000000007','Registro de curso','Course registration',0,1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "INSERT INTO security_module (id,code,name_es,name_en,sort_order,active,internal_only,created_at,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000003','packages','Paquetes','Packages',1,1,1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "INSERT INTO security_action (id,code,name_es,name_en,sensitive,grantable,active,created_at,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000004','access','Acceder','Access',0,1,1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "INSERT INTO security_permission (id,code,module_id,action_id,resource_scope,name_es,name_en,sensitive,public_metadata,active,created_at,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000005','packages.access','00000000-0000-4000-8000-000000000003','00000000-0000-4000-8000-000000000004','module','Acceso a paquetes','Packages access',0,0,1,CURRENT_TIMESTAMP,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "INSERT INTO role_permission (id,role_id,permission_id,active,created_at,version) VALUES ('00000000-0000-4000-8000-000000000006','00000000-0000-4000-8000-000000000002','00000000-0000-4000-8000-000000000005',1,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"slug\" VARCHAR NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"city\" VARCHAR NULL,\
        \\"country_code\" VARCHAR NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"hero_image_url\" VARCHAR NULL,\
        \\"spotify_artist_id\" VARCHAR NULL,\
        \\"spotify_url\" VARCHAR NULL,\
        \\"youtube_channel_id\" VARCHAR NULL,\
        \\"youtube_url\" VARCHAR NULL,\
        \\"website_url\" VARCHAR NULL,\
        \\"featured_video_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"highlights\" VARCHAR NULL,\
        \\"stripe_account_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NULL,\
        \CONSTRAINT \"unique_artist_profile\" UNIQUE (\"artist_party_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\")\
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
        "CREATE TABLE IF NOT EXISTS \"course_registration\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"course_slug\" VARCHAR NOT NULL,\
        \\"party_id\" INTEGER NULL,\
        \\"full_name\" VARCHAR NULL,\
        \\"email\" VARCHAR NULL,\
        \\"phone_e164\" VARCHAR NULL,\
        \\"source\" VARCHAR NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"admin_notes\" VARCHAR NULL,\
        \\"how_heard\" VARCHAR NULL,\
        \\"utm_source\" VARCHAR NULL,\
        \\"utm_medium\" VARCHAR NULL,\
        \\"utm_campaign\" VARCHAR NULL,\
        \\"utm_content\" VARCHAR NULL,\
        \\"stripe_payment_intent_id\" VARCHAR NULL,\
        \\"stripe_subscription_id\" VARCHAR NULL,\
        \\"subscription_status\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"pipeline_card\" (\
        \\"id\" uuid PRIMARY KEY,\
        \\"service_kind\" VARCHAR NOT NULL,\
        \\"service_offering_id\" VARCHAR NULL,\
        \\"title\" VARCHAR NOT NULL,\
        \\"artist\" VARCHAR NULL,\
        \\"stage\" VARCHAR NOT NULL,\
        \\"workflow_state_id\" VARCHAR NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL\
        \)"
        []

initializeLocalePreferenceReferenceSchema :: SqlPersistT IO ()
initializeLocalePreferenceReferenceSchema = do
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"locale_reference\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"code\" VARCHAR NOT NULL UNIQUE,\
        \\"language_id\" VARCHAR NOT NULL,\
        \\"country_id\" VARCHAR NULL,\
        \\"name_es\" VARCHAR NOT NULL,\
        \\"name_en\" VARCHAR NOT NULL,\
        \\"description_es\" VARCHAR NULL,\
        \\"description_en\" VARCHAR NULL,\
        \\"fallback_locale_id\" VARCHAR NULL,\
        \\"default_for_platform\" BOOLEAN NOT NULL,\
        \\"source_version\" VARCHAR NOT NULL,\
        \\"last_synced_at\" TIMESTAMP NOT NULL,\
        \\"deprecated_at\" TIMESTAMP NULL,\
        \\"replacement_id\" VARCHAR NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"version\" INTEGER NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"currency_reference\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"code\" VARCHAR NOT NULL UNIQUE,\
        \\"numeric_code\" VARCHAR NULL,\
        \\"name_es\" VARCHAR NOT NULL,\
        \\"name_en\" VARCHAR NOT NULL,\
        \\"description_es\" VARCHAR NULL,\
        \\"description_en\" VARCHAR NULL,\
        \\"symbol\" VARCHAR NOT NULL,\
        \\"minor_units\" INTEGER NOT NULL,\
        \\"standard\" VARCHAR NOT NULL,\
        \\"source_version\" VARCHAR NOT NULL,\
        \\"effective_from\" DATE NULL,\
        \\"effective_until\" DATE NULL,\
        \\"deprecated_at\" TIMESTAMP NULL,\
        \\"replacement_id\" VARCHAR NULL,\
        \\"last_synced_at\" TIMESTAMP NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"version\" INTEGER NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"deployment_locale_enablement\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"deployment_code\" VARCHAR NOT NULL,\
        \\"locale_id\" VARCHAR NOT NULL,\
        \\"enabled\" BOOLEAN NOT NULL,\
        \\"default_locale\" BOOLEAN NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \\"version\" INTEGER NOT NULL,\
        \UNIQUE(\"deployment_code\", \"locale_id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"deployment_currency_enablement\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"deployment_code\" VARCHAR NOT NULL,\
        \\"currency_id\" VARCHAR NOT NULL,\
        \\"enabled\" BOOLEAN NOT NULL,\
        \\"default_currency\" BOOLEAN NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \\"version\" INTEGER NOT NULL,\
        \UNIQUE(\"deployment_code\", \"currency_id\")\
        \)"
        []
    rawExecute
        "INSERT INTO locale_reference (id,code,language_id,name_es,name_en,default_for_platform,source_version,last_synced_at,active,sort_order,version) VALUES ('00000000-0000-4000-8000-000000000401','en','00000000-0000-4000-8000-000000000402','Inglés','English',1,'test',CURRENT_TIMESTAMP,1,0,1)"
        []
    rawExecute
        "INSERT INTO currency_reference (id,code,name_es,name_en,symbol,minor_units,standard,source_version,last_synced_at,active,sort_order,version) VALUES ('00000000-0000-4000-8000-000000000403','USD','Dólar estadounidense','US dollar','$',2,'ISO 4217','test',CURRENT_TIMESTAMP,1,0,1)"
        []
    rawExecute
        "INSERT INTO deployment_locale_enablement (id,deployment_code,locale_id,enabled,default_locale,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000404','default','00000000-0000-4000-8000-000000000401',1,1,CURRENT_TIMESTAMP,1)"
        []
    rawExecute
        "INSERT INTO deployment_currency_enablement (id,deployment_code,currency_id,enabled,default_currency,updated_at,version) VALUES ('00000000-0000-4000-8000-000000000405','default','00000000-0000-4000-8000-000000000403',1,1,CURRENT_TIMESTAMP,1)"
        []

initializeChatSchema :: SqlPersistT IO ()
initializeChatSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"chat_thread\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"dm_party_a\" INTEGER NOT NULL,\
        \\"dm_party_b\" INTEGER NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_chat_thread\" UNIQUE (\"dm_party_a\", \"dm_party_b\"),\
        \FOREIGN KEY(\"dm_party_a\") REFERENCES \"party\"(\"id\"),\
        \FOREIGN KEY(\"dm_party_b\") REFERENCES \"party\"(\"id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"chat_message\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"thread_id\" INTEGER NOT NULL,\
        \\"sender_party_id\" INTEGER NOT NULL,\
        \\"body\" VARCHAR NOT NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \FOREIGN KEY(\"thread_id\") REFERENCES \"chat_thread\"(\"id\"),\
        \FOREIGN KEY(\"sender_party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []

initializeResourceSchema :: SqlPersistT IO ()
initializeResourceSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"room\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"is_bookable\" BOOLEAN NOT NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"channel_count\" INTEGER NULL,\
        \\"default_sample_rate\" INTEGER NULL,\
        \\"patchbay_notes\" VARCHAR NULL,\
        \CONSTRAINT \"unique_room_name\" UNIQUE (\"name\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"resource\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"name\" VARCHAR NOT NULL,\
        \\"slug\" VARCHAR NOT NULL,\
        \\"resource_type\" VARCHAR NOT NULL,\
        \\"capacity\" INTEGER NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \CONSTRAINT \"unique_resource_slug\" UNIQUE (\"slug\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"booking\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"title\" VARCHAR NOT NULL,\
        \\"service_order_id\" INTEGER NULL,\
        \\"party_id\" INTEGER NULL,\
        \\"service_type\" VARCHAR NULL,\
        \\"service_offering_id\" VARCHAR NULL,\
        \\"booking_type_id\" VARCHAR NULL,\
        \\"workflow_state_id\" VARCHAR NULL,\
        \\"engineer_party_id\" INTEGER NULL,\
        \\"engineer_name\" VARCHAR NULL,\
        \\"starts_at\" TIMESTAMP NOT NULL,\
        \\"ends_at\" TIMESTAMP NOT NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"created_by\" INTEGER NULL,\
        \\"notes\" VARCHAR NULL,\
        \\"stripe_customer_id\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"booking_resource\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"booking_id\" INTEGER NOT NULL,\
        \\"resource_id\" INTEGER NOT NULL,\
        \\"role\" VARCHAR NOT NULL,\
        \CONSTRAINT \"unique_booking_res\" UNIQUE (\"booking_id\", \"resource_id\", \"role\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_resource_selection_mode\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"code\" VARCHAR NOT NULL,\
        \\"active\" BOOLEAN NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"service_offering_default_resource\" (\
        \\"id\" VARCHAR PRIMARY KEY,\
        \\"service_offering_id\" VARCHAR NOT NULL,\
        \\"resource_id\" INTEGER NOT NULL,\
        \\"selection_mode_id\" VARCHAR NULL,\
        \\"selection_mode\" VARCHAR NULL,\
        \\"sort_order\" INTEGER NOT NULL,\
        \\"active\" BOOLEAN NOT NULL,\
        \\"version\" INTEGER NOT NULL,\
        \CONSTRAINT \"unique_service_offering_default_resource\" UNIQUE (\"service_offering_id\", \"resource_id\")\
        \)"
        []

insertBookingResourceFixture :: T.Text -> T.Text -> SqlPersistT IO (Key Resource)
insertBookingResourceFixture name slug =
    insert Resource
        { resourceName = name
        , resourceSlug = slug
        , resourceResourceType = Room
        , resourceCapacity = Nothing
        , resourceActive = True
        }

bookingServiceOfferingFixture :: T.Text -> Bool -> Entity Catalog.ServiceOffering
bookingServiceOfferingFixture code requiresEngineerFlag =
    Entity fixtureOfferingKey Catalog.ServiceOffering
        { Catalog.serviceOfferingCatalogId = fixtureUuidKey "30000000-0000-4000-8000-000000000001"
        , Catalog.serviceOfferingCategoryId = fixtureUuidKey "30000000-0000-4000-8000-000000000002"
        , Catalog.serviceOfferingLegacyServiceCatalogId = Nothing
        , Catalog.serviceOfferingCode = code
        , Catalog.serviceOfferingNameEs = code
        , Catalog.serviceOfferingNameEn = code
        , Catalog.serviceOfferingDescriptionEs = Nothing
        , Catalog.serviceOfferingDescriptionEn = Nothing
        , Catalog.serviceOfferingCurrentSlug = Just code
        , Catalog.serviceOfferingPricingModelId = Just (fixtureUuidKey "30000000-0000-4000-8000-000000000006")
        , Catalog.serviceOfferingLegacyPricingModelCode = Nothing
        , Catalog.serviceOfferingDefaultRateCents = Nothing
        , Catalog.serviceOfferingTaxRateId = Nothing
        , Catalog.serviceOfferingLegacyTaxRateCode = Nothing
        , Catalog.serviceOfferingCurrencyId = fixtureUuidKey "30000000-0000-4000-8000-000000000003"
        , Catalog.serviceOfferingBillingUnitEs = Just "hora"
        , Catalog.serviceOfferingBillingUnitEn = Just "hour"
        , Catalog.serviceOfferingDefaultDurationMinutes = Just 60
        , Catalog.serviceOfferingRequiresEngineer = requiresEngineerFlag
        , Catalog.serviceOfferingSortOrder = 0
        , Catalog.serviceOfferingActive = True
        , Catalog.serviceOfferingWorkflowStateId = fixtureUuidKey "30000000-0000-4000-8000-000000000004"
        , Catalog.serviceOfferingCreatedBy = Nothing
        , Catalog.serviceOfferingUpdatedBy = Nothing
        , Catalog.serviceOfferingApprovedBy = Nothing
        , Catalog.serviceOfferingCreatedAt = fixtureBookingTime
        , Catalog.serviceOfferingUpdatedAt = fixtureBookingTime
        , Catalog.serviceOfferingEffectiveFrom = Nothing
        , Catalog.serviceOfferingEffectiveUntil = Nothing
        , Catalog.serviceOfferingDeprecatedAt = Nothing
        , Catalog.serviceOfferingReplacementId = Nothing
        , Catalog.serviceOfferingUsageCount = 0
        , Catalog.serviceOfferingVersion = 1
        }
  where
    fixtureOfferingKey = fixtureUuidKey "30000000-0000-4000-8000-000000000005"

insertBookingDefaultResourceFixture
    :: T.Text
    -> Entity Catalog.ServiceOffering
    -> Key Resource
    -> T.Text
    -> Int
    -> SqlPersistT IO ()
insertBookingDefaultResourceFixture relationshipId (Entity offeringKey _) resourceKey selectionMode sortOrder =
    let selectionModeKey = fixtureUuidKey $
            if selectionMode == "all"
                then "30000000-0000-4000-8000-000000000007"
                else "30000000-0000-4000-8000-000000000008"
    in do
        rawExecute
            "INSERT OR IGNORE INTO service_resource_selection_mode (id, code, active) VALUES (?, ?, TRUE)"
            [toPersistValue selectionModeKey, PersistText selectionMode]
        insertKey (fixtureUuidKey relationshipId) Catalog.ServiceOfferingDefaultResource
            { Catalog.serviceOfferingDefaultResourceServiceOfferingId = offeringKey
            , Catalog.serviceOfferingDefaultResourceResourceId = resourceKey
            , Catalog.serviceOfferingDefaultResourceSelectionModeId = Just selectionModeKey
            , Catalog.serviceOfferingDefaultResourceLegacySelectionModeCode = Nothing
            , Catalog.serviceOfferingDefaultResourceSortOrder = sortOrder
            , Catalog.serviceOfferingDefaultResourceActive = True
            , Catalog.serviceOfferingDefaultResourceVersion = 1
            }

fixtureBookingTime :: UTCTime
fixtureBookingTime = UTCTime (fromGregorian 2026 4 1) (secondsToDiffTime 0)

fixtureUuidKey :: PathPiece a => T.Text -> a
fixtureUuidKey raw =
    case fromPathPiece raw of
        Just keyVal -> keyVal
        Nothing -> error "Expected UUID fixture key to parse"

insertBookingResourceHoldFixture
    :: T.Text -> Key Resource -> UTCTime -> UTCTime -> SqlPersistT IO ()
insertBookingResourceHoldFixture bookingTitleVal resourceId startsAt endsAt = do
    bookingId <- insert Booking
        { bookingTitle = bookingTitleVal
        , bookingServiceOrderId = Nothing
        , bookingPartyId = Nothing
        , bookingServiceType = Nothing
        , bookingEngineerPartyId = Nothing
        , bookingEngineerName = Nothing
        , bookingStartsAt = startsAt
        , bookingEndsAt = endsAt
        , bookingStatus = Confirmed
        , bookingCreatedBy = Nothing
        , bookingNotes = Nothing
        , bookingServiceOfferingId = Nothing
        , bookingBookingTypeId = Nothing
        , bookingWorkflowStateId = Nothing
        , bookingCreatedAt = startsAt
        }
    _ <- insert BookingResource
        { bookingResourceBookingId = bookingId
        , bookingResourceResourceId = resourceId
        , bookingResourceRole = "primary"
        }
    pure ()

fixtureInstagramMessage
    :: Int -> UTCTime -> T.Text -> T.Text -> T.Text -> Entity M.InstagramMessage
fixtureInstagramMessage keyVal now externalId direction senderId =
    Entity (toSqlKey (fromIntegral keyVal)) M.InstagramMessage
        { M.instagramMessageExternalId = externalId
        , M.instagramMessageSenderId = senderId
        , M.instagramMessageSenderName = Just "Ada"
        , M.instagramMessageText = Just "Original message"
        , M.instagramMessageDirection = direction
        , M.instagramMessageAdExternalId = Nothing
        , M.instagramMessageAdName = Nothing
        , M.instagramMessageCampaignExternalId = Nothing
        , M.instagramMessageCampaignName = Nothing
        , M.instagramMessageMetadata = Nothing
        , M.instagramMessageReplyStatus =
            if direction == "incoming" then "pending" else "sent"
        , M.instagramMessageHoldReason = Nothing
        , M.instagramMessageHoldRequiredFields = Nothing
        , M.instagramMessageLastAttemptAt = Nothing
        , M.instagramMessageAttemptCount = 0
        , M.instagramMessageRepliedAt = Nothing
        , M.instagramMessageReplyText = Nothing
        , M.instagramMessageReplyError = Nothing
        , M.instagramMessageDeletedAt = Nothing
        , M.instagramMessageCreatedAt = now
        }

fixtureFacebookMessage
    :: Int -> UTCTime -> T.Text -> T.Text -> T.Text -> Entity ME.FacebookMessage
fixtureFacebookMessage keyVal now externalId direction senderId =
    Entity (toSqlKey (fromIntegral keyVal)) ME.FacebookMessage
        { ME.facebookMessageExternalId = externalId
        , ME.facebookMessageSenderId = senderId
        , ME.facebookMessageSenderName = Just "Ada"
        , ME.facebookMessageText = Just "Original message"
        , ME.facebookMessageDirection = direction
        , ME.facebookMessageAdExternalId = Nothing
        , ME.facebookMessageAdName = Nothing
        , ME.facebookMessageCampaignExternalId = Nothing
        , ME.facebookMessageCampaignName = Nothing
        , ME.facebookMessageMetadata = Nothing
        , ME.facebookMessageReplyStatus =
            if direction == "incoming" then "pending" else "sent"
        , ME.facebookMessageHoldReason = Nothing
        , ME.facebookMessageHoldRequiredFields = Nothing
        , ME.facebookMessageLastAttemptAt = Nothing
        , ME.facebookMessageAttemptCount = 0
        , ME.facebookMessageRepliedAt = Nothing
        , ME.facebookMessageReplyText = Nothing
        , ME.facebookMessageReplyError = Nothing
        , ME.facebookMessageDeletedAt = Nothing
        , ME.facebookMessageCreatedAt = now
        }

fixtureWhatsAppMessage
    :: Int -> UTCTime -> T.Text -> T.Text -> T.Text -> Entity ME.WhatsAppMessage
fixtureWhatsAppMessage keyVal now externalId direction phone =
    Entity (toSqlKey (fromIntegral keyVal)) ME.WhatsAppMessage
        { ME.whatsAppMessageExternalId = externalId
        , ME.whatsAppMessageSenderId = phone
        , ME.whatsAppMessageSenderName = Just "Ada"
        , ME.whatsAppMessagePartyId = Nothing
        , ME.whatsAppMessageActorPartyId = Nothing
        , ME.whatsAppMessagePhoneE164 = Just phone
        , ME.whatsAppMessageContactEmail = Nothing
        , ME.whatsAppMessageText = Just "Original message"
        , ME.whatsAppMessageDirection = direction
        , ME.whatsAppMessageAdExternalId = Nothing
        , ME.whatsAppMessageAdName = Nothing
        , ME.whatsAppMessageCampaignExternalId = Nothing
        , ME.whatsAppMessageCampaignName = Nothing
        , ME.whatsAppMessageMetadata = Nothing
        , ME.whatsAppMessageReplyStatus =
            if direction == "incoming" then "pending" else "sent"
        , ME.whatsAppMessageHoldReason = Nothing
        , ME.whatsAppMessageHoldRequiredFields = Nothing
        , ME.whatsAppMessageLastAttemptAt = Nothing
        , ME.whatsAppMessageAttemptCount = 0
        , ME.whatsAppMessageRepliedAt = Nothing
        , ME.whatsAppMessageReplyText = Nothing
        , ME.whatsAppMessageReplyError = Nothing
        , ME.whatsAppMessageDeliveryStatus =
            if direction == "incoming" then "received" else "sent"
        , ME.whatsAppMessageDeliveryUpdatedAt = Nothing
        , ME.whatsAppMessageDeliveryError = Nothing
        , ME.whatsAppMessageTransportPayload = Nothing
        , ME.whatsAppMessageStatusPayload = Nothing
        , ME.whatsAppMessageSource = Just "server_spec_seed"
        , ME.whatsAppMessageResendOfMessageId = Nothing
        , ME.whatsAppMessageCreatedAt = now
        }

calendarConfigEntity :: Int -> Text -> Entity Cal.GoogleCalendarConfig
calendarConfigEntity keyVal calendarIdVal =
    Entity (toSqlKey (fromIntegral keyVal)) Cal.GoogleCalendarConfig
        { Cal.googleCalendarConfigOwnerId = Nothing
        , Cal.googleCalendarConfigCalendarId = calendarIdVal
        , Cal.googleCalendarConfigAccessToken = Just "access-token"
        , Cal.googleCalendarConfigRefreshToken = Nothing
        , Cal.googleCalendarConfigTokenType = Just "Bearer"
        , Cal.googleCalendarConfigTokenExpiresAt = Nothing
        , Cal.googleCalendarConfigSyncCursor = Nothing
        , Cal.googleCalendarConfigSyncedAt = Nothing
        , Cal.googleCalendarConfigCreatedAt = calendarConfigFixtureTime
        , Cal.googleCalendarConfigUpdatedAt = calendarConfigFixtureTime
        }

calendarConfigFixtureTime :: UTCTime
calendarConfigFixtureTime =
    UTCTime (fromGregorian 2026 4 30) (secondsToDiffTime 0)

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
