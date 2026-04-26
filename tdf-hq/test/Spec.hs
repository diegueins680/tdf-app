{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, bracket)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode, (.=))
import qualified Data.Aeson as A
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text
import Data.Time (UTCTime (..), addDays, addUTCTime, fromGregorian, secondsToDiffTime)
import Database.Persist (Entity (..), Key, insert, insert_, insertKey, selectList)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (defaultRequest)
import Network.Wai.Internal (Request (..))
import Servant (ServerError (..), ServerT, (:<|>) (..))
import Servant.Multipart (FileData (..), FromMultipart (fromMultipart), Input (..), MultipartData (..), Tmp)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Web.PathPieces (toPathPiece)

import TDF.API (WhatsAppConsentRequest (..), WhatsAppOptOutRequest (..))
import TDF.API.Feedback (FeedbackPayload (..))
import TDF.API.Admin (AdminEmailBroadcastRequest)
import qualified TDF.API.InstagramOAuth as InstagramOAuth
import TDF.API.LiveSessions
    ( LiveSessionIntakePayload (..),
      LiveSessionMusicianPayload (..),
      LiveSessionSongPayload (..),
      resolveLiveSessionSetlistSortOrders )
import TDF.API.Radio (RadioAPI)
import TDF.API.SocialEventsAPI (EventImageUploadForm (..))
import TDF.API.SocialSyncAPI (SocialSyncAPI)
import TDF.API.Types
    ( InternTaskUpdate (..),
      RadioImportRequest (..),
      RadioMetadataRefreshRequest (..),
      RadioNowPlayingRequest (..),
      RadioNowPlayingResult (..),
      RadioPresenceDTO (..),
      RadioPresenceUpsert (..),
      RadioStreamUpsert (..),
      RadioTransmissionRequest (..) )
import TDF.API.WhatsApp
    ( CompleteReq (..),
      PreviewReq (..),
      ensureLeadCompletionUpdated,
      extractFirstWebhookMessage,
      validateHookVerifyRequest,
      validateLeadCompletionId,
      validateLeadCompletionLookup,
      validateLeadCompletionRequest,
      leadCompletionConsumedToken )
import TDF.App.Boot (validateSeedDatabaseStartup)
import qualified TDF.APITypesSpec as APITypesSpec
import TDF.Cors
    ( corsPolicy,
      deriveCorsOriginFromAppBase,
      isTrustedPreviewOrigin,
      lookupFirstNonEmptyEnv )
import TDF.Cron (Directive (..), parseDirective)
import TDF.DB (Env (..))
import qualified TDF.DTO as DTO
import qualified TDF.Invoice.SRI as Sri
import TDF.DTO.SocialEventsDTO
    ( ArtistDTO (..),
      EventMetadataUpdateDTO (..),
      EventMomentCommentCreateDTO (..),
      EventMomentCreateDTO (..),
      EventMomentReactionRequestDTO (..),
      EventUpdateDTO (..),
      InvitationUpdateDTO (..),
      NullableFieldUpdate (..),
      TicketCheckInRequestDTO (..),
      VenueUpdateDTO (..),
      eudMetadataUpdate,
      emuBudgetCents,
      emuTicketUrl,
      iudMessageUpdate,
      vcuPhone,
      vudContactUpdate )
import TDF.DTO.SocialSyncDTO
    ( SocialSyncIngestRequest (..),
      SocialSyncIngestResponse (..),
      SocialSyncPostDTO (..),
      SocialSyncPostIn (..),
      maxSocialSyncIngestPosts )
import TDF.Models.SocialEventsModels
    ( EventFinanceEntry (..),
      EventInvitationId,
      EventTicket (..),
      EventTicketOrder (..),
      EventTicketTier (..),
      SocialEvent (..),
      SocialEventId )
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Models (ArtistProfile (..), Party (..), RoleEnum (..), SocialSyncPost (..), SocialSyncRun (..))
import qualified TDF.ModelsExtra as ME
import qualified TDF.Profiles.ArtistSpec as ArtistSpec
import qualified TDF.ServerAdminSpec as ServerAdminSpec
import qualified TDF.ServerProposalsSpec as ServerProposalsSpec
import TDF.ServerRadio
    ( radioServer,
      resolveRadioTransmissionEnvBase,
      resolveRadioNowPlayingFetchResult,
      validateRadioImportLimit,
      validateRadioImportSources,
      validateRadioMetadataRefreshLimit,
      validateRadioOptionalMetadataField,
      validateRadioStreamUrl,
      validateRadioTransmissionIngestBase,
      validateRadioTransmissionWhipBase,
      validateRadioTransmissionPublicBase )
import TDF.RagStore
    ( availabilityOverlaps,
      validateEmbeddingModelDimensions,
      validateEmbeddingResponseDimensions,
      validateEmbeddingResponseOrder )
import TDF.ServerAdmin (parseSocialErrorsChannel, validateAdminEmailCtaUrl, validateSocialErrorsLimit)
import TDF.Contracts.Server (decodeStoredContract, validateContractId, validateContractPayload, validateContractSendPayload)
import TDF.ServerInternships
    ( parseKey,
      validateInternProjectDateRange,
      validateInternProjectDateUpdate,
      validateInternProjectStatusInput,
      validateInternPermissionDateRange,
      validateInternProfileDateUpdate,
      validateInternProjectTitle,
      validateInternProjectTitleUpdate,
      validateInternTaskTitle,
      validateInternTaskTitleUpdate,
      validateInternTodoText,
      validateInternTodoTextUpdate,
      validateInternTaskUpdatePermissions,
      validateOptionalInternPermissionStatusInput,
      validateOptionalInternPartyIdInput,
      validateOptionalInternPartyIdUpdate,
      validateInternTaskProgressUpdate,
      validateOptionalInternProjectStatusInput,
      validateOptionalInternTaskStatusInput )
import TDF.ServerProposals
    ( ProposalContentSource (..),
      validateOptionalProposalClientPartyId,
      validateOptionalProposalContactEmail,
      validateOptionalProposalNotes,
      validateOptionalProposalContactPhone,
      validateOptionalProposalStatus,
      validateProposalContentSource,
      validateProposalTitle,
      validateProposalStatus,
      validateProposalVersionNumber,
      validateTemplateKey )
import TDF.ServerFeedback
    ( normalizeOptionalFeedbackText,
      sanitizeFeedbackAttachmentFileName,
      validateFeedbackCategory,
      validateFeedbackDescription,
      validateFeedbackAttachmentSize,
      validateFeedbackTitle,
      validateFeedbackConsent,
      validateFeedbackSeverity,
      validateOptionalFeedbackContactEmail )
import TDF.ServerInstagramOAuth (instagramOAuthServer, resolveInstagramRedirectUri)
import TDF.Server
    ( buildWhatsappCtaFor,
      resolveDrivePublicUrl,
      resolveProvidedDriveAccessToken,
      sanitizeStoredCoursePublicUrl,
      validateWhatsAppConsentDisplayName,
      validateWhatsAppConsentSource,
      validateWhatsAppOptOutReason,
      validateCoursePublicUrlField,
      validateDatafastCheckoutId,
      validateDatafastCredential,
      validateDatafastBaseUrl,
      validatePayPalPayerEmailField,
      validatePayPalCreateOrderIdField,
      validatePayPalCaptureStatusField,
      validatePayPalApprovalUrl )
import TDF.ServerLiveSessions
    ( buildLiveSessionUsernameCollisionCandidate,
      LiveSessionMusicianLookup (..),
      resolveLiveSessionMusicianLookup,
      sanitizeLiveSessionRiderFileName,
      validateLiveSessionReferencedPartyEmail,
      validateLiveSessionTermsAcceptance )
import TDF.Services.InstagramMessaging (sendInstagramTextWithContext)
import TDF.Server.SocialSync
    ( socialSyncServer,
      validateSocialSyncArtistPartyId,
      validateSocialSyncExternalPostId,
      validateSocialSyncArtistProfileId,
      validateSocialSyncPlatform,
      validateSocialSyncPostsLimit,
      validateSocialSyncIngestSource,
      validateSocialSyncPermalink,
      validateSocialSyncMediaUrls )
import TDF.Server.SocialEventsHandlers (
    normalizeBudgetLineType,
    normalizeEventStatus,
    normalizeEventType,
    normalizeFinanceDirection,
    normalizeFinanceEntryStatus,
    normalizeFinanceSource,
    validateFinanceEntryCurrencyInput,
    validateOptionalBudgetLineIdInput,
    normalizeArtistGenres,
    normalizeInvitationStatus,
    normalizeMomentCaption,
    normalizeMomentCommentBody,
    normalizeMomentMediaType,
    normalizeMomentReaction,
    normalizePositivePartyIdText,
    validateStoredFinanceEntryDimensions,
    parseEventStatusQueryParamEither,
    parseEventTypeQueryParamEither,
    parseFollowerQueryParamEither,
    parseVenueIdEither,
    validateEventCreateUpdateDimensions,
    validateVenueCreateUpdateFields,
    normalizeTicketOrderStatus,
    normalizeTicketStatus,
    parseNearQueryEither,
    parseInvitationIdsEither,
    isImageUpload,
    TicketCheckInLookup (..),
    validateInvitationToPartyId,
    validateInvitationStatusInput,
    validateInvitationStatusUpdateInput,
    validateEventArtistIds,
    validateRsvpStatus,
    validateTicketCheckInLookup,
    validateTicketCheckInOrderStatus,
    validateTicketCheckInTicketStatus,
    findTicketForCheckIn,
    validateOptionalTicketBuyerPartyId,
    validateTicketPurchaseBuyerEmail,
    validateTicketTierCurrencyInput,
    validateEventCurrencyInput,
    validateEventCreateTypeStatus,
    validateEventMetadataUpdate,
    validateBudgetLineTypeInput,
 )
import TDF.Auth (extractToken, extractTokenFromHeaders, sessionCookieHeader)
import TDF.Config
    ( appPort,
      assetsRootDir,
      chatKitApiBase,
      chatKitWorkflowId,
      courseInstructorAvatarFallback,
      courseMapFallback,
      courseSlugFallback,
      dbConnString,
      emailConfig,
      facebookGraphBase,
      facebookMessagingApiBase,
      facebookMessagingPageId,
      instagramGraphBase,
      instagramMessagingApiBase,
      instagramMessagingAccountId,
      loadConfig,
      openAiEmbedModel,
      openAiModel,
      ragAvailabilityDays,
      ragAvailabilityPerResource,
      ragChunkOverlap,
      ragChunkWords,
      ragEmbedBatchSize,
      ragRefreshHours,
      ragTopK,
      resolveConfiguredAppBase,
      resolveConfiguredAssetsBase,
      resetDb,
      runMigrations,
      seedDatabase,
      seedTriggerToken,
      sessionCookieDomain,
      sessionCookieName,
      sessionCookiePath,
      sessionCookieSameSite,
      sessionCookieSecure,
      smtpPort )
import TDF.Version (VersionInfo (..), getVersionInfo)
import TDF.Seed (seededCredentialSeedingAllowed)
import qualified TDF.ServerAuthSpec as ServerAuthSpec
import qualified TDF.ServerSpec as ServerSpec
import qualified TDF.ServerExtraSpec as ServerExtraSpec
import qualified TDF.Social.FollowHandlerSpec as FollowHandlerSpec
import qualified TDF.Social.FollowSpec as FollowSpec
import qualified TDF.Trials.PublicLeadSpec as PublicLeadSpec
import qualified TDF.Trials.DTO as TrialsDTO
import qualified TDF.WhatsApp.Client as WhatsAppClient
import qualified TDF.WhatsApp.HistorySpec as WhatsAppHistorySpec
import qualified TDF.WhatsApp.Service as WhatsAppService
import qualified TDF.WhatsApp.Types as WA

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

clearRagEnv :: [(String, Maybe String)]
clearRagEnv =
    map
        (\key -> (key, Nothing))
        [ "RAG_TOP_K"
        , "RAG_CHUNK_WORDS"
        , "RAG_CHUNK_OVERLAP"
        , "RAG_AVAILABILITY_DAYS"
        , "RAG_AVAILABILITY_PER_RESOURCE"
        , "RAG_REFRESH_HOURS"
        , "RAG_EMBED_BATCH_SIZE"
        ]

clearWhatsAppProviderCredentialEnv :: [(String, Maybe String)]
clearWhatsAppProviderCredentialEnv =
    [ ("WA_TOKEN", Nothing)
    , ("WHATSAPP_TOKEN", Nothing)
    , ("WA_PHONE_ID", Nothing)
    , ("WHATSAPP_PHONE_NUMBER_ID", Nothing)
    ]

initializeTicketCheckInSchema :: SqlPersistT IO ()
initializeTicketCheckInSchema = do
    rawExecute "PRAGMA foreign_keys = ON" []
    rawExecute
        "CREATE TABLE social_event (\
        \id INTEGER PRIMARY KEY,\
        \organizer_party_id VARCHAR NULL,\
        \title VARCHAR NOT NULL,\
        \description VARCHAR NULL,\
        \venue_id INTEGER NULL,\
        \start_time TIMESTAMP NOT NULL,\
        \end_time TIMESTAMP NOT NULL,\
        \price_cents INTEGER NULL,\
        \capacity INTEGER NULL,\
        \metadata VARCHAR NULL,\
        \created_at TIMESTAMP NOT NULL,\
        \updated_at TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE event_ticket_tier (\
        \id INTEGER PRIMARY KEY,\
        \event_id INTEGER NOT NULL,\
        \code VARCHAR NOT NULL,\
        \name VARCHAR NOT NULL,\
        \description VARCHAR NULL,\
        \price_cents INTEGER NOT NULL,\
        \currency VARCHAR NOT NULL,\
        \quantity_total INTEGER NOT NULL,\
        \quantity_sold INTEGER NOT NULL,\
        \sales_start TIMESTAMP NULL,\
        \sales_end TIMESTAMP NULL,\
        \is_active BOOLEAN NOT NULL,\
        \position INTEGER NULL,\
        \created_at TIMESTAMP NOT NULL,\
        \updated_at TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE event_ticket_order (\
        \id INTEGER PRIMARY KEY,\
        \event_id INTEGER NOT NULL,\
        \tier_id INTEGER NOT NULL,\
        \buyer_party_id VARCHAR NULL,\
        \buyer_name VARCHAR NULL,\
        \buyer_email VARCHAR NULL,\
        \quantity INTEGER NOT NULL,\
        \amount_cents INTEGER NOT NULL,\
        \currency VARCHAR NOT NULL,\
        \status VARCHAR NOT NULL,\
        \metadata VARCHAR NULL,\
        \purchased_at TIMESTAMP NOT NULL,\
        \created_at TIMESTAMP NOT NULL,\
        \updated_at TIMESTAMP NOT NULL\
        \)"
        []
    rawExecute
        "CREATE TABLE event_ticket (\
        \id INTEGER PRIMARY KEY,\
        \event_id INTEGER NOT NULL,\
        \tier_ref_id INTEGER NOT NULL,\
        \order_ref_id INTEGER NOT NULL,\
        \holder_name VARCHAR NULL,\
        \holder_email VARCHAR NULL,\
        \code VARCHAR NOT NULL,\
        \status VARCHAR NOT NULL,\
        \checked_in_at TIMESTAMP NULL,\
        \created_at TIMESTAMP NOT NULL,\
        \updated_at TIMESTAMP NOT NULL\
        \)"
        []

sampleSriScriptLine :: Sri.SriScriptLine
sampleSriScriptLine =
    Sri.SriScriptLine
            (Just "SRV-001")
            Nothing
            "Studio session"
            1
            9000
            (Just 1500)
            Nothing
            Nothing

sampleSriScriptRequest :: Sri.SriScriptRequest
sampleSriScriptRequest =
    Sri.SriScriptRequest
        (Sri.SriScriptCustomer "1790012345001" "TDF Test Customer" Nothing Nothing)
        [sampleSriScriptLine]
        "001"
        "001"
        "cash"
        False
        Nothing

main :: IO ()
main = hspec $ do
    describe "seededCredentialSeedingAllowed" $ do
        it "allows seeded demo credentials in local development by default" $
            seededCredentialSeedingAllowed []
                `shouldBe` True

        it "blocks seeded demo credentials in hosted runtimes" $ do
            seededCredentialSeedingAllowed [("FLY_APP_NAME", "tdf-hq")]
                `shouldBe` False
            seededCredentialSeedingAllowed [("KOYEB_APP_NAME", "tdf-hq")]
                `shouldBe` False
            seededCredentialSeedingAllowed [("FLY_APP_NAME", "   ")]
                `shouldBe` True

        it "blocks seeded demo credentials in production-like environments" $ do
            seededCredentialSeedingAllowed [("APP_ENV", "production")]
                `shouldBe` False
            seededCredentialSeedingAllowed [("ENVIRONMENT", " prod ")]
                `shouldBe` False
            seededCredentialSeedingAllowed [("NODE_ENV", "test")]
                `shouldBe` True

    describe "validateSeedDatabaseStartup" $ do
        it "allows database seeding in local development" $
            validateSeedDatabaseStartup True []
                `shouldBe` Right ()

        it "allows hosted or production startup when database seeding is disabled" $
            validateSeedDatabaseStartup False [("FLY_APP_NAME", "tdf-hq")]
                `shouldBe` Right ()

        it "rejects database seeding in hosted or production runtimes" $ do
            let seedDbGuardMessage (Left msg) =
                    "SEED_DB=true is not allowed" `isInfixOf` msg
                seedDbGuardMessage _ = False
            validateSeedDatabaseStartup True [("FLY_APP_NAME", "tdf-hq")]
                `shouldSatisfy` seedDbGuardMessage
            validateSeedDatabaseStartup True [("APP_ENV", "production")]
                `shouldSatisfy` seedDbGuardMessage

    describe "getVersionInfo" $ do
        let clearEnv keys = map (\key -> (key, Nothing)) keys
            commitEnvKeys =
                [ "GIT_SHA"
                , "GIT_COMMIT"
                , "GIT_COMMIT_SHA"
                , "COMMIT_SHA"
                , "SOURCE_COMMIT"
                , "SOURCE_VERSION"
                , "SOURCE_SHA"
                , "GITHUB_SHA"
                , "RENDER_GIT_COMMIT"
                , "RENDER_GIT_COMMIT_SHA"
                , "VERCEL_GIT_COMMIT_SHA"
                , "KOYEB_GIT_SHA"
                , "KOYEB_GIT_COMMIT"
                , "KOYEB_GIT_COMMIT_SHA"
                , "KOYEB_DEPLOYMENT_GIT_SHA"
                , "KOYEB_DEPLOYMENT_GIT_COMMIT"
                , "FLY_GIT_SHA"
                ]
            buildTimeEnvKeys =
                [ "BUILD_TIME"
                , "SOURCE_BUILD_TIME"
                , "RENDER_BUILD_TIME"
                , "FLY_BUILD_TIME"
                ]

        it "skips blank or sentinel commit aliases while discovering deployed metadata" $
            withEnvOverrides
                (clearEnv commitEnvKeys
                    ++ [ ("GIT_SHA", Just " UNKNOWN ")
                       , ("GITHUB_SHA", Just "abc123def456")
                       ])
                $ do
                    info <- getVersionInfo
                    commit info `shouldBe` "abc123def456"

        it "skips deployment-template sentinel aliases during runtime metadata discovery" $
            withEnvOverrides
                (clearEnv commitEnvKeys
                    ++ clearEnv buildTimeEnvKeys
                    ++ [ ("GIT_SHA", Just " undefined ")
                       , ("GITHUB_SHA", Just "abc123def456")
                       , ("BUILD_TIME", Just "null")
                       , ("SOURCE_BUILD_TIME", Just "2026-04-18T01:02:03Z")
                       ])
                $ do
                    info <- getVersionInfo
                    commit info `shouldBe` "abc123def456"
                    buildTime info `shouldBe` "2026-04-18T01:02:03Z"

        it "skips control-bearing runtime metadata instead of exposing ambiguous version fields" $
            withEnvOverrides
                (clearEnv commitEnvKeys
                    ++ clearEnv buildTimeEnvKeys
                    ++ [ ("GIT_SHA", Just "abc123\ndef456")
                       , ("GITHUB_SHA", Just "abc123def456")
                       , ("BUILD_TIME", Just "2026-04-18T01:02:03Z\nsource=deploy")
                       , ("SOURCE_BUILD_TIME", Just "2026-04-18T01:02:03Z")
                       ])
                $ do
                    info <- getVersionInfo
                    commit info `shouldBe` "abc123def456"
                    buildTime info `shouldBe` "2026-04-18T01:02:03Z"

        it "skips whitespace-bearing commit aliases instead of exposing ambiguous version metadata" $
            withEnvOverrides
                (clearEnv commitEnvKeys
                    ++ [ ("GIT_SHA", Just "abc123 def456")
                       , ("GITHUB_SHA", Just "abc123def456")
                       ])
                $ do
                    info <- getVersionInfo
                    commit info `shouldBe` "abc123def456"

        it "skips non-SHA commit aliases instead of exposing branch labels as deploy metadata" $
            withEnvOverrides
                (clearEnv commitEnvKeys
                    ++ [ ("GIT_SHA", Just "release/main")
                       , ("GITHUB_SHA", Just "abc123def456")
                       ])
                $ do
                    info <- getVersionInfo
                    commit info `shouldBe` "abc123def456"

        it "skips blank build-time aliases instead of returning empty version metadata" $
            withEnvOverrides
                (clearEnv buildTimeEnvKeys
                    ++ [ ("BUILD_TIME", Just "   ")
                       , ("SOURCE_BUILD_TIME", Just "2026-04-18T01:02:03Z")
                       ])
                $ do
                    info <- getVersionInfo
                    buildTime info `shouldBe` "2026-04-18T01:02:03Z"

    describe "loadConfig" $ do
        it "rejects malformed APP_PORT instead of booting on an unintended port" $ do
            let assertInvalid rawPort =
                    withEnvOverrides
                        [ ("APP_PORT", Just rawPort) ]
                        $ loadConfig `shouldThrow` \err ->
                            "APP_PORT must be a positive integer"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "not-a-port"
            assertInvalid "0"
            assertInvalid "-1"

            withEnvOverrides
                [ ("APP_PORT", Just " 9090 ") ]
                $ do
                    cfg <- loadConfig
                    appPort cfg `shouldBe` 9090

            withEnvOverrides
                [ ("APP_PORT", Just "   ") ]
                $ do
                    cfg <- loadConfig
                    appPort cfg `shouldBe` 8080

        it "rejects malformed SMTP_PORT when SMTP is configured" $ do
            let assertInvalid rawPort =
                    withEnvOverrides
                        [ ("SMTP_PORT", Just rawPort)
                        , ("SMTP_HOST", Just "smtp.example.com")
                        , ("SMTP_USER", Just "mailer")
                        , ("SMTP_PASS", Just "secret")
                        , ("SMTP_FROM", Just "tdf@example.com")
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            "SMTP_PORT must be a port number between 1 and 65535"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "smtp"
            assertInvalid "0"
            assertInvalid "70000"

            withEnvOverrides
                [ ("SMTP_PORT", Just " 2525 ")
                , ("SMTP_HOST", Just "smtp.example.com")
                , ("SMTP_USER", Just "mailer")
                , ("SMTP_PASS", Just "secret")
                , ("SMTP_FROM", Just "tdf@example.com")
                ]
                $ do
                    cfg <- loadConfig
                    fmap smtpPort (emailConfig cfg) `shouldBe` Just 2525

        it "rejects malformed startup boolean flags instead of silently changing boot behavior" $ do
            withEnvOverrides
                [ ("RUN_MIGRATIONS", Just "tru")
                , ("RESET_DB", Nothing)
                , ("SEED_DB", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "RUN_MIGRATIONS must be a boolean flag"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("RUN_MIGRATIONS", Just " off ")
                , ("RESET_DB", Just "1")
                , ("SEED_DB", Just "yes")
                ]
                $ do
                    cfg <- loadConfig
                    runMigrations cfg `shouldBe` False
                    resetDb cfg `shouldBe` True
                    seedDatabase cfg `shouldBe` True

        it "rejects malformed seed trigger tokens before enabling seed endpoints" $ do
            withEnvOverrides
                [ ("SEED_TRIGGER_TOKEN", Just " seed-token_123456 ") ]
                $ do
                    cfg <- loadConfig
                    seedTriggerToken cfg `shouldBe` Just "seed-token_123456"

            let assertInvalid rawToken expectedMessage =
                    withEnvOverrides
                        [ ("SEED_TRIGGER_TOKEN", Just rawToken) ]
                        $ loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "short-token"
                "SEED_TRIGGER_TOKEN must be at least 16 characters"
            assertInvalid
                "seed token"
                "SEED_TRIGGER_TOKEN must not contain whitespace or control characters"
            assertInvalid
                "seed-token\nInjected: value"
                "SEED_TRIGGER_TOKEN must not contain whitespace or control characters"
            assertInvalid
                (replicate 513 'a')
                "SEED_TRIGGER_TOKEN must be 512 characters or fewer"

        it "treats blank SMTP templates as unconfigured but rejects partial SMTP config" $ do
            withEnvOverrides
                [ ("SMTP_HOST", Just "   ")
                , ("SMTP_USERNAME", Nothing)
                , ("SMTP_USER", Just " ")
                , ("SMTP_PASSWORD", Nothing)
                , ("SMTP_PASS", Just "\t")
                , ("SMTP_FROM", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    case emailConfig cfg of
                        Nothing -> pure ()
                        Just value ->
                            expectationFailure
                                ("Expected blank SMTP settings to stay unconfigured, got " <> show value)

            withEnvOverrides
                [ ("SMTP_HOST", Just "smtp.example.com")
                , ("SMTP_USERNAME", Nothing)
                , ("SMTP_USER", Just "mailer")
                , ("SMTP_PASSWORD", Nothing)
                , ("SMTP_PASS", Just "secret")
                , ("SMTP_FROM", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "SMTP configuration requires non-empty SMTP_HOST"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("SMTP_HOST", Just "smtp.example.com")
                , ("SMTP_USERNAME", Nothing)
                , ("SMTP_USER", Just "mailer")
                , ("SMTP_PASSWORD", Nothing)
                , ("SMTP_PASS", Just "secret")
                , ("SMTP_FROM", Just "tdf @example.com")
                ]
                $ loadConfig `shouldThrow` \err ->
                    "SMTP_FROM must be a valid email address"
                        `isInfixOf` show (err :: IOException)

        it "rejects SameSite=None unless session cookies are secure" $ do
            withEnvOverrides
                [ ("SESSION_COOKIE_SAMESITE", Just "None")
                , ("SESSION_COOKIE_SECURE", Just "false")
                , ("HQ_APP_URL", Just "https://hq.example.com")
                ]
                $ loadConfig `shouldThrow` \err ->
                    "SESSION_COOKIE_SAMESITE=None requires secure session cookies"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("SESSION_COOKIE_SAMESITE", Just "None")
                , ("SESSION_COOKIE_SECURE", Just "true")
                , ("HQ_APP_URL", Just "http://localhost:5173")
                ]
                $ do
                    cfg <- loadConfig
                    sessionCookieSecure cfg `shouldBe` True
                    sessionCookieSameSite cfg `shouldBe` "None"

        it "rejects malformed session cookie Secure values instead of emitting ambiguous cookies" $
            withEnvOverrides
                [ ("SESSION_COOKIE_SECURE", Just "maybe")
                , ("HQ_APP_URL", Just "https://hq.example.com")
                ]
                $ loadConfig `shouldThrow` \err ->
                    "SESSION_COOKIE_SECURE must be a boolean flag"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed session cookie SameSite values instead of silently using Lax" $ do
            withEnvOverrides
                [ ("SESSION_COOKIE_SAMESITE", Just " Strict ") ]
                $ do
                    cfg <- loadConfig
                    sessionCookieSameSite cfg `shouldBe` "Strict"

            withEnvOverrides
                [ ("SESSION_COOKIE_SAMESITE", Just "nonee") ]
                $ loadConfig `shouldThrow` \err ->
                    "SESSION_COOKIE_SAMESITE must be one of: Lax, Strict, None"
                        `isInfixOf` show (err :: IOException)

        it "normalizes valid session cookie paths before emitting Set-Cookie headers" $
            withEnvOverrides
                [ ("SESSION_COOKIE_PATH", Just " /hq ") ]
                $ do
                    cfg <- loadConfig
                    sessionCookiePath cfg `shouldBe` "/hq"

        it "normalizes configured session cookie domains before emitting Set-Cookie headers" $
            withEnvOverrides
                [ ("HQ_APP_URL", Nothing)
                , ("SESSION_COOKIE_NAME", Nothing)
                , ("SESSION_COOKIE_DOMAIN", Just " .Example.COM ")
                , ("SESSION_COOKIE_PATH", Nothing)
                , ("SESSION_COOKIE_SECURE", Nothing)
                , ("SESSION_COOKIE_SAMESITE", Nothing)
                , ("SESSION_COOKIE_MAX_AGE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    sessionCookieDomain cfg `shouldBe` Just "example.com"
                    sessionCookieHeader cfg "session-token"
                        `shouldBe` "tdf_session=session-token; Path=/; HttpOnly; SameSite=Lax; Domain=example.com; Max-Age=2592000"

        it "rejects malformed session cookie names before emitting ambiguous Set-Cookie headers" $ do
            withEnvOverrides
                [ ("SESSION_COOKIE_NAME", Just " tdf_session_admin ") ]
                $ do
                    cfg <- loadConfig
                    sessionCookieName cfg `shouldBe` "tdf_session_admin"

            let assertInvalid rawName =
                    withEnvOverrides
                        [ ("SESSION_COOKIE_NAME", Just rawName) ]
                        $ loadConfig `shouldThrow` \err ->
                            "SESSION_COOKIE_NAME must be a cookie token"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "tdf session"
            assertInvalid "tdf_session; Secure"
            assertInvalid "tdf/session"
            assertInvalid "tdf_session,alt"

        it "rejects malformed session cookie domains before emitting ambiguous Set-Cookie headers" $ do
            let assertInvalid rawDomain =
                    withEnvOverrides
                        [ ("SESSION_COOKIE_NAME", Nothing)
                        , ("SESSION_COOKIE_DOMAIN", Just rawDomain)
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            "SESSION_COOKIE_DOMAIN must be a cookie domain"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "https://example.com"
            assertInvalid "example.com:443"
            assertInvalid "example.com; Secure"
            assertInvalid "bad_domain.example.com"
            assertInvalid "bad..example.com"
            assertInvalid "-bad.example.com"

        it "rejects malformed session cookie paths before emitting ambiguous Set-Cookie headers" $ do
            let assertInvalid rawPath =
                    withEnvOverrides
                        [ ("SESSION_COOKIE_PATH", Just rawPath) ]
                        $ loadConfig `shouldThrow` \err ->
                            "SESSION_COOKIE_PATH must start with / and contain no whitespace, semicolons, commas, or control characters"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "hq"
            assertInvalid "/hq; Secure"
            assertInvalid "/hq admin"
            assertInvalid "/hq,admin"

        it "rejects malformed session cookie max-age values instead of silently using the default" $ do
            withEnvOverrides
                [ ("SESSION_COOKIE_MAX_AGE", Just " 3600 ") ]
                $ do
                    cfg <- loadConfig
                    let expectedHeader =
                            "tdf_session=session-token; Path=/; HttpOnly; SameSite=Lax; "
                                <> "Max-Age=3600"
                    sessionCookieHeader cfg "session-token"
                        `shouldBe` expectedHeader

            let assertInvalid rawMaxAge =
                    withEnvOverrides
                        [ ("SESSION_COOKIE_MAX_AGE", Just rawMaxAge) ]
                        $ loadConfig `shouldThrow` \err ->
                            "SESSION_COOKIE_MAX_AGE must be a positive integer number of seconds"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "0"
            assertInvalid "-1"
            assertInvalid "thirty-days"

        it "normalizes configured backend public base URLs before generating fallback links" $
            withEnvOverrides
                [ ("HQ_APP_URL", Just " https://hq.example.com/app/ ")
                , ("HQ_ASSETS_BASE_URL", Just " https://cdn.example.com/assets/ ")
                ]
                $ do
                    cfg <- loadConfig
                    resolveConfiguredAppBase cfg `shouldBe` "https://hq.example.com/app"
                    resolveConfiguredAssetsBase cfg `shouldBe` "https://cdn.example.com/assets"

        it "keeps explicit asset root directories authoritative during startup" $ do
            withEnvOverrides
                [ ("HQ_ASSETS_DIR", Just " assets ") ]
                $ do
                    cfg <- loadConfig
                    assetsRootDir cfg `shouldBe` "assets"

            withEnvOverrides
                [ ("HQ_ASSETS_DIR", Just "/tmp/tdf-hq-missing-assets-dir-never-created") ]
                $ loadConfig `shouldThrow` \err ->
                    "HQ_ASSETS_DIR must point to an existing directory"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed backend public base URLs at startup" $ do
            let assertInvalid envName rawUrl expectedMessage =
                    withEnvOverrides
                        [ (envName, Just rawUrl) ]
                        $ loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "HQ_APP_URL"
                "javascript:alert(1)"
                "HQ_APP_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_APP_URL"
                "/hq"
                "HQ_APP_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_ASSETS_BASE_URL"
                "https://cdn.example.com/assets copy"
                "HQ_ASSETS_BASE_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_ASSETS_BASE_URL"
                "https://files_example.com/assets"
                "HQ_ASSETS_BASE_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_APP_URL"
                "https://hq-admin/app"
                "HQ_APP_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_ASSETS_BASE_URL"
                "https://cdn/assets"
                "HQ_ASSETS_BASE_URL must be an absolute http(s) URL"
            assertInvalid
                "HQ_APP_URL"
                "https://hq.example.com/app?preview=1"
                "HQ_APP_URL must be an absolute http(s) URL without query or fragment"
            assertInvalid
                "HQ_ASSETS_BASE_URL"
                "https://cdn.example.com/assets#logo"
                "HQ_ASSETS_BASE_URL must be an absolute http(s) URL without query or fragment"
            assertInvalid
                "HQ_APP_URL"
                "https://hq.example.com/app\SOHadmin"
                "HQ_APP_URL must not contain control characters"

        it "normalizes configured outbound API base URLs before building requests" $
            withEnvOverrides
                [ ("CHATKIT_API_BASE", Just " https://api.openai.com/ ")
                , ("FACEBOOK_GRAPH_BASE", Just " https://graph.facebook.com/v21.0/ ")
                , ( "FACEBOOK_MESSAGING_API_BASE"
                  , Just " https://graph.facebook.com/v22.0/ "
                  )
                , ("INSTAGRAM_GRAPH_BASE", Just " https://graph.instagram.com/ ")
                , ( "INSTAGRAM_MESSAGING_API_BASE"
                  , Just " https://graph.facebook.com/v23.0/ "
                  )
                ]
                $ do
                    cfg <- loadConfig
                    chatKitApiBase cfg `shouldBe` "https://api.openai.com"
                    facebookGraphBase cfg `shouldBe` "https://graph.facebook.com/v21.0"
                    facebookMessagingApiBase cfg
                        `shouldBe` "https://graph.facebook.com/v22.0"
                    instagramGraphBase cfg `shouldBe` "https://graph.instagram.com"
                    instagramMessagingApiBase cfg
                        `shouldBe` "https://graph.facebook.com/v23.0"

        it "rejects malformed outbound API base URLs before token-bearing requests are built" $ do
            let apiBaseKeys =
                    [ "CHATKIT_API_BASE"
                    , "FACEBOOK_GRAPH_BASE"
                    , "FACEBOOK_MESSAGING_API_BASE"
                    , "INSTAGRAM_GRAPH_BASE"
                    , "INSTAGRAM_MESSAGING_API_BASE"
                    ]
                onlyApiBase envName rawUrl =
                    [ (key, if key == envName then Just rawUrl else Nothing)
                    | key <- apiBaseKeys
                    ]
                assertInvalid envName rawUrl expectedMessage =
                    withEnvOverrides (onlyApiBase envName rawUrl) $
                        loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "FACEBOOK_MESSAGING_API_BASE"
                "http://graph.facebook.com/v20.0"
                "FACEBOOK_MESSAGING_API_BASE must be an absolute https URL"
            assertInvalid
                "INSTAGRAM_MESSAGING_API_BASE"
                "https://169.254.169.254/latest"
                "INSTAGRAM_MESSAGING_API_BASE must be an absolute https URL"
            assertInvalid
                "FACEBOOK_GRAPH_BASE"
                "https://192.0.2.10/v20.0"
                "FACEBOOK_GRAPH_BASE must be an absolute https URL"
            assertInvalid
                "CHATKIT_API_BASE"
                "https://api.openai.com?proxy=1"
                "CHATKIT_API_BASE must be an absolute https URL without query or fragment"

        it "normalizes configured Graph messaging node ids before building send URLs" $
            withEnvOverrides
                [ ("FACEBOOK_MESSAGING_PAGE_ID", Just "  page_123-abc.456  ")
                , ("FACEBOOK_PAGE_ID", Nothing)
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Just "  17841400000000000  ")
                ]
                $ do
                    cfg <- loadConfig
                    facebookMessagingPageId cfg `shouldBe` Just "page_123-abc.456"
                    instagramMessagingAccountId cfg
                        `shouldBe` Just "17841400000000000"

        it "rejects malformed Graph messaging node ids before token-bearing requests are built" $ do
            let graphIdKeys =
                    [ "FACEBOOK_MESSAGING_PAGE_ID"
                    , "FACEBOOK_PAGE_ID"
                    , "INSTAGRAM_MESSAGING_ACCOUNT_ID"
                    ]
                onlyGraphId envName rawNodeId =
                    [ (key, if key == envName then Just rawNodeId else Nothing)
                    | key <- graphIdKeys
                    ]
                assertInvalid envName rawNodeId =
                    withEnvOverrides (onlyGraphId envName rawNodeId) $
                        loadConfig `shouldThrow` \err ->
                            ( envName
                                <> " must be a Graph node id using only ASCII letters"
                            )
                                `isInfixOf` show (err :: IOException)
            assertInvalid "FACEBOOK_MESSAGING_PAGE_ID" "page/123"
            assertInvalid "FACEBOOK_PAGE_ID" "page?debug=1"
            assertInvalid "FACEBOOK_PAGE_ID" "---"
            assertInvalid "INSTAGRAM_MESSAGING_ACCOUNT_ID" "..."
            assertInvalid "INSTAGRAM_MESSAGING_ACCOUNT_ID" "1784\naccess"

        it "normalizes configured OpenAI embedding models before sizing RAG storage" $
            withEnvOverrides
                [ ("OPENAI_EMBED_MODEL", Just " TEXT-EMBEDDING-3-LARGE ") ]
                $ do
                    cfg <- loadConfig
                    openAiEmbedModel cfg `shouldBe` "text-embedding-3-large"

        it "rejects unknown OpenAI embedding models instead of defaulting RAG dimensions" $
            withEnvOverrides
                [ ("OPENAI_EMBED_MODEL", Just "text-embedding-unknown") ]
                $ loadConfig `shouldThrow` \err ->
                    "OPENAI_EMBED_MODEL must be one of"
                        `isInfixOf` show (err :: IOException)

        it "normalizes configured OpenAI chat models before fallback requests are built" $
            withEnvOverrides
                [ ("OPENAI_MODEL", Just " gpt-4.1-mini ") ]
                $ do
                    cfg <- loadConfig
                    openAiModel cfg `shouldBe` "gpt-4.1-mini"

        it "rejects malformed OpenAI chat models instead of building ambiguous fallback requests" $ do
            let assertInvalid rawModel expectedMessage =
                    withEnvOverrides
                        [ ("OPENAI_MODEL", Just rawModel) ]
                        $ loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "gpt 4.1"
                "OPENAI_MODEL must not contain whitespace"
            assertInvalid
                "gpt-4.1\nsource"
                "OPENAI_MODEL must not contain whitespace"
            assertInvalid
                "gpt/4?debug=1"
                "OPENAI_MODEL must use only ASCII letters"
            assertInvalid
                (Data.Text.unpack (Data.Text.replicate 257 "a"))
                "OPENAI_MODEL must be 256 characters or fewer"

        it "normalizes configured RAG tuning integers before building retrieval plans" $ do
            withEnvOverrides
                (clearRagEnv
                    ++ [ ("RAG_TOP_K", Just " 5 ")
                       , ("RAG_CHUNK_WORDS", Just "180")
                       , ("RAG_CHUNK_OVERLAP", Just "0")
                       , ("RAG_AVAILABILITY_DAYS", Just "21")
                       , ("RAG_AVAILABILITY_PER_RESOURCE", Just "4")
                       , ("RAG_REFRESH_HOURS", Just "12")
                       , ("RAG_EMBED_BATCH_SIZE", Just "32")
                       ])
                $ do
                    cfg <- loadConfig
                    ragTopK cfg `shouldBe` 5
                    ragChunkWords cfg `shouldBe` 180
                    ragChunkOverlap cfg `shouldBe` 0
                    ragAvailabilityDays cfg `shouldBe` 21
                    ragAvailabilityPerResource cfg `shouldBe` 4
                    ragRefreshHours cfg `shouldBe` 12
                    ragEmbedBatchSize cfg `shouldBe` 32

            withEnvOverrides
                (clearRagEnv ++ [("RAG_CHUNK_WORDS", Just "20")])
                $ do
                    cfg <- loadConfig
                    ragChunkWords cfg `shouldBe` 20
                    ragChunkOverlap cfg `shouldBe` 19

        it "rejects malformed RAG tuning integers instead of silently using defaults" $ do
            let assertInvalid envName rawValue expectedMessage =
                    withEnvOverrides
                        (clearRagEnv ++ [(envName, Just rawValue)])
                        $ loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "RAG_TOP_K"
                "many"
                "RAG_TOP_K must be a positive integer"
            assertInvalid
                "RAG_CHUNK_OVERLAP"
                "-1"
                "RAG_CHUNK_OVERLAP must be a non-negative integer"

            withEnvOverrides
                (clearRagEnv
                    ++ [ ("RAG_CHUNK_WORDS", Just "40")
                       , ("RAG_CHUNK_OVERLAP", Just "40")
                       ])
                $ loadConfig `shouldThrow` \err ->
                    "RAG_CHUNK_OVERLAP must be less than RAG_CHUNK_WORDS"
                        `isInfixOf` show (err :: IOException)

        it "normalizes configured ChatKit workflow fallbacks before creating sessions" $ do
            withEnvOverrides
                [ ("CHATKIT_WORKFLOW_ID", Just "  wf_default  ")
                , ("VITE_CHATKIT_WORKFLOW_ID", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    chatKitWorkflowId cfg `shouldBe` Just "wf_default"

            withEnvOverrides
                [ ("CHATKIT_WORKFLOW_ID", Nothing)
                , ("VITE_CHATKIT_WORKFLOW_ID", Just "  wf_public  ")
                ]
                $ do
                    cfg <- loadConfig
                    chatKitWorkflowId cfg `shouldBe` Just "wf_public"

        it "rejects malformed ChatKit workflow fallbacks at startup" $ do
            let assertInvalid :: String -> String -> String -> Expectation
                assertInvalid envName rawWorkflowId expectedMessage =
                    withEnvOverrides
                        [ ( "CHATKIT_WORKFLOW_ID"
                          , if envName == "CHATKIT_WORKFLOW_ID"
                              then Just rawWorkflowId
                              else Nothing
                          )
                        , ( "VITE_CHATKIT_WORKFLOW_ID"
                          , if envName == "VITE_CHATKIT_WORKFLOW_ID"
                              then Just rawWorkflowId
                              else Nothing
                          )
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                "CHATKIT_WORKFLOW_ID"
                "wf default"
                "CHATKIT_WORKFLOW_ID must not contain whitespace"
            assertInvalid
                "VITE_CHATKIT_WORKFLOW_ID"
                "wf\ndefault"
                "VITE_CHATKIT_WORKFLOW_ID must not contain whitespace"
            assertInvalid
                "CHATKIT_WORKFLOW_ID"
                "wf/default?preview=1"
                "CHATKIT_WORKFLOW_ID must use only ASCII letters"
            assertInvalid
                "CHATKIT_WORKFLOW_ID"
                (Data.Text.unpack (Data.Text.replicate 257 "a"))
                "CHATKIT_WORKFLOW_ID must be 256 characters or fewer"

        it "normalizes configured public course fallback URLs before serving metadata" $
            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just " https://maps.example.com/studio ")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Just " https://cdn.example.com/instructors/esteban.jpg ")
                ]
                $ do
                    cfg <- loadConfig
                    courseMapFallback cfg `shouldBe` "https://maps.example.com/studio"
                    courseInstructorAvatarFallback cfg
                        `shouldBe` "https://cdn.example.com/instructors/esteban.jpg"

        it "normalizes configured public course fallback slugs before matching fallback metadata" $
            withEnvOverrides
                [ ("COURSE_DEFAULT_SLUG", Just " Produccion-Musical-MAY-2026 ") ]
                $ do
                    cfg <- loadConfig
                    courseSlugFallback cfg `shouldBe` "produccion-musical-may-2026"

        it "rejects malformed public course fallback slugs at startup" $ do
            withEnvOverrides
                [ ("COURSE_DEFAULT_SLUG", Just "produccion musical") ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_SLUG must use only ASCII letters, numbers, and hyphens"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_SLUG", Just "---") ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_SLUG must use only ASCII letters, numbers, and hyphens"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed configured public course fallback URLs at startup" $ do
            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just "http://maps.example.com/studio")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_MAP_URL must be an absolute https URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Nothing)
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Just "https://cdn.example.com/avatar copy.jpg")
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_INSTRUCTOR_AVATAR must be an absolute https URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just "https://192.168.1.20/studio")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_MAP_URL must be an absolute https URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just "https://999.999.999.999/studio")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_MAP_URL must be an absolute https URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just "https://studio.localhost/map")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_MAP_URL must be an absolute https URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("COURSE_DEFAULT_MAP_URL", Just "https://maps.example.com/studio\SOHdebug")
                , ("COURSE_DEFAULT_INSTRUCTOR_AVATAR", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "COURSE_DEFAULT_MAP_URL must not contain control characters"
                        `isInfixOf` show (err :: IOException)

        it "normalizes WhatsApp enrollment fallback config before minting public links" $
            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("COURSE_EDITION_SLUG", Just "  ")
                , ("COURSE_DEFAULT_SLUG", Just " Produccion-Musical-MAY-2026 ")
                , ("COURSE_REG_URL", Just "  ")
                , ("HQ_APP_URL", Just " https://hq.example.com/app/ ")
                , ("WA_GRAPH_API_VERSION", Just "  ")
                , ("WHATSAPP_API_VERSION", Just " v21.0 ")
                ])
                $ do
                    cfg <- WhatsAppService.loadWhatsAppConfig
                    WhatsAppService.courseSlug cfg `shouldBe` "produccion-musical-may-2026"
                    WhatsAppService.courseRegUrl cfg `shouldBe` Nothing
                    WhatsAppService.appBaseUrl cfg `shouldBe` "https://hq.example.com/app/"
                    WhatsAppService.waApiVersion cfg `shouldBe` "v21.0"

        it "discovers canonical WhatsApp provider credential aliases before enrollment sends" $
            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("WHATSAPP_TOKEN", Just " token_123 ")
                , ("WHATSAPP_PHONE_NUMBER_ID", Just " 1234567890 ")
                ])
                $ do
                    cfg <- WhatsAppService.loadWhatsAppConfig
                    WhatsAppService.waToken cfg `shouldBe` "token_123"
                    WhatsAppService.waPhoneId cfg `shouldBe` "1234567890"

        it "rejects malformed WhatsApp provider credentials before enrollment sends" $ do
            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("WHATSAPP_TOKEN", Just "token value") ])
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "Invalid WhatsApp access token"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("WHATSAPP_PHONE_NUMBER_ID", Just "123/messages") ])
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "Invalid WhatsApp phone number id"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed WhatsApp enrollment fallback URLs before sending unsafe links" $ do
            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("COURSE_REG_URL", Just "javascript:alert(1)") ])
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "COURSE_REG_URL must be an absolute http(s) URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                (clearWhatsAppProviderCredentialEnv ++
                [ ("HQ_APP_URL", Just "https://hq.example.com/app copy") ])
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "HQ_APP_URL must be an absolute http(s) URL"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed WhatsApp API versions before building Graph request paths" $ do
            let assertInvalid overrides =
                    withEnvOverrides (clearWhatsAppProviderCredentialEnv ++ overrides) $
                        WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                            "WhatsApp API version must look like v20.0"
                                `isInfixOf` show (err :: IOException)
            assertInvalid
                [ ("WA_GRAPH_API_VERSION", Just "v21.0/messages")
                , ("WHATSAPP_API_VERSION", Just "v21.0")
                ]
            assertInvalid
                [ ("WA_GRAPH_API_VERSION", Just "   ")
                , ("WHATSAPP_API_VERSION", Just "2024-01")
                ]

        it "surfaces WhatsApp provider send failures before reporting enrollment success" $ do
            let success =
                    WhatsAppClient.SendTextResult
                        { WhatsAppClient.sendTextPayload = A.object []
                        , WhatsAppClient.sendTextMessageId = Just "wamid.ok"
                        }
            WhatsAppService.requireWhatsAppSendSuccess
                (Left "HTTP 401: bad token" :: Either String WhatsAppClient.SendTextResult)
                `shouldBe` Left "WhatsApp send failed: HTTP 401: bad token"
            WhatsAppService.requireWhatsAppSendSuccess (Right success)
                `shouldBe` Right (A.object ["ok" .= True])

        it "requires a usable WhatsApp provider message id before reporting enrollment success" $ do
            let result mMessageId =
                    WhatsAppClient.SendTextResult
                        { WhatsAppClient.sendTextPayload = A.object []
                        , WhatsAppClient.sendTextMessageId = mMessageId
                        }
            WhatsAppService.requireWhatsAppSendSuccess (Right (result Nothing))
                `shouldBe` Left "WhatsApp send failed: provider response did not include a message id"
            WhatsAppService.requireWhatsAppSendSuccess (Right (result (Just "   ")))
                `shouldBe` Left "WhatsApp send failed: provider response did not include a message id"

        it "normalizes WhatsApp provider message ids and ignores malformed candidates" $ do
            let payload =
                    A.object
                        [ "messages" .=
                            [ A.object ["id" .= ("   " :: Text)]
                            , A.object ["id" .= (" wamid.valid " :: Text)]
                            ]
                        ]
            WhatsAppClient.extractMessageId payload `shouldBe` Just "wamid.valid"
            let controlPayload =
                    A.object
                        [ "messages" .=
                            [ A.object ["id" .= ("wamid.\ninvalid" :: Text)]
                            ]
                        ]
            WhatsAppClient.extractMessageId controlPayload `shouldBe` Nothing
            let whitespacePayload =
                    A.object
                        [ "messages" .=
                            [ A.object ["id" .= ("wamid.invalid id" :: Text)]
                            ]
                        ]
            WhatsAppClient.extractMessageId whitespacePayload `shouldBe` Nothing

        it "rejects multiple WhatsApp provider message ids instead of choosing an ambiguous acknowledgement" $ do
            let payload =
                    A.object
                        [ "messages" .=
                            [ A.object ["id" .= ("wamid.first" :: Text)]
                            , A.object ["id" .= ("wamid.second" :: Text)]
                            ]
                        ]
            WhatsAppClient.extractMessageId payload `shouldBe` Nothing

        it "keeps DB_* connection settings authoritative when they are already configured" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq")
                , ("DB_HOST", Just "127.0.0.1")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "postgres")
                , ("DB_PASS", Just "postgres")
                , ("DB_NAME", Just "tdf_hq")
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "host=127.0.0.1 port=5432 user=postgres password=postgres dbname=tdf_hq target_session_attrs=read-write"

        it "requires keyword target_session_attrs to be a standalone connection option" $
            withEnvOverrides
                [ ("DATABASE_URL", Nothing)
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Just "tdf-hq-db.flycast")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "tdf_hq")
                , ("DB_PASS", Just "secret")
                , ("DB_NAME", Just "tdf_hq_target_session_attrs=debug")
                , ("DB_SSLMODE", Nothing)
                , ("PGSSLMODE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg
                        `shouldBe`
                            "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret "
                                <> "dbname=tdf_hq_target_session_attrs=debug "
                                <> "target_session_attrs=read-write"

        it "keeps fallback connection URL aliases from shaping complete DB_* settings" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "mysql://user:pass@db.internal:3306/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require")
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Just "tdf-hq-db.flycast")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "tdf_hq")
                , ("DB_PASS", Just "secret")
                , ("DB_NAME", Just "tdf_hq")
                , ("DB_SSLMODE", Nothing)
                , ("PGSSLMODE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret dbname=tdf_hq target_session_attrs=read-write"

        it "ignores sslmode from DATABASE_URL when DB_* vars stay authoritative" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=disable")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Just "tdf-hq-db.flycast")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "tdf_hq")
                , ("DB_PASS", Just "secret")
                , ("DB_NAME", Just "tdf_hq")
                , ("PGSSLMODE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret dbname=tdf_hq target_session_attrs=read-write"

        it "prefers explicit PGSSLMODE when DB_* vars stay authoritative" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Just "tdf-hq-db.flycast")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "tdf_hq")
                , ("DB_PASS", Just "secret")
                , ("DB_NAME", Just "tdf_hq")
                , ("PGSSLMODE", Just "disable")
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret dbname=tdf_hq sslmode=disable target_session_attrs=read-write"

        it "rejects malformed keyword sslmode values before building ambiguous DB connection strings" $
            withEnvOverrides
                [ ("DATABASE_URL", Nothing)
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Just "tdf-hq-db.flycast")
                , ("DB_PORT", Just "5432")
                , ("DB_USER", Just "tdf_hq")
                , ("DB_PASS", Just "secret")
                , ("DB_NAME", Just "tdf_hq")
                , ("DB_SSLMODE", Just "require target_session_attrs=any")
                , ("PGSSLMODE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DB_SSLMODE must be a single sslmode value"
                        `isInfixOf` show (err :: IOException)

        it "rejects keyword DB fields that could inject libpq connection options" $ do
            let baseKeywordDb =
                    [ ("DATABASE_URL", Nothing)
                    , ("DATABASE_PRIVATE_URL", Nothing)
                    , ("POSTGRES_URL", Nothing)
                    , ("POSTGRES_PRISMA_URL", Nothing)
                    , ("DB_HOST", Just "tdf-hq-db.flycast")
                    , ("DB_PORT", Just "5432")
                    , ("DB_USER", Just "tdf_hq")
                    , ("DB_PASS", Just "secret")
                    , ("DB_NAME", Just "tdf_hq")
                    , ("DB_SSLMODE", Nothing)
                    , ("PGSSLMODE", Nothing)
                    ]
                setOverride key value =
                    map
                        (\pair@(envKey, _) ->
                            if envKey == key then (envKey, Just value) else pair)
                        baseKeywordDb

            withEnvOverrides
                (setOverride "DB_HOST" "tdf-hq-db.flycast target_session_attrs=any")
                $ loadConfig `shouldThrow` \err ->
                    "DB_HOST/PGHOST must not contain whitespace or control characters"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                (setOverride "DB_PORT" "5432 target_session_attrs=any")
                $ loadConfig `shouldThrow` \err ->
                    "DB_PORT/PGPORT must be a port number between 1 and 65535"
                        `isInfixOf` show (err :: IOException)

        it "uses DATABASE_URL-style connection strings when keyword-style DB env vars are absent" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?target_session_attrs=read-write"

        it "keeps DATABASE_URL authoritative when only partial keyword DB env vars exist" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Just "pg.fly.internal")
                , ("PGPORT", Just "6543")
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?target_session_attrs=read-write"

        it "falls back to standard PG* env vars when DB_* vars are not configured" $
            withEnvOverrides
                [ ("DATABASE_URL", Nothing)
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Just "pg.fly.internal")
                , ("PGPORT", Just "6543")
                , ("PGUSER", Just "flyuser")
                , ("PGPASSWORD", Just "flypass")
                , ("PGDATABASE", Just "flydb")
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "host=pg.fly.internal port=6543 user=flyuser password=flypass dbname=flydb target_session_attrs=read-write"

        it "preserves an explicit read-write target_session_attrs setting on DATABASE_URL" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require&target_session_attrs=read-write")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ do
                    cfg <- loadConfig
                    dbConnString cfg `shouldBe` "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require&target_session_attrs=read-write"

        it "requires target_session_attrs to be an actual non-blank URL query parameter" $ do
            let baseUrl = "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                withoutKeywordDb databaseUrl =
                    [ ("DATABASE_URL", Just databaseUrl)
                    , ("DATABASE_PRIVATE_URL", Nothing)
                    , ("POSTGRES_URL", Nothing)
                    , ("POSTGRES_PRISMA_URL", Nothing)
                    , ("DB_HOST", Nothing)
                    , ("DB_PORT", Nothing)
                    , ("DB_USER", Nothing)
                    , ("DB_PASS", Nothing)
                    , ("DB_NAME", Nothing)
                    , ("PGHOST", Nothing)
                    , ("PGPORT", Nothing)
                    , ("PGUSER", Nothing)
                    , ("PGPASSWORD", Nothing)
                    , ("PGDATABASE", Nothing)
                    ]

            withEnvOverrides
                ( withoutKeywordDb
                    (baseUrl <> "?application_name=target_session_attrs=debug")
                )
                $ do
                    cfg <- loadConfig
                    dbConnString cfg
                        `shouldBe` baseUrl
                            <> "?application_name=target_session_attrs=debug"
                            <> "&target_session_attrs=read-write"

            withEnvOverrides
                (withoutKeywordDb (baseUrl <> "?target_session_attrs="))
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL target_session_attrs must not be blank"
                        `isInfixOf` show (err :: IOException)

        it "rejects explicit non-read-write target_session_attrs before fallback DB use" $ do
            let baseUrl = "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                withoutKeywordDb databaseUrl =
                    [ ("DATABASE_URL", Just databaseUrl)
                    , ("DATABASE_PRIVATE_URL", Nothing)
                    , ("POSTGRES_URL", Nothing)
                    , ("POSTGRES_PRISMA_URL", Nothing)
                    , ("DB_HOST", Nothing)
                    , ("DB_PORT", Nothing)
                    , ("DB_USER", Nothing)
                    , ("DB_PASS", Nothing)
                    , ("DB_NAME", Nothing)
                    , ("PGHOST", Nothing)
                    , ("PGPORT", Nothing)
                    , ("PGUSER", Nothing)
                    , ("PGPASSWORD", Nothing)
                    , ("PGDATABASE", Nothing)
                    ]
                expectRejected value =
                    withEnvOverrides
                        (withoutKeywordDb (baseUrl <> "?target_session_attrs=" <> value))
                        $ loadConfig `shouldThrow` \err ->
                            "DATABASE_URL target_session_attrs must be read-write"
                                `isInfixOf` show (err :: IOException)

            expectRejected "any"
            expectRejected "read-only"

        it "rejects blank or unsupported DATABASE_URL sslmode values before fallback use" $ do
            let baseUrl = "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                expectInvalid databaseUrl expected =
                    withEnvOverrides
                        [ ("DATABASE_URL", Just databaseUrl)
                        , ("DATABASE_PRIVATE_URL", Nothing)
                        , ("POSTGRES_URL", Nothing)
                        , ("POSTGRES_PRISMA_URL", Nothing)
                        , ("DB_HOST", Nothing)
                        , ("DB_PORT", Nothing)
                        , ("DB_USER", Nothing)
                        , ("DB_PASS", Nothing)
                        , ("DB_NAME", Nothing)
                        , ("PGHOST", Nothing)
                        , ("PGPORT", Nothing)
                        , ("PGUSER", Nothing)
                        , ("PGPASSWORD", Nothing)
                        , ("PGDATABASE", Nothing)
                        , ("DB_SSLMODE", Nothing)
                        , ("PGSSLMODE", Nothing)
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            expected `isInfixOf` show (err :: IOException)

            expectInvalid
                (baseUrl <> "?sslmode=")
                "DATABASE_URL sslmode must not be blank"
            expectInvalid
                (baseUrl <> "?sslmode=require%20target_session_attrs=any")
                "DATABASE_URL sslmode must be one of"

        it "rejects duplicate DATABASE_URL query invariants before fallback use" $ do
            let baseUrl = "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                withoutKeywordDb databaseUrl =
                    [ ("DATABASE_URL", Just databaseUrl)
                    , ("DATABASE_PRIVATE_URL", Nothing)
                    , ("POSTGRES_URL", Nothing)
                    , ("POSTGRES_PRISMA_URL", Nothing)
                    , ("DB_HOST", Nothing)
                    , ("DB_PORT", Nothing)
                    , ("DB_USER", Nothing)
                    , ("DB_PASS", Nothing)
                    , ("DB_NAME", Nothing)
                    , ("PGHOST", Nothing)
                    , ("PGPORT", Nothing)
                    , ("PGUSER", Nothing)
                    , ("PGPASSWORD", Nothing)
                    , ("PGDATABASE", Nothing)
                    , ("DB_SSLMODE", Nothing)
                    , ("PGSSLMODE", Nothing)
                    ]
                expectInvalid databaseUrl expected =
                    withEnvOverrides (withoutKeywordDb databaseUrl)
                        $ loadConfig `shouldThrow` \err ->
                            expected `isInfixOf` show (err :: IOException)

            expectInvalid
                (baseUrl <> "?sslmode=require&sslmode=disable")
                "DATABASE_URL sslmode must be provided at most once"
            expectInvalid
                (baseUrl <> "?target_session_attrs=read-write&target_session_attrs=read-write")
                "DATABASE_URL target_session_attrs must be provided at most once"

        it "rejects blank DATABASE_URL query parameter names before fallback use" $ do
            let baseUrl = "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                withoutKeywordDb databaseUrl =
                    [ ("DATABASE_URL", Just databaseUrl)
                    , ("DATABASE_PRIVATE_URL", Nothing)
                    , ("POSTGRES_URL", Nothing)
                    , ("POSTGRES_PRISMA_URL", Nothing)
                    , ("DB_HOST", Nothing)
                    , ("DB_PORT", Nothing)
                    , ("DB_USER", Nothing)
                    , ("DB_PASS", Nothing)
                    , ("DB_NAME", Nothing)
                    , ("PGHOST", Nothing)
                    , ("PGPORT", Nothing)
                    , ("PGUSER", Nothing)
                    , ("PGPASSWORD", Nothing)
                    , ("PGDATABASE", Nothing)
                    , ("DB_SSLMODE", Nothing)
                    , ("PGSSLMODE", Nothing)
                    ]
                expectInvalid databaseUrl =
                    withEnvOverrides (withoutKeywordDb databaseUrl)
                        $ loadConfig `shouldThrow` \err ->
                            "DATABASE_URL query parameters must include names"
                                `isInfixOf` show (err :: IOException)

            expectInvalid (baseUrl <> "?")
            expectInvalid (baseUrl <> "?sslmode=require&")
            expectInvalid (baseUrl <> "?=value")

        it "rejects unsupported DATABASE_URL schemes before building ambiguous DB connection strings" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "mysql://user:pass@db.internal:3306/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL must use postgres:// or postgresql://"
                        `isInfixOf` show (err :: IOException)

        it "rejects DATABASE_URL fallback values with userinfo but no host" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@:5432/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL must include a PostgreSQL host"
                        `isInfixOf` show (err :: IOException)

        it "rejects DATABASE_URL fallback values with malformed bracketed hosts" $ do
            let expectInvalidHost raw =
                    withEnvOverrides
                        [ ("DATABASE_URL", Just raw)
                        , ("DATABASE_PRIVATE_URL", Nothing)
                        , ("POSTGRES_URL", Nothing)
                        , ("POSTGRES_PRISMA_URL", Nothing)
                        , ("DB_HOST", Nothing)
                        , ("DB_PORT", Nothing)
                        , ("DB_USER", Nothing)
                        , ("DB_PASS", Nothing)
                        , ("DB_NAME", Nothing)
                        , ("PGHOST", Nothing)
                        , ("PGPORT", Nothing)
                        , ("PGUSER", Nothing)
                        , ("PGPASSWORD", Nothing)
                        , ("PGDATABASE", Nothing)
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            "DATABASE_URL must include a valid PostgreSQL host"
                                `isInfixOf` show (err :: IOException)
            expectInvalidHost "postgresql://flyuser:flypass@[db.fly.internal]:5432/tdf_hq"
            expectInvalidHost "postgresql://flyuser:flypass@[2001:::1]:5432/tdf_hq"

        it "rejects DATABASE_URL fallback values without an explicit database name" $ do
            let expectMissingDatabase raw =
                    withEnvOverrides
                        [ ("DATABASE_URL", Just raw)
                        , ("DATABASE_PRIVATE_URL", Nothing)
                        , ("POSTGRES_URL", Nothing)
                        , ("POSTGRES_PRISMA_URL", Nothing)
                        , ("DB_HOST", Nothing)
                        , ("DB_PORT", Nothing)
                        , ("DB_USER", Nothing)
                        , ("DB_PASS", Nothing)
                        , ("DB_NAME", Nothing)
                        , ("PGHOST", Nothing)
                        , ("PGPORT", Nothing)
                        , ("PGUSER", Nothing)
                        , ("PGPASSWORD", Nothing)
                        , ("PGDATABASE", Nothing)
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            "DATABASE_URL must include a database name"
                                `isInfixOf` show (err :: IOException)
            expectMissingDatabase "postgresql://flyuser:flypass@db.fly.internal:5432"
            expectMissingDatabase "postgresql://flyuser:flypass@db.fly.internal:5432?sslmode=require"
            expectMissingDatabase "postgresql://flyuser:flypass@db.fly.internal:5432/"

        it "rejects DATABASE_URL fallback values with ambiguous userinfo separators" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:fly@pass@db.fly.internal:5432/tdf_hq")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL must not contain multiple @ separators"
                        `isInfixOf` show (err :: IOException)

        it "rejects DATABASE_URL fallback values with fragments before building ambiguous DB connection strings" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq#primary")
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL must not include a fragment"
                        `isInfixOf` show (err :: IOException)

        it "rejects DATABASE_URL fallback values with control characters before building DB connection strings" $ do
            let databaseUrl =
                    "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq"
                        <> "\SOHtarget_session_attrs=any"
            withEnvOverrides
                [ ("DATABASE_URL", Just databaseUrl)
                , ("DATABASE_PRIVATE_URL", Nothing)
                , ("POSTGRES_URL", Nothing)
                , ("POSTGRES_PRISMA_URL", Nothing)
                , ("DB_HOST", Nothing)
                , ("DB_PORT", Nothing)
                , ("DB_USER", Nothing)
                , ("DB_PASS", Nothing)
                , ("DB_NAME", Nothing)
                , ("PGHOST", Nothing)
                , ("PGPORT", Nothing)
                , ("PGUSER", Nothing)
                , ("PGPASSWORD", Nothing)
                , ("PGDATABASE", Nothing)
                ]
                $ loadConfig `shouldThrow` \err ->
                    "DATABASE_URL must not contain control characters"
                        `isInfixOf` show (err :: IOException)

        it "rejects DATABASE_URL fallback values with malformed ports before opening DB connections" $ do
            let expectInvalidPort raw expected =
                    withEnvOverrides
                        [ ("DATABASE_URL", Just raw)
                        , ("DATABASE_PRIVATE_URL", Nothing)
                        , ("POSTGRES_URL", Nothing)
                        , ("POSTGRES_PRISMA_URL", Nothing)
                        , ("DB_HOST", Nothing)
                        , ("DB_PORT", Nothing)
                        , ("DB_USER", Nothing)
                        , ("DB_PASS", Nothing)
                        , ("DB_NAME", Nothing)
                        , ("PGHOST", Nothing)
                        , ("PGPORT", Nothing)
                        , ("PGUSER", Nothing)
                        , ("PGPASSWORD", Nothing)
                        , ("PGDATABASE", Nothing)
                        ]
                        $ loadConfig `shouldThrow` \err ->
                            expected `isInfixOf` show (err :: IOException)

            expectInvalidPort
                "postgresql://flyuser:flypass@db.fly.internal:pg/tdf_hq"
                "DATABASE_URL port must be numeric"
            expectInvalidPort
                "postgresql://flyuser:flypass@db.fly.internal:70000/tdf_hq"
                "DATABASE_URL port must be between 1 and 65535"

    describe "Instagram messaging context fallback" $ do
        let accountIdError label =
                label
                    <> " must be a Graph node id using only ASCII letters, numbers, "
                    <> "'.', '_' or '-' with at least one letter or number "
                    <> "(128 chars max)"

        it "rejects malformed Instagram send payloads before building Graph requests" $
            withEnvOverrides
                [ ("INSTAGRAM_APP_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_TOKEN", Just "configured-token")
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Just "configured-account")
                , ("INSTAGRAM_MESSAGING_API_BASE", Just "https://graph.example.com")
                ]
                $ do
                    cfg <- loadConfig
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "   "
                        "hola"
                        `shouldReturn` Left "Instagram recipient id requerido"
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "recipient 1"
                        "hola"
                        `shouldReturn` Left "Instagram recipient id must not contain whitespace"
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "recipient-1"
                        "   "
                        `shouldReturn` Left "Instagram message body requerido"
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "recipient-1"
                        (Data.Text.replicate 5001 "a")
                        `shouldReturn` Left "Instagram message body must be 5000 characters or fewer"
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "recipient-1"
                        "hola\NULops"
                        `shouldReturn` Left "Instagram message body must not contain control characters"

        it "does not use the configured fallback token when a targeted account has no connected token" $
            withEnvOverrides
                [ ("INSTAGRAM_APP_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_TOKEN", Just "configured-token")
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Just "configured-account")
                , ("INSTAGRAM_MESSAGING_API_BASE", Just "https://graph.example.com")
                ]
                $ do
                    cfg <- loadConfig
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        (Just "biz-missing")
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left "Instagram connected asset token no configurado"

        it "rejects malformed targeted Instagram context instead of falling back to configured credentials" $
            withEnvOverrides
                [ ("INSTAGRAM_APP_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_TOKEN", Just "configured-token")
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Just "configured-account")
                , ("INSTAGRAM_MESSAGING_API_BASE", Just "https://graph.example.com")
                ]
                $ do
                    cfg <- loadConfig
                    sendInstagramTextWithContext
                        cfg
                        (Just "connected token")
                        (Just "biz-account")
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left
                            "Instagram connected asset token must not contain whitespace or control characters"
                    sendInstagramTextWithContext
                        cfg
                        (Just "connected-token")
                        (Just "biz/account")
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left
                            (accountIdError "Instagram connected asset account id")
                    sendInstagramTextWithContext
                        cfg
                        (Just "connected-token")
                        (Just "---")
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left
                            (accountIdError "Instagram connected asset account id")

        it "requires an account id before building Instagram messaging send attempts" $ do
            withEnvOverrides
                [ ("INSTAGRAM_APP_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_TOKEN", Just "configured-token")
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Nothing)
                , ("INSTAGRAM_MESSAGING_API_BASE", Just "https://graph.example.com")
                ]
                $ do
                    cfg <- loadConfig
                    sendInstagramTextWithContext
                        cfg
                        Nothing
                        Nothing
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left "INSTAGRAM_MESSAGING_ACCOUNT_ID no configurado"

            withEnvOverrides
                [ ("INSTAGRAM_APP_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_TOKEN", Nothing)
                , ("INSTAGRAM_MESSAGING_ACCOUNT_ID", Nothing)
                , ("INSTAGRAM_MESSAGING_API_BASE", Just "https://graph.example.com")
                ]
                $ do
                    cfg <- loadConfig
                    sendInstagramTextWithContext
                        cfg
                        (Just "connected-token")
                        Nothing
                        "recipient-1"
                        "hola"
                        `shouldReturn` Left "Instagram connected asset account id no configurado"

    describe "SRI invoice script discovery" $ do
        it "decodes UTF-8 SRI script JSON output without corrupting localized statuses" $
            case Sri.decodeSriScriptOutput "{\"ok\":true,\"status\":\"autorización emitida\"}" of
                Left err ->
                    expectationFailure
                        ( "Expected UTF-8 SRI script output to decode, got: "
                            <> Data.Text.unpack err
                        )
                Right result ->
                    DTO.sirStatus result `shouldBe` "autorización emitida"

        it "rejects blank or control-character SRI statuses before invoice results are trusted" $ do
            let assertInvalid expected raw =
                    case Sri.decodeSriScriptOutput raw of
                        Left err ->
                            Data.Text.unpack err `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected malformed SRI script output to fail, got: " <> show value)
            assertInvalid
                "status is required"
                "{\"ok\":true,\"status\":\"   \"}"
            assertInvalid
                "status must not contain control characters"
                "{\"ok\":true,\"status\":\"autorizado\\nextra\"}"

        it "requires issued SRI results to carry confirmed document identifiers" $ do
            let assertInvalid expected raw =
                    case Sri.decodeSriScriptOutput raw of
                        Left err ->
                            Data.Text.unpack err `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected inconsistent SRI script output to fail, got: "
                                    <> show value
                                )
            assertInvalid
                "ok must be true when status is issued"
                ( "{\"ok\":false,\"status\":\"issued\","
                    <> "\"authorizationNumber\":\"123\","
                    <> "\"invoiceNumber\":\"001-100-000000001\"}"
                )
            assertInvalid
                "authorizationNumber is required when status is issued"
                "{\"ok\":true,\"status\":\"issued\",\"invoiceNumber\":\"001-100-000000001\"}"
            assertInvalid
                "invoiceNumber is required when status is issued"
                ( "{\"ok\":true,\"status\":\"issued\","
                    <> "\"authorizationNumber\":\"123\",\"invoiceNumber\":\"  \"}"
                )
            let completeIssued =
                    "{\"ok\":true,\"status\":\"issued\","
                        <> "\"authorizationNumber\":\" 123 \","
                        <> "\"invoiceNumber\":\" 001-100-000000001 \"}"
            case Sri.decodeSriScriptOutput completeIssued of
                Left err ->
                    expectationFailure
                        ( "Expected complete issued SRI script output to decode, got: "
                            <> Data.Text.unpack err
                        )
                Right result -> do
                    DTO.sirAuthorizationNumber result `shouldBe` Just "123"
                    DTO.sirInvoiceNumber result `shouldBe` Just "001-100-000000001"

        it "rejects negative SRI totals before invoice results are trusted" $
            case Sri.decodeSriScriptOutput
                ( "{\"ok\":true,\"status\":\"issued\","
                    <> "\"total\":-1,"
                    <> "\"authorizationNumber\":\"123\","
                    <> "\"invoiceNumber\":\"001-100-000000001\"}"
                ) of
                Left err ->
                    Data.Text.unpack err
                        `shouldContain` "total must be a finite non-negative number"
                Right value ->
                    expectationFailure
                        ( "Expected negative SRI script total to fail, got: "
                            <> show value
                        )

        it "rejects unexpected top-level or nested SRI keys so script schema drift fails explicitly" $ do
            let assertInvalid raw =
                    case Sri.decodeSriScriptOutput raw of
                        Left err ->
                            Data.Text.unpack err `shouldContain` "Invalid SRI script JSON output"
                        Right value ->
                            expectationFailure
                                ( "Expected unexpected SRI script keys to fail, got: "
                                    <> show value
                                )
            assertInvalid
                ( "{\"ok\":true,\"status\":\"issued\","
                    <> "\"authorizationNumber\":\"123\","
                    <> "\"invoiceNumber\":\"001-100-000000001\","
                    <> "\"traceId\":\"abc\"}"
                )
            assertInvalid
                ( "{\"ok\":true,\"status\":\"autorización emitida\","
                    <> "\"buyer\":{"
                    <> "\"ruc\":\"0999999999001\","
                    <> "\"legalName\":\"Cliente Demo\","
                    <> "\"country\":\"EC\"}}"
                )

        it "rejects malformed SRI requests before script discovery masks payload errors" $
            withEnvOverrides
                [ ( "SRI_INVOICE_SCRIPT"
                  , Just "/tmp/tdf-hq-missing-sri-script-never-created.mjs"
                  )
                ]
                $ do
                    result <-
                        Sri.runSriInvoiceScript
                            sampleSriScriptRequest { Sri.paymentMode = "wire" }
                    case result of
                        Left err ->
                            Data.Text.unpack err
                                `shouldContain` "paymentMode must be one of: cash, debit, credit"
                        Right value ->
                            expectationFailure
                                ("Expected invalid SRI payment mode to fail, got: " <> show value)

        it "requires explicit SRI IVA codes for tax rates the runner cannot infer" $ do
            let unsupportedTaxLine =
                    sampleSriScriptLine { Sri.taxBps = Just 1200 }
                explicitIvaLine =
                    unsupportedTaxLine { Sri.sriIvaCode = Just "2" }
            case Sri.validateSriScriptRequest
                sampleSriScriptRequest { Sri.lines = [unsupportedTaxLine] } of
                Left err ->
                    Data.Text.unpack err
                        `shouldContain` "taxBps must be 0, 500, or 1500 unless sriIvaCode is provided"
                Right value ->
                    expectationFailure
                        ( "Expected unsupported inferred SRI tax rate to fail, got: "
                            <> show value
                        )
            case Sri.validateSriScriptRequest
                sampleSriScriptRequest { Sri.lines = [explicitIvaLine] } of
                Left err ->
                    expectationFailure
                        ( "Expected explicit SRI IVA code to allow custom tax bps, got: "
                            <> Data.Text.unpack err
                        )
                Right validated ->
                    case Sri.lines validated of
                        [line] -> do
                            Sri.taxBps line `shouldBe` Just 1200
                            Sri.sriIvaCode line `shouldBe` Just "2"
                        linesValue ->
                            expectationFailure
                                ( "Expected one validated SRI line, got: "
                                    <> show linesValue
                                )

        it "requires three-digit SRI establishment and emission point codes before script discovery" $ do
            let assertInvalid expected request =
                    case Sri.validateSriScriptRequest request of
                        Left err ->
                            Data.Text.unpack err `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected malformed SRI point code to fail, got: "
                                    <> show value
                                )
            assertInvalid
                "establishment must contain exactly 3 digits"
                sampleSriScriptRequest { Sri.establishment = "1" }
            assertInvalid
                "emissionPoint must contain exactly 3 digits"
                sampleSriScriptRequest { Sri.emissionPoint = "0100" }
            case Sri.validateSriScriptRequest
                sampleSriScriptRequest
                    { Sri.establishment = " 001 "
                    , Sri.emissionPoint = " 002 "
                    } of
                Left err ->
                    expectationFailure
                        ( "Expected fixed-width SRI point codes to validate, got: "
                            <> Data.Text.unpack err
                        )
                Right validated -> do
                    Sri.establishment validated `shouldBe` "001"
                    Sri.emissionPoint validated `shouldBe` "002"

        it "keeps explicit SRI_INVOICE_SCRIPT paths authoritative when they are missing" $
            withEnvOverrides
                [ ( "SRI_INVOICE_SCRIPT"
                  , Just "/tmp/tdf-hq-missing-sri-script-never-created.mjs"
                  )
                ]
                $ do
                    let expected = "SRI_INVOICE_SCRIPT does not point to an existing file"
                    result <- Sri.runSriInvoiceScript sampleSriScriptRequest
                    case result of
                        Left err ->
                            Data.Text.unpack err `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected missing SRI script path to fail, got: " <> show value)

        it "rejects blank SRI_INVOICE_SCRIPT instead of falling through to default discovery" $
            withEnvOverrides [("SRI_INVOICE_SCRIPT", Just "   ")] $ do
                result <- Sri.runSriInvoiceScript sampleSriScriptRequest
                case result of
                    Left err ->
                        Data.Text.unpack err
                            `shouldContain` "SRI_INVOICE_SCRIPT must not be blank"
                    Right value ->
                        expectationFailure
                            ("Expected blank SRI script path to fail, got: " <> show value)

        it "rejects existing non-JavaScript script paths before invoking node" $
            withSystemTempFile "tdf-sri-script.txt" $ \scriptPath handle -> do
                hClose handle
                withEnvOverrides [("SRI_INVOICE_SCRIPT", Just scriptPath)] $ do
                    result <- Sri.runSriInvoiceScript sampleSriScriptRequest
                    case result of
                        Left err ->
                            Data.Text.unpack err
                                `shouldContain` "SRI_INVOICE_SCRIPT must point to a .mjs, .js, or .cjs Node script"
                        Right value ->
                            expectationFailure
                                ("Expected invalid SRI script path to fail, got: " <> show value)

        it "returns an explicit error when the configured Node runner cannot be started" $
            withSystemTempFile "tdf-sri-script.mjs" $ \scriptPath handle -> do
                hClose handle
                withEnvOverrides
                    [ ("SRI_INVOICE_SCRIPT", Just scriptPath)
                    , ("PATH", Just "/tmp/tdf-hq-node-missing-never-created")
                    ]
                    $ do
                        result <- Sri.runSriInvoiceScript sampleSriScriptRequest
                        case result of
                            Left err ->
                                Data.Text.unpack err
                                    `shouldContain` "SRI invoice script could not be started"
                            Right value ->
                                expectationFailure
                                    ("Expected missing Node runner to fail, got: " <> show value)

    describe "CORS environment fallback discovery" $ do
        it "falls through unset or blank primary names to documented CORS aliases" $
            withEnvOverrides
                [ ("ALLOWED_ORIGINS", Just "   ")
                , ("ALLOW_ORIGINS", Just "https://app.example.com")
                , ("ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_ALLOW_ALL_ORIGINS", Just "true")
                , ("CORS_DISABLE_DEFAULTS", Nothing)
                , ("DISABLE_DEFAULT_CORS", Just "1")
                ]
                $ do
                    lookupFirstNonEmptyEnv ["ALLOWED_ORIGINS", "ALLOW_ORIGINS"]
                        `shouldReturn` Just "https://app.example.com"
                    lookupFirstNonEmptyEnv ["ALLOW_ALL_ORIGINS", "CORS_ALLOW_ALL_ORIGINS"]
                        `shouldReturn` Just "true"
                    lookupFirstNonEmptyEnv ["CORS_DISABLE_DEFAULTS", "DISABLE_DEFAULT_CORS"]
                        `shouldReturn` Just "1"

        it "derives origin-only defaults from configured app fallback bases" $ do
            deriveCorsOriginFromAppBase " https://hq.example.com/app/ "
                `shouldBe` Right "https://hq.example.com"
            deriveCorsOriginFromAppBase "http://localhost:5173/hq"
                `shouldBe` Right "http://localhost:5173"
            case deriveCorsOriginFromAppBase "https://hq.example.com/app?preview=1" of
                Left msg -> msg `shouldContain` "HQ_APP_URL CORS fallback"
                Right origin ->
                    expectationFailure
                        ("Expected query-bearing fallback to fail, got: " <> origin)

        it "rejects malformed configured origins before building the credentialed policy" $ do
            let assertInvalid rawOrigin =
                    withEnvOverrides
                        [ ("ALLOWED_ORIGINS", Just rawOrigin)
                        , ("ALLOW_ORIGINS", Nothing)
                        , ("ALLOW_ORIGIN", Nothing)
                        , ("CORS_ALLOW_ORIGINS", Nothing)
                        , ("CORS_ALLOW_ORIGIN", Nothing)
                        , ("ALLOW_ALL_ORIGINS", Nothing)
                        , ("CORS_ALLOW_ALL_ORIGINS", Nothing)
                        ]
                        $ corsPolicy `shouldThrow` \err ->
                            "Configured CORS origins must be absolute http(s) origins"
                                `isInfixOf` show (err :: IOException)
            assertInvalid "https://app.example.com/admin"
            assertInvalid "javascript:alert(1)"
            assertInvalid "https://999.999.999.999"
            assertInvalid "https://1.2.3"

        it "rejects malformed boolean CORS flags instead of treating typos as false" $ do
            let baseOverrides =
                    [ ("ALLOWED_ORIGINS", Nothing)
                    , ("ALLOW_ORIGINS", Nothing)
                    , ("ALLOW_ORIGIN", Nothing)
                    , ("CORS_ALLOW_ORIGINS", Nothing)
                    , ("CORS_ALLOW_ORIGIN", Nothing)
                    , ("HQ_APP_URL", Nothing)
                    ]
                assertInvalid flagOverrides expectedMessage =
                    withEnvOverrides (baseOverrides <> flagOverrides) $
                        corsPolicy `shouldThrow` \err ->
                            expectedMessage `isInfixOf` show (err :: IOException)
            assertInvalid
                [ ("ALLOW_ALL_ORIGINS", Just "treu")
                , ("CORS_ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_DISABLE_DEFAULTS", Nothing)
                , ("DISABLE_DEFAULT_CORS", Nothing)
                ]
                "ALLOW_ALL_ORIGINS must be a boolean CORS flag"
            assertInvalid
                [ ("ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_ALLOW_ALL_ORIGINS", Nothing)
                , ("CORS_DISABLE_DEFAULTS", Just "maybe")
                , ("DISABLE_DEFAULT_CORS", Nothing)
                ]
                "CORS_DISABLE_DEFAULTS must be a boolean CORS flag"

    describe "CORS trusted preview origins" $ do
        it "allows only the known TDF Pages projects and their preview subdomains" $ do
            isTrustedPreviewOrigin "https://tdfui.pages.dev" `shouldBe` True
            isTrustedPreviewOrigin "https://preview.tdfui.pages.dev" `shouldBe` True
            isTrustedPreviewOrigin "https://tdf-app.pages.dev" `shouldBe` True
            isTrustedPreviewOrigin "https://branch.tdf-app.pages.dev" `shouldBe` True

        it "rejects arbitrary shared preview hosts instead of granting credentialed CORS broadly" $ do
            isTrustedPreviewOrigin "https://attacker.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://tdf-app.pages.dev.evil.example" `shouldBe` False
            isTrustedPreviewOrigin "http://preview.tdf-app.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://attacker.vercel.app" `shouldBe` False
            isTrustedPreviewOrigin "https://.tdf-app.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://preview..tdf-app.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://bad-.tdf-app.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://bad_.tdf-app.pages.dev" `shouldBe` False
            isTrustedPreviewOrigin "https://tdf-app.pages.dev:8443" `shouldBe` False
            isTrustedPreviewOrigin "https://branch.tdf-app.pages.dev:443" `shouldBe` False

    describe "extractToken" $ do
        let loadAuthConfig =
                withEnvOverrides [("SESSION_COOKIE_NAME", Just "tdf_session_test")] loadConfig
            requestWithHeaders headers =
                defaultRequest { requestHeaders = headers }

        it "accepts matching bearer and session cookie tokens without ambiguity" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Bearer shared-token")
                    , ("Cookie", "tdf_session_test=shared-token")
                    ])
                `shouldBe` Right "shared-token"

        it "rejects conflicting bearer and session cookie tokens instead of silently choosing one credential" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Bearer header-token")
                    , ("Cookie", "tdf_session_test=cookie-token")
                    ])
                `shouldBe` Left "Conflicting auth credentials found"

        it "uses the configured session cookie when no Authorization header is present" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Cookie", "other=1; tdf_session_test = cookie-token ; foo=bar")
                    ])
                `shouldBe` Right "cookie-token"

        it "rejects malformed Authorization headers instead of silently falling through to the cookie" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Token header-token")
                    , ("Cookie", "tdf_session_test=cookie-token")
                    ])
                `shouldBe` Left "Invalid Authorization header"

        it "rejects malformed bearer token values before auth DB lookup" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Bearer header\NULtoken")
                    , ("Cookie", "tdf_session_test=cookie-token")
                    ])
                `shouldBe` Left "Missing or invalid auth token"

        it "rejects duplicate Authorization headers instead of choosing an ambiguous token" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Bearer first-token")
                    , ("Authorization", "Bearer second-token")
                    , ("Cookie", "tdf_session_test=cookie-token")
                    ])
                `shouldBe` Left "Multiple Authorization headers found"

        it "rejects duplicate Cookie headers instead of choosing an ambiguous session token" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Cookie", "tdf_session_test=old-token")
                    , ("Cookie", "tdf_session_test=new-token")
                    ])
                `shouldBe` Left "Multiple Cookie headers found"

    describe "extractTokenFromHeaders" $ do
        let loadAuthConfig =
                withEnvOverrides [("SESSION_COOKIE_NAME", Just "tdf_session_test")] loadConfig

        it "uses the provided cookie header for anonymous-safe session lookups" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                Nothing
                (Just "other=1; tdf_session_test = cookie-token ; foo=bar")
                `shouldBe` Right "cookie-token"

        it "rejects duplicate session cookies instead of choosing an ambiguous token" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                Nothing
                (Just "tdf_session_test=old-token; tdf_session_test=new-token")
                `shouldBe` Left "Multiple session cookies found"

        it "rejects duplicate session cookie names even when one value is blank" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                Nothing
                (Just "tdf_session_test=active-token; tdf_session_test=   ")
                `shouldBe` Left "Multiple session cookies found"

        it "rejects malformed session cookie token values before auth DB lookup" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                Nothing
                (Just "tdf_session_test=cookie token")
                `shouldBe` Left "Missing or invalid auth token"

        it "keeps malformed authorization headers authoritative over cookies" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                (Just "Token header-token")
                (Just "tdf_session_test=cookie-token")
                `shouldBe` Left "Invalid Authorization header"

        it "rejects conflicting valid authorization and session cookie tokens" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                (Just "Bearer header-token")
                (Just "tdf_session_test=cookie-token")
                `shouldBe` Left "Conflicting auth credentials found"

    describe "resolveInstagramRedirectUri" $ do
        let loadInstagramConfig =
                withEnvOverrides [("HQ_APP_URL", Just "https://hq.example.com/admin")] loadConfig

        it "uses the configured Instagram callback fallback when the request omits redirectUri" $ do
            cfg <- loadInstagramConfig
            resolveInstagramRedirectUri cfg Nothing
                `shouldBe` Right "https://hq.example.com/admin/oauth/instagram/callback"
            resolveInstagramRedirectUri cfg (Just "   ")
                `shouldBe` Right "https://hq.example.com/admin/oauth/instagram/callback"

        it "normalizes valid explicit Instagram redirect URIs before token exchange" $ do
            cfg <- loadInstagramConfig
            resolveInstagramRedirectUri
                cfg
                (Just " https://tdf-app.pages.dev/oauth/instagram/callback ")
                `shouldBe` Right "https://tdf-app.pages.dev/oauth/instagram/callback"

        it "requires social inbox access before exchanging Instagram OAuth codes" $ do
            let fanUser =
                    AuthedUser
                        { auPartyId = toSqlKey 7
                        , auRoles = [Fan]
                        , auModules = modulesForRoles [Fan]
                        }
                payload =
                    InstagramOAuth.InstagramOAuthExchangeRequest
                        "oauth-code-123"
                        Nothing
                handler =
                    instagramOAuthServer fanUser
                        :: ServerT
                             InstagramOAuth.InstagramOAuthAPI
                             (ReaderT Env (ExceptT ServerError IO))
                unusedEnv =
                    Env
                        { envPool = error "envPool should be unused by Instagram OAuth authorization rejection"
                        , envConfig = error "envConfig should be unused by Instagram OAuth authorization rejection"
                        }
            result <- runExceptT (runReaderT (handler payload) unusedEnv)
            case result of
                Left serverErr -> do
                    errHTTPCode serverErr `shouldBe` 403
                    BL.unpack (errBody serverErr) `shouldContain` "Missing required module access"
                Right value ->
                    expectationFailure
                        ("Expected unauthorized Instagram OAuth exchange to be rejected, got " <> show value)

        it "rejects malformed explicit Instagram redirect URIs before contacting Facebook" $ do
            cfg <- loadInstagramConfig
            let assertInvalid rawRedirect =
                    case resolveInstagramRedirectUri cfg (Just rawRedirect) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL.unpack (errBody serverErr)
                                `shouldContain`
                                    "redirectUri must be an absolute http(s) Instagram OAuth callback URL without query or fragment"
                        Right value ->
                            expectationFailure
                                ("Expected invalid Instagram redirectUri to be rejected, got " <> show value)
            assertInvalid "/oauth/instagram/callback"
            assertInvalid "javascript:alert(1)"
            assertInvalid "https://tdf-app.pages.dev/oauth/instagram/other"
            assertInvalid "https://tdf-app.pages.dev/oauth/instagram/callback?next=/admin"
            assertInvalid "https://tdf-app.pages.dev/oauth/instagram/callback#token"

        it "rejects explicit Instagram redirect URIs that do not match the configured callback" $ do
            cfg <- loadInstagramConfig
            let assertMismatch rawRedirect =
                    case resolveInstagramRedirectUri cfg (Just rawRedirect) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL.unpack (errBody serverErr)
                                `shouldContain`
                                    "redirectUri must match the configured Instagram OAuth callback URL"
                        Right value ->
                            expectationFailure
                                ("Expected mismatched Instagram redirectUri to be rejected, got " <> show value)
            assertMismatch "https://tdf-app.pages.dev/oauth/instagram/callback"
            assertMismatch "https://hq.example.com/oauth/instagram/callback"

    describe "WhatsApp consent payloads" $ do
        it "accept canonical public consent and opt-out bodies" $ do
            case eitherDecode
                "{\"phone\":\"+593991234567\",\"name\":\"Ada\",\"consent\":true,\"source\":\"landing\",\"sendMessage\":false}" of
                Left err ->
                    expectationFailure ("Expected canonical consent payload to decode, got: " <> err)
                Right payload -> do
                    wcrPhone payload `shouldBe` "+593991234567"
                    wcrName payload `shouldBe` Just "Ada"
                    wcrConsent payload `shouldBe` True
                    wcrSource payload `shouldBe` Just "landing"
                    wcrSendMessage payload `shouldBe` Just False

            case eitherDecode
                "{\"phone\":\"+593991234567\",\"reason\":\"stop\",\"sendMessage\":true}" of
                Left err ->
                    expectationFailure ("Expected canonical opt-out payload to decode, got: " <> err)
                Right payload -> do
                    worPhone payload `shouldBe` "+593991234567"
                    worReason payload `shouldBe` Just "stop"
                    worSendMessage payload `shouldBe` Just True

        it "rejects unknown consent or opt-out keys so typoed public requests fail explicitly" $ do
            isLeft
                ( eitherDecode
                    "{\"phone\":\"+593991234567\",\"consent\":true,\"sendmessage\":false}"
                    :: Either String WhatsAppConsentRequest
                )
                `shouldBe` True
            isLeft
                ( eitherDecode
                    "{\"phone\":\"+593991234567\",\"reason\":\"stop\",\"send_message\":true}"
                    :: Either String WhatsAppOptOutRequest
                )
                `shouldBe` True

        it "normalizes optional consent metadata before storage or confirmation messages" $ do
            validateWhatsAppConsentDisplayName (Just "  Ada  ")
                `shouldBe` Right (Just "Ada")
            validateWhatsAppConsentDisplayName (Just "   ")
                `shouldBe` Right Nothing
            validateWhatsAppConsentSource "public" Nothing
                `shouldBe` Right (Just "public")
            validateWhatsAppConsentSource "public" (Just "  landing  ")
                `shouldBe` Right (Just "landing")
            validateWhatsAppOptOutReason (Just "  stop  ")
                `shouldBe` Right (Just "stop")

        it "rejects oversized or control-character consent metadata explicitly" $ do
            let assertInvalid expectedMessage result =
                    case result of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL.unpack (errBody serverErr) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected WhatsApp consent metadata rejection, got " <> show value)
            assertInvalid
                "name is too long"
                ( validateWhatsAppConsentDisplayName
                    (Just (Data.Text.replicate 121 "a"))
                )
            assertInvalid
                "source must not contain control characters"
                (validateWhatsAppConsentSource "public" (Just "landing\npage"))
            assertInvalid
                "reason is too long"
                ( validateWhatsAppOptOutReason
                    (Just (Data.Text.replicate 501 "x"))
                )

    describe "normalizeOptionalFeedbackText" $ do
        it "trims meaningful optional feedback metadata values" $ do
            normalizeOptionalFeedbackText (Just "  bug ") `shouldBe` Just "bug"
            normalizeOptionalFeedbackText (Just " P2 ") `shouldBe` Just "P2"
            normalizeOptionalFeedbackText (Just " user@example.com ") `shouldBe` Just "user@example.com"

        it "drops explicit blank feedback metadata values instead of storing ambiguous empty strings" $ do
            normalizeOptionalFeedbackText Nothing `shouldBe` Nothing
            normalizeOptionalFeedbackText (Just "   ") `shouldBe` Nothing

    describe "validateFeedbackCategory" $ do
        it "normalizes supported categories before storage and notifications" $ do
            validateFeedbackCategory Nothing `shouldBe` Right Nothing
            validateFeedbackCategory (Just "   ") `shouldBe` Right Nothing
            validateFeedbackCategory (Just "  BUG  ") `shouldBe` Right (Just "bug")
            validateFeedbackCategory (Just " Ux ") `shouldBe` Right (Just "ux")

        it "rejects unsupported or malformed feedback categories explicitly" $ do
            let assertInvalid raw expectedMessage =
                    case validateFeedbackCategory (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid feedback category, got " <> show value)
            assertInvalid "billing" "category must be one of: bug, idea, ux, datos"
            assertInvalid (Data.Text.replicate 81 "x") "category must be 80 characters or fewer"
            assertInvalid "bug\nidea" "category must not contain control characters"

    describe "validateFeedbackSeverity" $ do
        it "normalizes supported priorities before storage and notifications" $ do
            validateFeedbackSeverity Nothing `shouldBe` Right Nothing
            validateFeedbackSeverity (Just "   ") `shouldBe` Right Nothing
            validateFeedbackSeverity (Just " p1 ") `shouldBe` Right (Just "P1")
            validateFeedbackSeverity (Just "P4") `shouldBe` Right (Just "P4")

        it "rejects unsupported or malformed feedback priorities explicitly" $ do
            let assertInvalid raw expectedMessage =
                    case validateFeedbackSeverity (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid feedback severity, got " <> show value)
            assertInvalid "high" "severity must be one of: P1, P2, P3, P4"
            assertInvalid (Data.Text.replicate 81 "x") "severity must be 80 characters or fewer"
            assertInvalid "P1\nBcc: ops@example.com" "severity must not contain control characters"

    describe "validateFeedbackTitle" $ do
        it "trims valid feedback titles before storage and notification" $
            validateFeedbackTitle "  Broken checkout flow  "
                `shouldBe` Right "Broken checkout flow"

        it "rejects malformed feedback titles before building email subjects" $ do
            let assertInvalid raw expectedMessage =
                    case validateFeedbackTitle raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid feedback title, got " <> show value)
            assertInvalid "   " "title is required"
            assertInvalid
                "Bug\nBcc: attacker@example.com"
                "title must not contain control characters"
            assertInvalid (Data.Text.replicate 161 "x") "title must be 160 characters or fewer"

    describe "validateFeedbackDescription" $ do
        it "trims descriptions while preserving normal multiline details" $
            validateFeedbackDescription "  Step 1\topen cart\nStep 2: retry  "
                `shouldBe` Right "Step 1\topen cart\nStep 2: retry"

        it "rejects blank, oversized, or hidden-control feedback descriptions before storage" $ do
            let assertInvalid raw expectedMessage =
                    case validateFeedbackDescription raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid feedback description, got " <> show value)
            assertInvalid "   " "description is required"
            assertInvalid
                (Data.Text.replicate 5001 "x")
                "description must be 5000 characters or fewer"
            assertInvalid
                "steps\NULhidden"
                "description must not contain control characters"

    describe "validateFeedbackConsent" $ do
        it "requires explicit consent before the backend stores or emails feedback" $ do
            validateFeedbackConsent True `shouldBe` Right ()
            case validateFeedbackConsent False of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "consent must be accepted"
                Right value ->
                    expectationFailure ("Expected missing feedback consent to be rejected, got " <> show value)

    describe "validateOptionalFeedbackContactEmail" $ do
        it "normalizes valid optional feedback contact emails and keeps blanks unset" $ do
            validateOptionalFeedbackContactEmail Nothing `shouldBe` Right Nothing
            validateOptionalFeedbackContactEmail (Just "   ") `shouldBe` Right Nothing
            validateOptionalFeedbackContactEmail (Just " User@Example.com ")
                `shouldBe` Right (Just "user@example.com")
            validateOptionalFeedbackContactEmail (Just " User.Name+Feedback@Example.com ")
                `shouldBe` Right (Just "user.name+feedback@example.com")

        it "rejects malformed feedback contact emails instead of storing unusable contact data" $ do
            let assertInvalid raw = case validateOptionalFeedbackContactEmail (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "contactEmail must be a valid email address"
                    Right value ->
                        expectationFailure ("Expected invalid feedback contact email to be rejected, got " <> show value)
            assertInvalid "not-an-email"
            assertInvalid "user@example..com"
            assertInvalid "user@-example.com"
            assertInvalid "user@example-.com"
            assertInvalid ".user@example.com"
            assertInvalid "user.@example.com"
            assertInvalid "user..name@example.com"
            assertInvalid "user()@example.com"
            assertInvalid (Data.Text.replicate 65 "a" <> "@example.com")
            assertInvalid ("user@" <> Data.Text.replicate 64 "a" <> ".com")

        it "rejects oversized feedback contact emails before storage and notification" $
            case validateOptionalFeedbackContactEmail
                    (Just (Data.Text.replicate 245 "a" <> "@example.com")) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "contactEmail must be 254 characters or fewer"
                Right value ->
                    expectationFailure
                        ("Expected oversized feedback contact email to be rejected, got " <> show value)

    describe "validateCoursePublicUrlField" $ do
        it "accepts omitted or trimmed absolute HTTPS course URLs" $ do
            validateCoursePublicUrlField "landingUrl" Nothing `shouldBe` Right Nothing
            validateCoursePublicUrlField "landingUrl" (Just "  https://tdf.example.com/curso/produccion  ")
                `shouldBe` Right (Just "https://tdf.example.com/curso/produccion")
            validateCoursePublicUrlField "whatsappCtaUrl" (Just "https://wa.me/593991234567")
                `shouldBe` Right (Just "https://wa.me/593991234567")
            validateCoursePublicUrlField "whatsappCtaUrl" (Just "https://api.whatsapp.com:443/send?phone=593991234567")
                `shouldBe` Right (Just "https://api.whatsapp.com:443/send?phone=593991234567")

        it "rejects non-HTTPS course URLs instead of persisting insecure or broken public links" $ do
            let assertInvalid fieldName rawValue =
                    case validateCoursePublicUrlField fieldName (Just rawValue) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain`
                                    ( Data.Text.unpack fieldName
                                        <> " must be an absolute https URL"
                                    )
                        Right value ->
                            expectationFailure ("Expected invalid course URL to be rejected, got " <> show value)
            assertInvalid "whatsappCtaUrl" "http://wa.me/593991234567"
            assertInvalid "landingUrl" "javascript:alert(1)"
            assertInvalid "locationMapUrl" "/curso/produccion"
            assertInvalid "instructorAvatarUrl" "ftp://cdn.example.com/avatar.png"
            assertInvalid "landingUrl" "https://tdf.example.com/curso\NULpreview"

        it "rejects WhatsApp CTA URLs on non-default HTTPS ports so public course links stay canonical" $
            case validateCoursePublicUrlField "whatsappCtaUrl" (Just "https://wa.me:8443/593991234567") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain`
                            "whatsappCtaUrl must use wa.me, api.whatsapp.com, or web.whatsapp.com on the default HTTPS port"
                Right value ->
                    expectationFailure ("Expected non-default WhatsApp CTA port to be rejected, got " <> show value)

        it "drops stale persisted public course URLs before metadata serialization" $ do
            sanitizeStoredCoursePublicUrl "landingUrl" (Just "  https://tdf.example.com/curso/produccion  ")
                `shouldBe` Just "https://tdf.example.com/curso/produccion"
            sanitizeStoredCoursePublicUrl "landingUrl" (Just "http://tdf.example.com/curso/produccion")
                `shouldBe` Nothing
            sanitizeStoredCoursePublicUrl "locationMapUrl" (Just "https://localhost/studio")
                `shouldBe` Nothing
            sanitizeStoredCoursePublicUrl "whatsappCtaUrl" (Just "javascript:alert(1)")
                `shouldBe` Nothing
            sanitizeStoredCoursePublicUrl "instructorAvatarUrl" Nothing
                `shouldBe` Nothing

    describe "validateDatafastBaseUrl" $ do
        it "keeps the default OPPWA base and normalizes configured origins" $ do
            validateDatafastBaseUrl Nothing `shouldBe` Right "https://test.oppwa.com"
            validateDatafastBaseUrl (Just " https://eu-prod.oppwa.com/ ")
                `shouldBe` Right "https://eu-prod.oppwa.com"
            validateDatafastBaseUrl (Just "https://eu-prod.oppwa.com:443")
                `shouldBe` Right "https://eu-prod.oppwa.com:443"

        it "rejects malformed Datafast bases before payment requests are built" $ do
            let assertInvalid rawValue =
                    case validateDatafastBaseUrl (Just rawValue) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 500
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "DATAFAST_BASE_URL must be an absolute https origin"
                        Right value ->
                            expectationFailure ("Expected invalid Datafast base URL, got " <> show value)
            assertInvalid "   "
            assertInvalid "ftp://test.oppwa.com"
            assertInvalid "http://payments.example.com"
            assertInvalid "https://test.oppwa.com/v1"
            assertInvalid "https://test.oppwa.com?proxy=1"
            assertInvalid "http://localhost:8080"
            assertInvalid "https://payments.example.com"
            assertInvalid "https://oppwa.com.evil.example"
            assertInvalid "https://test.oppwa.com:8443"

    describe "validateDatafastCredential" $ do
        it "trims required Datafast credentials before payment requests are built" $ do
            validateDatafastCredential "DATAFAST_ENTITY_ID" (Just " entity-123 ")
                `shouldBe` Right "entity-123"
            validateDatafastCredential "DATAFAST_BEARER_TOKEN" (Just "\tbearer-token\n")
                `shouldBe` Right "bearer-token"

        it "rejects missing, blank, or control-character Datafast credentials before gateway calls" $ do
            let assertInvalid envName rawValue expectedMessage =
                    case validateDatafastCredential envName rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 500
                            BL.unpack (errBody err)
                                `shouldContain`
                                    Data.Text.unpack (envName <> " " <> expectedMessage)
                        Right value ->
                            expectationFailure
                                ("Expected invalid Datafast credential, got " <> show value)
            assertInvalid "DATAFAST_ENTITY_ID" Nothing "must be configured"
            assertInvalid "DATAFAST_ENTITY_ID" (Just "   ") "must be configured"
            assertInvalid
                "DATAFAST_BEARER_TOKEN"
                (Just "bearer\nvalue")
                "must not contain control characters"
            assertInvalid
                "DATAFAST_BEARER_TOKEN"
                (Just "bearer token")
                "must not contain control characters or whitespace"

    describe "validateDatafastCheckoutId" $ do
        it "normalizes safe Datafast checkout ids before building widget URLs" $ do
            validateDatafastCheckoutId "  8ac7a4a18c9d_test-01.02  "
                `shouldBe` Right "8ac7a4a18c9d_test-01.02"

        it "rejects malformed Datafast checkout ids before widget URLs are built" $ do
            let assertInvalid rawValue =
                    case validateDatafastCheckoutId rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 502
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "Datafast returned an invalid checkout id"
                        Right value ->
                            expectationFailure ("Expected invalid Datafast checkout id, got " <> show value)
            assertInvalid "   "
            assertInvalid "../payment"
            assertInvalid "checkout?entityId=other"
            assertInvalid "checkout&entityId=other"
            assertInvalid "checkout#fragment"

    describe "validatePayPalApprovalUrl" $ do
        it "requires a trimmed HTTPS PayPal approval URL before returning checkout data" $ do
            let liveApproval = "https://www.paypal.com/checkoutnow?token=ORDER-123"
                sandboxApproval =
                    "https://www.sandbox.paypal.com/checkoutnow?token=ORDER-123"
            validatePayPalApprovalUrl (Just ("  " <> liveApproval <> "  "))
                `shouldBe` Right liveApproval
            validatePayPalApprovalUrl (Just sandboxApproval)
                `shouldBe` Right sandboxApproval

        it "rejects missing or non-checkout PayPal approval URLs from the upstream response" $ do
            let invalidMessage = "PayPal returned an invalid approval URL"
            let assertInvalid rawValue expectedMessage =
                    case validatePayPalApprovalUrl rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 502
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid PayPal approval URL, got " <> show value)
            assertInvalid Nothing "PayPal response did not include an approval URL"
            assertInvalid (Just "   ") invalidMessage
            assertInvalid
                (Just "http://www.paypal.com/checkoutnow?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://paypal.com.evil.example/checkoutnow?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com@evil.example/checkoutnow?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://api.paypal.com/checkoutnow?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com:8443/checkoutnow?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/signin?token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?flow=1")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=ORDER-123&token=ORDER-456")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=ORDER-123&flow=checkout")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=https://evil.example/ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=&token=ORDER-123")
                invalidMessage
            assertInvalid
                (Just "https://www.paypal.com/checkoutnow?token=ORDER-123#fragment")
                invalidMessage

    describe "validatePayPalCreateOrderIdField" $ do
        it "rejects malformed upstream PayPal order ids before marketplace storage" $ do
            validatePayPalCreateOrderIdField " ORDER-123_ABC "
                `shouldBe` Right "ORDER-123_ABC"

            let assertInvalid rawValue =
                    case validatePayPalCreateOrderIdField rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 502
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "PayPal create response returned an invalid order id"
                        Right value ->
                            expectationFailure
                                ("Expected invalid PayPal order id, got " <> show value)
            assertInvalid "   "
            assertInvalid "ORDER/123"
            assertInvalid "ORDER?token=123"
            assertInvalid (Data.Text.replicate 129 "A")

    describe "validatePayPalCaptureStatusField" $ do
        it "requires PayPal capture responses to include a non-blank status" $ do
            validatePayPalCaptureStatusField (Just " COMPLETED ")
                `shouldBe` Right "COMPLETED"

            let assertInvalid rawValue expectedMessage =
                    case validatePayPalCaptureStatusField rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 502
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid PayPal capture status, got " <> show value)
            assertInvalid Nothing "PayPal capture response did not include a status"
            assertInvalid (Just "   ") "PayPal capture response status cannot be blank"
            assertInvalid
                (Just "COM PLETED")
                "PayPal capture response status must not contain control characters"
            assertInvalid
                (Just "COM\nPLETED")
                "PayPal capture response status must not contain control characters"

    describe "validatePayPalPayerEmailField" $ do
        it "normalizes optional PayPal payer emails before storing capture metadata" $ do
            validatePayPalPayerEmailField Nothing `shouldBe` Right Nothing
            validatePayPalPayerEmailField (Just "  PAYER@Example.COM  ")
                `shouldBe` Right (Just "payer@example.com")

        it "rejects malformed PayPal payer emails instead of storing ambiguous capture metadata" $ do
            let assertInvalid rawValue expectedMessage =
                    case validatePayPalPayerEmailField rawValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 502
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid PayPal payer email, got " <> show value)
            assertInvalid (Just "   ") "PayPal payer email cannot be blank"
            assertInvalid (Just "payer@example..com") "PayPal returned an invalid payer email"
            assertInvalid (Just "payer\n@example.com") "PayPal returned an invalid payer email"

    describe "buildWhatsappCtaFor" $ do
        it "uses a configured WhatsApp contact only after phone normalization accepts it" $ do
            buildWhatsappCtaFor
                (Just " +593 99 123 4567 ")
                "Curso de Producción Musical"
                "https://tdf.example.com/curso/produccion"
                `shouldSatisfy`
                    Data.Text.isPrefixOf "https://wa.me/593991234567?text="

        it "falls back to a numberless WhatsApp CTA when the configured contact is malformed or local-only" $ do
            buildWhatsappCtaFor
                (Just "593")
                "Curso de Producción Musical"
                "https://tdf.example.com/curso/produccion"
                `shouldSatisfy`
                    Data.Text.isPrefixOf "https://wa.me/?text="
            buildWhatsappCtaFor
                (Just "099 123 4567")
                "Curso de Producción Musical"
                "https://tdf.example.com/curso/produccion"
                `shouldSatisfy`
                    Data.Text.isPrefixOf "https://wa.me/?text="

    describe "resolveProvidedDriveAccessToken" $ do
        it "normalizes matching token sources and rejects conflicting upload credentials" $ do
            resolveProvidedDriveAccessToken (Just " header-token ") Nothing
                `shouldBe` Right (Just "header-token")
            resolveProvidedDriveAccessToken Nothing (Just " form-token ")
                `shouldBe` Right (Just "form-token")
            resolveProvidedDriveAccessToken (Just " same-token ") (Just "same-token")
                `shouldBe` Right (Just "same-token")
            case resolveProvidedDriveAccessToken (Just "header-token") (Just "form-token") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "Conflicting Google Drive access tokens"
                Right value ->
                    expectationFailure
                        ( "Expected conflicting Drive upload tokens to be rejected, got "
                            <> show value
                        )

        it "rejects malformed upload access tokens before building Authorization headers" $ do
            let assertInvalid headerToken formToken expectedMessage =
                    case resolveProvidedDriveAccessToken headerToken formToken of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected malformed Drive upload token to be rejected, got "
                                    <> show value
                                )
            assertInvalid
                (Just "ya29.valid token")
                Nothing
                "Google Drive access token must not contain whitespace"
            assertInvalid
                (Just "   ")
                (Just "form-token")
                "X-Goog-Access-Token must not be blank"
            assertInvalid
                Nothing
                (Just "   ")
                "accessToken must not be blank"
            assertInvalid
                Nothing
                (Just "ya29.valid\NULtoken")
                "Google Drive access token must not contain control characters"
            assertInvalid
                (Just (Data.Text.replicate 4097 "a"))
                Nothing
                "Google Drive access token must be 4096 characters or fewer"

    describe "resolveDrivePublicUrl" $ do
        it "keeps canonical Google Drive download links only when they point at the uploaded file" $ do
            resolveDrivePublicUrl
                "1A_B-99"
                (Just " https://drive.usercontent.google.com/download?id=1A_B-99&export=download ")
                Nothing
                (Just " rk_123 ")
                `shouldBe`
                    "https://drive.usercontent.google.com/download?id=1A_B-99&export=download&resourcekey=rk_123"

        it "falls back to the canonical download URL when Drive returns an ambiguous or mismatched content link" $ do
            resolveDrivePublicUrl
                "1A_B-99"
                (Just "https://drive.google.com/uc?export=download&id=other-file")
                Nothing
                (Just "rk_123")
                `shouldBe`
                    "https://drive.google.com/uc?export=download&id=1A_B-99&resourcekey=rk_123"
            resolveDrivePublicUrl
                "1A_B-99"
                (Just "https://drive.google.com/uc?export=download&id=1A_B-99&id=other-file")
                Nothing
                Nothing
                `shouldBe`
                    "https://drive.google.com/uc?export=download&id=1A_B-99"
            resolveDrivePublicUrl
                "1A_B-99"
                (Just "https://drive.google.com/settings/storage?id=1A_B-99")
                Nothing
                Nothing
                `shouldBe`
                    "https://drive.google.com/uc?export=download&id=1A_B-99"

    describe "sanitizeFeedbackAttachmentFileName" $ do
        it "reduces attachment names to a stable safe basename" $ do
            sanitizeFeedbackAttachmentFileName "  ../Bug report final?.png  "
                `shouldBe` "Bug-report-final-.png"
            sanitizeFeedbackAttachmentFileName " screenshot\t2026-04-11\nfinal.png "
                `shouldBe` "screenshot-2026-04-11-final.png"

        it "falls back when the upload filename is blank or only dangerous punctuation" $ do
            sanitizeFeedbackAttachmentFileName "   " `shouldBe` "attachment"
            sanitizeFeedbackAttachmentFileName "." `shouldBe` "attachment"
            sanitizeFeedbackAttachmentFileName ".." `shouldBe` "attachment"
            sanitizeFeedbackAttachmentFileName "..." `shouldBe` "attachment"
            sanitizeFeedbackAttachmentFileName "__--__" `shouldBe` "attachment"
            sanitizeFeedbackAttachmentFileName "/\\///" `shouldBe` "attachment"

        it "bounds sanitized attachment names before writing upload paths" $
            sanitizeFeedbackAttachmentFileName
                (Data.Text.replicate 160 "a" <> ".png")
                `shouldBe` (Data.Text.replicate 116 "a" <> ".png")

    describe "validateFeedbackAttachmentSize" $ do
        it "accepts non-empty boundary-sized feedback attachments" $ do
            validateFeedbackAttachmentSize 1 `shouldBe` Right ()
            validateFeedbackAttachmentSize (10 * 1024 * 1024) `shouldBe` Right ()

        it "rejects invalid or oversized feedback attachments before copying uploads" $ do
            let assertInvalid raw expectedMessage =
                    case validateFeedbackAttachmentSize raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure ("Expected invalid attachment size, got " <> show value)
            assertInvalid (-1) "attachment size is invalid"
            assertInvalid 0 "attachment must not be empty"
            assertInvalid (10 * 1024 * 1024 + 1) "attachment must be 10 MB or smaller"

    describe "feedback multipart parsing" $ do
        it "accepts omitted consent as false and normalizes explicit truthy values" $ do
            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "  Broken flow  ")
                    , ("description", "  Steps to reproduce  ")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    expectationFailure ("Expected feedback payload without consent to parse, got: " <> err)
                Right payload -> do
                    fpTitle payload `shouldBe` "Broken flow"
                    fpDescription payload `shouldBe` "Steps to reproduce"
                    fpConsent payload `shouldBe` False

            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    , ("consent", " yes ")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    expectationFailure ("Expected truthy feedback consent to parse, got: " <> err)
                Right payload ->
                    fpConsent payload `shouldBe` True

        it "rejects malformed consent values instead of silently coercing them to false" $
            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    , ("consent", "maybe")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "consent must be a boolean"
                Right payload ->
                    expectationFailure ("Expected invalid consent to be rejected, got: " <> show payload)

        it "rejects duplicate scalar fields instead of silently taking the first value" $ do
            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "Broken flow")
                    , ("title", "Shadow title")
                    , ("description", "Steps to reproduce")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "Duplicate field: title"
                Right payload ->
                    expectationFailure ("Expected duplicate title field to be rejected, got: " <> show payload)

            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    , ("consent", "true")
                    , ("consent", "false")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "Duplicate field: consent"
                Right payload ->
                    expectationFailure ("Expected duplicate consent field to be rejected, got: " <> show payload)

        it "rejects duplicate attachment fields instead of arbitrarily picking one upload" $
            case fromMultipart (mkFeedbackMultipartWithFiles
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    ]
                    [ mkFeedbackAttachment "first.png"
                    , mkFeedbackAttachment "second.png"
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "Duplicate file field: attachment"
                Right payload ->
                    expectationFailure ("Expected duplicate attachment field to be rejected, got: " <> show payload)

        it "rejects unexpected scalar or file fields instead of silently ignoring typos" $ do
            case fromMultipart (mkFeedbackMultipart
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    , ("priority", "p1")
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "Unexpected field: priority"
                Right payload ->
                    expectationFailure ("Expected unexpected feedback field to be rejected, got: " <> show payload)

            case fromMultipart (mkFeedbackMultipartWithFiles
                    [ ("title", "Broken flow")
                    , ("description", "Steps to reproduce")
                    ]
                    [ mkUnexpectedFeedbackAttachment "screenshot" "screen.png"
                    ]) :: Either String FeedbackPayload of
                Left err ->
                    err `shouldContain` "Unexpected file field: screenshot"
                Right payload ->
                    expectationFailure ("Expected unexpected feedback file field to be rejected, got: " <> show payload)

    describe "normalizeInvitationStatus" $ do
        it "falls back to pending when missing" $ do
            normalizeInvitationStatus Nothing `shouldBe` "pending"

        it "trims and canonicalizes supported invitation statuses" $ do
            normalizeInvitationStatus (Just "  Accepted ") `shouldBe` "accepted"
            normalizeInvitationStatus (Just "DECLINED") `shouldBe` "declined"

        it "treats blank or invalid stored statuses as pending" $ do
            normalizeInvitationStatus (Just "   ") `shouldBe` "pending"
            normalizeInvitationStatus (Just "later") `shouldBe` "pending"

    describe "validateInvitationStatusInput" $ do
        it "defaults omitted or blank invitation statuses to pending and canonicalizes supported values" $ do
            validateInvitationStatusInput Nothing `shouldBe` Right "pending"
            validateInvitationStatusInput (Just "   ") `shouldBe` Right "pending"
            validateInvitationStatusInput (Just " Accepted ") `shouldBe` Right "accepted"
            validateInvitationStatusInput (Just "DECLINED") `shouldBe` Right "declined"

        it "rejects unsupported invitation statuses instead of persisting arbitrary labels" $ do
            case validateInvitationStatusInput (Just "later") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "pending, accepted, declined"
                Right value ->
                    expectationFailure ("Expected invalid invitation status to be rejected, got " <> show value)

    describe "validateInvitationStatusUpdateInput" $ do
        it "preserves the current invitation status when an update omits the field" $ do
            validateInvitationStatusUpdateInput Nothing `shouldBe` Right Nothing

        it "canonicalizes explicit update statuses and rejects blank updates that would silently reset state" $ do
            validateInvitationStatusUpdateInput (Just " Accepted ")
                `shouldBe` Right (Just "accepted")
            case validateInvitationStatusUpdateInput (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "pending, accepted, declined"
                Right value ->
                    expectationFailure
                        ("Expected blank invitation update status to be rejected, got " <> show value)

    describe "validateInvitationToPartyId" $ do
        it "accepts positive numeric ids and canonicalizes them" $ do
            validateInvitationToPartyId " 0042 " `shouldBe` Right "42"

        it "rejects blank or malformed ids instead of persisting ambiguous invitation recipients" $ do
            let assertInvalid expected raw =
                    case validateInvitationToPartyId raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid invitationToPartyId to be rejected, got " <> show value)
            assertInvalid "invitationToPartyId is required" "   "
            assertInvalid "invitationToPartyId must be a positive integer" "abc"
            assertInvalid "invitationToPartyId must be a positive integer" "0"

    describe "validateRsvpStatus" $ do
        it "trims and canonicalizes supported RSVP states" $ do
            validateRsvpStatus " Accepted " `shouldBe` Right "accepted"
            validateRsvpStatus "DECLINED" `shouldBe` Right "declined"
            validateRsvpStatus "maybe" `shouldBe` Right "maybe"

        it "rejects blank or unknown RSVP states instead of persisting arbitrary labels" $ do
            let assertInvalid raw = case validateRsvpStatus raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "accepted, declined, maybe"
                    Right value ->
                        expectationFailure ("Expected invalid RSVP status to be rejected, got " <> show value)
            assertInvalid "   "
            assertInvalid "interested"

    describe "normalizeArtistGenres" $ do
        it "trims genres, drops blanks, and deduplicates case-insensitively" $ do
            normalizeArtistGenres ["  Salsa ", "", "salsa", " Rock ", "ROCK", "Pop"] `shouldBe` ["Salsa", "Rock", "Pop"]

    describe "social sync id validation" $ do
        it "accepts positive artist references after trimming and canonicalizing the numeric value" $ do
            validateSocialSyncArtistPartyId " 0042 " `shouldBe` Right 42
            validateSocialSyncArtistProfileId " 0007 " `shouldBe` Right 7

        it "rejects blank, malformed, zero, or negative social sync references with precise 400s" $ do
            let assertInvalid expected validator raw =
                    case validator raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid social sync id to be rejected, got " <> show value)
            assertInvalid "artistPartyId must be a positive integer" validateSocialSyncArtistPartyId "   "
            assertInvalid "artistPartyId must be a positive integer" validateSocialSyncArtistPartyId "-12"
            assertInvalid "artistPartyId must be a positive integer" validateSocialSyncArtistPartyId "0"
            assertInvalid "artistProfileId must be a positive integer" validateSocialSyncArtistProfileId "abc"
            assertInvalid "artistProfileId must be a positive integer" validateSocialSyncArtistProfileId "-1"

    describe "social sync platform validation" $ do
        it "normalizes supported post platforms before querying or persisting rows" $ do
            validateSocialSyncPlatform " Instagram " `shouldBe` Right "instagram"
            validateSocialSyncPlatform "FACEBOOK" `shouldBe` Right "facebook"

        it "rejects blank or unsupported platforms instead of storing typoed post identities" $ do
            let assertInvalid raw =
                    case validateSocialSyncPlatform raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "platform must be one of: instagram, facebook"
                        Right value ->
                            expectationFailure ("Expected invalid social sync platform to be rejected, got " <> show value)
            assertInvalid "   "
            assertInvalid "threads"

    describe "social sync external post id validation" $ do
        it "trims meaningful external ids before dedupe and storage" $ do
            validateSocialSyncExternalPostId "  ig-media-42  " `shouldBe` Right "ig-media-42"

        it "rejects blank external ids instead of collapsing distinct posts under an empty key" $ do
            case validateSocialSyncExternalPostId "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "externalPostId is required"
                Right value ->
                    expectationFailure ("Expected blank social sync externalPostId to be rejected, got " <> show value)

        it "rejects ambiguous external ids before they become persisted post identities" $ do
            let assertInvalid expected raw =
                    case validateSocialSyncExternalPostId raw of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected invalid social sync externalPostId to be rejected, got " <> show value)
            assertInvalid "externalPostId must not contain whitespace" "ig media 42"
            assertInvalid "externalPostId must not contain whitespace" "ig-media\n42"
            assertInvalid
                "externalPostId must be 256 characters or fewer"
                (Data.Text.replicate 257 "a")

    describe "social sync ingest source validation" $ do
        it "defaults omitted sources and normalizes explicit source keys before audit storage" $ do
            validateSocialSyncIngestSource Nothing `shouldBe` Right "manual"
            validateSocialSyncIngestSource (Just "  Meta_Ads-Backfill  ")
                `shouldBe` Right "meta_ads-backfill"

        it "rejects blank or malformed source labels instead of storing ambiguous audit values" $ do
            let assertInvalid raw expected =
                    case validateSocialSyncIngestSource (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected invalid social sync ingestSource to be rejected, got "
                                    <> show value
                                )
            assertInvalid "   " "ingestSource must be omitted or a non-empty ASCII label"
            assertInvalid "meta ads" "ingestSource must contain only ASCII letters"
            assertInvalid "campaña" "ingestSource must contain only ASCII letters"
            assertInvalid
                (Data.Text.replicate 65 "a")
                "ingestSource must be 64 characters or fewer"

    describe "social sync permalink validation" $ do
        it "normalizes omitted, blank, and valid public permalink URLs before storage" $ do
            validateSocialSyncPermalink Nothing `shouldBe` Right Nothing
            validateSocialSyncPermalink (Just "   ") `shouldBe` Right Nothing
            validateSocialSyncPermalink (Just "  https://instagram.com/p/post-42  ")
                `shouldBe` Right (Just "https://instagram.com/p/post-42")

        it "rejects unsafe or ambiguous permalink URLs instead of persisting user-facing bad links" $ do
            let assertInvalid raw expected =
                    case validateSocialSyncPermalink (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected invalid social sync permalink to be rejected, got "
                                    <> show value
                                )
            assertInvalid "https://instagram.com/p/post 42" "permalink must not contain whitespace"
            assertInvalid "/p/post-42" "permalink must be an absolute public http(s) URL"
            assertInvalid "javascript:alert(1)" "permalink must be an absolute public http(s) URL"
            assertInvalid "https://localhost/p/post-42" "permalink must be an absolute public http(s) URL"
            assertInvalid "https://user@example.com/p/post-42" "permalink must be an absolute public http(s) URL"
            assertInvalid
                ("https://instagram.com/p/" <> Data.Text.replicate 2049 "a")
                "permalink must be 2048 characters or fewer"

    describe "social sync media URL validation" $ do
        it "normalizes valid media URL lists before newline storage" $ do
            validateSocialSyncMediaUrls Nothing `shouldBe` Right Nothing
            validateSocialSyncMediaUrls (Just []) `shouldBe` Right Nothing
            validateSocialSyncMediaUrls
                (Just [" https://cdn.example.com/post.jpg ", "https://cdn.example.com/clip.mp4"])
                `shouldBe` Right (Just "https://cdn.example.com/post.jpg\nhttps://cdn.example.com/clip.mp4")

        it "rejects blank or whitespace-containing media URLs instead of silently dropping or splitting entries" $ do
            let assertInvalid raw expected =
                    case validateSocialSyncMediaUrls (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected invalid social sync mediaUrls to be rejected, got "
                                    <> show value
                                )
            assertInvalid ["https://cdn.example.com/post.jpg", "   "] "mediaUrls entries must not be blank"
            assertInvalid ["https://cdn.example.com/post 42.jpg"] "mediaUrls entries must not contain whitespace"
            assertInvalid
                [" https://cdn.example.com/post.jpg ", "https://cdn.example.com/post.jpg"]
                "mediaUrls entries must be unique"

        it "rejects oversized media URL payloads before newline storage" $ do
            let assertInvalid raw expected =
                    case validateSocialSyncMediaUrls (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected oversized social sync mediaUrls to be rejected, got "
                                    <> show value
                                )
                numberedUrl n =
                    "https://cdn.example.com/post-"
                        <> Data.Text.pack (show (n :: Int))
                        <> ".jpg"
            assertInvalid
                (map numberedUrl [1..21])
                "mediaUrls must contain at most 20 entries"
            assertInvalid
                ["https://cdn.example.com/" <> Data.Text.replicate 2049 "a"]
                "mediaUrls entries must be 2048 characters or fewer"

        it "rejects unsafe or non-public media URLs before social sync rows are stored" $ do
            let assertInvalid raw =
                    case validateSocialSyncMediaUrls (Just [raw]) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "absolute public http(s) URLs"
                        Right value ->
                            expectationFailure
                                ( "Expected invalid social sync mediaUrl to be rejected, got "
                                    <> show value
                                )
            assertInvalid "/uploads/post.jpg"
            assertInvalid "javascript:alert(1)"
            assertInvalid "https://localhost/post.jpg"
            assertInvalid "https://user@example.com/post.jpg"

    describe "social sync ingest JSON contract" $ do
        it "accepts canonical ingest payloads and rejects unexpected keys at both request levels" $ do
            case (eitherDecode "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"caption\":\"New single out now\",\"mediaUrls\":[\"https://cdn.example.com/post.jpg\"],\"likeCount\":12,\"commentCount\":3}]}" :: Either String SocialSyncIngestRequest) of
                Left err ->
                    expectationFailure ("Expected canonical social sync ingest payload to decode, got: " <> err)
                Right _ ->
                    pure ()
            (eitherDecode "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"caption\":\"New single out now\",\"unexpected\":true}]}" :: Either String SocialSyncIngestRequest)
                `shouldSatisfy` isLeft
            (eitherDecode "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\"}],\"unexpected\":true}" :: Either String SocialSyncIngestRequest)
                `shouldSatisfy` isLeft

        it "rejects empty ingest batches so the server cannot record misleading no-op sync runs" $ do
            case (eitherDecode "{\"posts\":[]}" :: Either String SocialSyncIngestRequest) of
                Left err ->
                    err `shouldContain` "posts must contain at least one post"
                Right value ->
                    expectationFailure ("Expected empty social sync ingest batch to be rejected, got: " <> show value)

        it "rejects oversized ingest batches before unbounded social-sync writes can start" $ do
            let mkPost n =
                    A.object
                        [ "platform" .= ("instagram" :: Text)
                        , "externalPostId" .= ("ig-media-" <> Data.Text.pack (show n))
                        ]
                payload =
                    A.encode $
                        A.object
                            [ "posts" .= map mkPost [1 .. maxSocialSyncIngestPosts + 1]
                            ]
            case (eitherDecode payload :: Either String SocialSyncIngestRequest) of
                Left err ->
                    err `shouldContain`
                        ("posts must contain at most " <> show maxSocialSyncIngestPosts <> " posts")
                Right value ->
                    expectationFailure
                        ("Expected oversized social sync ingest batch to be rejected, got: " <> show value)

        it "rejects duplicate normalized post identities instead of making batch counts order-dependent" $ do
            case (eitherDecode "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\" ig-media-42 \"},{\"platform\":\" Instagram \",\"externalPostId\":\"ig-media-42\"}]}" :: Either String SocialSyncIngestRequest) of
                Left err ->
                    err `shouldContain` "posts must not contain duplicate platform/externalPostId pairs"
                Right value ->
                    expectationFailure ("Expected duplicate social sync post identity to be rejected, got: " <> show value)

        it "rejects negative engagement metrics instead of persisting impossible social analytics" $ do
            let assertInvalid fieldName payload =
                    case (eitherDecode payload :: Either String SocialSyncIngestRequest) of
                        Left err ->
                            err `shouldContain` (fieldName <> " must be greater than or equal to 0")
                        Right value ->
                            expectationFailure ("Expected invalid social sync metric payload to be rejected, got: " <> show value)
            assertInvalid "likeCount" "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"likeCount\":-1}]}"
            assertInvalid "commentCount" "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"commentCount\":-1}]}"
            assertInvalid "shareCount" "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"shareCount\":-1}]}"
            assertInvalid "viewCount" "{\"posts\":[{\"platform\":\"instagram\",\"externalPostId\":\"ig-media-42\",\"viewCount\":-1}]}"

    describe "social sync ingest handler" $ do
        it "records mixed batch audit labels explicitly instead of inheriting the first post" $ do
            let mkIngestPost platform externalPostId ingestSource =
                    SocialSyncPostIn
                        { sspPlatform = platform
                        , sspExternalPostId = externalPostId
                        , sspCaption = Nothing
                        , sspPermalink = Nothing
                        , sspMediaUrls = Nothing
                        , sspPostedAt = Nothing
                        , sspArtistPartyId = Nothing
                        , sspArtistProfileId = Nothing
                        , sspIngestSource = ingestSource
                        , sspLikeCount = Nothing
                        , sspCommentCount = Nothing
                        , sspShareCount = Nothing
                        , sspViewCount = Nothing
                        }
                request =
                    SocialSyncIngestRequest
                        [ mkIngestPost "instagram" "ig-media-42" (Just "manual")
                        , mkIngestPost "facebook" "fb-post-7" (Just "meta_ads")
                        ]
            (result, posts, runs) <- runSocialSyncIngestHandler request
            case result of
                Left err ->
                    expectationFailure ("Expected mixed social sync ingest to succeed, got: " <> show err)
                Right response -> do
                    ssirInserted response `shouldBe` 2
                    ssirUpdated response `shouldBe` 0
                    ssirTotal response `shouldBe` 2
            length posts `shouldBe` 2
            case runs of
                [run] -> do
                    socialSyncRunPlatform run `shouldBe` "mixed"
                    socialSyncRunIngestSource run `shouldBe` "mixed"
                _ ->
                    expectationFailure ("Expected one social sync run audit row, got: " <> show runs)

        it "rejects ambiguous media URL lists before recording ingest audit rows" $ do
            let request =
                    SocialSyncIngestRequest
                        [ SocialSyncPostIn
                            { sspPlatform = "instagram"
                            , sspExternalPostId = "ig-media-blank-url"
                            , sspCaption = Nothing
                            , sspPermalink = Nothing
                            , sspMediaUrls = Just ["https://cdn.example.com/post.jpg", "   "]
                            , sspPostedAt = Nothing
                            , sspArtistPartyId = Nothing
                            , sspArtistProfileId = Nothing
                            , sspIngestSource = Nothing
                            , sspLikeCount = Nothing
                            , sspCommentCount = Nothing
                            , sspShareCount = Nothing
                            , sspViewCount = Nothing
                            }
                        ]
            (result, posts, runs) <- runSocialSyncIngestHandler request
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "mediaUrls entries must not be blank"
                Right response ->
                    expectationFailure ("Expected invalid social sync ingest to fail, got: " <> show response)
            length posts `shouldBe` 0
            length runs `shouldBe` 0

        it "rejects the full batch before writing any posts when a later payload is invalid" $ do
            let validPost =
                    SocialSyncPostIn
                        { sspPlatform = "instagram"
                        , sspExternalPostId = "ig-media-42"
                        , sspCaption = Just "New single out now"
                        , sspPermalink = Nothing
                        , sspMediaUrls = Just ["https://cdn.example.com/post.jpg"]
                        , sspPostedAt = Nothing
                        , sspArtistPartyId = Nothing
                        , sspArtistProfileId = Nothing
                        , sspIngestSource = Nothing
                        , sspLikeCount = Nothing
                        , sspCommentCount = Nothing
                        , sspShareCount = Nothing
                        , sspViewCount = Nothing
                        }
                invalidPost =
                    SocialSyncPostIn
                        { sspPlatform = "instagram"
                        , sspExternalPostId = "ig-media-43"
                        , sspCaption = Nothing
                        , sspPermalink = Nothing
                        , sspMediaUrls = Just ["https://cdn.example.com/post-2.jpg", "   "]
                        , sspPostedAt = Nothing
                        , sspArtistPartyId = Nothing
                        , sspArtistProfileId = Nothing
                        , sspIngestSource = Nothing
                        , sspLikeCount = Nothing
                        , sspCommentCount = Nothing
                        , sspShareCount = Nothing
                        , sspViewCount = Nothing
                        }
                request = SocialSyncIngestRequest [validPost, invalidPost]
            (result, posts, runs) <- runSocialSyncIngestHandler request
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "mediaUrls entries must not be blank"
                Right response ->
                    expectationFailure ("Expected invalid social sync batch to fail, got: " <> show response)
            length posts `shouldBe` 0
            length runs `shouldBe` 0

        it "infers artistPartyId from artistProfileId before persisting social sync rows" $ do
            let setup = do
                    partyId <- insertSocialSyncPartyFixture 21 "Artist Party"
                    _ <- insertSocialSyncArtistProfileFixture 7 partyId
                    pure ()
                request =
                    SocialSyncIngestRequest
                        [ SocialSyncPostIn
                            { sspPlatform = "instagram"
                            , sspExternalPostId = "ig-media-profile-only"
                            , sspCaption = Just "Profile-linked post"
                            , sspPermalink = Nothing
                            , sspMediaUrls = Nothing
                            , sspPostedAt = Nothing
                            , sspArtistPartyId = Nothing
                            , sspArtistProfileId = Just "7"
                            , sspIngestSource = Just "manual"
                            , sspLikeCount = Nothing
                            , sspCommentCount = Nothing
                            , sspShareCount = Nothing
                            , sspViewCount = Nothing
                            }
                        ]
            (result, posts, runs) <- runSocialSyncIngestHandlerWithSetup setup request
            case result of
                Left err ->
                    expectationFailure ("Expected profile-linked social sync ingest to succeed, got: " <> show err)
                Right response -> do
                    ssirInserted response `shouldBe` 1
                    ssirUpdated response `shouldBe` 0
                    ssirTotal response `shouldBe` 1
            case posts of
                [post] -> do
                    fmap fromSqlKey (socialSyncPostArtistPartyId post) `shouldBe` Just 21
                    fmap fromSqlKey (socialSyncPostArtistProfileId post) `shouldBe` Just 7
                _ ->
                    expectationFailure ("Expected one stored social sync post, got: " <> show posts)
            length runs `shouldBe` 1

        it "rejects unknown artistProfileId values before any social sync rows are written" $ do
            let request =
                    SocialSyncIngestRequest
                        [ SocialSyncPostIn
                            { sspPlatform = "instagram"
                            , sspExternalPostId = "ig-media-missing-profile"
                            , sspCaption = Nothing
                            , sspPermalink = Nothing
                            , sspMediaUrls = Nothing
                            , sspPostedAt = Nothing
                            , sspArtistPartyId = Nothing
                            , sspArtistProfileId = Just "999"
                            , sspIngestSource = Nothing
                            , sspLikeCount = Nothing
                            , sspCommentCount = Nothing
                            , sspShareCount = Nothing
                            , sspViewCount = Nothing
                            }
                        ]
            (result, posts, runs) <- runSocialSyncIngestHandler request
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 404
                    BL.unpack (errBody err) `shouldContain` "artistProfileId not found"
                Right response ->
                    expectationFailure ("Expected missing artistProfileId ingest to fail, got: " <> show response)
            length posts `shouldBe` 0
            length runs `shouldBe` 0

        it "rejects mismatched artistPartyId and artistProfileId before writing social sync rows" $ do
            let setup = do
                    _ <- insertSocialSyncPartyFixture 21 "Artist Party A"
                    otherPartyId <- insertSocialSyncPartyFixture 22 "Artist Party B"
                    _ <- insertSocialSyncArtistProfileFixture 7 otherPartyId
                    pure ()
                request =
                    SocialSyncIngestRequest
                        [ SocialSyncPostIn
                            { sspPlatform = "instagram"
                            , sspExternalPostId = "ig-media-mismatched-artist"
                            , sspCaption = Nothing
                            , sspPermalink = Nothing
                            , sspMediaUrls = Nothing
                            , sspPostedAt = Nothing
                            , sspArtistPartyId = Just "21"
                            , sspArtistProfileId = Just "7"
                            , sspIngestSource = Nothing
                            , sspLikeCount = Nothing
                            , sspCommentCount = Nothing
                            , sspShareCount = Nothing
                            , sspViewCount = Nothing
                            }
                        ]
            (result, posts, runs) <- runSocialSyncIngestHandlerWithSetup setup request
            case result of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "artistProfileId must belong to artistPartyId"
                Right response ->
                    expectationFailure ("Expected mismatched artist references to fail, got: " <> show response)
            length posts `shouldBe` 0
            length runs `shouldBe` 0

    describe "social sync posts limit validation" $ do
        it "keeps the default only when the caller omits the limit and preserves valid explicit values" $ do
            validateSocialSyncPostsLimit Nothing `shouldBe` Right 50
            validateSocialSyncPostsLimit (Just 1) `shouldBe` Right 1
            validateSocialSyncPostsLimit (Just 500) `shouldBe` Right 500

        it "rejects out-of-range explicit limits instead of silently clamping social sync queries" $ do
            let assertInvalid raw = case validateSocialSyncPostsLimit (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "limit must be between 1 and 500"
                    Right value ->
                        expectationFailure ("Expected invalid social sync limit to be rejected, got " <> show value)
            assertInvalid 0
            assertInvalid (-5)
            assertInvalid 501

    describe "social sync posts list handler" $ do
        it "applies the tag filter before the response limit so tagged queries do not miss older matches" $ do
            let setup = do
                    let now = currentSocialSyncTestTime
                    insert_ (mkSocialSyncPost "instagram" "ig-general-newest" (Just "show") now)
                    insert_ (mkSocialSyncPost "instagram" "ig-release-match" (Just "release") (addUTCTime (-60) now))
                    insert_ (mkSocialSyncPost "instagram" "ig-general-older" Nothing (addUTCTime (-120) now))
            result <- runSocialSyncListHandler setup Nothing Nothing Nothing (Just " release ") (Just 1)
            case result of
                Left err ->
                    expectationFailure ("Expected tagged social sync query to succeed, got: " <> show err)
                Right posts ->
                    map sspdExternalPostId posts `shouldBe` ["ig-release-match"]

        it "treats blank tag filters as omitted instead of returning an accidental empty result set" $ do
            let setup = do
                    let now = currentSocialSyncTestTime
                    insert_ (mkSocialSyncPost "instagram" "ig-general-newest" Nothing now)
                    insert_ (mkSocialSyncPost "instagram" "ig-release-older" (Just "release") (addUTCTime (-60) now))
            result <- runSocialSyncListHandler setup Nothing Nothing Nothing (Just "   ") (Just 1)
            case result of
                Left err ->
                    expectationFailure ("Expected blank-tag social sync query to succeed, got: " <> show err)
                Right posts ->
                    map sspdExternalPostId posts `shouldBe` ["ig-general-newest"]

    describe "social events update payload parsing" $ do
        it "distinguishes missing metadata fields from explicit nulls for event updates" $ do
            let payload = "{\"eventTitle\":\"Test\",\"eventStart\":\"2026-01-01T00:00:00Z\",\"eventEnd\":\"2026-01-01T01:00:00Z\",\"eventArtists\":[],\"eventTicketUrl\":null,\"eventBudgetCents\":4500}"
            case eitherDecode payload :: Either String EventUpdateDTO of
                Left err -> expectationFailure err
                Right parsed -> do
                    emuTicketUrl (eudMetadataUpdate parsed) `shouldBe` FieldNull
                    emuBudgetCents (eudMetadataUpdate parsed) `shouldBe` FieldValue 4500

        it "rejects unexpected event update keys so typoed writes fail instead of silently no-oping" $
            case eitherDecode
                "{\"eventTitle\":\"Test\",\"eventStart\":\"2026-01-01T00:00:00Z\",\"eventEnd\":\"2026-01-01T01:00:00Z\",\"eventArtists\":[],\"eventTicketUrl\":null,\"unexpected\":true}"
                :: Either String EventUpdateDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure ("Expected unexpected event update keys to be rejected, got " <> show parsed)

        it "captures venue contact nulls and invitation message nulls in update payloads" $ do
            let venuePayload = "{\"venueName\":\"Sala Uno\",\"venuePhone\":null}"
                invitationPayload = "{\"invitationToPartyId\":\"12\",\"invitationMessage\":null}"
            case eitherDecode venuePayload :: Either String VenueUpdateDTO of
                Left err -> expectationFailure err
                Right parsed ->
                    vcuPhone (vudContactUpdate parsed) `shouldBe` FieldNull
            case eitherDecode invitationPayload :: Either String InvitationUpdateDTO of
                Left err -> expectationFailure err
                Right parsed ->
                    iudMessageUpdate parsed `shouldBe` FieldNull

        it "rejects unexpected venue update keys so contact typos fail instead of no-oping" $
            case eitherDecode
                "{\"venueName\":\"Sala Uno\",\"venuePhoneNumber\":\"+593991234567\"}"
                :: Either String VenueUpdateDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure ("Expected unexpected venue update keys to be rejected, got " <> show parsed)

        it "rejects unexpected invitation update keys before status defaults can hide typos" $
            case eitherDecode
                "{\"invitationToPartyId\":\"12\",\"invitationMessage\":null,\"message\":\"typo\"}"
                :: Either String InvitationUpdateDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure
                        ("Expected unexpected invitation update keys to be rejected, got " <> show parsed)

    describe "social event moment request parsing" $ do
        it "accepts canonical moment payloads and rejects unexpected keys before handlers silently ignore them" $ do
            case eitherDecode
                "{\"emCreateAuthorName\":\"Ada\",\"emCreateCaption\":\"Aftermovie\",\"emCreateMediaUrl\":\"https://cdn.example.com/moment.jpg\",\"emCreateMediaType\":\"image\",\"emCreateMediaWidth\":1080}"
                :: Either String EventMomentCreateDTO of
                Left err ->
                    expectationFailure ("Expected canonical moment create payload to decode, got " <> err)
                Right parsed -> do
                    emCreateAuthorName parsed `shouldBe` Just "Ada"
                    emCreateCaption parsed `shouldBe` Just "Aftermovie"
                    emCreateMediaUrl parsed `shouldBe` "https://cdn.example.com/moment.jpg"
                    emCreateMediaType parsed `shouldBe` "image"
                    emCreateMediaWidth parsed `shouldBe` Just 1080

            case eitherDecode
                "{\"emrrReaction\":\"fire\"}"
                :: Either String EventMomentReactionRequestDTO of
                Left err ->
                    expectationFailure ("Expected canonical moment reaction payload to decode, got " <> err)
                Right parsed ->
                    emrrReaction parsed `shouldBe` "fire"

            case eitherDecode
                "{\"emccAuthorName\":\"Ada\",\"emccBody\":\"Set impecable\"}"
                :: Either String EventMomentCommentCreateDTO of
                Left err ->
                    expectationFailure ("Expected canonical moment comment payload to decode, got " <> err)
                Right parsed -> do
                    emccAuthorName parsed `shouldBe` Just "Ada"
                    emccBody parsed `shouldBe` "Set impecable"

            case eitherDecode
                "{\"emCreateMediaUrl\":\"https://cdn.example.com/moment.jpg\",\"emCreateMediaType\":\"image\",\"unexpected\":true}"
                :: Either String EventMomentCreateDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure ("Expected unexpected moment create key to be rejected, got " <> show parsed)

            case eitherDecode
                "{\"emrrReaction\":\"fire\",\"reaction\":\"love\"}"
                :: Either String EventMomentReactionRequestDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure ("Expected unexpected moment reaction key to be rejected, got " <> show parsed)

            case eitherDecode
                "{\"emccBody\":\"Set impecable\",\"comment\":\"typo\"}"
                :: Either String EventMomentCommentCreateDTO of
                Left err ->
                    err `shouldContain` "unknown fields"
                Right parsed ->
                    expectationFailure ("Expected unexpected moment comment key to be rejected, got " <> show parsed)

    describe "social event image upload multipart parsing" $ do
        it "accepts the canonical file plus optional display name" $
            case fromMultipart
                (mkEventImageMultipart
                    [("name", "  Poster final  ")]
                    [mkEventImageFile "file" "poster.png"])
                :: Either String EventImageUploadForm of
                Left err ->
                    expectationFailure ("Expected canonical image upload form to parse, got: " <> err)
                Right parsed -> do
                    eiuName parsed `shouldBe` Just "Poster final"
                    fdFileName (eiuFile parsed) `shouldBe` "poster.png"

        it "rejects duplicate or unexpected multipart parts instead of silently ignoring upload intent" $ do
            case fromMultipart
                (mkEventImageMultipart
                    [("name", "Poster"), ("name", "Other")]
                    [mkEventImageFile "file" "poster.png"])
                :: Either String EventImageUploadForm of
                Left err ->
                    err `shouldContain` "Duplicate field: name"
                Right _ ->
                    expectationFailure "Expected duplicate name field to be rejected"

            case fromMultipart
                (mkEventImageMultipart
                    [("alt", "Poster")]
                    [mkEventImageFile "file" "poster.png"])
                :: Either String EventImageUploadForm of
                Left err ->
                    err `shouldContain` "Unexpected field: alt"
                Right _ ->
                    expectationFailure "Expected unexpected image field to be rejected"

            case fromMultipart
                (mkEventImageMultipart
                    []
                    [ mkEventImageFile "file" "poster.png"
                    , mkEventImageFile "stagePlot" "stage.pdf"
                    ])
                :: Either String EventImageUploadForm of
                Left err ->
                    err `shouldContain` "Unexpected file field: stagePlot"
                Right _ ->
                    expectationFailure "Expected unexpected image file field to be rejected"

            case fromMultipart
                (mkEventImageMultipart
                    []
                    [ mkEventImageFile "file" "poster.png"
                    , mkEventImageFile "file" "poster-copy.png"
                    ])
                :: Either String EventImageUploadForm of
                Left err ->
                    err `shouldContain` "Duplicate file field: file"
                Right _ ->
                    expectationFailure "Expected duplicate image file field to be rejected"

    describe "validateEventMetadataUpdate" $ do
        let baseUpdate = EventMetadataUpdateDTO
                { emuTicketUrl = FieldMissing
                , emuImageUrl = FieldMissing
                , emuIsPublic = FieldMissing
                , emuType = FieldMissing
                , emuStatus = FieldMissing
                , emuCurrency = FieldMissing
                , emuBudgetCents = FieldMissing
                }

        it "normalizes supported event type/status updates and keeps blank values as explicit clears" $ do
            validateEventMetadataUpdate
                baseUpdate
                    { emuType = FieldValue " FESTIVAL "
                    , emuStatus = FieldValue " canceled "
                    , emuCurrency = FieldValue " usd "
                    }
                `shouldBe` Right
                    baseUpdate
                        { emuType = FieldValue "festival"
                        , emuStatus = FieldValue "cancelled"
                        , emuCurrency = FieldValue "USD"
                        }
            validateEventMetadataUpdate baseUpdate { emuType = FieldValue "   " }
                `shouldBe` Right baseUpdate { emuType = FieldNull }
            validateEventMetadataUpdate baseUpdate { emuCurrency = FieldValue "   " }
                `shouldBe` Right baseUpdate { emuCurrency = FieldNull }

        it "rejects invalid explicit event metadata updates instead of silently ignoring them" $ do
            let assertInvalid updateValue expected =
                    case validateEventMetadataUpdate updateValue of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid event metadata update to be rejected, got " <> show value)
            assertInvalid
                baseUpdate { emuType = FieldValue "warehouse" }
                "eventType must be one of: party, concert, festival, conference, showcase, other"
            assertInvalid
                baseUpdate { emuStatus = FieldValue "sold_out" }
                "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"
            assertInvalid
                baseUpdate { emuCurrency = FieldValue "usdollars" }
                "eventCurrency must be a 3-letter ISO code"

    describe "validateEventCreateTypeStatus" $ do
        it "defaults omitted or blank create values and normalizes supported explicit values" $ do
            validateEventCreateTypeStatus Nothing Nothing
                `shouldBe` Right ("party", "planning")
            validateEventCreateTypeStatus (Just "   ") (Just " canceled ")
                `shouldBe` Right ("party", "cancelled")
            validateEventCreateTypeStatus (Just " FESTIVAL ") Nothing
                `shouldBe` Right ("festival", "planning")

        it "rejects invalid explicit create values instead of silently falling back" $ do
            let assertInvalid result expected =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid event create metadata to be rejected, got " <> show value)
            assertInvalid
                (validateEventCreateTypeStatus (Just "warehouse") Nothing)
                "eventType must be one of: party, concert, festival, conference, showcase, other"
            assertInvalid
                (validateEventCreateTypeStatus Nothing (Just "sold_out"))
                "eventStatus must be one of: planning, announced, on_sale, live, completed, cancelled"

    describe "validateEventCurrencyInput" $ do
        it "defaults omitted or blank create currencies to USD and normalizes explicit ISO codes" $ do
            validateEventCurrencyInput Nothing `shouldBe` Right "USD"
            validateEventCurrencyInput (Just "   ") `shouldBe` Right "USD"
            validateEventCurrencyInput (Just " eur ") `shouldBe` Right "EUR"

        it "rejects malformed explicit event currencies instead of storing opaque metadata" $ do
            case validateEventCurrencyInput (Just "usdollars") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "eventCurrency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure ("Expected invalid event currency to be rejected, got " <> show value)

    describe "validateEventCreateUpdateDimensions" $ do
        it "accepts omitted, free, and finite non-negative event pricing dimensions" $ do
            validateEventCreateUpdateDimensions Nothing Nothing Nothing `shouldBe` Right ()
            validateEventCreateUpdateDimensions (Just 0) (Just 0) (Just 0) `shouldBe` Right ()
            validateEventCreateUpdateDimensions (Just 2500) (Just 120) (Just 9000) `shouldBe` Right ()

        it "rejects negative event price, capacity, and budget values instead of storing invalid event dimensions" $ do
            let assertInvalid dims expected =
                    case dims of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid event dimensions to be rejected, got " <> show value)
            assertInvalid
                (validateEventCreateUpdateDimensions (Just (-1)) Nothing Nothing)
                "event price must be >= 0"
            assertInvalid
                (validateEventCreateUpdateDimensions Nothing (Just (-5)) Nothing)
                "event capacity must be >= 0"
            assertInvalid
                (validateEventCreateUpdateDimensions Nothing Nothing (Just (-10)))
                "event budget must be >= 0"

    describe "validateVenueCreateUpdateFields" $ do
        it "accepts named venues with omitted or bounded coordinate/capacity data" $ do
            validateVenueCreateUpdateFields "  Teatro TDF  " Nothing Nothing Nothing
                `shouldBe` Right ()
            validateVenueCreateUpdateFields "Teatro TDF" (Just (-0.18)) (Just (-78.48)) (Just 0)
                `shouldBe` Right ()
            validateVenueCreateUpdateFields "Teatro TDF" (Just 90) (Just 180) (Just 250)
                `shouldBe` Right ()

        it "rejects ambiguous venue dimensions before persistence" $ do
            let assertInvalid result expected =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid venue fields to be rejected, got " <> show value)
            assertInvalid
                (validateVenueCreateUpdateFields "   " Nothing Nothing Nothing)
                "venue name is required"
            assertInvalid
                (validateVenueCreateUpdateFields "Teatro\nTDF" Nothing Nothing Nothing)
                "venue name must not contain control characters"
            assertInvalid
                (validateVenueCreateUpdateFields "Teatro TDF" (Just (-0.18)) Nothing Nothing)
                "venue latitude and longitude must be provided together"
            assertInvalid
                (validateVenueCreateUpdateFields "Teatro TDF" (Just 91) (Just (-78.48)) Nothing)
                "venue latitude must be between -90 and 90"
            assertInvalid
                (validateVenueCreateUpdateFields "Teatro TDF" (Just (-0.18)) (Just 181) Nothing)
                "venue longitude must be between -180 and 180"
            assertInvalid
                (validateVenueCreateUpdateFields
                    "Teatro TDF"
                    (Just (0 / 0))
                    (Just (-78.48))
                    Nothing)
                "Invalid venue latitude"
            assertInvalid
                (validateVenueCreateUpdateFields "Teatro TDF" Nothing Nothing (Just (-1)))
                "venue capacity must be >= 0"

    describe "isImageUpload" $ do
        it "requires matching raster MIME and extension for event image uploads" $ do
            isImageUpload " image/jpeg " "poster.JPG" `shouldBe` True
            isImageUpload "image/png; charset=binary" "poster.png" `shouldBe` True
            isImageUpload "application/octet-stream" "poster.jpg" `shouldBe` False
            isImageUpload "image/jpeg" "poster.txt" `shouldBe` False
            isImageUpload "image/png" "poster.jpg" `shouldBe` False
            isImageUpload "image/svg+xml" "poster.svg" `shouldBe` False

    describe "validateEventArtistIds" $ do
        let mkArtist rawArtistId =
                ArtistDTO
                    { artistId = rawArtistId
                    , artistPartyId = Nothing
                    , artistName = ""
                    , artistGenres = []
                    , artistBio = Nothing
                    , artistAvatarUrl = Nothing
                    , artistSocialLinks = Nothing
                    , artistCreatedAt = Nothing
                    , artistUpdatedAt = Nothing
                    }

        it "preserves omitted artist references and canonicalizes explicit positive ids" $ do
            fmap (map fromSqlKey)
                (validateEventArtistIds [mkArtist Nothing, mkArtist (Just " 0042 "), mkArtist (Just "7")])
                `shouldBe` Right [42, 7]

        it "rejects duplicate artist ids before event writes hit the join-table constraint" $
            case validateEventArtistIds [mkArtist (Just "42"), mkArtist (Just "0042")] of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "eventArtists[].artistId must be unique"
                Right value ->
                    expectationFailure ("Expected duplicate event artist ids to be rejected, got " <> show value)

        it "rejects malformed explicit artist ids instead of silently dropping event links" $ do
            let assertInvalid rawArtistId =
                    case validateEventArtistIds [mkArtist (Just rawArtistId)] of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "eventArtists[].artistId must be a positive integer"
                        Right value ->
                            expectationFailure ("Expected invalid event artist id to be rejected, got " <> show value)
            assertInvalid "   "
            assertInvalid "artist-42"
            assertInvalid "0"
            assertInvalid "-5"

    describe "parseVenueIdEither" $ do
        it "canonicalizes positive venue ids before social event lookups and writes" $ do
            fmap fromSqlKey (parseVenueIdEither " 0042 ")
                `shouldBe` Right 42

        it "rejects blank, non-numeric, or non-positive venue ids instead of issuing ambiguous event queries" $ do
            let assertInvalid rawVenueId =
                    case parseVenueIdEither rawVenueId of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "Invalid venue id"
                        Right value ->
                            expectationFailure ("Expected invalid venue id to be rejected, got " <> show (fromSqlKey value))
            assertInvalid "   "
            assertInvalid "venue-42"
            assertInvalid "0"
            assertInvalid "-5"

    describe "normalizePositivePartyIdText" $ do
        it "accepts positive numeric ids and canonicalizes them" $ do
            normalizePositivePartyIdText " 0042 " `shouldBe` Just "42"

        it "rejects blank and non-numeric ids" $ do
            normalizePositivePartyIdText "   " `shouldBe` Nothing
            normalizePositivePartyIdText "abc" `shouldBe` Nothing

    describe "moment normalizers" $ do
        it "normalizes supported media types and reactions" $ do
            normalizeMomentMediaType " PHOTO " `shouldBe` Just "image"
            normalizeMomentMediaType "clip" `shouldBe` Just "video"
            normalizeMomentMediaType "audio" `shouldBe` Nothing
            normalizeMomentReaction "heart" `shouldBe` Just "love"
            normalizeMomentReaction "CLAP" `shouldBe` Just "applause"
            normalizeMomentReaction "wow" `shouldBe` Nothing

        it "accepts blank captions as missing and rejects oversize captions" $ do
            normalizeMomentCaption (Just "   ") `shouldBe` Right Nothing
            normalizeMomentCaption (Just "  aftermovie  ") `shouldBe` Right (Just "aftermovie")
            case normalizeMomentCaption (Just (Data.Text.replicate 281 "a")) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment caption must be 280 characters or less"
                Right value ->
                    expectationFailure ("Expected oversize caption to fail, got " <> show value)

        it "requires non-empty comment bodies and enforces comment length" $ do
            normalizeMomentCommentBody "  impecable set  " `shouldBe` Right "impecable set"
            case normalizeMomentCommentBody "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment comment body is required"
                Right value ->
                    expectationFailure ("Expected blank moment comment to fail, got " <> show value)
            case normalizeMomentCommentBody (Data.Text.replicate 501 "b") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Moment comment body must be 500 characters or less"
                Right value ->
                    expectationFailure ("Expected oversize moment comment to fail, got " <> show value)

    describe "parseFollowerQueryParamEither" $ do
        it "canonicalizes numeric follower query params before delete lookups" $ do
            parseFollowerQueryParamEither (Just " 0042 ") `shouldBe` Right "42"

        it "rejects missing or blank follower query params" $ do
            case parseFollowerQueryParamEither Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param is required"
                Right _ -> expectationFailure "Expected missing follower query param to be rejected"
            case parseFollowerQueryParamEither (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param is required"
                Right _ -> expectationFailure "Expected blank follower query param to be rejected"

        it "rejects non-positive or non-numeric follower query params" $ do
            case parseFollowerQueryParamEither (Just "abc") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param must be a positive integer"
                Right _ -> expectationFailure "Expected invalid follower query param to be rejected"
            case parseFollowerQueryParamEither (Just "0") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "follower query param must be a positive integer"
                Right _ -> expectationFailure "Expected non-positive follower query param to be rejected"

    describe "validateOptionalTicketBuyerPartyId" $ do
        it "treats omitted or blank buyer ids as absent and canonicalizes positive ids" $ do
            validateOptionalTicketBuyerPartyId "buyerPartyId" Nothing
                `shouldBe` Right Nothing
            validateOptionalTicketBuyerPartyId "buyerPartyId" (Just "   ")
                `shouldBe` Right Nothing
            validateOptionalTicketBuyerPartyId "buyerPartyId" (Just " 0042 ")
                `shouldBe` Right (Just "42")

        it "rejects malformed buyer ids before ticket order filters or writes become ambiguous" $ do
            let assertInvalid field raw expected =
                    case validateOptionalTicketBuyerPartyId field (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected invalid optional ticket buyer id to be rejected, got " <> show value)
            assertInvalid "buyerPartyId" "buyer-42" "buyerPartyId must be a positive integer"
            assertInvalid "buyerPartyId" "0" "buyerPartyId must be a positive integer"
            assertInvalid "ticketPurchaseBuyerPartyId" "-7" "ticketPurchaseBuyerPartyId must be a positive integer"

    describe "parseInvitationIdsEither" $ do
        it "parses numeric ids into typed keys" $ do
            let expected :: (SocialEventId, EventInvitationId)
                expected = (toSqlKey 42, toSqlKey 77)
            parseInvitationIdsEither "42" "77" `shouldBe` Right expected

        it "returns a ServerError for invalid ids" $ do
            case parseInvitationIdsEither "x" "1" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid event or invitation id"
                Right _ -> expectationFailure "Expected an error for invalid ids"

        it "rejects non-positive ids" $ do
            case parseInvitationIdsEither "0" "-3" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid event or invitation id"
                Right _ -> expectationFailure "Expected an error for non-positive ids"

    describe "parseNearQueryEither" $ do
        it "parses finite coordinates and default radius" $ do
            parseNearQueryEither "-0.18, -78.48" `shouldBe` Right (-0.18, -78.48, 25)

        it "rejects non-finite coordinates and radius values" $ do
            case parseNearQueryEither "NaN,-78.48" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid near latitude"
                Right _ -> expectationFailure "Expected NaN latitude to be rejected"
            case parseNearQueryEither "-0.18,-78.48,Infinity" of
                Left err -> BL.unpack (errBody err) `shouldContain` "Invalid near radiusKm"
                Right _ -> expectationFailure "Expected Infinity radius to be rejected"

    describe "normalizeTicketOrderStatus" $ do
        it "defaults to pending for empty/unknown values" $ do
            normalizeTicketOrderStatus Nothing `shouldBe` "pending"
            normalizeTicketOrderStatus (Just "  ") `shouldBe` "pending"
            normalizeTicketOrderStatus (Just "unknown") `shouldBe` "pending"

        it "normalizes valid statuses" $ do
            normalizeTicketOrderStatus (Just "PAID") `shouldBe` "paid"
            normalizeTicketOrderStatus (Just "canceled") `shouldBe` "cancelled"

    describe "normalizeTicketStatus" $ do
        it "defaults to issued when missing" $ do
            normalizeTicketStatus Nothing `shouldBe` "issued"

        it "normalizes alternate ticket status spellings" $ do
            normalizeTicketStatus (Just "checkedin") `shouldBe` "checked_in"
            normalizeTicketStatus (Just "CANCELED") `shouldBe` "cancelled"

    describe "validateTicketPurchaseBuyerEmail" $ do
        it "normalizes valid optional buyer emails before ticket order storage" $ do
            validateTicketPurchaseBuyerEmail Nothing `shouldBe` Right Nothing
            validateTicketPurchaseBuyerEmail (Just "   ") `shouldBe` Right Nothing
            validateTicketPurchaseBuyerEmail (Just " Buyer+Ticket@Example.COM ")
                `shouldBe` Right (Just "buyer+ticket@example.com")

        it "rejects malformed buyer emails instead of storing unusable ticket contacts" $ do
            let assertInvalid raw expected =
                    case validateTicketPurchaseBuyerEmail (Just raw) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected invalid ticket buyer email to be rejected, got "
                                    <> show value
                                )
            assertInvalid
                "not-an-email"
                "ticketPurchaseBuyerEmail must be a valid email address"
            assertInvalid
                "buyer @example.com"
                "ticketPurchaseBuyerEmail must be a valid email address"
            assertInvalid
                "buyer@example..com"
                "ticketPurchaseBuyerEmail must be a valid email address"
            assertInvalid
                (Data.Text.replicate 245 "a" <> "@example.com")
                "ticketPurchaseBuyerEmail must be 254 characters or fewer"

    describe "validateTicketCheckInLookup" $ do
        it "rejects unknown check-in request fields before lookup fallback handling" $ do
            let decoded =
                    eitherDecode @TicketCheckInRequestDTO
                        "{\"ticketCheckInTicketCode\":\"AB-123\",\"ticketCheckInTicketCod\":\"typo\"}"
                        :: Either String TicketCheckInRequestDTO
            decoded `shouldSatisfy` isLeft

        it "accepts exactly one lookup field and canonicalizes ticket ids and codes" $ do
            validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Just " 0042 "
                    , ticketCheckInTicketCode = Nothing
                    }
                `shouldBe` Right (TicketCheckInLookupById "42")
            validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Nothing
                    , ticketCheckInTicketCode = Just " tdf-ab12cd34ef56 "
                    }
                `shouldBe` Right (TicketCheckInLookupByCode "TDF-AB12CD34EF56")

        it "rejects ambiguous check-in payloads that provide both id and code" $ do
            case validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Just "42"
                    , ticketCheckInTicketCode = Just "ab-123"
                    } of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Provide exactly one of ticketCheckInTicketId or ticketCheckInTicketCode"
                Right value ->
                    expectationFailure ("Expected ambiguous ticket check-in payload to be rejected, got " <> show value)

            case validateTicketCheckInLookup
                TicketCheckInRequestDTO
                    { ticketCheckInTicketId = Just "42"
                    , ticketCheckInTicketCode = Just "   "
                    } of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Provide exactly one of ticketCheckInTicketId or ticketCheckInTicketCode"
                Right value ->
                    expectationFailure ("Expected blank secondary ticket check-in lookup to be rejected, got " <> show value)

        it "rejects non-numeric or non-positive ticket ids before lookup" $ do
            let assertInvalid rawTicketId =
                    case validateTicketCheckInLookup
                        TicketCheckInRequestDTO
                            { ticketCheckInTicketId = Just rawTicketId
                            , ticketCheckInTicketCode = Nothing
                            } of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "ticketCheckInTicketId must be a positive integer"
                        Right value ->
                            expectationFailure ("Expected invalid ticket id to be rejected, got " <> show value)
            assertInvalid "   "
            assertInvalid "0"
            assertInvalid "-7"
            assertInvalid "ticket-42"

        it "rejects malformed ticket codes before database lookup fallback" $ do
            let assertInvalid rawCode =
                    case validateTicketCheckInLookup
                        TicketCheckInRequestDTO
                            { ticketCheckInTicketId = Nothing
                            , ticketCheckInTicketCode = Just rawCode
                            } of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "ticketCheckInTicketCode must be a generated ticket code"
                        Right value ->
                            expectationFailure ("Expected invalid ticket code to be rejected, got " <> show value)
            assertInvalid "AB-123"
            assertInvalid "TDF-123"
            assertInvalid "TDF-ABCDEFGHIJKL"
            assertInvalid "TDF-AB12 CD34EF"

    describe "validateTicketCheckInOrderStatus" $ do
        it "accepts canonical paid and refunded order states for ticket check-in decisions" $ do
            validateTicketCheckInOrderStatus (Just " PAID ") `shouldBe` Right "paid"
            validateTicketCheckInOrderStatus (Just "canceled") `shouldBe` Right "cancelled"

        it "rejects missing or invalid stored order states instead of pretending the ticket is merely unpaid" $ do
            let assertInvariant expectedMessage result =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 500
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ("Expected invalid ticket check-in order state to be rejected, got " <> show value)
            assertInvariant "Ticket order could not be loaded"
                (validateTicketCheckInOrderStatus Nothing)
            assertInvariant "Stored ticket order status is invalid"
                (validateTicketCheckInOrderStatus (Just ""))
            assertInvariant "Stored ticket order status is invalid"
                (validateTicketCheckInOrderStatus (Just "unknown"))

    describe "validateTicketCheckInTicketStatus" $ do
        it "accepts canonical ticket states for ticket check-in decisions" $ do
            validateTicketCheckInTicketStatus " ISSUED " `shouldBe` Right "issued"
            validateTicketCheckInTicketStatus "checkedin" `shouldBe` Right "checked_in"
            validateTicketCheckInTicketStatus "CANCELED" `shouldBe` Right "cancelled"

        it "rejects invalid stored ticket states instead of treating them as issued" $ do
            let assertInvariant rawStatus =
                    case validateTicketCheckInTicketStatus rawStatus of
                        Left err -> do
                            errHTTPCode err `shouldBe` 500
                            BL.unpack (errBody err) `shouldContain` "Stored ticket status is invalid"
                        Right value ->
                            expectationFailure
                                ("Expected invalid ticket check-in state to be rejected, got " <> show value)
            assertInvariant ""
            assertInvariant "unknown"

    describe "findTicketForCheckIn" $ do
        it "keeps numeric ticket-id lookup scoped to the requested event" $ do
            result <- runStdoutLoggingT $ do
                pool <- createSqlitePool ":memory:" 1
                liftIO $ runSqlPool initializeTicketCheckInSchema pool
                liftIO $ runSqlPool (do
                    let now = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)
                    firstEventId <-
                        insert
                            SocialEvent
                                { socialEventOrganizerPartyId = Just "1"
                                , socialEventTitle = "First Event"
                                , socialEventDescription = Nothing
                                , socialEventVenueId = Nothing
                                , socialEventStartTime = now
                                , socialEventEndTime = addUTCTime 3600 now
                                , socialEventPriceCents = Nothing
                                , socialEventCapacity = Nothing
                                , socialEventMetadata = Nothing
                                , socialEventCreatedAt = now
                                , socialEventUpdatedAt = now
                                }
                    secondEventId <-
                        insert
                            SocialEvent
                                { socialEventOrganizerPartyId = Just "2"
                                , socialEventTitle = "Second Event"
                                , socialEventDescription = Nothing
                                , socialEventVenueId = Nothing
                                , socialEventStartTime = now
                                , socialEventEndTime = addUTCTime 5400 now
                                , socialEventPriceCents = Nothing
                                , socialEventCapacity = Nothing
                                , socialEventMetadata = Nothing
                                , socialEventCreatedAt = now
                                , socialEventUpdatedAt = now
                                }
                    tierId <-
                        insert
                            EventTicketTier
                                { eventTicketTierEventId = secondEventId
                                , eventTicketTierCode = "GEN"
                                , eventTicketTierName = "General"
                                , eventTicketTierDescription = Nothing
                                , eventTicketTierPriceCents = 1500
                                , eventTicketTierCurrency = "USD"
                                , eventTicketTierQuantityTotal = 10
                                , eventTicketTierQuantitySold = 1
                                , eventTicketTierSalesStart = Nothing
                                , eventTicketTierSalesEnd = Nothing
                                , eventTicketTierIsActive = True
                                , eventTicketTierPosition = Nothing
                                , eventTicketTierCreatedAt = now
                                , eventTicketTierUpdatedAt = now
                                }
                    orderId <-
                        insert
                            EventTicketOrder
                                { eventTicketOrderEventId = secondEventId
                                , eventTicketOrderTierId = tierId
                                , eventTicketOrderBuyerPartyId = Just "2"
                                , eventTicketOrderBuyerName = Just "Ada"
                                , eventTicketOrderBuyerEmail = Just "ada@example.com"
                                , eventTicketOrderQuantity = 1
                                , eventTicketOrderAmountCents = 1500
                                , eventTicketOrderCurrency = "USD"
                                , eventTicketOrderStatus = "paid"
                                , eventTicketOrderMetadata = Nothing
                                , eventTicketOrderPurchasedAt = now
                                , eventTicketOrderCreatedAt = now
                                , eventTicketOrderUpdatedAt = now
                                }
                    ticketId <-
                        insert
                            EventTicket
                                { eventTicketEventId = secondEventId
                                , eventTicketTierRefId = tierId
                                , eventTicketOrderRefId = orderId
                                , eventTicketHolderName = Just "Ada"
                                , eventTicketHolderEmail = Just "ada@example.com"
                                , eventTicketCode = "TDF-AB12CD34EF56"
                                , eventTicketStatus = "issued"
                                , eventTicketCheckedInAt = Nothing
                                , eventTicketCreatedAt = now
                                , eventTicketUpdatedAt = now
                                }
                    scopedMiss <- findTicketForCheckIn firstEventId (TicketCheckInLookupById (Data.Text.pack (show (fromSqlKey ticketId))))
                    scopedHit <- findTicketForCheckIn secondEventId (TicketCheckInLookupById (Data.Text.pack (show (fromSqlKey ticketId))))
                    pure (scopedMiss, scopedHit)) pool

            case result of
                (Nothing, Just (Entity foundId foundTicket)) -> do
                    fromSqlKey foundId `shouldSatisfy` (> 0)
                    eventTicketCode foundTicket `shouldBe` "TDF-AB12CD34EF56"
                other ->
                    expectationFailure
                        ("Expected cross-event ticket-id lookup to miss and same-event lookup to hit, got " <> show other)

    describe "validateRadioStreamUrl" $ do
        it "trims surrounding whitespace and accepts http(s) stream URLs" $
            validateRadioStreamUrl "  HTTPS://radio.example.com/live  "
                `shouldBe` Right "HTTPS://radio.example.com/live"

        it "accepts explicit numeric ports, including bracketed IPv6 hosts" $ do
            validateRadioStreamUrl "https://radio.example.com:8443/live"
                `shouldBe` Right "https://radio.example.com:8443/live"
            validateRadioStreamUrl "https://[2001:4860:4860::8888]:8000/live"
                `shouldBe` Right "https://[2001:4860:4860::8888]:8000/live"

        it "rejects blank stream URLs with a precise 400" $
            case validateRadioStreamUrl "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl is required"
                Right _ -> expectationFailure "Expected blank streamUrl to be rejected"

        it "rejects malformed absolute URLs that would fail later in the radio pipeline" $ do
            case validateRadioStreamUrl "https://" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must include a host"
                Right _ -> expectationFailure "Expected hostless streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com/live stream" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must not contain whitespace"
                Right _ -> expectationFailure "Expected whitespace-containing streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com/live\DEL" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must not contain control characters"
                Right _ -> expectationFailure "Expected control-character streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com/live#main" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must not include a fragment"
                Right _ -> expectationFailure "Expected fragment-bearing streamUrl to be rejected"
            case validateRadioStreamUrl
                    ("https://radio.example.com/" <> Data.Text.replicate 2049 "a") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "streamUrl must be 2048 characters or fewer"
                Right _ -> expectationFailure "Expected overlong streamUrl to be rejected"

        it "rejects hostless or malformed-port URLs before they can be stored" $ do
            case validateRadioStreamUrl "https://:8443/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must include a host"
                Right _ -> expectationFailure "Expected hostless streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com:/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl port must be numeric"
                Right _ -> expectationFailure "Expected empty-port streamUrl to be rejected"
            case validateRadioStreamUrl "https://radio.example.com:abc/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl port must be numeric"
                Right _ -> expectationFailure "Expected non-numeric port streamUrl to be rejected"

        it "rejects out-of-range ports instead of storing unreachable stream endpoints" $ do
            let assertInvalid rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "streamUrl port must be between 1 and 65535"
                        Right value ->
                            expectationFailure ("Expected invalid streamUrl port to be rejected, got " <> show value)
            assertInvalid "https://radio.example.com:0/live"
            assertInvalid "https://radio.example.com:65536/live"
            assertInvalid "https://[2001:4860:4860::8888]:70000/live"

        it "rejects malformed host labels instead of storing unusable stream endpoints" $ do
            let assertInvalid rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "streamUrl must include a valid host"
                        Right value ->
                            expectationFailure ("Expected malformed streamUrl host to be rejected, got " <> show value)
            assertInvalid "https://-radio.example.com/live"
            assertInvalid "https://radio-.example.com/live"
            assertInvalid "https://radio..example.com/live"
            assertInvalid "https://radio.example.com./live"
            assertInvalid "https://rádio.example.com/live"
            assertInvalid "https://[2001:db8::１]/live"

        it "rejects malformed bracketed IPv6 hosts before metadata fetches can use them" $ do
            let assertInvalid rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "streamUrl must include a valid host"
                        Right value ->
                            expectationFailure
                                ("Expected malformed bracketed IPv6 host to be rejected, got " <> show value)
            assertInvalid "https://[2001:4860:4860::88888]/live"
            assertInvalid "https://[2001:4860::4860::8888]/live"
            assertInvalid "https://[2001:4860:4860:4860:4860:4860:4860]/live"
            assertInvalid "https://[2001:4860:4860::0:0:0:0:8888]/live"

        it "rejects single-label hosts that could resolve through private search domains" $ do
            let assertInvalid rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain` "streamUrl host must be a public hostname or IP address"
                        Right value ->
                            expectationFailure
                                ("Expected single-label streamUrl host to be rejected, got " <> show value)
            assertInvalid "https://radio/live"
            assertInvalid "https://intranet:8443/live"

        it "rejects ambiguous numeric host shortcuts before they can bypass host checks" $ do
            let assertInvalid rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "streamUrl must include a valid host"
                        Right value ->
                            expectationFailure
                                ("Expected ambiguous numeric streamUrl host to be rejected, got " <> show value)
            assertInvalid "https://127.1/live"
            assertInvalid "https://127.0.1/live"
            assertInvalid "https://2130706433/live"
            assertInvalid "https://0177.0.0.1/live"
            assertInvalid "https://[::ffff:0177.0.0.1]/live"

        it "rejects localhost and private-network targets before the server fetches them" $ do
            let assertPrivateTarget rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain` "streamUrl must not target localhost or private network addresses"
                        Right value ->
                            expectationFailure
                                ("Expected private streamUrl target to be rejected, got " <> show value)
            assertPrivateTarget "https://localhost/live"
            assertPrivateTarget "https://radio.localhost/live"
            assertPrivateTarget "https://127.0.0.1/live"
            assertPrivateTarget "https://192.168.1.23/live"
            assertPrivateTarget "https://[::1]/live"
            assertPrivateTarget "https://[fd12::1234]/live"
            assertPrivateTarget "https://[::ffff:127.0.0.1]/live"
            assertPrivateTarget "https://[::ffff:7f00:1]/live"
            assertPrivateTarget "https://[0:0:0:0:0:ffff:c0a8:117]/live"

        it "rejects reserved IP ranges before metadata/import fetches can target them" $ do
            let assertReservedTarget rawUrl =
                    case validateRadioStreamUrl rawUrl of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain` "reserved ranges"
                        Right value ->
                            expectationFailure
                                ("Expected reserved streamUrl target to be rejected, got " <> show value)
            assertReservedTarget "https://192.0.2.10/live"
            assertReservedTarget "https://198.51.100.20/live"
            assertReservedTarget "https://203.0.113.5/live"
            assertReservedTarget "https://198.19.0.1/live"
            assertReservedTarget "https://224.0.0.1/live"
            assertReservedTarget "https://255.255.255.255/live"
            assertReservedTarget "https://[2001:db8::1]/live"
            assertReservedTarget "https://[ff02::1]/live"

        it "rejects authorities that embed user info instead of storing credential-like stream URLs" $
            case validateRadioStreamUrl "https://dj@radio.example.com/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must not include user info"
                Right _ -> expectationFailure "Expected userinfo-bearing streamUrl to be rejected"

        it "rejects non-http stream URLs before they can be stored" $
            case validateRadioStreamUrl "ftp://radio.example.com/live" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "streamUrl must be http(s)"
                Right _ -> expectationFailure "Expected non-http streamUrl to be rejected"

    describe "validateRadioTransmissionPublicBase" $ do
        it "normalizes the configured public base before appending generated stream keys" $
            validateRadioTransmissionPublicBase "  HTTPS://radio.example.com/live/  "
                `shouldBe` Right "https://radio.example.com/live"

        it "rejects query or fragment bases because generated stream keys would be ambiguous" $ do
            let assertQueryInvalid rawBase =
                    case validateRadioTransmissionPublicBase rawBase of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "RADIO_PUBLIC_BASE must not include query or fragment"
                        Right value ->
                            expectationFailure
                                ("Expected invalid public radio base to be rejected, got " <> show value)
                assertFragmentInvalid rawBase =
                    case validateRadioTransmissionPublicBase rawBase of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "RADIO_PUBLIC_BASE must not include a fragment"
                        Right value ->
                            expectationFailure
                                ("Expected invalid public radio base to be rejected, got " <> show value)
            assertQueryInvalid "https://radio.example.com/live?token=abc"
            assertFragmentInvalid "https://radio.example.com/live#main"

        it "rejects malformed or private public bases before transmission URLs are persisted" $ do
            let assertInvalid rawBase expected =
                    case validateRadioTransmissionPublicBase rawBase of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected invalid public radio base to be rejected, got " <> show value)
            assertInvalid "https://" "RADIO_PUBLIC_BASE must include a host"
            assertInvalid
                "http://127.0.0.1/live"
                "RADIO_PUBLIC_BASE must not target localhost or private network addresses"

    describe "resolveRadioTransmissionEnvBase" $ do
        it "uses fallback bases only when transmission env vars are absent" $ do
            resolveRadioTransmissionEnvBase
                "RADIO_PUBLIC_BASE"
                "https://tdf-hq.fly.dev/live"
                Nothing
                `shouldBe` Right "https://tdf-hq.fly.dev/live"
            resolveRadioTransmissionEnvBase
                "RADIO_PUBLIC_BASE"
                "https://tdf-hq.fly.dev/live"
                (Just "  https://radio.example.com/live  ")
                `shouldBe` Right "https://radio.example.com/live"

        it "rejects explicitly blank transmission env vars instead of silently falling back" $ do
            let assertBlank label rawValue =
                    case
                        resolveRadioTransmissionEnvBase
                            label
                            "https://fallback.example.com"
                            (Just rawValue)
                    of
                        Left err -> do
                            errHTTPCode err `shouldBe` 500
                            BL.unpack (errBody err)
                                `shouldContain`
                                    Data.Text.unpack (label <> " is configured but blank")
                        Right value ->
                            expectationFailure
                                ("Expected blank radio base env to be rejected, got " <> show value)
            assertBlank "RADIO_PUBLIC_BASE" "   "
            assertBlank "RADIO_INGEST_BASE" "\t\n"
            assertBlank "RADIO_WHIP_BASE" ""

    describe "validateRadioTransmission endpoint bases" $ do
        it "normalizes configured ingest and WHIP bases before appending generated stream keys" $ do
            validateRadioTransmissionIngestBase "  RTMPS://stream.example.com/live/  "
                `shouldBe` Right "rtmps://stream.example.com/live"
            validateRadioTransmissionWhipBase "  HTTPS://stream.example.com/whip/  "
                `shouldBe` Right "https://stream.example.com/whip"

        it "rejects malformed endpoint overrides before returning unusable transmission URLs" $ do
            let assertInvalid result expected =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ( "Expected invalid transmission endpoint base, got "
                                    <> show value
                                )
            assertInvalid
                (validateRadioTransmissionIngestBase "https://stream.example.com/live")
                "RADIO_INGEST_BASE must be rtmp(s)"
            assertInvalid
                (validateRadioTransmissionIngestBase "rtmp://stream.example.com/live?token=abc")
                "RADIO_INGEST_BASE must not include query or fragment"
            assertInvalid
                (validateRadioTransmissionIngestBase "rtmp://stream.example.com/live\DEL")
                "RADIO_INGEST_BASE must not contain control characters"
            assertInvalid
                (validateRadioTransmissionWhipBase "rtmp://stream.example.com/whip")
                "RADIO_WHIP_BASE must be https"
            assertInvalid
                (validateRadioTransmissionWhipBase "http://stream.example.com/whip")
                "RADIO_WHIP_BASE must be https"
            assertInvalid
                (validateRadioTransmissionWhipBase "https://127.0.0.1/whip")
                "RADIO_WHIP_BASE must not target localhost or private network addresses"

    describe "validateRadioOptionalMetadataField" $ do
        it "trims optional radio metadata and treats blank values as omitted" $ do
            validateRadioOptionalMetadataField "rtrName" 160 Nothing `shouldBe` Right Nothing
            validateRadioOptionalMetadataField "rtrName" 160 (Just "  TDF Live  ")
                `shouldBe` Right (Just "TDF Live")
            validateRadioOptionalMetadataField "rtrName" 160 (Just "   ")
                `shouldBe` Right Nothing

        it "rejects malformed transmission metadata before generated streams are persisted" $ do
            let assertInvalid result expected =
                    case result of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure
                                ("Expected invalid radio metadata to be rejected, got " <> show value)
            assertInvalid
                (validateRadioOptionalMetadataField
                    "rtrName"
                    160
                    (Just (Data.Text.replicate 161 "a")))
                "rtrName must be 160 characters or fewer"
            assertInvalid
                (validateRadioOptionalMetadataField "rtrGenre" 120 (Just "ambient\ntechno"))
                "rtrGenre must not contain control characters"

    describe "validateRadioImportSources" $ do
        it "uses defaults only when sources are omitted and canonicalizes explicit public import URLs" $ do
            validateRadioImportSources Nothing
                `shouldBe` Right
                    [ "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
                    , "https://www.rcast.net/dir"
                    , "https://www.internet-radio.com"
                    , "https://raw.githubusercontent.com/junguler/m3u-radio-music-playlists/master/all.m3u"
                    ]
            validateRadioImportSources
                (Just ["  https://github.com/mikepierce/internet-radio-streams/blob/master/streams.csv  "])
                `shouldBe` Right
                    [ "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv" ]

        it "de-duplicates canonical-equivalent explicit sources so the importer does not fetch the same catalog twice" $
            validateRadioImportSources
                ( Just
                    [ "https://github.com/mikepierce/internet-radio-streams/blob/master/streams.csv"
                    , " https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv "
                    , "https://www.internet-radio.com"
                    , "https://WWW.internet-radio.com"
                    ]
                )
                `shouldBe` Right
                    [ "https://raw.githubusercontent.com/mikepierce/internet-radio-streams/master/streams.csv"
                    , "https://www.internet-radio.com"
                    ]

        it "only canonicalizes exact GitHub catalog repositories, not lookalike hosts or repo prefixes" $ do
            let spoofedHost = "https://example.com/cache/github.com/mikepierce/internet-radio-streams/blob/master/streams.csv"
                repoPrefix = "https://github.com/mikepierce/internet-radio-streams-extra/blob/master/streams.csv"
            validateRadioImportSources (Just [spoofedHost, repoPrefix])
                `shouldBe` Right [spoofedHost, repoPrefix]

        it "rejects explicit source lists above the importer fan-out budget" $ do
            let source n =
                    "https://radio"
                        <> Data.Text.pack (show n)
                        <> ".example.com/catalog.csv"
            case validateRadioImportSources (Just (map source [(1 :: Int) .. 9])) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "sources must include at most 8 public http(s) URLs"
                Right value ->
                    expectationFailure
                        ("Expected too many radio import sources to be rejected, got " <> show value)

        it "rejects explicit empty or invalid source lists instead of silently falling back to defaults" $ do
            let assertInvalid rawSources expected = case validateRadioImportSources rawSources of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid radio import sources to be rejected, got " <> show value)
            assertInvalid (Just []) "sources must include at least one public http(s) URL"
            assertInvalid (Just ["   "]) "sources must include at least one public http(s) URL"
            assertInvalid (Just ["ftp://radio.example.com/catalog.csv"]) "source must be http(s)"
            assertInvalid (Just ["http://127.0.0.1/catalog.csv"]) "source must not target localhost or private network addresses"

    describe "validateRadioImportLimit" $ do
        it "uses the import default only when the caller omits the limit and caps explicit requests at the import batch budget" $ do
            validateRadioImportLimit Nothing `shouldBe` Right 800
            validateRadioImportLimit (Just 1) `shouldBe` Right 1
            validateRadioImportLimit (Just 800) `shouldBe` Right 800

        it "rejects out-of-range explicit import limits instead of silently clamping them" $ do
            let assertRejected rawLimit =
                    case validateRadioImportLimit (Just rawLimit) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "limit must be between 1 and 800"
                        Right value ->
                            expectationFailure ("Expected invalid radio import limit to be rejected, got " <> show value)
            assertRejected 0
            assertRejected 801

    describe "validateRadioMetadataRefreshLimit" $ do
        it "uses the refresh default only when the caller omits the limit and caps explicit requests at the refresh batch budget" $ do
            validateRadioMetadataRefreshLimit Nothing `shouldBe` Right 400
            validateRadioMetadataRefreshLimit (Just 1) `shouldBe` Right 1
            validateRadioMetadataRefreshLimit (Just 400) `shouldBe` Right 400

        it "rejects out-of-range explicit refresh limits instead of silently clamping them" $ do
            let assertRejected rawLimit =
                    case validateRadioMetadataRefreshLimit (Just rawLimit) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "limit must be between 1 and 400"
                        Right value ->
                            expectationFailure ("Expected invalid radio refresh limit to be rejected, got " <> show value)
            assertRejected 0
            assertRejected 401

    describe "resolveRadioNowPlayingFetchResult" $ do
        it "keeps reachable-but-empty metadata distinct from upstream fetch failures" $ do
            case resolveRadioNowPlayingFetchResult (Right Nothing) of
                Right value -> do
                    rnpTitle value `shouldBe` Nothing
                    rnpArtist value `shouldBe` Nothing
                    rnpTrack value `shouldBe` Nothing
                Left err ->
                    expectationFailure
                        ("Expected absent now-playing metadata to stay successful, got " <> show err)

            case resolveRadioNowPlayingFetchResult (Left "connection timed out") of
                Left err -> do
                    errHTTPCode err `shouldBe` 502
                    BL.unpack (errBody err)
                        `shouldContain` "Unable to fetch now-playing metadata"
                Right value ->
                    expectationFailure
                        ("Expected now-playing fetch failure to be rejected, got " <> show value)

        it "normalizes stream titles into title, artist, and track fields" $
            case resolveRadioNowPlayingFetchResult (Right (Just "  Los Nin - Tarika  ")) of
                Right value -> do
                    rnpTitle value `shouldBe` Just "Los Nin - Tarika"
                    rnpArtist value `shouldBe` Just "Los Nin"
                    rnpTrack value `shouldBe` Just "Tarika"
                Left err ->
                    expectationFailure
                        ("Expected now-playing title metadata to parse, got " <> show err)

    describe "radio request JSON contracts" $ do
        it "accepts canonical radio request payloads used by current handlers" $ do
            case eitherDecode "{\"rirSources\":[\"https://radio.example.com/catalog.csv\"],\"rirLimit\":25}" of
                Left err ->
                    expectationFailure ("Expected canonical radio import payload to decode, got: " <> err)
                Right payload -> do
                    rirSources payload `shouldBe` Just ["https://radio.example.com/catalog.csv"]
                    rirLimit payload `shouldBe` Just 25

            case eitherDecode "{\"rmrLimit\":10,\"rmrOnlyMissing\":true}" of
                Left err ->
                    expectationFailure ("Expected canonical radio metadata refresh payload to decode, got: " <> err)
                Right payload -> do
                    rmrLimit payload `shouldBe` Just 10
                    rmrOnlyMissing payload `shouldBe` Just True

            case eitherDecode $
                "{\"rsuStreamUrl\":\"https://radio.example.com/live\""
                    <> ",\"rsuName\":\"TDF Live\""
                    <> ",\"rsuCountry\":\"EC\""
                    <> ",\"rsuGenre\":\"ambient\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical radio stream upsert payload to decode, got: " <> err)
                Right payload -> do
                    rsuStreamUrl payload `shouldBe` "https://radio.example.com/live"
                    rsuName payload `shouldBe` Just "TDF Live"
                    rsuCountry payload `shouldBe` Just "EC"
                    rsuGenre payload `shouldBe` Just "ambient"

            case eitherDecode "{\"rnpStreamUrl\":\"https://radio.example.com/live\"}" of
                Left err ->
                    expectationFailure ("Expected canonical radio now-playing payload to decode, got: " <> err)
                Right payload ->
                    rnpStreamUrl payload `shouldBe` "https://radio.example.com/live"

            case eitherDecode "{\"rtrName\":\"TDF Live\",\"rtrGenre\":\"ambient\",\"rtrCountry\":\"EC\"}" of
                Left err ->
                    expectationFailure ("Expected canonical radio transmission payload to decode, got: " <> err)
                Right payload -> do
                    rtrName payload `shouldBe` Just "TDF Live"
                    rtrGenre payload `shouldBe` Just "ambient"
                    rtrCountry payload `shouldBe` Just "EC"

            case eitherDecode $
                "{\"rpuStreamUrl\":\"https://radio.example.com/live\""
                    <> ",\"rpuStationName\":\"Radio Uno\""
                    <> ",\"rpuStationId\":\"station-uno\"}"
             of
                Left err ->
                    expectationFailure ("Expected canonical radio presence payload to decode, got: " <> err)
                Right payload -> do
                    rpuStreamUrl payload `shouldBe` "https://radio.example.com/live"
                    rpuStationName payload `shouldBe` Just "Radio Uno"
                    rpuStationId payload `shouldBe` Just "station-uno"

        it "rejects typoed radio request keys instead of silently falling back to default behavior" $ do
            ( eitherDecode
                "{\"sources\":[\"https://radio.example.com/catalog.csv\"],\"limit\":25}"
                    :: Either String RadioImportRequest
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"limit\":10,\"onlyMissing\":true}"
                    :: Either String RadioMetadataRefreshRequest
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"rsuStreamUrl\":\"https://radio.example.com/live\",\"streamUrl\":\"https://typo.example.com/live\"}"
                    :: Either String RadioStreamUpsert
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"rnpStreamUrl\":\"https://radio.example.com/live\",\"streamUrl\":\"https://typo.example.com/live\"}"
                    :: Either String RadioNowPlayingRequest
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"name\":\"TDF Live\",\"genre\":\"ambient\"}"
                    :: Either String RadioTransmissionRequest
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"streamUrl\":\"https://radio.example.com/live\",\"stationName\":\"Radio Uno\"}"
                    :: Either String RadioPresenceUpsert
                )
                `shouldSatisfy` isLeft

        it "rejects extra radio presence keys so typoed station metadata cannot be silently ignored" $
            ( eitherDecode
                "{\"rpuStreamUrl\":\"https://radio.example.com/live\",\"rpuStationName\":\"Radio Uno\",\"stationName\":\"typo\"}"
                    :: Either String RadioPresenceUpsert
                )
                `shouldSatisfy` isLeft

    describe "radio presence updates" $ do
        it "clears stale station metadata when a user switches streams without sending fresh station labels" $ do
            let initialPayload =
                    RadioPresenceUpsert
                        { rpuStreamUrl = "https://radio.example.com/live"
                        , rpuStationName = Just "Radio Uno"
                        , rpuStationId = Just "station-uno"
                        }
                switchedPayload =
                    RadioPresenceUpsert
                        { rpuStreamUrl = "https://radio.example.com/alt"
                        , rpuStationName = Nothing
                        , rpuStationId = Nothing
                        }
                _searchStreams
                    :<|> _upsertActive
                    :<|> _importStreams
                    :<|> _refreshMetadata
                    :<|> _nowPlaying
                    :<|> _createTransmission
                    :<|> getSelfPresenceHandler
                    :<|> upsertPresenceHandler
                    :<|> _clearPresence
                    :<|> _getPresenceByParty =
                        radioServer radioPresenceUser :: ServerT RadioAPI RadioPresenceTestM

            result <- runRadioPresenceTest $ do
                _ <- upsertPresenceHandler initialPayload
                updated <- upsertPresenceHandler switchedPayload
                current <- getSelfPresenceHandler
                pure (updated, current)

            case result of
                Left err ->
                    expectationFailure
                        ("Expected radio presence stream switch to succeed, got " <> show err)
                Right (updated, current) -> do
                    rpStreamUrl updated `shouldBe` "https://radio.example.com/alt"
                    rpStationName updated `shouldBe` Nothing
                    rpStationId updated `shouldBe` Nothing
                    case current of
                        Nothing ->
                            expectationFailure "Expected current radio presence to remain readable"
                        Just persisted -> do
                            rpStreamUrl persisted `shouldBe` "https://radio.example.com/alt"
                            rpStationName persisted `shouldBe` Nothing
                            rpStationId persisted `shouldBe` Nothing

        it "rejects malformed station metadata before overwriting the current presence row" $ do
            let initialPayload =
                    RadioPresenceUpsert
                        { rpuStreamUrl = "https://radio.example.com/live"
                        , rpuStationName = Just "Radio Uno"
                        , rpuStationId = Just "station-uno"
                        }
                invalidPayload =
                    RadioPresenceUpsert
                        { rpuStreamUrl = "https://radio.example.com/alt"
                        , rpuStationName = Just "Radio\NULUno"
                        , rpuStationId = Just (Data.Text.replicate 161 "a")
                        }
                _searchStreams
                    :<|> _upsertActive
                    :<|> _importStreams
                    :<|> _refreshMetadata
                    :<|> _nowPlaying
                    :<|> _createTransmission
                    :<|> getSelfPresenceHandler
                    :<|> upsertPresenceHandler
                    :<|> _clearPresence
                    :<|> _getPresenceByParty =
                        radioServer radioPresenceUser :: ServerT RadioAPI RadioPresenceTestM

            result <- runRadioPresenceTest $ do
                _ <- upsertPresenceHandler initialPayload
                env <- ask
                rejected <- liftIO $
                    runExceptT (runReaderT (upsertPresenceHandler invalidPayload) env)
                current <- getSelfPresenceHandler
                pure (rejected, current)

            case result of
                Left err ->
                    expectationFailure
                        ("Expected radio presence validation test to complete, got " <> show err)
                Right (rejected, current) -> do
                    case rejected of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain` "rpuStationName must not contain control characters"
                        Right value ->
                            expectationFailure
                                ("Expected malformed radio presence metadata to be rejected, got " <> show value)
                    case current of
                        Nothing ->
                            expectationFailure "Expected previous radio presence to remain readable"
                        Just persisted -> do
                            rpStreamUrl persisted `shouldBe` "https://radio.example.com/live"
                            rpStationName persisted `shouldBe` Just "Radio Uno"
                            rpStationId persisted `shouldBe` Just "station-uno"

    describe "validateProposalTitle" $ do
        it "trims valid proposal titles before CRM persistence" $
            validateProposalTitle "  TDF Live Sessions Proposal  "
                `shouldBe` Right "TDF Live Sessions Proposal"

        it "rejects blank, oversized, or control-character proposal titles before they reach storage or PDF generation" $ do
            let assertInvalid raw expected = case validateProposalTitle raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid proposal title to be rejected, got: " <> show value)
            assertInvalid "   " "title is required"
            assertInvalid (Data.Text.replicate 161 "x") "title must be 160 characters or fewer"
            assertInvalid "Launch\nBcc: ops@example.com" "title must not contain control characters"

    describe "validateTemplateKey" $ do
        it "trims and canonicalizes proposal template keys before lookup" $ do
            validateTemplateKey "  tdf_live_sessions  " `shouldBe` Right "tdf_live_sessions"
            validateTemplateKey "  TDF_Live_Sessions  " `shouldBe` Right "tdf_live_sessions"

        it "rejects blank, separator-only, or unsafe template keys with a 400 instead of a missing-template 404" $ do
            let assertInvalid raw expected = case validateTemplateKey raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid templateKey to be rejected, got: " <> show value)
            assertInvalid "   " "templateKey required"
            assertInvalid "---" "include at least one letter or number"
            assertInvalid "../proposal" "ASCII letters, numbers, hyphens, or underscores"
            assertInvalid (Data.Text.replicate 97 "a") "96 characters or fewer"

    describe "validateProposalContentSource" $ do
        it "accepts exactly one normalized proposal content source" $ do
            validateProposalContentSource (Just "\\section*{Proposal}") Nothing
                `shouldBe` Right (ProposalInlineLatex "\\section*{Proposal}")
            validateProposalContentSource (Just "   ") (Just "  tdf_live_sessions  ")
                `shouldBe` Right (ProposalTemplateKey "tdf_live_sessions")
            validateProposalContentSource Nothing (Just "  TDF_Live_Sessions  ")
                `shouldBe` Right (ProposalTemplateKey "tdf_live_sessions")

        it "rejects missing or ambiguous content source input with a 400" $ do
            let assertInvalid rawLatex rawTemplate expected = case validateProposalContentSource rawLatex rawTemplate of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid proposal content source to be rejected, got: " <> show value)
            assertInvalid Nothing Nothing "latex or templateKey required"
            assertInvalid (Just "\\section*{Proposal}") (Just "tdf_live_sessions") "Provide either latex or templateKey, not both"

    describe "proposal status validation" $ do
        it "defaults omitted create status to draft and normalizes supported explicit statuses" $ do
            validateProposalStatus Nothing `shouldBe` Right "draft"
            validateProposalStatus (Just " Sent ") `shouldBe` Right "sent"
            validateOptionalProposalStatus Nothing `shouldBe` Right Nothing
            validateOptionalProposalStatus (Just " DRAFT ")
                `shouldBe` Right (Just "draft")

        it "rejects blank or unknown proposal statuses instead of persisting UI-opaque values" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "status must be one of: draft, sent"
                    Right value ->
                        expectationFailure ("Expected invalid proposal status to be rejected, got: " <> show value)
            assertInvalid (validateProposalStatus (Just "   "))
            assertInvalid (validateProposalStatus (Just "archived"))
            assertInvalid (validateOptionalProposalStatus (Just "queued"))

    describe "validateProposalVersionNumber" $ do
        it "accepts positive proposal version numbers for explicit version lookups" $ do
            validateProposalVersionNumber 1 `shouldBe` Right 1
            validateProposalVersionNumber 7 `shouldBe` Right 7

        it "rejects zero or negative version numbers instead of falling through to a misleading 404" $ do
            let assertInvalid raw = case validateProposalVersionNumber raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "version must be a positive integer"
                    Right value ->
                        expectationFailure ("Expected invalid proposal version to be rejected, got " <> show value)
            assertInvalid 0
            assertInvalid (-2)

    describe "validateOptionalProposalContactEmail" $ do
        it "normalizes valid proposal contact emails and treats blanks as unset" $ do
            validateOptionalProposalContactEmail Nothing `shouldBe` Right Nothing
            validateOptionalProposalContactEmail (Just "   ") `shouldBe` Right Nothing
            validateOptionalProposalContactEmail (Just " Sales@Example.com ")
                `shouldBe` Right (Just "sales@example.com")

        it "rejects malformed proposal contact emails instead of storing unusable CRM contact data" $ do
            let assertInvalid raw = case validateOptionalProposalContactEmail (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "contactEmail must be a valid email address"
                    Right value ->
                        expectationFailure ("Expected invalid proposal contact email to be rejected, got " <> show value)
            assertInvalid "not-an-email"
            assertInvalid "sales@example..com"
            assertInvalid "sales@-example.com"
            assertInvalid "sales@example-.com"
            assertInvalid ".sales@example.com"
            assertInvalid "sales.@example.com"
            assertInvalid "sales..team@example.com"
            assertInvalid "sales()@example.com"
            assertInvalid (Data.Text.replicate 65 "a" <> "@example.com")
            assertInvalid ("sales@" <> Data.Text.replicate 64 "a" <> ".com")

        it "rejects oversized proposal contact emails with an explicit length error" $
            case validateOptionalProposalContactEmail
                    (Just (Data.Text.replicate 245 "a" <> "@example.com")) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "contactEmail must be 254 characters or fewer"
                Right value ->
                    expectationFailure
                        ("Expected oversized proposal contact email to be rejected, got " <> show value)

    describe "validateOptionalProposalContactPhone" $ do
        it "normalizes valid proposal contact phones and treats blanks as unset" $ do
            validateOptionalProposalContactPhone Nothing `shouldBe` Right Nothing
            validateOptionalProposalContactPhone (Just "   ") `shouldBe` Right Nothing
            validateOptionalProposalContactPhone (Just " +593 99 123 4567 ")
                `shouldBe` Right (Just "+593991234567")

        it "rejects malformed proposal contact phones before CRM storage" $ do
            let assertInvalid raw expected = case validateOptionalProposalContactPhone (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err)
                            `shouldContain` expected
                    Right value ->
                        expectationFailure
                            ("Expected invalid proposal contact phone to be rejected, got " <> show value)
            assertInvalid "---" "contactPhone must be a valid phone number"
            assertInvalid "12345" "contactPhone must be a valid phone number"
            assertInvalid "+1234567890123456" "contactPhone must be a valid phone number"
            assertInvalid "call me at 099 123 4567" "contactPhone must be a valid phone number"
            assertInvalid "+593 99\n123 4567" "contactPhone must not contain control characters"

    describe "validateOptionalProposalNotes" $ do
        it "normalizes blank proposal notes and preserves multiline notes" $ do
            validateOptionalProposalNotes "notes" Nothing `shouldBe` Right Nothing
            validateOptionalProposalNotes "notes" (Just "   ") `shouldBe` Right Nothing
            validateOptionalProposalNotes "notes" (Just "  Linea uno\nLinea dos\tOK  ")
                `shouldBe` Right (Just "Linea uno\nLinea dos\tOK")

        it "rejects unsafe control characters in proposal notes instead of persisting ambiguous backend text" $ do
            let assertInvalid fieldName raw expected = case validateOptionalProposalNotes fieldName (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid proposal notes to be rejected, got " <> show value)
            assertInvalid "notes" "Confirmado\NULinternamente" "notes must not contain control characters other than tabs or line breaks"
            assertInvalid "versionNotes" "Compartido\ESCinternamente" "versionNotes must not contain control characters other than tabs or line breaks"

    describe "validateOptionalProposalClientPartyId" $ do
        it "preserves omitted ids and accepts positive client party ids" $ do
            validateOptionalProposalClientPartyId Nothing `shouldBe` Right Nothing
            validateOptionalProposalClientPartyId (Just 42) `shouldBe` Right (Just 42)

        it "rejects zero or negative client party ids before proposals reach ambiguous DB behavior" $ do
            let assertInvalid raw = case validateOptionalProposalClientPartyId (Just raw) of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "clientPartyId must be a positive integer"
                    Right value ->
                        expectationFailure ("Expected invalid proposal clientPartyId to be rejected, got " <> show value)
            assertInvalid 0
            assertInvalid (-9)

    describe "validateContractId" $ do
        it "accepts UUID-shaped contract ids and canonicalizes surrounding whitespace" $
            validateContractId " 550e8400-e29b-41d4-a716-446655440000 "
                `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"

        it "rejects non-UUID ids with a 400 instead of falling through to ambiguous lookups" $ do
            let assertInvalid raw = case validateContractId raw of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "Invalid contract id"
                    Right value ->
                        expectationFailure ("Expected invalid contract id to be rejected, got: " <> show value)
            assertInvalid "contract-123"
            assertInvalid "../contracts/store"

    describe "validateContractPayload" $ do
        it "requires object payloads and normalizes the explicit stored contract kind" $ do
            validateContractPayload
                (A.object
                    [ "kind" .= ("  Event_Vendor_Contract  " :: Text)
                    , "amountCents" .= (25000 :: Int)
                    ]
                )
                `shouldBe`
                Right
                    ( "event_vendor_contract"
                    , A.object
                        [ "kind" .= ("event_vendor_contract" :: Text)
                        , "amountCents" .= (25000 :: Int)
                        ]
                    )
        it "rejects non-object or malformed kind values instead of silently storing generic contracts" $ do
            let assertInvalid payload expected = case validateContractPayload payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid contract payload to be rejected, got: " <> show value)
            assertInvalid (A.String "not-an-object") "Contract payload must be a JSON object"
            assertInvalid
                (A.object ["amountCents" .= (25000 :: Int)])
                "Contract payload must include a kind field"
            assertInvalid (A.object ["kind" .= ("" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= ("event vendor" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid
                (A.object ["kind" .= ("---" :: Text)])
                "Contract payload kind must include at least one ASCII letter or number"
            assertInvalid
                (A.object ["kind" .= ("___" :: Text)])
                "Contract payload kind must include at least one ASCII letter or number"
            assertInvalid
                (A.object ["kind" .= Data.Text.replicate 65 "a"])
                "Contract payload kind must be 64 characters or fewer"
            assertInvalid (A.object ["kind" .= A.Null]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= (42 :: Int)]) "Contract payload kind must be a non-empty slug"

        it "rejects server-managed envelope keys inside contract payloads so create requests cannot over-post stored metadata" $ do
            let assertInvalid payload expected = case validateContractPayload payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected reserved-key contract payload to be rejected, got: " <> show value)
            assertInvalid
                (A.object ["kind" .= ("generic" :: Text), "id" .= ("550e8400-e29b-41d4-a716-446655440000" :: Text)])
                "Contract payload must not include server-managed field: id"
            assertInvalid
                (A.object ["kind" .= ("generic" :: Text), "created_at" .= ("2026-01-01T00:00:00Z" :: Text)])
                "Contract payload must not include server-managed field: created_at"

    describe "validateContractSendPayload" $ do
        it "requires an object body with a canonical recipient email" $
            validateContractSendPayload (A.object ["email" .= (" Sales@Example.com " :: Text)])
                `shouldBe` Right "sales@example.com"

        it "rejects missing, malformed, or unexpected send fields instead of silently ignoring the request body" $ do
            let assertInvalid payload expected =
                    case validateContractSendPayload payload of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expected
                        Right value ->
                            expectationFailure ("Expected invalid contract send payload to be rejected, got: " <> show value)
            assertInvalid (A.String "sales@example.com") "Contract send payload must be a JSON object"
            assertInvalid (A.object []) "Contract send payload must include a valid email"
            assertInvalid (A.object ["email" .= ("" :: Text)]) "Contract send payload must include a valid email"
            assertInvalid (A.object ["email" .= ("not-an-email" :: Text)]) "Contract send payload must include a valid email"
            assertInvalid
                (A.object ["email" .= (Data.Text.replicate 65 "a" <> "@example.com")])
                "Contract send payload must include a valid email"
            assertInvalid
                (A.object ["email" .= ("sales@" <> Data.Text.replicate 64 "a" <> ".com")])
                "Contract send payload must include a valid email"
            assertInvalid
                (A.object ["email" .= ("sales@example.com" :: Text), "subject" .= ("Contract" :: Text)])
                "Contract send payload only supports the email field"

    describe "decodeStoredContract" $ do
        it "accepts persisted contracts with the expected stored shape" $
            case decodeStoredContract "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"kind\":\"generic\",\"payload\":{\"kind\":\"generic\",\"amountCents\":25000},\"created_at\":\"2026-01-01T00:00:00Z\"}" of
                Left err ->
                    expectationFailure ("Expected stored contract row to decode, got: " <> show err)
                Right _ ->
                    pure ()

        it "rejects malformed stored contracts instead of letting handlers report a misleading 404" $
            case decodeStoredContract "{\"id\":true}" of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored contract payload is unreadable"
                Right _ ->
                    expectationFailure "Expected malformed stored contract row to be rejected"

        it "rejects unexpected stored contract keys instead of silently accepting schema drift" $
            case decodeStoredContract "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"kind\":\"generic\",\"payload\":{\"kind\":\"generic\"},\"created_at\":\"2026-01-01T00:00:00Z\",\"pdf_path\":\"contracts/store/generic.pdf\"}" of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored contract payload is unreadable"
                Right _ ->
                    expectationFailure "Expected stored contract with unexpected keys to be rejected"

        it "rejects stored contracts with invalid persisted ids instead of rendering the wrong contract identity" $
            case decodeStoredContract "{\"id\":\"not-a-uuid\",\"kind\":\"generic\",\"payload\":{\"kind\":\"generic\"},\"created_at\":\"2026-01-01T00:00:00Z\"}" of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored contract id is invalid"
                Right _ ->
                    expectationFailure "Expected stored contract with invalid id to be rejected"

        it "rejects stored contracts whose top-level kind disagrees with payload.kind" $
            case decodeStoredContract "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\",\"kind\":\"nda\",\"payload\":{\"kind\":\"msa\",\"amountCents\":25000},\"created_at\":\"2026-01-01T00:00:00Z\"}" of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored contract kind does not match payload kind"
                Right _ ->
                    expectationFailure "Expected mismatched stored contract kinds to be rejected"

        it "rejects stored contracts with separator-only payload kinds before PDF rendering" $ do
            let storedPayload =
                    BL.concat
                        [ "{\"id\":\"550e8400-e29b-41d4-a716-446655440000\""
                        , ",\"kind\":\"generic\""
                        , ",\"payload\":{\"kind\":\"---\"}"
                        , ",\"created_at\":\"2026-01-01T00:00:00Z\"}"
                        ]
            case decodeStoredContract storedPayload of
                Left err ->
                    Data.Text.unpack err
                        `shouldContain` "Contract payload kind must include at least one ASCII letter or number"
                Right _ ->
                    expectationFailure "Expected separator-only stored contract payload kind to be rejected"

    describe "internship status validation" $ do
        it "defaults omitted project statuses and normalizes supported explicit values" $ do
            validateInternProjectStatusInput Nothing `shouldBe` Right "active"
            validateInternProjectStatusInput (Just " COMPLETED ") `shouldBe` Right "completed"
            validateOptionalInternProjectStatusInput Nothing `shouldBe` Right Nothing
            validateOptionalInternProjectStatusInput (Just " paused ")
                `shouldBe` Right (Just "paused")
            validateOptionalInternTaskStatusInput Nothing `shouldBe` Right Nothing
            validateOptionalInternTaskStatusInput (Just " DOING ")
                `shouldBe` Right (Just "doing")
            validateOptionalInternPermissionStatusInput Nothing `shouldBe` Right Nothing
            validateOptionalInternPermissionStatusInput (Just " APPROVED ")
                `shouldBe` Right (Just "approved")

        it "rejects blank or unknown internship statuses instead of storing values the UI cannot map" $ do
            let assertInvalid result expected = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid internship status to be rejected, got " <> show value)
            assertInvalid
                (validateInternProjectStatusInput (Just "   "))
                "projectStatus must be one of: active, paused, completed"
            assertInvalid
                (validateOptionalInternProjectStatusInput (Just "archived"))
                "projectStatus must be one of: active, paused, completed"
            assertInvalid
                (validateOptionalInternTaskStatusInput (Just "review"))
                "taskStatus must be one of: todo, doing, blocked, done"
            assertInvalid
                (validateOptionalInternPermissionStatusInput (Just "maybe"))
                "permissionStatus must be one of: pending, approved, rejected"

    describe "internship task progress validation" $ do
        it "preserves omitted progress and accepts explicit values inside the supported range" $ do
            validateInternTaskProgressUpdate Nothing `shouldBe` Right Nothing
            validateInternTaskProgressUpdate (Just 0) `shouldBe` Right (Just 0)
            validateInternTaskProgressUpdate (Just 67) `shouldBe` Right (Just 67)
            validateInternTaskProgressUpdate (Just 100) `shouldBe` Right (Just 100)

        it "rejects out-of-range progress updates instead of silently clamping them" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "taskProgress must be between 0 and 100"
                    Right value ->
                        expectationFailure ("Expected invalid internship task progress to be rejected, got " <> show value)
            assertInvalid (validateInternTaskProgressUpdate (Just (-1)))
            assertInvalid (validateInternTaskProgressUpdate (Just 101))

    describe "internship project title validation" $ do
        it "trims project titles while preserving omitted update payloads" $ do
            validateInternProjectTitle "  Backline inventory audit  "
                `shouldBe` Right "Backline inventory audit"
            validateInternProjectTitleUpdate Nothing `shouldBe` Right Nothing
            validateInternProjectTitleUpdate (Just "  Studio onboarding  ")
                `shouldBe` Right (Just "Studio onboarding")

        it "rejects blank project titles instead of persisting unnamed projects" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "project title is required"
                    Right value ->
                        expectationFailure ("Expected invalid internship project title, got " <> show value)
            assertInvalid (validateInternProjectTitle "   ")
            assertInvalid (validateInternProjectTitleUpdate (Just "   "))

    describe "internship task title validation" $ do
        it "trims task titles while preserving omitted update payloads" $ do
            validateInternTaskTitle "  Label metadata pass  "
                `shouldBe` Right "Label metadata pass"
            validateInternTaskTitleUpdate Nothing `shouldBe` Right Nothing
            validateInternTaskTitleUpdate (Just "  Update venue checklist  ")
                `shouldBe` Right (Just "Update venue checklist")

        it "rejects blank task titles instead of persisting unnamed tasks" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "task title is required"
                    Right value ->
                        expectationFailure ("Expected invalid internship task title, got " <> show value)
            assertInvalid (validateInternTaskTitle "   ")
            assertInvalid (validateInternTaskTitleUpdate (Just "   "))

    describe "internship todo text validation" $ do
        it "trims meaningful todo text while preserving omitted update payloads" $ do
            validateInternTodoText "  Confirm drum mics  "
                `shouldBe` Right "Confirm drum mics"
            validateInternTodoTextUpdate Nothing `shouldBe` Right Nothing
            validateInternTodoTextUpdate (Just "  Export stems  ")
                `shouldBe` Right (Just "Export stems")

        it "rejects blank todo text instead of persisting whitespace-only records" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "todo text is required"
                    Right value ->
                        expectationFailure ("Expected invalid internship todo text to be rejected, got " <> show value)
            assertInvalid (validateInternTodoText "   ")
            assertInvalid (validateInternTodoTextUpdate (Just "   "))

    describe "internship party id validation" $ do
        it "preserves omitted optional ids, clear operations, and positive party references" $ do
            validateOptionalInternPartyIdInput "partyId" Nothing `shouldBe` Right Nothing
            validateOptionalInternPartyIdInput "partyId" (Just 42) `shouldBe` Right (Just 42)
            validateOptionalInternPartyIdUpdate "assignedTo" Nothing `shouldBe` Right Nothing
            validateOptionalInternPartyIdUpdate "assignedTo" (Just Nothing)
                `shouldBe` Right (Just Nothing)
            validateOptionalInternPartyIdUpdate "assignedTo" (Just (Just 7))
                `shouldBe` Right (Just (Just 7))

        it "rejects non-positive internship party ids instead of silently producing dangling filters or assignees" $ do
            let assertInvalid expected result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid internship party id to be rejected, got " <> show value)
            assertInvalid "partyId must be a positive integer"
                (validateOptionalInternPartyIdInput "partyId" (Just 0))
            assertInvalid "assignedTo must be a positive integer"
                (validateOptionalInternPartyIdInput "assignedTo" (Just (-1)))
            assertInvalid "assignedTo must be a positive integer"
                (validateOptionalInternPartyIdUpdate "assignedTo" (Just (Just 0)))

    describe "internship path identifier parsing" $ do
        it "accepts positive persistent ids used by internship capture routes" $
            case (parseKey @ME.InternProject "42" :: Either ServerError (Key ME.InternProject)) of
                Left err ->
                    expectationFailure ("Expected positive internship path id to parse, got " <> show err)
                Right projectKey ->
                    toPathPiece projectKey `shouldBe` "42"

        it "rejects zero, negative, or malformed ids before internship handlers hit the database" $ do
            let assertInvalid rawId expectedMessage =
                    case (parseKey @ME.InternProject rawId :: Either ServerError (Key ME.InternProject)) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right _ ->
                            expectationFailure "Expected invalid internship path id to be rejected"
            assertInvalid "0" "positive integer"
            assertInvalid "-3" "positive integer"
            assertInvalid "project-7" "Invalid identifier"

    describe "internship permission date validation" $ do
        it "accepts open-ended and same-day permission ranges" $ do
            validateInternPermissionDateRange
                (fromGregorian 2026 4 11)
                Nothing
                `shouldBe` Right ()
            validateInternPermissionDateRange
                (fromGregorian 2026 4 11)
                (Just (fromGregorian 2026 4 11))
                `shouldBe` Right ()

        it "rejects permission end dates before the start date instead of storing impossible requests" $ do
            case validateInternPermissionDateRange
                (fromGregorian 2026 4 11)
                (Just (fromGregorian 2026 4 10)) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "endAt must be on or after startAt"
                Right value ->
                    expectationFailure ("Expected invalid internship permission date range to be rejected, got " <> show value)

    describe "internship project date validation" $ do
        it "accepts open-ended and forward project schedules" $ do
            validateInternProjectDateRange
                Nothing
                (Just (fromGregorian 2026 4 14))
                `shouldBe` Right ()
            validateInternProjectDateRange
                (Just (fromGregorian 2026 4 11))
                Nothing
                `shouldBe` Right ()
            validateInternProjectDateRange
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 14))
                `shouldBe` Right ()

        it "rejects project due dates that precede their start date" $ do
            case validateInternProjectDateRange
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 10)) of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "dueAt must be on or after startAt"
                Right value ->
                    expectationFailure ("Expected invalid internship project date range to be rejected, got " <> show value)

    describe "internship project date update validation" $ do
        it "accepts no-op updates, valid new ranges, and clearing one side of the schedule" $ do
            validateInternProjectDateUpdate
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 14))
                Nothing
                Nothing
                `shouldBe` Right ()
            validateInternProjectDateUpdate
                Nothing
                Nothing
                (Just (Just (fromGregorian 2026 4 11)))
                (Just (Just (fromGregorian 2026 4 14)))
                `shouldBe` Right ()
            validateInternProjectDateUpdate
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 14))
                Nothing
                (Just Nothing)
                `shouldBe` Right ()
            validateInternProjectDateUpdate
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 14))
                (Just Nothing)
                Nothing
                `shouldBe` Right ()

        it "rejects updates that would leave the effective project due date before start date" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "dueAt must be on or after startAt"
                    Right value ->
                        expectationFailure ("Expected invalid internship project date update to be rejected, got " <> show value)
            assertInvalid
                (validateInternProjectDateUpdate
                    Nothing
                    Nothing
                    (Just (Just (fromGregorian 2026 4 11)))
                    (Just (Just (fromGregorian 2026 4 10))))
            assertInvalid
                (validateInternProjectDateUpdate
                    (Just (fromGregorian 2026 4 11))
                    (Just (fromGregorian 2026 4 14))
                    Nothing
                    (Just (Just (fromGregorian 2026 4 10))))
            assertInvalid
                (validateInternProjectDateUpdate
                    (Just (fromGregorian 2026 4 11))
                    (Just (fromGregorian 2026 4 14))
                    (Just (Just (fromGregorian 2026 4 15)))
                    Nothing)

    describe "internship profile date validation" $ do
        it "accepts no-op updates, forward ranges, and clearing an existing start date" $ do
            validateInternProfileDateUpdate
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 12))
                Nothing
                Nothing
                `shouldBe` Right ()
            validateInternProfileDateUpdate
                Nothing
                Nothing
                (Just (Just (fromGregorian 2026 4 11)))
                (Just (Just (fromGregorian 2026 4 12)))
                `shouldBe` Right ()
            validateInternProfileDateUpdate
                (Just (fromGregorian 2026 4 11))
                (Just (fromGregorian 2026 4 10))
                (Just Nothing)
                Nothing
                `shouldBe` Right ()

        it "rejects updates that would leave the effective profile end date before start date" $ do
            let assertInvalid result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "endAt must be on or after startAt"
                    Right value ->
                        expectationFailure ("Expected invalid internship profile date range to be rejected, got " <> show value)
            assertInvalid
                (validateInternProfileDateUpdate
                    Nothing
                    Nothing
                    (Just (Just (fromGregorian 2026 4 11)))
                    (Just (Just (fromGregorian 2026 4 10))))
            assertInvalid
                (validateInternProfileDateUpdate
                    (Just (fromGregorian 2026 4 11))
                    Nothing
                    Nothing
                    (Just (Just (fromGregorian 2026 4 10))))
            assertInvalid
                (validateInternProfileDateUpdate
                    (Just (fromGregorian 2026 4 10))
                    (Just (fromGregorian 2026 4 11))
                    (Just (Just (fromGregorian 2026 4 12)))
                    Nothing)

    describe "internship task update permissions" $ do
        it "allows interns to change status/progress while keeping admin-only task edits available to admins" $ do
            validateInternTaskUpdatePermissions False
                (InternTaskUpdate Nothing Nothing (Just "doing") (Just 55) Nothing Nothing)
                `shouldBe` Right ()
            validateInternTaskUpdatePermissions True
                (InternTaskUpdate (Just "Retitle")
                    (Just (Just "Add checklist"))
                    Nothing
                    Nothing
                    (Just (Just 7))
                    (Just Nothing))
                `shouldBe` Right ()

        it "rejects non-admin attempts to change title, description, assignee, or due date instead of silently ignoring them" $ do
            let assertForbidden payload = case validateInternTaskUpdatePermissions False payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 403
                        BL.unpack (errBody err)
                            `shouldContain` "Only admins can update task title, description, assignee, or due date"
                    Right value ->
                        expectationFailure
                            ("Expected non-admin task update to be rejected, got " <> show value)
            assertForbidden (InternTaskUpdate (Just "Retitle") Nothing Nothing Nothing Nothing Nothing)
            assertForbidden (InternTaskUpdate Nothing (Just (Just "Details")) Nothing Nothing Nothing Nothing)
            assertForbidden (InternTaskUpdate Nothing Nothing Nothing Nothing (Just (Just 7)) Nothing)
            assertForbidden (InternTaskUpdate Nothing Nothing Nothing Nothing Nothing (Just Nothing))

    describe "event finance normalizers" $ do
        it "normalizes event type and status with safe fallbacks" $ do
            normalizeEventType (Just " FESTIVAL ") `shouldBe` Just "festival"
            normalizeEventType (Just "unknown-type") `shouldBe` Nothing
            normalizeEventStatus (Just "canceled") `shouldBe` Just "cancelled"
            normalizeEventStatus (Just "not-real") `shouldBe` Nothing

        it "rejects invalid explicit budget line types instead of silently rewriting them to expense" $ do
            validateBudgetLineTypeInput " income " `shouldBe` Right "income"
            validateBudgetLineTypeInput "EXPENSE" `shouldBe` Right "expense"
            case validateBudgetLineTypeInput "whatever" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "budget line type must be income or expense"
                Right value ->
                    expectationFailure
                        ("Expected invalid budget line type to be rejected, got " <> show value)

        it "normalizes budget and accounting dimensions" $ do
            normalizeBudgetLineType (Just "INCOME") `shouldBe` "income"
            normalizeBudgetLineType (Just "whatever") `shouldBe` "expense"
            normalizeFinanceDirection (Just "income") `shouldBe` "income"
            normalizeFinanceDirection (Just "invalid") `shouldBe` "expense"
            normalizeFinanceSource (Just "VENDOR_PAYMENT") `shouldBe` "vendor_payment"
            normalizeFinanceSource (Just "nonsense") `shouldBe` "manual"
            normalizeFinanceEntryStatus (Just "draft") `shouldBe` "draft"
            normalizeFinanceEntryStatus (Just "bad") `shouldBe` "posted"

        it "rejects invalid explicit finance entry currencies instead of storing opaque codes" $ do
            validateFinanceEntryCurrencyInput "usd" " eur " `shouldBe` Right "EUR"
            validateFinanceEntryCurrencyInput "eur" "   " `shouldBe` Right "EUR"
            case validateFinanceEntryCurrencyInput "USD" "usdollars" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "finance entry currency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure
                        ("Expected invalid finance entry currency to be rejected, got " <> show value)
            case validateFinanceEntryCurrencyInput "usdollars" "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    BL.unpack (errBody err)
                        `shouldContain` "event default currency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure
                        ("Expected invalid inherited event currency to be rejected, got " <> show value)

        it "rejects blank finance budget line ids instead of silently clearing the relationship" $ do
            validateOptionalBudgetLineIdInput Nothing `shouldBe` Right Nothing
            validateOptionalBudgetLineIdInput (Just "  42  ") `shouldBe` Right (Just "42")
            case validateOptionalBudgetLineIdInput (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "budgetLineId must be omitted or null when no budget line should be linked"
                Right value ->
                    expectationFailure
                        ("Expected blank budgetLineId to be rejected, got " <> show value)

        it "validates ticket tier currencies before ticket tiers can persist opaque codes" $ do
            validateTicketTierCurrencyInput "eur" " usd " `shouldBe` Right "USD"
            validateTicketTierCurrencyInput "eur" "   " `shouldBe` Right "EUR"
            case validateTicketTierCurrencyInput "USD" "usdollars" of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "ticket tier currency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure
                        ("Expected invalid ticket tier currency to be rejected, got " <> show value)
            case validateTicketTierCurrencyInput "usdollars" "   " of
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    BL.unpack (errBody err)
                        `shouldContain` "event default currency must be a 3-letter ISO code"
                Right value ->
                    expectationFailure
                        ("Expected invalid inherited event currency to be rejected, got " <> show value)

    describe "stored finance entry invariants" $ do
        let financeTimestamp = UTCTime (fromGregorian 2026 1 1) 0
            validStoredFinanceEntry =
                EventFinanceEntry
                    { eventFinanceEntryEventId = toSqlKey 1
                    , eventFinanceEntryBudgetLineId = Nothing
                    , eventFinanceEntryDirection = "income"
                    , eventFinanceEntrySource = "manual"
                    , eventFinanceEntryCategory = "general"
                    , eventFinanceEntryConcept = "Initial deposit"
                    , eventFinanceEntryAmountCents = 2500
                    , eventFinanceEntryCurrency = "USD"
                    , eventFinanceEntryStatus = "draft"
                    , eventFinanceEntryExternalRef = Nothing
                    , eventFinanceEntryNotes = Nothing
                    , eventFinanceEntryMetadata = Nothing
                    , eventFinanceEntryOccurredAt = financeTimestamp
                    , eventFinanceEntryRecordedByPartyId = Nothing
                    , eventFinanceEntryCreatedAt = financeTimestamp
                    , eventFinanceEntryUpdatedAt = financeTimestamp
                    }

        it "accepts persisted finance entries whose enum-like dimensions are already canonical" $
            validateStoredFinanceEntryDimensions validStoredFinanceEntry
                `shouldBe` Right ("income", "manual", "draft")

        it "rejects invalid persisted finance statuses instead of rewriting them to posted" $
            case validateStoredFinanceEntryDimensions validStoredFinanceEntry { eventFinanceEntryStatus = "bad-status" } of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored finance entry status is invalid"
                Right value ->
                    expectationFailure ("Expected invalid stored finance status to be rejected, got " <> show value)

        it "rejects invalid persisted finance currencies instead of normalizing arbitrary strings" $
            case validateStoredFinanceEntryDimensions validStoredFinanceEntry { eventFinanceEntryCurrency = "usdollars" } of
                Left err ->
                    Data.Text.unpack err `shouldContain` "Stored finance entry currency is invalid"
                Right value ->
                    expectationFailure ("Expected invalid stored finance currency to be rejected, got " <> show value)

    describe "event list query validation" $ do
        it "accepts blank filters and canonicalizes supported event type and status values" $ do
            parseEventTypeQueryParamEither Nothing `shouldBe` Right Nothing
            parseEventTypeQueryParamEither (Just "   ") `shouldBe` Right Nothing
            parseEventTypeQueryParamEither (Just " FESTIVAL ") `shouldBe` Right (Just "festival")
            parseEventStatusQueryParamEither (Just " canceled ") `shouldBe` Right (Just "cancelled")

        it "rejects unsupported filters instead of silently broadening the result set" $ do
            let assertInvalid expectedMessage result = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expectedMessage
                    Right value ->
                        expectationFailure ("Expected invalid event list filter error, got " <> show value)
            assertInvalid "eventType must be one of"
                (parseEventTypeQueryParamEither (Just "meetup"))
            assertInvalid "eventStatus must be one of"
                (parseEventStatusQueryParamEither (Just "paused"))

    describe "availabilityOverlaps" $ do
        let day = fromGregorian 2025 1 1
            windowStart = UTCTime day (secondsToDiffTime 0)
            windowEnd = UTCTime (addDays 1 day) (secondsToDiffTime 0)

        it "includes events that overlap the start" $ do
            let eventStart = addUTCTime (-3600) windowStart
                eventEnd = addUTCTime 3600 windowStart
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

        it "includes events that overlap the end" $ do
            let eventStart = addUTCTime (-3600) windowEnd
                eventEnd = addUTCTime 3600 windowEnd
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` True

        it "excludes events entirely before the window" $ do
            let eventStart = addUTCTime (-7200) windowStart
                eventEnd = addUTCTime (-3600) windowStart
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

        it "excludes events entirely after the window" $ do
            let eventStart = addUTCTime 3600 windowEnd
                eventEnd = addUTCTime 7200 windowEnd
            availabilityOverlaps windowStart windowEnd eventStart eventEnd `shouldBe` False

    describe "validateEmbeddingModelDimensions" $ do
        it "accepts known embedding models" $ do
            validateEmbeddingModelDimensions "text-embedding-3-small" `shouldBe` Right 1536
            validateEmbeddingModelDimensions "text-embedding-ada-002" `shouldBe` Right 1536
            validateEmbeddingModelDimensions "TEXT-EMBEDDING-3-LARGE" `shouldBe` Right 3072

        it "rejects unknown embedding models" $ do
            validateEmbeddingModelDimensions "mystery-embedder" `shouldSatisfy` isLeft

    describe "validateEmbeddingResponseOrder" $ do
        it "orders embeddings by their upstream response indexes" $
            validateEmbeddingResponseOrder
                3
                [(2, [2.0]), (0, [0.0]), (1, [1.0])]
                `shouldBe` Right [[0.0], [1.0], [2.0]]

        it "rejects incomplete, duplicate, or out-of-range response indexes" $ do
            validateEmbeddingResponseOrder 3 [(0, [0.0]), (2, [2.0])]
                `shouldSatisfy` isLeft
            validateEmbeddingResponseOrder 2 [(0, [0.0]), (0, [1.0])]
                `shouldSatisfy` isLeft
            validateEmbeddingResponseOrder 2 [(0, [0.0]), (2, [2.0])]
                `shouldSatisfy` isLeft

    describe "validateEmbeddingResponseDimensions" $ do
        it "accepts embeddings with the expected vector length" $
            validateEmbeddingResponseDimensions 3 [[0.0, 1.0, 2.0], [3.0, 4.0, 5.0]]
                `shouldBe` Right [[0.0, 1.0, 2.0], [3.0, 4.0, 5.0]]

        it "rejects wrong-sized embeddings before pgvector persistence" $ do
            validateEmbeddingResponseDimensions 3 [[0.0, 1.0], [2.0, 3.0, 4.0]]
                `shouldSatisfy` isLeft
            validateEmbeddingResponseDimensions 0 [[0.0]]
                `shouldSatisfy` isLeft

        it "rejects non-finite embedding values before pgvector persistence" $ do
            let nanValue = 0 / 0 :: Double
                infinityValue = 1 / 0 :: Double
            validateEmbeddingResponseDimensions 1 [[nanValue]]
                `shouldSatisfy` isLeft
            validateEmbeddingResponseDimensions 1 [[infinityValue]]
                `shouldSatisfy` isLeft

    describe "parseDirective" $ do
        it "parses SEND/HOLD directives regardless of casing" $ do
            parseDirective "send: Hola!" `shouldBe` Right (Send "Hola!")
            parseDirective "hold: Confirma nombre\nneed: email" `shouldBe` Right (Hold "Confirma nombre" (Just "email"))

        it "allows leading blank lines in HOLD body and keeps NEED optional" $ do
            parseDirective "HOLD:\n\nFalta el teléfono\nNEED:   telefono  " `shouldBe` Right (Hold "Falta el teléfono" (Just "telefono"))
            parseDirective "HOLD: Falta confirmar datos\nNEED:   " `shouldBe` Right (Hold "Falta confirmar datos" Nothing)

        it "requires HOLD reason even when NEED exists" $ do
            parseDirective "HOLD:\nNEED: email" `shouldBe` Left "HOLD directive empty"

    describe "validateHookVerifyRequest" $ do
        it "accepts subscribe verification requests with a matching token" $ do
            validateHookVerifyRequest (Just "SuBsCrIbE") (Just "challenge-123") (Just "secret") (Just "secret")
                `shouldBe` Right "challenge-123"

        it "rejects unsafe challenge echoes before returning webhook verification text" $ do
            case validateHookVerifyRequest
                (Just "subscribe")
                (Just "challenge-123\nInjected")
                (Just "secret")
                (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "hub.challenge must not contain control characters"
                Right value ->
                    expectationFailure
                        ("Expected control-character challenge to be rejected, got " <> show value)
            case validateHookVerifyRequest
                (Just "subscribe")
                (Just (Data.Text.replicate 513 "x"))
                (Just "secret")
                (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "hub.challenge must be 512 characters or fewer"
                Right value ->
                    expectationFailure ("Expected oversized challenge to be rejected, got " <> show value)

        it "rejects missing verification query params with precise 400s" $ do
            case validateHookVerifyRequest Nothing (Just "challenge-123") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.mode is required"
                Right _ -> expectationFailure "Expected missing hub.mode to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "   ") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.challenge is required"
                Right _ -> expectationFailure "Expected blank hub.challenge to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") Nothing (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.verify_token is required"
                Right _ -> expectationFailure "Expected missing hub.verify_token to be rejected"

        it "distinguishes bad mode, token mismatch, and missing server config" $ do
            case validateHookVerifyRequest (Just "publish") (Just "challenge-123") (Just "secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "hub.mode must be subscribe"
                Right _ -> expectationFailure "Expected unsupported hub.mode to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") (Just "wrong-secret") (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 403
                    BL.unpack (errBody err) `shouldContain` "hub.verify_token mismatch"
                Right _ -> expectationFailure "Expected mismatched verify token to be rejected"
            case validateHookVerifyRequest (Just "subscribe") (Just "challenge-123") (Just "secret") Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 503
                    BL.unpack (errBody err) `shouldContain` "WhatsApp verify token not configured"
                Right _ -> expectationFailure "Expected missing verify-token config to be rejected"

        it "rejects control-bearing verify tokens before returning misleading mismatches" $ do
            case validateHookVerifyRequest
                    (Just "subscribe")
                    (Just "challenge-123")
                    (Just "secret\nInjected")
                    (Just "secret") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err)
                        `shouldContain` "hub.verify_token must not contain control characters"
                Right _ -> expectationFailure "Expected unsafe hub.verify_token to be rejected"
            case validateHookVerifyRequest
                    (Just "subscribe")
                    (Just "challenge-123")
                    (Just "secret")
                    (Just "secret\nInjected") of
                Left err -> do
                    errHTTPCode err `shouldBe` 503
                    BL.unpack (errBody err)
                        `shouldContain` "WhatsApp verify token is misconfigured"
                Right _ -> expectationFailure "Expected unsafe verify-token config to be rejected"

    describe "extractFirstWebhookMessage" $ do
        it "scans all webhook entries and changes before treating a batch as no-message" $ do
            let enrollmentMessage =
                    WA.WAMessage
                        (Just "wamid.1")
                        "text"
                        "+593991234567"
                        (Just (WA.WAText "INSCRIBIRME"))
                        Nothing
                        Nothing
                        (Just "1770000000")
                payload =
                    WA.WAMetaWebhook
                        [ WA.WAEntry
                            [ WA.WAChange
                                ( WA.WAValue
                                    Nothing
                                    Nothing
                                    (Just [WA.WAStatus Nothing (Just "sent") Nothing Nothing Nothing])
                                )
                            ]
                        , WA.WAEntry
                            [ WA.WAChange (WA.WAValue (Just []) Nothing Nothing)
                            , WA.WAChange (WA.WAValue (Just [enrollmentMessage]) Nothing Nothing)
                            ]
                        ]
            case extractFirstWebhookMessage payload of
                Just (WA.WAMessage msgId msgType senderId msgText _ _ msgTimestamp) -> do
                    msgId `shouldBe` Just "wamid.1"
                    msgType `shouldBe` "text"
                    senderId `shouldBe` "+593991234567"
                    case msgText of
                        Just (WA.WAText messageBody) ->
                            messageBody `shouldBe` "INSCRIBIRME"
                        Nothing ->
                            expectationFailure "Expected enrollment text to be selected"
                    msgTimestamp `shouldBe` Just "1770000000"
                Nothing ->
                    expectationFailure "Expected batched webhook message to be selected"

        it "skips non-text or blank text rows before selecting an actionable webhook message" $ do
            let imageMessage =
                    WA.WAMessage
                        (Just "wamid.image")
                        "image"
                        "+593991234567"
                        Nothing
                        Nothing
                        Nothing
                        (Just "1770000000")
                blankTextMessage =
                    WA.WAMessage
                        (Just "wamid.blank")
                        "text"
                        "+593991234567"
                        (Just (WA.WAText "   "))
                        Nothing
                        Nothing
                        (Just "1770000001")
                enrollmentMessage =
                    WA.WAMessage
                        (Just "wamid.2")
                        "text"
                        "+593991234567"
                        (Just (WA.WAText "INSCRIBIRME"))
                        Nothing
                        Nothing
                        (Just "1770000002")
                payload =
                    WA.WAMetaWebhook
                        [ WA.WAEntry
                            [ WA.WAChange
                                ( WA.WAValue
                                    (Just [imageMessage, blankTextMessage, enrollmentMessage])
                                    Nothing
                                    Nothing
                                )
                            ]
                        ]
            case extractFirstWebhookMessage payload of
                Just (WA.WAMessage msgId msgType _ msgText _ _ _) -> do
                    msgId `shouldBe` Just "wamid.2"
                    msgType `shouldBe` "text"
                    case msgText of
                        Just (WA.WAText messageBody) ->
                            messageBody `shouldBe` "INSCRIBIRME"
                        Nothing ->
                            expectationFailure "Expected text body to be selected"
                Nothing ->
                    expectationFailure "Expected actionable webhook text to be selected"

        it "skips blank-sender text rows before selecting a later batched webhook message" $ do
            let blankSenderMessage =
                    WA.WAMessage
                        (Just "wamid.blank-sender")
                        "text"
                        "   "
                        (Just (WA.WAText "INSCRIBIRME"))
                        Nothing
                        Nothing
                        (Just "1770000001")
                enrollmentMessage =
                    WA.WAMessage
                        (Just "wamid.valid-sender")
                        "text"
                        "+593991234567"
                        (Just (WA.WAText "INSCRIBIRME"))
                        Nothing
                        Nothing
                        (Just "1770000002")
                payload =
                    WA.WAMetaWebhook
                        [ WA.WAEntry
                            [ WA.WAChange
                                ( WA.WAValue
                                    (Just [blankSenderMessage, enrollmentMessage])
                                    Nothing
                                    Nothing
                                )
                            ]
                        ]
            case extractFirstWebhookMessage payload of
                Just (WA.WAMessage msgId _ senderId msgText _ _ _) -> do
                    msgId `shouldBe` Just "wamid.valid-sender"
                    senderId `shouldBe` "+593991234567"
                    case msgText of
                        Just (WA.WAText messageBody) ->
                            messageBody `shouldBe` "INSCRIBIRME"
                        Nothing ->
                            expectationFailure "Expected later text body to be selected"
                Nothing ->
                    expectationFailure "Expected later valid webhook text to be selected"

    describe "PreviewReq" $ do
        it "accepts canonical preview-link bodies and normalizes user-entered phone formatting up front" $ do
            case eitherDecode "{\"phone\":\"+593991234567\"}" of
                Left err ->
                    expectationFailure ("Expected canonical preview-link payload to decode, got: " <> err)
                Right payload ->
                    phone payload `shouldBe` "+593991234567"
            case eitherDecode "{\"phone\":\" +593 99 123 4567 \"}" of
                Left err ->
                    expectationFailure ("Expected formatted preview-link payload to decode, got: " <> err)
                Right payload ->
                    phone payload `shouldBe` "+593991234567"

        it "rejects malformed or over-posted preview-link request bodies before the handler can mint ambiguous leads" $ do
            (eitherDecode "{\"phone\":\"+593991234567\",\"status\":\"COMPLETED\"}" :: Either String PreviewReq)
                `shouldSatisfy` isLeft
            (eitherDecode "{\"phone\":\"call me maybe\"}" :: Either String PreviewReq)
                `shouldSatisfy` isLeft

    describe "validateLeadCompletionRequest" $ do
        it "accepts canonical lead-completion request bodies and rejects unexpected keys" $ do
            case eitherDecode "{\"token\":\"Abc123Def456Ghi789Jk\",\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\"}" of
                Left err ->
                    expectationFailure ("Expected canonical lead completion payload to decode, got: " <> err)
                Right payload ->
                    payload `shouldBe` CompleteReq "Abc123Def456Ghi789Jk" "Ada Lovelace" "ada@example.com"
            (eitherDecode "{\"token\":\"Abc123Def456Ghi789Jk\",\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"unexpected\":true}" :: Either String CompleteReq)
                `shouldSatisfy` isLeft

        it "trims and canonicalizes meaningful lead-completion payload fields before persistence" $ do
            validateLeadCompletionRequest (CompleteReq " Abc123Def456Ghi789Jk " " Ada Lovelace " " Ada@Example.com ")
                `shouldBe` Right (CompleteReq "Abc123Def456Ghi789Jk" "Ada Lovelace" "ada@example.com")

        it "rejects blank tokens, blank names, and malformed emails with precise 400s" $ do
            let validToken = "Abc123Def456Ghi789Jk"
            let assertInvalid payload expected = case validateLeadCompletionRequest payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid lead completion payload to be rejected, got " <> show value)
            assertInvalid (CompleteReq "   " "Ada Lovelace" "ada@example.com") "Completion token is required"
            assertInvalid (CompleteReq validToken "   " "ada@example.com") "Invalid name: must be 1-200 characters"
            assertInvalid (CompleteReq validToken "Ada\nLovelace" "ada@example.com") "Invalid name: must not contain control characters"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada @example.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada@example..com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada@-example.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada@example-.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" ".ada@example.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada.@example.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada..lovelace@example.com") "Invalid email format"
            assertInvalid (CompleteReq validToken "Ada Lovelace" "ada()@example.com") "Invalid email format"
            assertInvalid
                (CompleteReq validToken "Ada Lovelace" (Data.Text.replicate 65 "a" <> "@example.com"))
                "Invalid email format"
            assertInvalid
                (CompleteReq validToken "Ada Lovelace" ("ada@" <> Data.Text.replicate 64 "a" <> ".com"))
                "Invalid email format"
            assertInvalid
                (CompleteReq validToken "Ada Lovelace" (Data.Text.replicate 245 "a" <> "@example.com"))
                "Invalid email: must be 254 characters or fewer"

        it "rejects malformed completion tokens before lookup falls through to a misleading 403" $ do
            let assertInvalid rawToken = case validateLeadCompletionRequest (CompleteReq rawToken "Ada Lovelace" "ada@example.com") of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "Completion token format is invalid"
                    Right value ->
                        expectationFailure ("Expected malformed completion token to be rejected, got " <> show value)
            assertInvalid "token-123"
            assertInvalid "token 123"
            assertInvalid "token/123"
            assertInvalid "token?123"
            assertInvalid "Abc123Def456Ghi789J-"
            assertInvalid (Data.Text.replicate 19 "a")
            assertInvalid (Data.Text.replicate 21 "a")

    describe "validateLeadCompletionId" $ do
        it "accepts only positive lead identifiers before the completion lookup runs" $
            validateLeadCompletionId 42 `shouldBe` Right 42

        it "rejects zero or negative lead identifiers with a precise 400" $ do
            let assertInvalid rawLeadId = case validateLeadCompletionId rawLeadId of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "leadId must be a positive integer"
                    Right value ->
                        expectationFailure ("Expected invalid lead id to be rejected, got " <> show value)
            assertInvalid 0
            assertInvalid (-7)

    describe "validateLeadCompletionLookup" $ do
        it "allows only matching non-completed lead records" $ do
            let validToken = "Abc123Def456Ghi789Jk"
            validateLeadCompletionLookup validToken (Just ("NEW", Just validToken))
                `shouldBe` Right ()
            validateLeadCompletionLookup validToken (Just ("LINK_SENT", Just validToken))
                `shouldBe` Right ()

        it "returns explicit 404/403/409 errors for missing, invalid-token, unavailable, blocked, and completed links" $ do
            let validToken = "Abc123Def456Ghi789Jk"
                otherToken = "Zyx987Wvu654Tsr321Qp"
            let assertLookupFailure result expectedStatus expectedBody = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` expectedStatus
                        BL.unpack (errBody err) `shouldContain` expectedBody
                    Right _ ->
                        expectationFailure ("Expected lookup failure with body containing " <> show expectedBody)
            assertLookupFailure
                (validateLeadCompletionLookup validToken Nothing)
                404
                "Lead not found"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("LINK_SENT", Just otherToken)))
                403
                "Invalid completion token"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("LINK_SENT", Nothing)))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("LINK_SENT", Just "   ")))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("COLD", Just validToken)))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("ARCHIVED", Just validToken)))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup validToken (Just ("completed", Nothing)))
                409
                "Lead already completed"

    describe "ensureLeadCompletionUpdated" $ do
        it "requires the completion update to affect exactly one row" $ do
            ensureLeadCompletionUpdated 1 `shouldBe` Right ()
            case ensureLeadCompletionUpdated 0 of
                Left err -> do
                    errHTTPCode err `shouldBe` 409
                    BL.unpack (errBody err) `shouldContain` "Lead completion could not be applied"
                Right _ -> expectationFailure "Expected zero-row lead completion update to be rejected"

    describe "leadCompletionConsumedToken" $ do
        it "creates a non-null tombstone that cannot be resubmitted as a completion credential" $ do
            let consumed = leadCompletionConsumedToken 42
            consumed `shouldBe` "completed:42"
            consumed `shouldNotBe` leadCompletionConsumedToken 43
            Data.Text.null consumed `shouldBe` False
            case validateLeadCompletionRequest (CompleteReq consumed "Ada Lovelace" "ada@example.com") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "Completion token format is invalid"
                Right value ->
                    expectationFailure ("Expected consumed lead token to be rejected, got " <> show value)

    describe "parseSocialErrorsChannel" $ do
        it "normalizes valid channel values" $ do
            parseSocialErrorsChannel (Just " WhatsApp ") `shouldBe` Right "whatsapp"
            parseSocialErrorsChannel (Just "FACEBOOK") `shouldBe` Right "facebook"

        it "rejects missing or blank channels instead of falling back implicitly" $ do
            case parseSocialErrorsChannel Nothing of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel requerido"
                Right _ -> expectationFailure "Expected missing channel to be rejected"
            case parseSocialErrorsChannel (Just "   ") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel requerido"
                Right _ -> expectationFailure "Expected blank channel to be rejected"

        it "rejects unknown channel values" $
            case parseSocialErrorsChannel (Just "telegram") of
                Left err -> do
                    errHTTPCode err `shouldBe` 400
                    BL.unpack (errBody err) `shouldContain` "channel inválido"
                Right _ -> expectationFailure "Expected invalid channel to be rejected"

    describe "validateSocialErrorsLimit" $ do
        it "keeps the default only when the caller omits the limit" $ do
            validateSocialErrorsLimit Nothing `shouldBe` Right 50
            validateSocialErrorsLimit (Just 1) `shouldBe` Right 1
            validateSocialErrorsLimit (Just 200) `shouldBe` Right 200

        it "rejects out-of-range explicit limits instead of silently clamping them" $ do
            let assertRejected rawLimit =
                    case validateSocialErrorsLimit (Just rawLimit) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "limit debe estar entre 1 y 200"
                        Right value ->
                            expectationFailure ("Expected invalid social errors limit to be rejected, got " <> show value)
            assertRejected 0
            assertRejected 201

    describe "validateAdminEmailCtaUrl" $ do
        it "accepts blank values or canonical public https URLs for email CTA links" $ do
            validateAdminEmailCtaUrl Nothing `shouldBe` Right Nothing
            validateAdminEmailCtaUrl (Just "   ") `shouldBe` Right Nothing
            validateAdminEmailCtaUrl (Just "  https://example.com/launch?utm_source=hq  ")
                `shouldBe` Right (Just "https://example.com/launch?utm_source=hq")

        it "rejects insecure or non-public CTA URLs instead of sending ambiguous email links" $ do
            let assertRejected rawUrl expectedMessage =
                    case validateAdminEmailCtaUrl (Just rawUrl) of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure ("Expected invalid CTA URL to be rejected, got " <> show value)
            assertRejected "http://example.com/launch" "CTA URL must be an absolute https URL"
            assertRejected "https://example.com@evil.test/launch" "CTA URL must not include user info"
            assertRejected "https://localhost/launch" "CTA URL must be an absolute public https URL"

    describe "AdminEmailBroadcastRequest" $ do
        it "rejects unknown JSON fields so typos cannot silently change send behavior" $ do
            let payload =
                    "{\"subject\":\"Launch\","
                    <> "\"bodyLines\":[\"Line 1\"],"
                    <> "\"dryRun\":true,"
                    <> "\"dryrun\":false}"
            isLeft (eitherDecode payload :: Either String AdminEmailBroadcastRequest) `shouldBe` True

    describe "FanProfileUpdate" $ do
        it "accepts canonical fan profile update payloads and rejects typoed keys" $ do
            case eitherDecode
                "{\"fpuDisplayName\":\"Ada\",\"fpuCity\":\"Quito\"}" :: Either String DTO.FanProfileUpdate of
                Left err ->
                    expectationFailure ("Expected canonical fan profile update payload to decode, got: " <> err)
                Right payload -> do
                    DTO.fpuDisplayName payload `shouldBe` Just "Ada"
                    DTO.fpuCity payload `shouldBe` Just "Quito"
                    DTO.fpuAvatarUrl payload `shouldBe` Nothing
                    DTO.fpuFavoriteGenres payload `shouldBe` Nothing
                    DTO.fpuBio payload `shouldBe` Nothing

            isLeft
                ( eitherDecode
                    "{\"fpuDisplayName\":\"Ada\",\"fpuDisplayname\":\"Quito\"}"
                    :: Either String DTO.FanProfileUpdate
                )
                `shouldBe` True

    describe "CreateInvoiceReq" $ do
        it "accepts canonical direct invoice payloads used by the invoicing API" $ do
            let rawPayload =
                    "{\"ciCustomerId\":42,"
                    <> "\"ciCurrency\":\"USD\","
                    <> "\"ciNumber\":\"INV-2026-001\","
                    <> "\"ciNotes\":\"April session\","
                    <> "\"ciLineItems\":[{"
                    <> "\"cilDescription\":\"Studio session\","
                    <> "\"cilQuantity\":1,"
                    <> "\"cilUnitCents\":9000,"
                    <> "\"cilTaxBps\":1500,"
                    <> "\"cilServiceOrderId\":7"
                    <> "}],"
                    <> "\"ciGenerateReceipt\":true}"
            case
                (eitherDecode rawPayload :: Either String DTO.CreateInvoiceReq) of
                Left err ->
                    expectationFailure ("Expected canonical invoice payload to decode, got: " <> err)
                Right invoiceReq -> do
                    DTO.ciCustomerId invoiceReq `shouldBe` 42
                    DTO.ciCurrency invoiceReq `shouldBe` Just "USD"
                    DTO.ciNumber invoiceReq `shouldBe` Just "INV-2026-001"
                    DTO.ciGenerateReceipt invoiceReq `shouldBe` Just True
                    case DTO.ciLineItems invoiceReq of
                        [lineItem] -> do
                            DTO.cilDescription lineItem `shouldBe` "Studio session"
                            DTO.cilQuantity lineItem `shouldBe` 1
                            DTO.cilUnitCents lineItem `shouldBe` 9000
                            DTO.cilTaxBps lineItem `shouldBe` Just 1500
                            DTO.cilServiceOrderId lineItem `shouldBe` Just 7
                        lineItems ->
                            expectationFailure ("Expected one direct invoice line item, got: " <> show lineItems)

        it "rejects unexpected top-level or nested line-item keys so typoed invoice writes fail explicitly" $ do
            let typoedTopLevel =
                    "{\"ciCustomerId\":42,"
                    <> "\"ciLineItems\":[{"
                    <> "\"cilDescription\":\"Studio session\","
                    <> "\"cilQuantity\":1,"
                    <> "\"cilUnitCents\":9000"
                    <> "}],"
                    <> "\"ciGenerateReceipt\":true,"
                    <> "\"generateReceipt\":false}"
                typoedLineItem =
                    "{\"ciCustomerId\":42,"
                    <> "\"ciLineItems\":[{"
                    <> "\"cilDescription\":\"Studio session\","
                    <> "\"cilQuantity\":1,"
                    <> "\"cilUnitCents\":9000,"
                    <> "\"unitAmountCents\":9500"
                    <> "}]}"
            isLeft
                (eitherDecode typoedTopLevel :: Either String DTO.CreateInvoiceReq)
                `shouldBe` True
            isLeft
                (eitherDecode typoedLineItem :: Either String DTO.CreateInvoiceReq)
                `shouldBe` True

    describe "GenerateSessionInvoiceReq" $ do
        it "accepts canonical session-invoice payloads used by the invoicing flow" $
            case
                ( eitherDecode
                    "{\"customerId\":42,\"currency\":\"USD\",\"number\":\"INV-2026-001\",\"notes\":\"  April session  \",\"lineItems\":[{\"description\":\"Studio session\",\"quantity\":1,\"unitCents\":9000,\"taxBps\":1500,\"serviceOrderId\":7,\"sriCode\":\"SRV-001\"}],\"generateReceipt\":true,\"issueSri\":false}"
                    :: Either String DTO.GenerateSessionInvoiceReq
                ) of
                Left err ->
                    expectationFailure ("Expected canonical generate-session-invoice payload to decode, got: " <> err)
                Right payload -> do
                    DTO.gsiCustomerId payload `shouldBe` Just 42
                    DTO.gsiCurrency payload `shouldBe` Just "USD"
                    DTO.gsiNumber payload `shouldBe` Just "INV-2026-001"
                    DTO.gsiGenerateReceipt payload `shouldBe` Just True
                    DTO.gsiIssueSri payload `shouldBe` Just False
                    case DTO.gsiLineItems payload of
                        [lineItem] -> do
                            DTO.gsilDescription lineItem `shouldBe` "Studio session"
                            DTO.gsilQuantity lineItem `shouldBe` 1
                            DTO.gsilUnitCents lineItem `shouldBe` 9000
                            DTO.gsilTaxBps lineItem `shouldBe` Just 1500
                            DTO.gsilServiceOrderId lineItem `shouldBe` Just 7
                            DTO.gsilSriCode lineItem `shouldBe` Just "SRV-001"
                        lineItems ->
                            expectationFailure ("Expected one canonical invoice line item, got: " <> show lineItems)

        it "rejects unexpected top-level or nested line-item keys so typoed invoice writes fail explicitly" $ do
            isLeft
                ( eitherDecode
                    "{\"customerId\":42,\"currency\":\"USD\",\"lineItems\":[{\"description\":\"Studio session\",\"quantity\":1,\"unitCents\":9000}],\"generateReceipt\":true,\"generate_receipt\":false}"
                    :: Either String DTO.GenerateSessionInvoiceReq
                )
                `shouldBe` True
            isLeft
                ( eitherDecode
                    "{\"customerId\":42,\"currency\":\"USD\",\"lineItems\":[{\"description\":\"Studio session\",\"quantity\":1,\"unitCents\":9000,\"unitAmountCents\":9500}]}"
                    :: Either String DTO.GenerateSessionInvoiceReq
                )
                `shouldBe` True

    describe "ClassSessionUpdate" $ do
        it "accepts canonical patch payloads and rejects typo-only bodies so class-session updates cannot silently no-op" $ do
            case (eitherDecode "{\"teacherId\":12}" :: Either String TrialsDTO.ClassSessionUpdate) of
                Left err ->
                    expectationFailure ("Expected canonical class-session patch payload to decode, got: " <> err)
                Right (TrialsDTO.ClassSessionUpdate teacherIdValue subjectIdValue studentIdValue startAtValue endAtValue roomIdValue bookingIdValue notesValue) -> do
                    teacherIdValue `shouldBe` Just 12
                    subjectIdValue `shouldBe` Nothing
                    studentIdValue `shouldBe` Nothing
                    startAtValue `shouldBe` Nothing
                    endAtValue `shouldBe` Nothing
                    roomIdValue `shouldBe` Nothing
                    bookingIdValue `shouldBe` Nothing
                    notesValue `shouldBe` Nothing

            isLeft
                ( eitherDecode
                    "{\"teacherID\":12}"
                    :: Either String TrialsDTO.ClassSessionUpdate
                )
                `shouldBe` True

    describe "buildLiveSessionUsernameCollisionCandidate" $ do
        it "preserves the collision suffix within the 60-character username budget" $ do
            let base = Data.Text.replicate 60 "a"
                candidate = buildLiveSessionUsernameCollisionCandidate base "12"
            Data.Text.length candidate `shouldBe` 60
            candidate `shouldBe` (Data.Text.replicate 57 "a" <> "-12")
            candidate `shouldNotBe` base

    describe "resolveLiveSessionMusicianLookup" $ do
        it "only matches existing live-session musicians by normalized email" $ do
            resolveLiveSessionMusicianLookup (Just " Player@Example.com ")
                `shouldBe` LookupLiveSessionMusicianByEmail "player@example.com"
            resolveLiveSessionMusicianLookup Nothing `shouldBe` CreateLiveSessionMusician
            resolveLiveSessionMusicianLookup (Just "   ") `shouldBe` CreateLiveSessionMusician

    describe "validateLiveSessionReferencedPartyEmail" $ do
        it "accepts omitted or matching emails for referenced musicians without turning intake rows into party updates" $ do
            validateLiveSessionReferencedPartyEmail
                (Just " Artist@Example.com ")
                Nothing
                `shouldBe` Right (Just "artist@example.com")
            validateLiveSessionReferencedPartyEmail
                (Just " Artist@Example.com ")
                (Just "artist@example.com")
                `shouldBe` Right (Just "artist@example.com")

        it "rejects mismatched or newly introduced emails for referenced musicians so intake payloads cannot rewrite party identity data" $ do
            let assertInvalid existingEmail suppliedEmail = do
                    case validateLiveSessionReferencedPartyEmail existingEmail suppliedEmail of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` "must match the existing party email"
                        Right value ->
                            expectationFailure
                                ( "Expected referenced musician email mismatch to be rejected, got "
                                    <> show value
                                )

            assertInvalid
                (Just "artist@example.com")
                (Just "other@example.com")
            assertInvalid
                Nothing
                (Just "artist@example.com")

    describe "validateLiveSessionTermsAcceptance" $ do
        it "requires explicit accepted terms before live-session intake persistence" $ do
            validateLiveSessionTermsAcceptance True (Just " TDF Live Sessions v2 ")
                `shouldBe` Right "TDF Live Sessions v2"

            let assertInvalid accepted rawVersion expectedMessage =
                    case validateLiveSessionTermsAcceptance accepted rawVersion of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err) `shouldContain` expectedMessage
                        Right value ->
                            expectationFailure
                                ( "Expected invalid live-session terms acceptance, got "
                                    <> show value
                                )

            assertInvalid False (Just "TDF Live Sessions v2") "acceptedTerms must be true"
            assertInvalid True Nothing "termsVersion is required"
            assertInvalid True (Just "   ") "termsVersion is required"
            assertInvalid True (Just (Data.Text.replicate 161 "v")) "termsVersion must be 160 characters or fewer"
            assertInvalid True (Just "TDF Live Sessions v2\NUL") "termsVersion must not contain control characters"

    describe "sanitizeLiveSessionRiderFileName" $ do
        it "reduces rider upload names to a stable safe basename before persistence" $ do
            sanitizeLiveSessionRiderFileName "  ../Stage rider final?.pdf  "
                `shouldBe` "Stage-rider-final-.pdf"
            sanitizeLiveSessionRiderFileName " input\tlist\nfinal.txt "
                `shouldBe` "input-list-final.txt"

        it "falls back when rider upload names contain no stable filename characters" $ do
            sanitizeLiveSessionRiderFileName "   " `shouldBe` "rider"
            sanitizeLiveSessionRiderFileName "." `shouldBe` "rider"
            sanitizeLiveSessionRiderFileName ".." `shouldBe` "rider"
            sanitizeLiveSessionRiderFileName "..." `shouldBe` "rider"
            sanitizeLiveSessionRiderFileName "___" `shouldBe` "rider"
            sanitizeLiveSessionRiderFileName "???" `shouldBe` "rider"

    describe "resolveLiveSessionSetlistSortOrders" $ do
        let mkSong title sortOrder =
                LiveSessionSongPayload
                    { lssTitle = title
                    , lssBpm = Nothing
                    , lssSongKey = Nothing
                    , lssLyrics = Nothing
                    , lssSortOrder = sortOrder
                    }

        it "preserves explicit setlist sortOrder values and falls back to submission order only when omitted" $ do
            resolveLiveSessionSetlistSortOrders
                [ mkSong "Intro Jam" (Just 3)
                , mkSong "Finale" Nothing
                ]
                `shouldBe` Right [3, 1]

        it "rejects negative or duplicate resolved setlist sort orders instead of silently persisting ambiguous ordering" $ do
            resolveLiveSessionSetlistSortOrders [mkSong "Intro Jam" (Just (-1))]
                `shouldBe` Left "each setlist song sortOrder must be greater than or equal to 0"
            resolveLiveSessionSetlistSortOrders
                [ mkSong "Intro Jam" (Just 1)
                , mkSong "Finale" Nothing
                ]
                `shouldBe` Left "setlist songs must resolve to distinct sortOrder values"

    describe "live session intake multipart parsing" $ do
        it "normalizes blank optional text fields to Nothing while preserving versioned consent" $ do
            let parsed = fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "  The House Band  ")
                    , ("contactEmail", "   ")
                    , ("acceptedTerms", " yes ")
                    , ("termsVersion", "  TDF Live Sessions v2  ")
                    , ("musicians", "[]")
                    ])
            case parsed :: Either String LiveSessionIntakePayload of
                Left err -> expectationFailure err
                Right payload -> do
                    lsiBandName payload `shouldBe` "The House Band"
                    lsiContactEmail payload `shouldBe` Nothing
                    lsiAcceptedTerms payload `shouldBe` True
                    lsiTermsVersion payload `shouldBe` Just "TDF Live Sessions v2"

        it "normalizes valid contact and musician emails before the intake reaches persistence" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactEmail", " Lead@Example.com ")
                    , ( "musicians"
                      , "[{\"lsmName\":\"  Keys  \",\"lsmEmail\":\" Player@Example.com \",\"lsmIsExisting\":false}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    expectationFailure ("Expected valid intake emails to be accepted, got: " <> err)
                Right payload -> do
                    lsiContactEmail payload `shouldBe` Just "lead@example.com"
                    case lsiMusicians payload of
                        [musician] -> do
                            lsmPartyId musician `shouldBe` Nothing
                            lsmName musician `shouldBe` "Keys"
                            lsmEmail musician `shouldBe` Just "player@example.com"
                            lsmInstrument musician `shouldBe` Nothing
                            lsmRole musician `shouldBe` Nothing
                            lsmNotes musician `shouldBe` Nothing
                            lsmIsExisting musician `shouldBe` False
                        musicians ->
                            expectationFailure ("Expected exactly one normalized musician, got: " <> show musicians)

        it "normalizes valid contact phones before the intake reaches persistence" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactPhone", " +593 99 123 4567 ")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    expectationFailure ("Expected valid contactPhone to be accepted, got: " <> err)
                Right payload ->
                    lsiContactPhone payload `shouldBe` Just "+593991234567"

        it "accepts the canonical frontend musician and setlist keys instead of requiring internal prefixes" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ( "musicians"
                      , "[{\"name\":\"  Keys  \",\"email\":\" Player@Example.com \",\"isExisting\":false}]"
                      )
                    , ( "setlist"
                      , "[{\"title\":\"Intro Jam\",\"songKey\":\"C#m\",\"sortOrder\":3}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    expectationFailure ("Expected canonical live session payload to parse, got: " <> err)
                Right payload -> do
                    case lsiMusicians payload of
                        [musician] -> do
                            lsmName musician `shouldBe` "Keys"
                            lsmEmail musician `shouldBe` Just "player@example.com"
                            lsmIsExisting musician `shouldBe` False
                        musicians ->
                            expectationFailure ("Expected one musician from canonical payload, got: " <> show musicians)
                    case lsiSetlist payload of
                        [song] -> do
                            lssTitle song `shouldBe` "Intro Jam"
                            lssSongKey song `shouldBe` Just "C#m"
                            lssSortOrder song `shouldBe` Just 3
                        songs ->
                            expectationFailure ("Expected one song from canonical payload, got: " <> show songs)

        it "rejects conflicting canonical and legacy musician keys instead of accepting ambiguous payloads" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ( "musicians"
                      , "[{\"name\":\"Keys\",\"lsmName\":\"Drums\",\"isExisting\":false}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Conflicting fields: name and lsmName must match when both are provided"
                Right payload ->
                    expectationFailure ("Expected conflicting musician aliases to be rejected, got: " <> show payload)

        it "rejects unexpected nested musician or setlist fields instead of silently ignoring typos" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ( "musicians"
                      , "[{\"name\":\"Keys\",\"isExisting\":false,\"nickname\":\"Synths\"}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Unexpected fields in LiveSessionMusicianPayload: nickname"
                Right payload ->
                    expectationFailure ("Expected unexpected musician field to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ( "setlist"
                      , "[{\"title\":\"Intro Jam\",\"tempo\":120}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Unexpected fields in LiveSessionSongPayload: tempo"
                Right payload ->
                    expectationFailure ("Expected unexpected setlist field to be rejected, got: " <> show payload)

        it "rejects accepted terms without a terms version" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("acceptedTerms", "true")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "termsVersion is required when acceptedTerms is true"
                Right payload ->
                    expectationFailure ("Expected missing termsVersion to be rejected, got: " <> show payload)

        it "rejects malformed acceptedTerms values instead of silently coercing them to false" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("acceptedTerms", "maybe")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "acceptedTerms must be a boolean"
                Right payload ->
                    expectationFailure ("Expected invalid acceptedTerms to be rejected, got: " <> show payload)

        it "rejects blank required text fields before persistence sees empty identifiers" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "   ")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Missing field: bandName"
                Right payload ->
                    expectationFailure ("Expected blank bandName to be rejected, got: " <> show payload)

        it "rejects malformed contact or musician emails instead of storing unusable addresses" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactEmail", "not-an-email")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "contactEmail must be a valid email address"
                Right payload ->
                    expectationFailure ("Expected invalid contactEmail to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactEmail", "lead<ops>@example.com")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "contactEmail must be a valid email address"
                Right payload ->
                    expectationFailure ("Expected malformed contactEmail local part to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ( "musicians"
                      , "[{\"lsmName\":\"Keys\",\"lsmEmail\":\"not-an-email\",\"lsmIsExisting\":false}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "musician email must be a valid email address"
                Right payload ->
                    expectationFailure ("Expected invalid musician email to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ( "musicians"
                      , "[{\"lsmName\":\"Keys\",\"lsmEmail\":\"keys..player@example.com\",\"lsmIsExisting\":false}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "musician email must be a valid email address"
                Right payload ->
                    expectationFailure ("Expected malformed musician email local part to be rejected, got: " <> show payload)

        it "rejects malformed contact phones instead of storing ambiguous free-form contact text" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactPhone", "call me at 099 123 4567")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "contactPhone must be a valid phone number"
                Right payload ->
                    expectationFailure ("Expected invalid contactPhone to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("contactPhone", "12345")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "contactPhone must be a valid phone number"
                Right payload ->
                    expectationFailure ("Expected short contactPhone to be rejected, got: " <> show payload)

        it "rejects anonymous musician rows instead of creating placeholder parties" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[{\"lsmName\":\"   \",\"lsmEmail\":\"   \",\"lsmIsExisting\":false}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "each musician must include a non-blank name, email, or partyId"
                Right payload ->
                    expectationFailure ("Expected anonymous musician row to be rejected, got: " <> show payload)

        it "rejects blank setlist titles instead of silently dropping songs from the intake" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ("setlist", "[{\"title\":\"   \",\"songKey\":\"C#m\"}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "each setlist song must include a non-blank title"
                Right payload ->
                    expectationFailure ("Expected blank setlist title to be rejected, got: " <> show payload)

        it "rejects non-positive setlist bpm values instead of persisting impossible tempo metadata" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ("setlist", "[{\"title\":\"Intro Jam\",\"bpm\":0}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "setlist song bpm must be a positive integer"
                Right payload ->
                    expectationFailure ("Expected non-positive setlist bpm to be rejected, got: " <> show payload)

        it "rejects negative or duplicate resolved setlist sortOrder values instead of accepting ambiguous ordering" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ("setlist", "[{\"title\":\"Intro Jam\",\"sortOrder\":-1}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "sortOrder must be greater than or equal to 0"
                Right payload ->
                    expectationFailure ("Expected negative setlist sortOrder to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ("setlist", "[{\"title\":\"Intro Jam\",\"sortOrder\":1},{\"title\":\"Finale\"}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "distinct sortOrder values"
                Right payload ->
                    expectationFailure ("Expected duplicate resolved setlist sortOrder values to be rejected, got: " <> show payload)

        it "rejects non-positive musician party ids before any database lookup" $
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[{\"lsmPartyId\":0,\"lsmName\":\"Existing musician\",\"lsmIsExisting\":true}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "musician partyId must be a positive integer"
                Right payload ->
                    expectationFailure ("Expected invalid musician partyId to be rejected, got: " <> show payload)

        it "rejects contradictory existing-musician identity flags instead of guessing whether the row references an existing party" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[{\"lsmName\":\"Existing musician\",\"lsmIsExisting\":true}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "existing musicians must include a positive partyId"
                Right payload ->
                    expectationFailure ("Expected isExisting=true without partyId to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[{\"lsmPartyId\":42,\"lsmName\":\"Existing musician\",\"lsmIsExisting\":false}]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "musician partyId requires isExisting=true"
                Right payload ->
                    expectationFailure ("Expected partyId with isExisting=false to be rejected, got: " <> show payload)

        it "rejects duplicate scalar fields instead of silently taking the first multipart value" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("bandName", "Shadow Band")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Duplicate field: bandName"
                Right payload ->
                    expectationFailure ("Expected duplicate bandName field to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("acceptedTerms", "true")
                    , ("acceptedTerms", "false")
                    , ("musicians", "[]")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Duplicate field: acceptedTerms"
                Right payload ->
                    expectationFailure ("Expected duplicate acceptedTerms field to be rejected, got: " <> show payload)

        it "rejects duplicate rider uploads instead of arbitrarily picking one file" $
            case fromMultipart (mkLiveSessionMultipartWithFiles
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    ]
                    [ mkLiveSessionRider "first.pdf"
                    , mkLiveSessionRider "second.pdf"
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Duplicate file field: rider"
                Right payload ->
                    expectationFailure ("Expected duplicate rider files to be rejected, got: " <> show payload)

        it "rejects unexpected top-level multipart fields and files instead of ignoring typoed intake data" $ do
            case fromMultipart (mkLiveSessionMultipart
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    , ("nickname", "House")
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Unexpected field: nickname"
                Right payload ->
                    expectationFailure ("Expected unexpected multipart field to be rejected, got: " <> show payload)

            case fromMultipart (mkLiveSessionMultipartWithFiles
                    [ ("bandName", "The House Band")
                    , ("musicians", "[]")
                    ]
                    [ mkLiveSessionFile "stagePlot" "stage.pdf"
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "Unexpected file field: stagePlot"
                Right payload ->
                    expectationFailure ("Expected unexpected multipart file to be rejected, got: " <> show payload)

    APITypesSpec.spec
    ArtistSpec.spec
    ServerAuthSpec.spec
    ServerSpec.spec
    ServerAdminSpec.spec
    ServerProposalsSpec.spec
    ServerExtraSpec.spec
    FollowSpec.spec
    FollowHandlerSpec.spec
    PublicLeadSpec.spec
    WhatsAppHistorySpec.spec

mkLiveSessionMultipart :: [(Text, Text)] -> MultipartData Tmp
mkLiveSessionMultipart fields =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = []
        }

mkLiveSessionMultipartWithFiles :: [(Text, Text)] -> [FileData Tmp] -> MultipartData Tmp
mkLiveSessionMultipartWithFiles fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkLiveSessionRider :: Text -> FileData Tmp
mkLiveSessionRider fileName =
    mkLiveSessionFile "rider" fileName

mkLiveSessionFile :: Text -> Text -> FileData Tmp
mkLiveSessionFile inputName fileName =
    FileData
        { fdInputName = inputName
        , fdFileName = fileName
        , fdFileCType = "application/pdf"
        , fdPayload = "/tmp/mock-live-session-rider"
        }

mkEventImageMultipart :: [(Text, Text)] -> [FileData Tmp] -> MultipartData Tmp
mkEventImageMultipart fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkEventImageFile :: Text -> Text -> FileData Tmp
mkEventImageFile inputName fileName =
    FileData
        { fdInputName = inputName
        , fdFileName = fileName
        , fdFileCType = "image/png"
        , fdPayload = "/tmp/mock-event-image"
        }

mkFeedbackMultipart :: [(Text, Text)] -> MultipartData Tmp
mkFeedbackMultipart fields =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = []
        }

mkFeedbackMultipartWithFiles :: [(Text, Text)] -> [FileData Tmp] -> MultipartData Tmp
mkFeedbackMultipartWithFiles fields uploads =
    MultipartData
        { inputs = map (uncurry Input) fields
        , files = uploads
        }

mkFeedbackAttachment :: Text -> FileData Tmp
mkFeedbackAttachment fileName =
    FileData
        { fdInputName = "attachment"
        , fdFileName = fileName
        , fdFileCType = "image/png"
        , fdPayload = "/tmp/mock-feedback-upload"
        }

mkUnexpectedFeedbackAttachment :: Text -> Text -> FileData Tmp
mkUnexpectedFeedbackAttachment inputName fileName =
    FileData
        { fdInputName = inputName
        , fdFileName = fileName
        , fdFileCType = "image/png"
        , fdPayload = "/tmp/mock-feedback-upload"
        }

type SocialSyncTestM = ReaderT Env (ExceptT ServerError IO)

currentSocialSyncTestTime :: UTCTime
currentSocialSyncTestTime =
    UTCTime (fromGregorian 2026 4 15) (secondsToDiffTime 3600)

runSocialSyncIngestHandler
    :: SocialSyncIngestRequest
    -> IO (Either ServerError SocialSyncIngestResponse, [SocialSyncPost], [SocialSyncRun])
runSocialSyncIngestHandler =
    runSocialSyncIngestHandlerWithSetup (pure ())

runSocialSyncIngestHandlerWithSetup
    :: SqlPersistT IO ()
    -> SocialSyncIngestRequest
    -> IO (Either ServerError SocialSyncIngestResponse, [SocialSyncPost], [SocialSyncRun])
runSocialSyncIngestHandlerWithSetup setup request =
    runNoLoggingT $ do
        pool <- createSqlitePool ":memory:" 1
        liftIO $ runSqlPool initializeSocialSyncSchema pool
        liftIO $ runSqlPool setup pool
        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused in social sync ingest tests"
                    }
        result <- liftIO $
            runExceptT (runReaderT (socialSyncIngestHandlerFor socialSyncAdminUser request) env)
        posts <- liftIO $
            runSqlPool
                (fmap entityVal <$> (selectList [] [] :: SqlPersistT IO [Entity SocialSyncPost]))
                pool
        runs <- liftIO $
            runSqlPool
                (fmap entityVal <$> (selectList [] [] :: SqlPersistT IO [Entity SocialSyncRun]))
                pool
        pure (result, posts, runs)

runSocialSyncListHandler
    :: SqlPersistT IO ()
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Int
    -> IO (Either ServerError [SocialSyncPostDTO])
runSocialSyncListHandler setup mPlatform mParty mProfile mTag mLimit =
    runNoLoggingT $ do
        pool <- createSqlitePool ":memory:" 1
        liftIO $ runSqlPool initializeSocialSyncSchema pool
        liftIO $ runSqlPool setup pool
        let env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused in social sync list tests"
                    }
        liftIO $ runExceptT (runReaderT (socialSyncListHandlerFor socialSyncAdminUser mPlatform mParty mProfile mTag mLimit) env)

socialSyncAdminUser :: AuthedUser
socialSyncAdminUser =
    AuthedUser
        { auPartyId = toSqlKey 1
        , auRoles = [Admin]
        , auModules = modulesForRoles [Admin]
        }

socialSyncListHandlerFor
    :: AuthedUser
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Int
    -> SocialSyncTestM [SocialSyncPostDTO]
socialSyncListHandlerFor user =
    case (socialSyncServer user :: ServerT SocialSyncAPI SocialSyncTestM) of
        _ingest :<|> listPosts ->
            listPosts

socialSyncIngestHandlerFor
    :: AuthedUser
    -> SocialSyncIngestRequest
    -> SocialSyncTestM SocialSyncIngestResponse
socialSyncIngestHandlerFor user =
    case (socialSyncServer user :: ServerT SocialSyncAPI SocialSyncTestM) of
        ingestPosts :<|> _listPosts ->
            ingestPosts

type RadioPresenceTestM = ReaderT Env (ExceptT ServerError IO)

radioPresenceUser :: AuthedUser
radioPresenceUser =
    AuthedUser
        { auPartyId = toSqlKey 1
        , auRoles = [Fan]
        , auModules = modulesForRoles [Fan]
        }

runRadioPresenceTest :: RadioPresenceTestM a -> IO (Either ServerError a)
runRadioPresenceTest action =
    runNoLoggingT $ do
        pool <- createSqlitePool ":memory:" 1
        let now = UTCTime (fromGregorian 2026 4 15) (secondsToDiffTime 0)
            env =
                Env
                    { envPool = pool
                    , envConfig = error "envConfig should be unused in radio presence tests"
                    }
        liftIO $ runSqlPool initializeRadioPresenceSchema pool
        liftIO $ runSqlPool (insert_ seedRadioPresenceParty { partyCreatedAt = now }) pool
        liftIO $ runExceptT (runReaderT action env)

seedRadioPresenceParty :: Party
seedRadioPresenceParty =
    Party
        { partyLegalName = Nothing
        , partyDisplayName = "Radio Presence Tester"
        , partyIsOrg = False
        , partyTaxId = Nothing
        , partyPrimaryEmail = Just "radio.presence@example.com"
        , partyPrimaryPhone = Nothing
        , partyWhatsapp = Nothing
        , partyInstagram = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes = Nothing
        , partyCreatedAt = UTCTime (fromGregorian 2026 4 15) (secondsToDiffTime 0)
        }

initializeSocialSyncSchema :: SqlPersistT IO ()
initializeSocialSyncSchema = do
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
        "CREATE TABLE IF NOT EXISTS \"artist_profile\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"artist_party_id\" INTEGER NOT NULL,\
        \\"slug\" VARCHAR NULL,\
        \\"bio\" VARCHAR NULL,\
        \\"city\" VARCHAR NULL,\
        \\"hero_image_url\" VARCHAR NULL,\
        \\"spotify_artist_id\" VARCHAR NULL,\
        \\"spotify_url\" VARCHAR NULL,\
        \\"youtube_channel_id\" VARCHAR NULL,\
        \\"youtube_url\" VARCHAR NULL,\
        \\"website_url\" VARCHAR NULL,\
        \\"featured_video_url\" VARCHAR NULL,\
        \\"genres\" VARCHAR NULL,\
        \\"highlights\" VARCHAR NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NULL,\
        \CONSTRAINT \"unique_artist_profile_party\" UNIQUE (\"artist_party_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_sync_post\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"account_id\" INTEGER NULL,\
        \\"platform\" VARCHAR NOT NULL,\
        \\"external_post_id\" VARCHAR NOT NULL,\
        \\"artist_party_id\" INTEGER NULL,\
        \\"artist_profile_id\" INTEGER NULL,\
        \\"caption\" VARCHAR NULL,\
        \\"permalink\" VARCHAR NULL,\
        \\"media_urls\" VARCHAR NULL,\
        \\"posted_at\" TIMESTAMP NULL,\
        \\"fetched_at\" TIMESTAMP NOT NULL,\
        \\"tags\" VARCHAR NULL,\
        \\"summary\" VARCHAR NULL,\
        \\"ingest_source\" VARCHAR NOT NULL,\
        \\"like_count\" INTEGER NULL,\
        \\"comment_count\" INTEGER NULL,\
        \\"share_count\" INTEGER NULL,\
        \\"view_count\" INTEGER NULL,\
        \\"created_at\" TIMESTAMP NOT NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \UNIQUE(\"platform\", \"external_post_id\"),\
        \FOREIGN KEY(\"artist_party_id\") REFERENCES \"party\"(\"id\"),\
        \FOREIGN KEY(\"artist_profile_id\") REFERENCES \"artist_profile\"(\"id\")\
        \)"
        []
    rawExecute
        "CREATE TABLE IF NOT EXISTS \"social_sync_run\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"platform\" VARCHAR NOT NULL,\
        \\"ingest_source\" VARCHAR NOT NULL,\
        \\"started_at\" TIMESTAMP NOT NULL,\
        \\"ended_at\" TIMESTAMP NULL,\
        \\"status\" VARCHAR NOT NULL,\
        \\"new_posts\" INTEGER NOT NULL,\
        \\"updated_posts\" INTEGER NOT NULL,\
        \\"error_message\" VARCHAR NULL\
        \)"
        []

insertSocialSyncPartyFixture :: Int -> Text -> SqlPersistT IO (Key Party)
insertSocialSyncPartyFixture keyVal displayName =
    let partyId = toSqlKey (fromIntegral keyVal)
    in do
        insertKey
            partyId
            Party
                { partyLegalName = Nothing
                , partyDisplayName = displayName
                , partyIsOrg = False
                , partyTaxId = Nothing
                , partyPrimaryEmail = Nothing
                , partyPrimaryPhone = Nothing
                , partyWhatsapp = Nothing
                , partyInstagram = Nothing
                , partyEmergencyContact = Nothing
                , partyNotes = Nothing
                , partyCreatedAt = currentSocialSyncTestTime
                }
        pure partyId

insertSocialSyncArtistProfileFixture :: Int -> Key Party -> SqlPersistT IO (Key ArtistProfile)
insertSocialSyncArtistProfileFixture keyVal partyId =
    let profileId = toSqlKey (fromIntegral keyVal)
    in do
        insertKey
            profileId
            ArtistProfile
                { artistProfileArtistPartyId = partyId
                , artistProfileSlug = Just "artist-profile"
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
                , artistProfileCreatedAt = currentSocialSyncTestTime
                , artistProfileUpdatedAt = Nothing
                }
        pure profileId

initializeRadioPresenceSchema :: SqlPersistT IO ()
initializeRadioPresenceSchema = do
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
        "CREATE TABLE IF NOT EXISTS \"party_radio_presence\" (\
        \\"id\" INTEGER PRIMARY KEY,\
        \\"party_id\" INTEGER NOT NULL,\
        \\"stream_url\" VARCHAR NOT NULL,\
        \\"station_name\" VARCHAR NULL,\
        \\"station_id\" VARCHAR NULL,\
        \\"updated_at\" TIMESTAMP NOT NULL,\
        \CONSTRAINT \"unique_party_presence\" UNIQUE (\"party_id\"),\
        \FOREIGN KEY(\"party_id\") REFERENCES \"party\"(\"id\")\
        \)"
        []

mkSocialSyncPost :: Text -> Text -> Maybe Text -> UTCTime -> SocialSyncPost
mkSocialSyncPost platform externalPostId tags postedAt =
    SocialSyncPost
        { socialSyncPostAccountId = Nothing
        , socialSyncPostPlatform = platform
        , socialSyncPostExternalPostId = externalPostId
        , socialSyncPostArtistPartyId = Nothing
        , socialSyncPostArtistProfileId = Nothing
        , socialSyncPostCaption = Just ("caption for " <> externalPostId)
        , socialSyncPostPermalink = Nothing
        , socialSyncPostMediaUrls = Nothing
        , socialSyncPostPostedAt = Just postedAt
        , socialSyncPostFetchedAt = postedAt
        , socialSyncPostTags = tags
        , socialSyncPostSummary = Nothing
        , socialSyncPostIngestSource = "manual"
        , socialSyncPostLikeCount = Nothing
        , socialSyncPostCommentCount = Nothing
        , socialSyncPostShareCount = Nothing
        , socialSyncPostViewCount = Nothing
        , socialSyncPostCreatedAt = postedAt
        , socialSyncPostUpdatedAt = postedAt
        }
