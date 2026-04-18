{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (IOException, bracket)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (eitherDecode, (.=))
import qualified Data.Aeson as A
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text
import Data.Time (UTCTime (..), addDays, addUTCTime, fromGregorian, secondsToDiffTime)
import Database.Persist (Key, insert_)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, rawExecute, runSqlPool, toSqlKey)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai (defaultRequest)
import Network.Wai.Internal (Request (..))
import Servant (ServerError (..), ServerT, (:<|>) (..))
import Servant.Multipart (FileData (..), FromMultipart (fromMultipart), Input (..), MultipartData (..), Tmp)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec
import Web.PathPieces (toPathPiece)

import TDF.API (WhatsAppConsentRequest (..), WhatsAppOptOutRequest (..))
import TDF.API.Feedback (FeedbackPayload (..))
import TDF.API.Admin (AdminEmailBroadcastRequest)
import TDF.API.LiveSessions
    ( LiveSessionIntakePayload (..),
      LiveSessionMusicianPayload (..),
      LiveSessionSongPayload (..),
      resolveLiveSessionSetlistSortOrders )
import TDF.API.Radio (RadioAPI)
import TDF.API.SocialSyncAPI (SocialSyncAPI)
import TDF.API.Types
    ( InternTaskUpdate (..),
      RadioImportRequest (..),
      RadioMetadataRefreshRequest (..),
      RadioPresenceDTO (..),
      RadioPresenceUpsert (..),
      RadioTransmissionRequest (..) )
import TDF.API.WhatsApp
    ( CompleteReq (..),
      PreviewReq (..),
      ensureLeadCompletionUpdated,
      validateHookVerifyRequest,
      validateLeadCompletionId,
      validateLeadCompletionLookup,
      validateLeadCompletionRequest )
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
import TDF.DTO.SocialSyncDTO (SocialSyncIngestRequest, SocialSyncPostDTO (..))
import TDF.Models.SocialEventsModels (EventFinanceEntry (..), EventInvitationId, SocialEventId)
import TDF.Auth (AuthedUser (..), modulesForRoles)
import TDF.Models (Party (..), RoleEnum (..), SocialSyncPost (..))
import qualified TDF.ModelsExtra as ME
import qualified TDF.Profiles.ArtistSpec as ArtistSpec
import qualified TDF.ServerAdminSpec as ServerAdminSpec
import qualified TDF.ServerProposalsSpec as ServerProposalsSpec
import TDF.ServerRadio
    ( radioServer,
      validateRadioImportLimit,
      validateRadioImportSources,
      validateRadioMetadataRefreshLimit,
      validateRadioStreamUrl,
      validateRadioTransmissionIngestBase,
      validateRadioTransmissionWhipBase,
      validateRadioTransmissionPublicBase )
import TDF.RagStore (availabilityOverlaps, validateEmbeddingModelDimensions)
import TDF.ServerAdmin (parseSocialErrorsChannel, validateSocialErrorsLimit)
import TDF.Contracts.Server (decodeStoredContract, validateContractId, validateContractPayload, validateContractSendPayload)
import TDF.ServerInternships
    ( parseKey,
      validateInternProjectStatusInput,
      validateInternPermissionDateRange,
      validateInternProfileDateUpdate,
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
      validateOptionalProposalStatus,
      validateProposalContentSource,
      validateProposalStatus,
      validateProposalVersionNumber,
      validateTemplateKey )
import TDF.ServerFeedback
    ( normalizeOptionalFeedbackText,
      sanitizeFeedbackAttachmentFileName,
      validateFeedbackAttachmentSize,
      validateFeedbackTitle,
      validateOptionalFeedbackContactEmail )
import TDF.ServerInstagramOAuth (resolveInstagramRedirectUri)
import TDF.Server
    ( buildWhatsappCtaFor,
      sanitizeStoredCoursePublicUrl,
      validateCoursePublicUrlField,
      validateDatafastBaseUrl )
import TDF.ServerLiveSessions
    ( buildLiveSessionUsernameCollisionCandidate,
      sanitizeLiveSessionRiderFileName )
import TDF.Server.SocialSync
    ( socialSyncServer,
      validateSocialSyncArtistPartyId,
      validateSocialSyncExternalPostId,
      validateSocialSyncArtistProfileId,
      validateSocialSyncPlatform,
      validateSocialSyncPostsLimit,
      validateSocialSyncIngestSource )
import TDF.Server.SocialEventsHandlers (
    normalizeBudgetLineType,
    normalizeEventStatus,
    normalizeEventType,
    normalizeFinanceDirection,
    normalizeFinanceEntryStatus,
    normalizeFinanceSource,
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
    normalizeTicketOrderStatus,
    normalizeTicketStatus,
    parseNearQueryEither,
    parseInvitationIdsEither,
    TicketCheckInLookup (..),
    validateInvitationToPartyId,
    validateInvitationStatusInput,
    validateEventArtistIds,
    validateRsvpStatus,
    validateTicketCheckInLookup,
    validateTicketCheckInOrderStatus,
    validateEventCurrencyInput,
    validateEventCreateTypeStatus,
    validateEventMetadataUpdate,
    validateBudgetLineTypeInput,
 )
import TDF.Auth (extractToken, extractTokenFromHeaders)
import TDF.Config
    ( appPort,
      chatKitApiBase,
      courseInstructorAvatarFallback,
      courseMapFallback,
      courseSlugFallback,
      dbConnString,
      emailConfig,
      facebookGraphBase,
      facebookMessagingApiBase,
      instagramGraphBase,
      instagramMessagingApiBase,
      loadConfig,
      resolveConfiguredAppBase,
      resolveConfiguredAssetsBase,
      sessionCookieName,
      sessionCookiePath,
      sessionCookieSameSite,
      sessionCookieSecure,
      smtpPort )
import qualified TDF.ServerSpec as ServerSpec
import qualified TDF.ServerExtraSpec as ServerExtraSpec
import qualified TDF.Social.FollowHandlerSpec as FollowHandlerSpec
import qualified TDF.Social.FollowSpec as FollowSpec
import qualified TDF.Trials.PublicLeadSpec as PublicLeadSpec
import qualified TDF.Trials.DTO as TrialsDTO
import qualified TDF.WhatsApp.HistorySpec as WhatsAppHistorySpec
import qualified TDF.WhatsApp.Service as WhatsAppService

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

sampleSriScriptRequest :: Sri.SriScriptRequest
sampleSriScriptRequest =
    Sri.SriScriptRequest
        (Sri.SriScriptCustomer "1790012345001" "TDF Test Customer" Nothing Nothing)
        [ Sri.SriScriptLine
            (Just "SRV-001")
            Nothing
            "Studio session"
            1
            9000
            (Just 1500)
            Nothing
            Nothing
        ]
        "001"
        "001"
        "cash"
        False
        Nothing

main :: IO ()
main = hspec $ do
    describe "loadConfig" $ do
        it "falls back to default ports when Fly-style env values are malformed" $
            withEnvOverrides
                [ ("APP_PORT", Just "not-a-port")
                , ("SMTP_PORT", Just "smtp")
                , ("SMTP_HOST", Just "smtp.example.com")
                , ("SMTP_USER", Just "mailer")
                , ("SMTP_PASS", Just "secret")
                , ("SMTP_FROM", Just "tdf@example.com")
                ]
                $ do
                    cfg <- loadConfig
                    appPort cfg `shouldBe` 8080
                    fmap smtpPort (emailConfig cfg) `shouldBe` Just 587

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

        it "normalizes valid session cookie paths before emitting Set-Cookie headers" $
            withEnvOverrides
                [ ("SESSION_COOKIE_PATH", Just " /hq ") ]
                $ do
                    cfg <- loadConfig
                    sessionCookiePath cfg `shouldBe` "/hq"

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

        it "normalizes configured backend public base URLs before generating fallback links" $
            withEnvOverrides
                [ ("HQ_APP_URL", Just " https://hq.example.com/app/ ")
                , ("HQ_ASSETS_BASE_URL", Just " https://cdn.example.com/assets/ ")
                ]
                $ do
                    cfg <- loadConfig
                    resolveConfiguredAppBase cfg `shouldBe` "https://hq.example.com/app"
                    resolveConfiguredAssetsBase cfg `shouldBe` "https://cdn.example.com/assets"

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
                "https://hq.example.com/app?preview=1"
                "HQ_APP_URL must be an absolute http(s) URL without query or fragment"
            assertInvalid
                "HQ_ASSETS_BASE_URL"
                "https://cdn.example.com/assets#logo"
                "HQ_ASSETS_BASE_URL must be an absolute http(s) URL without query or fragment"

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
                "CHATKIT_API_BASE"
                "https://api.openai.com?proxy=1"
                "CHATKIT_API_BASE must be an absolute https URL without query or fragment"

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

        it "normalizes WhatsApp enrollment fallback config before minting public links" $
            withEnvOverrides
                [ ("COURSE_EDITION_SLUG", Just "  ")
                , ("COURSE_DEFAULT_SLUG", Just " Produccion-Musical-MAY-2026 ")
                , ("COURSE_REG_URL", Just "  ")
                , ("HQ_APP_URL", Just " https://hq.example.com/app/ ")
                , ("WA_GRAPH_API_VERSION", Just "  ")
                , ("WHATSAPP_API_VERSION", Just " v21.0 ")
                ]
                $ do
                    cfg <- WhatsAppService.loadWhatsAppConfig
                    WhatsAppService.courseSlug cfg `shouldBe` "produccion-musical-may-2026"
                    WhatsAppService.courseRegUrl cfg `shouldBe` Nothing
                    WhatsAppService.appBaseUrl cfg `shouldBe` "https://hq.example.com/app/"
                    WhatsAppService.waApiVersion cfg `shouldBe` "v21.0"

        it "rejects malformed WhatsApp enrollment fallback URLs before sending unsafe links" $ do
            withEnvOverrides
                [ ("COURSE_REG_URL", Just "javascript:alert(1)") ]
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "COURSE_REG_URL must be an absolute http(s) URL"
                        `isInfixOf` show (err :: IOException)

            withEnvOverrides
                [ ("HQ_APP_URL", Just "https://hq.example.com/app copy") ]
                $ WhatsAppService.loadWhatsAppConfig `shouldThrow` \err ->
                    "HQ_APP_URL must be an absolute http(s) URL"
                        `isInfixOf` show (err :: IOException)

        it "rejects malformed WhatsApp API versions before building Graph request paths" $ do
            let assertInvalid overrides =
                    withEnvOverrides overrides $
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

        it "skips malformed connection URL aliases when complete DB_* settings are authoritative" $
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
                    dbConnString cfg `shouldBe` "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret dbname=tdf_hq sslmode=require target_session_attrs=read-write"

        it "inherits sslmode from DATABASE_URL when DB_* vars stay authoritative" $
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
                    dbConnString cfg `shouldBe` "host=tdf-hq-db.flycast port=5432 user=tdf_hq password=secret dbname=tdf_hq sslmode=disable target_session_attrs=read-write"

        it "prefers explicit PGSSLMODE over the DATABASE_URL sslmode fallback" $
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

        it "preserves an explicit target_session_attrs setting on DATABASE_URL" $
            withEnvOverrides
                [ ("DATABASE_URL", Just "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require&target_session_attrs=any")
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
                    dbConnString cfg `shouldBe` "postgresql://flyuser:flypass@db.fly.internal:5432/tdf_hq?sslmode=require&target_session_attrs=any"

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

    describe "SRI invoice script discovery" $
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

    describe "extractToken" $ do
        let loadAuthConfig =
                withEnvOverrides [("SESSION_COOKIE_NAME", Just "tdf_session_test")] loadConfig
            requestWithHeaders headers =
                defaultRequest { requestHeaders = headers }

        it "accepts a valid bearer token and keeps it authoritative over the cookie" $ do
            cfg <- loadAuthConfig
            extractToken
                cfg
                (requestWithHeaders
                    [ ("Authorization", "Bearer header-token")
                    , ("Cookie", "tdf_session_test=cookie-token")
                    ])
                `shouldBe` Right "header-token"

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

        it "keeps malformed authorization headers authoritative over cookies" $ do
            cfg <- loadAuthConfig
            extractTokenFromHeaders
                cfg
                (Just "Token header-token")
                (Just "tdf_session_test=cookie-token")
                `shouldBe` Left "Invalid Authorization header"

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

        it "rejects malformed explicit Instagram redirect URIs before contacting Facebook" $ do
            cfg <- loadInstagramConfig
            let assertInvalid rawRedirect =
                    case resolveInstagramRedirectUri cfg (Just rawRedirect) of
                        Left serverErr -> do
                            errHTTPCode serverErr `shouldBe` 400
                            BL.unpack (errBody serverErr)
                                `shouldContain`
                                    "redirectUri must be an absolute http(s) URL without query or fragment"
                        Right value ->
                            expectationFailure
                                ("Expected invalid Instagram redirectUri to be rejected, got " <> show value)
            assertInvalid "/oauth/instagram/callback"
            assertInvalid "javascript:alert(1)"
            assertInvalid "https://tdf-app.pages.dev/oauth/instagram/callback?next=/admin"
            assertInvalid "https://tdf-app.pages.dev/oauth/instagram/callback#token"

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

    describe "normalizeOptionalFeedbackText" $ do
        it "trims meaningful optional feedback metadata values" $ do
            normalizeOptionalFeedbackText (Just "  bug ") `shouldBe` Just "bug"
            normalizeOptionalFeedbackText (Just " P2 ") `shouldBe` Just "P2"
            normalizeOptionalFeedbackText (Just " user@example.com ") `shouldBe` Just "user@example.com"

        it "drops explicit blank feedback metadata values instead of storing ambiguous empty strings" $ do
            normalizeOptionalFeedbackText Nothing `shouldBe` Nothing
            normalizeOptionalFeedbackText (Just "   ") `shouldBe` Nothing

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

    describe "validateCoursePublicUrlField" $ do
        it "accepts omitted or trimmed absolute HTTPS course URLs" $ do
            validateCoursePublicUrlField "landingUrl" Nothing `shouldBe` Right Nothing
            validateCoursePublicUrlField "landingUrl" (Just "  https://tdf.example.com/curso/produccion  ")
                `shouldBe` Right (Just "https://tdf.example.com/curso/produccion")
            validateCoursePublicUrlField "whatsappCtaUrl" (Just "https://wa.me/593991234567")
                `shouldBe` Right (Just "https://wa.me/593991234567")

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

    describe "buildWhatsappCtaFor" $ do
        it "uses a configured WhatsApp contact only after phone normalization accepts it" $ do
            buildWhatsappCtaFor
                (Just " +593 99 123 4567 ")
                "Curso de Producción Musical"
                "https://tdf.example.com/curso/produccion"
                `shouldSatisfy`
                    Data.Text.isPrefixOf "https://wa.me/593991234567?text="

        it "falls back to a numberless WhatsApp CTA when the configured contact is malformed" $ do
            buildWhatsappCtaFor
                (Just "593")
                "Curso de Producción Musical"
                "https://tdf.example.com/curso/produccion"
                `shouldSatisfy`
                    Data.Text.isPrefixOf "https://wa.me/?text="

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
            sanitizeFeedbackAttachmentFileName "/\\///" `shouldBe` "attachment"

    describe "validateFeedbackAttachmentSize" $ do
        it "accepts empty and boundary-sized feedback attachments" $ do
            validateFeedbackAttachmentSize 0 `shouldBe` Right ()
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
                    , ticketCheckInTicketCode = Just " ab-123 "
                    }
                `shouldBe` Right (TicketCheckInLookupByCode "AB-123")

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
            assertInvalid "0"
            assertInvalid "-7"
            assertInvalid "ticket-42"

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

    describe "validateRadioStreamUrl" $ do
        it "trims surrounding whitespace and accepts http(s) stream URLs" $
            validateRadioStreamUrl "  HTTPS://radio.example.com/live  "
                `shouldBe` Right "HTTPS://radio.example.com/live"

        it "accepts explicit numeric ports, including bracketed IPv6 hosts" $ do
            validateRadioStreamUrl "https://radio.example.com:8443/live"
                `shouldBe` Right "https://radio.example.com:8443/live"
            validateRadioStreamUrl "https://[2001:db8::1]:8000/live"
                `shouldBe` Right "https://[2001:db8::1]:8000/live"

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
            assertInvalid "https://[2001:db8::1]:70000/live"

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
            let assertInvalid rawBase =
                    case validateRadioTransmissionPublicBase rawBase of
                        Left err -> do
                            errHTTPCode err `shouldBe` 400
                            BL.unpack (errBody err)
                                `shouldContain`
                                    "RADIO_PUBLIC_BASE must not include query or fragment"
                        Right value ->
                            expectationFailure
                                ("Expected invalid public radio base to be rejected, got " <> show value)
            assertInvalid "https://radio.example.com/live?token=abc"
            assertInvalid "https://radio.example.com/live#main"

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
                (validateRadioTransmissionWhipBase "rtmp://stream.example.com/whip")
                "RADIO_WHIP_BASE must be http(s)"
            assertInvalid
                (validateRadioTransmissionWhipBase "https://127.0.0.1/whip")
                "RADIO_WHIP_BASE must not target localhost or private network addresses"

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
                "{\"name\":\"TDF Live\",\"genre\":\"ambient\"}"
                    :: Either String RadioTransmissionRequest
                )
                `shouldSatisfy` isLeft

            ( eitherDecode
                "{\"streamUrl\":\"https://radio.example.com/live\",\"stationName\":\"Radio Uno\"}"
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
        it "requires object payloads and normalizes the stored contract kind" $ do
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
            validateContractPayload (A.object ["amountCents" .= (25000 :: Int)])
                `shouldBe`
                Right
                    ( "generic"
                    , A.object
                        [ "kind" .= ("generic" :: Text)
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
            assertInvalid (A.object ["kind" .= ("" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= ("event vendor" :: Text)]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= A.Null]) "Contract payload kind must be a non-empty slug"
            assertInvalid (A.object ["kind" .= (42 :: Int)]) "Contract payload kind must be a non-empty slug"

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

    describe "PreviewReq" $ do
        it "accepts only the canonical preview-link request body" $ do
            case eitherDecode "{\"phone\":\"+593991234567\"}" of
                Left err ->
                    expectationFailure ("Expected canonical preview-link payload to decode, got: " <> err)
                Right payload ->
                    phone payload `shouldBe` "+593991234567"
            (eitherDecode "{\"phone\":\"+593991234567\",\"status\":\"COMPLETED\"}" :: Either String PreviewReq)
                `shouldSatisfy` isLeft

    describe "validateLeadCompletionRequest" $ do
        it "accepts canonical lead-completion request bodies and rejects unexpected keys" $ do
            case eitherDecode "{\"token\":\"token-123\",\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\"}" of
                Left err ->
                    expectationFailure ("Expected canonical lead completion payload to decode, got: " <> err)
                Right payload ->
                    payload `shouldBe` CompleteReq "token-123" "Ada Lovelace" "ada@example.com"
            (eitherDecode "{\"token\":\"token-123\",\"name\":\"Ada Lovelace\",\"email\":\"ada@example.com\",\"unexpected\":true}" :: Either String CompleteReq)
                `shouldSatisfy` isLeft

        it "trims and canonicalizes meaningful lead-completion payload fields before persistence" $ do
            validateLeadCompletionRequest (CompleteReq " token-123 " " Ada Lovelace " " Ada@Example.com ")
                `shouldBe` Right (CompleteReq "token-123" "Ada Lovelace" "ada@example.com")

        it "rejects blank tokens, blank names, and malformed emails with precise 400s" $ do
            let assertInvalid payload expected = case validateLeadCompletionRequest payload of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` expected
                    Right value ->
                        expectationFailure ("Expected invalid lead completion payload to be rejected, got " <> show value)
            assertInvalid (CompleteReq "   " "Ada Lovelace" "ada@example.com") "Completion token is required"
            assertInvalid (CompleteReq "token-123" "   " "ada@example.com") "Invalid name: must be 1-200 characters"
            assertInvalid (CompleteReq "token-123" "Ada\nLovelace" "ada@example.com") "Invalid name: must not contain control characters"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada @example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@example..com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@-example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada@example-.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" ".ada@example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada.@example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada..lovelace@example.com") "Invalid email format"
            assertInvalid (CompleteReq "token-123" "Ada Lovelace" "ada()@example.com") "Invalid email format"

        it "rejects malformed completion tokens before lookup falls through to a misleading 403" $ do
            let assertInvalid rawToken = case validateLeadCompletionRequest (CompleteReq rawToken "Ada Lovelace" "ada@example.com") of
                    Left err -> do
                        errHTTPCode err `shouldBe` 400
                        BL.unpack (errBody err) `shouldContain` "Completion token format is invalid"
                    Right value ->
                        expectationFailure ("Expected malformed completion token to be rejected, got " <> show value)
            assertInvalid "token 123"
            assertInvalid "token/123"
            assertInvalid "token?123"
            assertInvalid (Data.Text.replicate 129 "a")

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
            validateLeadCompletionLookup "token-123" (Just ("NEW", Just "token-123"))
                `shouldBe` Right ()
            validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Just "token-123"))
                `shouldBe` Right ()

        it "returns explicit 404/403/409 errors for missing, invalid-token, unavailable, blocked, and completed links" $ do
            let assertLookupFailure result expectedStatus expectedBody = case result of
                    Left err -> do
                        errHTTPCode err `shouldBe` expectedStatus
                        BL.unpack (errBody err) `shouldContain` expectedBody
                    Right _ ->
                        expectationFailure ("Expected lookup failure with body containing " <> show expectedBody)
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" Nothing)
                404
                "Lead not found"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Just "other-token")))
                403
                "Invalid completion token"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Nothing)))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("LINK_SENT", Just "   ")))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("COLD", Just "token-123")))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("ARCHIVED", Just "token-123")))
                409
                "Lead completion is not available"
            assertLookupFailure
                (validateLeadCompletionLookup "token-123" (Just ("completed", Nothing)))
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
                    , ( "musicians"
                      , "[{\"lsmName\":\"Keys\",\"lsmEmail\":\"not-an-email\",\"lsmIsExisting\":false}]"
                      )
                    ]) :: Either String LiveSessionIntakePayload of
                Left err ->
                    err `shouldContain` "musician email must be a valid email address"
                Right payload ->
                    expectationFailure ("Expected invalid musician email to be rejected, got: " <> show payload)

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

    APITypesSpec.spec
    ArtistSpec.spec
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
    FileData
        { fdInputName = "rider"
        , fdFileName = fileName
        , fdFileCType = "application/pdf"
        , fdPayload = "/tmp/mock-live-session-rider"
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
        \UNIQUE(\"platform\", \"external_post_id\")\
        \)"
        []

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
