{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TDF.Server where

import           Control.Applicative ((<|>))
import           Control.Exception (SomeException, displayException, try)
import           Control.Concurrent (forkIO)
import           Control.Monad (foldM, forM, forM_, void, when, unless, (>=>), join)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.Int (Int64)
import           Data.List (find, foldl', nub, isInfixOf, isPrefixOf, sortOn)
import           Data.Ord (Down(..))
import           Data.Foldable (for_)
import           Data.Char (isDigit, isSpace, isAlphaNum, toLower)
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import           Data.Aeson (ToJSON(..), Value(..), defaultOptions, object, (.=), eitherDecode, FromJSON(..), encode, genericParseJSON, genericToJSON)
import qualified Data.Aeson.Key as AKey
import           Data.Aeson.Types (camelTo2, fieldLabelModifier, parseMaybe, withObject, (.:), (.:?), (.!=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Scientific as Sci
import           Data.Time (Day, UTCTime (..), fromGregorian, getCurrentTime, toGregorian, utctDay, addUTCTime, secondsToDiffTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)
import qualified Network.Wai as Wai (Request)
import           Servant
import           Servant.Multipart (FileData(..), Tmp)
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import           Web.PathPieces (fromPathPiece, toPathPiece)

import           Database.Persist
import           Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, rawSql, runSqlPool, toSqlKey)
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.API.Types (RolePayload(..), UserRoleSummaryDTO(..), UserRoleUpdatePayload(..), AccountStatusDTO(..), MarketplaceItemDTO(..), MarketplaceCartDTO(..), MarketplaceCartItemUpdate(..), MarketplaceCartItemDTO(..), MarketplaceOrderDTO(..), MarketplaceOrderItemDTO(..), MarketplaceOrderUpdate(..), MarketplaceCheckoutReq(..), DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..), LabelTrackDTO(..), LabelTrackCreate(..), LabelTrackUpdate(..), DriveUploadDTO(..), DriveTokenExchangeRequest(..), DriveTokenRefreshRequest(..), DriveTokenResponse(..), PartyRelatedDTO(..), PartyRelatedBooking(..), PartyRelatedClassSession(..), PartyRelatedLabelTrack(..))
import qualified TDF.API      as Api
import           TDF.API.Marketplace (MarketplaceAPI, MarketplaceAdminAPI)
import           TDF.API.Label (LabelAPI)
import           TDF.API.Drive (DriveAPI, DriveUploadForm(..))
import           TDF.Contracts.API (ContractsAPI)
import           TDF.Config (AppConfig(..), courseInstructorAvatarFallback, courseMapFallback, courseSlugFallback, resolveConfiguredAppBase, resolveConfiguredAssetsBase)
import           TDF.DB
import           TDF.Models
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import           TDF.DTO
import qualified TDF.DTO as DTO
import           TDF.Auth (AuthedUser(..), ModuleAccess(..), authContext, hasModuleAccess, moduleName, loadAuthedUser)
import           TDF.Seed       (seedAll, seedInventoryAssets, seedMarketplaceListings)
import           TDF.ServerAdmin (adminServer)
import qualified TDF.LogBuffer as LogBuf
import           TDF.Server.SocialEventsHandlers (socialEventsServer)
import           TDF.ServerExtra (bandsServer, facebookServer, facebookWebhookServer, instagramServer, instagramWebhookServer, inventoryServer, loadBandForParty, paymentsServer, pipelinesServer, roomsPublicServer, roomsServer, serviceCatalogPublicServer, serviceCatalogServer, sessionsServer)
import           TDF.ServerInstagramOAuth (instagramOAuthServer)
import           TDF.ServerInternships (internshipsServer)
import           TDF.Server.SocialSync (socialSyncServer)
import qualified Data.Map.Strict            as Map
import           TDF.ServerFuture (futureServer)
import           TDF.ServerRadio (radioServer)
import           TDF.ServerLiveSessions (liveSessionsServer)
import           TDF.ServerFeedback (feedbackServer)
import qualified TDF.Contracts.Server as Contracts
import           TDF.ServerProposals (proposalsServer)
import           TDF.Trials.API (TrialsAPI)
import           TDF.Trials.Server (trialsServer)
import qualified TDF.Trials.Models as Trials
import qualified TDF.Meta as Meta
import           TDF.Version      (VersionInfo(..), getVersionInfo)
import qualified TDF.Handlers.InputList as InputList
import qualified TDF.Email as Email
import qualified TDF.Email.Service as EmailSvc
import           TDF.Profiles.Artist ( fetchArtistProfileMap
                                     , fetchPartyNameMap
                                     , loadAllArtistProfilesDTO
                                     , loadArtistProfileDTO
                                     , loadOrCreateArtistProfileDTO
                                     , upsertArtistProfileRecord
                                     )
import           TDF.Routes.Academy ( AcademyAPI
                                    , EnrollReq(..)
                                    , ProgressReq(..)
                                    , ReferralClaimReq(..)
                                    , MicrocourseDTO(..)
                                    , LessonDTO(..)
                                    , NextCohortDTO(..)
                                    )
import           TDF.Routes.Courses ( CoursesPublicAPI
                                    , WhatsAppHooksAPI
                                    , WhatsAppWebhookAPI
                                    , CourseMetadata(..)
                                    , CourseSession(..)
                                    , SyllabusItem(..)
                                    , CourseRegistrationRequest(..)
                                    , CourseRegistrationResponse(..)
                                    , CourseRegistrationStatusUpdate(..)
                                    , UTMTags(..)
                                    , CourseUpsert(..)
                                    , CourseSessionIn(..)
                                    , CourseSyllabusIn(..)
                                    )
import qualified TDF.Routes.Courses as Courses
import           TDF.WhatsApp.Types ( WAMetaWebhook(..)
                                    , WAEntry(..)
                                    , WAChange(..)
                                    , WAMessage(..)
                                    , WAContext(..)
                                    , WAReferral(..)
                                    , WAValue(..)
                                    )
import qualified TDF.WhatsApp.Types as WA
import           TDF.WhatsApp.Client (sendText)
import           TDF.RagStore        (retrieveRagContext)
import           Network.HTTP.Client (Manager, RequestBody(..), Response, newManager, httpLbs, parseRequest, Request(..), responseBody, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.URI (urlEncode, renderQuery, renderSimpleQuery)
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment (lookupEnv)
import qualified TDF.CMS.Models as CMS
import qualified TDF.Calendar.Models as Cal
import qualified TDF.API.Calendar as CalAPI

type AppM = ReaderT Env Handler

data GoogleToken = GoogleToken
  { access_token  :: Text
  , refresh_token :: Maybe Text
  , expires_in    :: Maybe Int
  , token_type    :: Maybe Text
  } deriving (Generic, Show)
instance FromJSON GoogleToken

data GoogleEventsPage = GoogleEventsPage
  { items         :: [Value]
  , nextPageToken :: Maybe Text
  , nextSyncToken :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON GoogleEventsPage where
  parseJSON = withObject "GoogleEventsPage" $ \o ->
    GoogleEventsPage
      <$> o .:? "items" .!= []
      <*> o .:? "nextPageToken"
      <*> o .:? "nextSyncToken"

data ParsedEvent = ParsedEvent
  { peGoogleId   :: Text
  , peStatus     :: Text
  , peSummary    :: Maybe Text
  , peDescription :: Maybe Text
  , peLocation   :: Maybe Text
  , peStartAt    :: Maybe UTCTime
  , peEndAt      :: Maybe UTCTime
  , peUpdatedAt  :: Maybe UTCTime
  , peHtmlLink   :: Maybe Text
  , peAttendees  :: Maybe CMS.AesonValue
  , peRawPayload :: Maybe CMS.AesonValue
  }

data GoogleIdTokenInfo = GoogleIdTokenInfo
  { gitAud            :: Text
  , gitEmail          :: Text
  , gitEmailVerified  :: Bool
  , gitName           :: Maybe Text
  , gitPicture        :: Maybe Text
  , gitSub            :: Text
  , gitIss            :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON GoogleIdTokenInfo where
  parseJSON = withObject "GoogleIdTokenInfo" $ \o -> do
    gitAud <- o .: "aud"
    gitEmail <- o .: "email"
    gitSub <- o .: "sub"
    gitName <- o .:? "name"
    gitPicture <- o .:? "picture"
    gitIss <- o .:? "iss"
    gitEmailVerified <- parseEmailVerified o
    pure GoogleIdTokenInfo{..}
    where
      parseEmailVerified obj = do
        mVal <- obj .:? "email_verified"
        pure $ case mVal of
          Just (Bool b)   -> b
          Just (String t) -> let lowered = T.toLower (T.strip t) in lowered == "true" || lowered == "1"
          _               -> False

data GoogleProfile = GoogleProfile
  { gpEmail   :: Text
  , gpName    :: Maybe Text
  , gpPicture :: Maybe Text
  } deriving (Show)

runDB :: SqlPersistT IO a -> AppM a
runDB = runDb

runDb :: SqlPersistT IO a -> AppM a
runDb action = do
  Env{..} <- ask
  liftIO $ runSqlPool action envPool

type CombinedAPI = TrialsAPI :<|> API

mkApp :: Env -> Application
mkApp env =
  let apiProxy = Proxy :: Proxy API
      combinedProxy = Proxy :: Proxy CombinedAPI
      ctxProxy = Proxy :: Proxy '[AuthHandler Wai.Request AuthedUser]
      ctx      = authContext env
      trials   = trialsServer (envPool env)
      apiSrv   = hoistServerWithContext apiProxy ctxProxy (nt env) (server env)
  in serveWithContext combinedProxy ctx (trials :<|> apiSrv)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

server :: Env -> ServerT API AppM
server env =
  let Env{envConfig} = env
      assetsRoot = assetsRootDir envConfig
  in
       versionServer
  :<|> health
  :<|> mcpServer
  :<|> login
  :<|> googleLogin
  :<|> signup
  :<|> changePassword
  :<|> authV1Server
  :<|> fanPublicServer
  :<|> coursesPublicServer
  :<|> instagramWebhookServer
  :<|> facebookWebhookServer
  :<|> whatsappHooksServer
  :<|> whatsappWebhookServer
  :<|> metaServer
  :<|> academyServer
  :<|> seedTrigger
  :<|> inputListServer
  :<|> adsPublicServer
  :<|> cmsPublicServer
  :<|> whatsappConsentPublicServer
  :<|> marketplacePublicServer
  :<|> contractsServer
  :<|> radioPresencePublicServer
  :<|> roomsPublicServer
  :<|> serviceCatalogPublicServer
  :<|> listEngineersPublic
  :<|> bookingPublicServer
  :<|> inventoryStaticServer assetsRoot
  :<|> assetsServeServer assetsRoot
  :<|> protectedServer

authV1Server :: ServerT Api.AuthV1API AppM
authV1Server = signup :<|> passwordReset :<|> passwordResetConfirm :<|> changePassword

versionServer :: ServerT Api.VersionAPI AppM
versionServer = liftIO getVersionInfo

mcpServer :: ServerT Api.McpAPI AppM
mcpServer rawRequest =
  case parseMcpRequest rawRequest of
    Nothing -> pure (mcpErrorValue Nothing (-32600) "Invalid Request" Nothing)
    Just req -> handleMcpRequest req

data McpRequest = McpRequest
  { mcpReqId :: Maybe Value
  , mcpReqMethod :: Text
  , mcpReqParams :: Maybe Value
  } deriving (Show)

mcpProtocolVersion :: Text
mcpProtocolVersion = "2024-11-05"

mcpTools :: [Value]
mcpTools =
  [ object
      [ "name" .= ("tdf_health_check" :: Text)
      , "description" .= ("Return service health and version metadata." :: Text)
      , "inputSchema" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
      ]
  ]

parseMcpRequest :: Value -> Maybe McpRequest
parseMcpRequest = parseMaybe $ withObject "McpRequest" $ \o ->
  McpRequest
    <$> o .:? "id"
    <*> o .: "method"
    <*> o .:? "params"

parseToolCallParams :: Value -> Maybe (Text, Value)
parseToolCallParams = parseMaybe $ withObject "ToolCallParams" $ \o -> do
  toolName <- o .: "name"
  args <- o .:? "arguments" .!= object []
  pure (toolName, args)

handleMcpRequest :: McpRequest -> AppM Value
handleMcpRequest req@McpRequest{ mcpReqMethod = method, mcpReqParams = params } =
  case method of
    "initialize" -> do
      info <- liftIO getVersionInfo
      let VersionInfo { name = appName, appVer } = info
          result =
            object
              [ "protocolVersion" .= mcpProtocolVersion
              , "capabilities" .= object
                  [ "tools" .= object []
                  , "resources" .= object []
                  , "prompts" .= object []
                  ]
              , "serverInfo" .= object
                  [ "name" .= appName
                  , "version" .= appVer
                  ]
              ]
      pure (mcpSuccess req result)
    "tools/list" ->
      pure (mcpSuccess req (object ["tools" .= mcpTools]))
    "tools/call" ->
      handleMcpToolCall req params
    "resources/list" ->
      pure (mcpSuccess req (object ["resources" .= ([] :: [Value])]))
    "prompts/list" ->
      pure (mcpSuccess req (object ["prompts" .= ([] :: [Value])]))
    "initialized" ->
      pure (mcpSuccess req (object []))
    _ ->
      pure (mcpErrorValue (mcpReqId req) (-32601) "Method not found" Nothing)

handleMcpToolCall :: McpRequest -> Maybe Value -> AppM Value
handleMcpToolCall req rawParams =
  case rawParams >>= parseToolCallParams of
    Nothing -> pure (mcpErrorValue (mcpReqId req) (-32602) "Invalid params" Nothing)
    Just (toolName, _) ->
      case toolName of
        "tdf_health_check" -> do
          info <- liftIO getVersionInfo
          now <- liftIO getCurrentTime
          let VersionInfo { appVer, commit, buildTime } = info
              timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
              message = T.intercalate "\n"
                [ "status: ok"
                , "time: " <> timestamp
                , "version: " <> appVer
                , "commit: " <> commit
                , "buildTime: " <> buildTime
                ]
              result =
                object
                  [ "content" .=
                      [ object
                          [ "type" .= ("text" :: Text)
                          , "text" .= message
                          ]
                      ]
                  ]
          pure (mcpSuccess req result)
        _ ->
          pure (mcpErrorValue (mcpReqId req) (-32602) ("Unknown tool: " <> toolName) Nothing)

mcpSuccess :: McpRequest -> Value -> Value
mcpSuccess req result =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= fromMaybe Null (mcpReqId req)
    , "result" .= result
    ]

mcpErrorValue :: Maybe Value -> Int -> Text -> Maybe Value -> Value
mcpErrorValue mReqId code message mData =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= fromMaybe Null mReqId
    , "error" .= object (baseFields <> dataField)
    ]
  where
    baseFields =
      [ "code" .= code
      , "message" .= message
      ]
    dataField = maybe [] (\datum -> ["data" .= datum]) mData

inputListServer :: ServerT Api.InputListAPI AppM
inputListServer = publicRoutes :<|> seedRoutes
  where
    publicRoutes =
           listInventory
      :<|> getSessionInputList
      :<|> getSessionInputListPdf

    seedRoutes =
           seedInventory
      :<|> seedHQ

listInventory
  :: Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> AppM [Entity InputList.InventoryItem]
listInventory mFieldParam mSessionParam mChannelParam = do
  parsedField <- case mFieldParam of
    Nothing -> pure Nothing
    Just rawField ->
      case InputList.parseAssetField rawField of
        Nothing    -> throwBadRequest "Unsupported field"
        Just field -> pure (Just field)
  sessionKey <- case mSessionParam of
    Nothing   -> pure Nothing
    Just raw  ->
      case fromPathPiece raw of
        Nothing -> throwBadRequest "Invalid sessionId"
        Just k  -> pure (Just k)
  when (isJust mChannelParam && isNothing sessionKey) $
    throwBadRequest "channel requires sessionId"
  when (maybe False (< 1) mChannelParam) $
    throwBadRequest "channel must be greater than or equal to 1"
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool (InputList.listInventoryDB parsedField sessionKey mChannelParam)

seedInventory :: Maybe Text -> AppM NoContent
seedInventory rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool InputList.seedInventoryDB
  pure NoContent

seedHQ :: Maybe Text -> AppM NoContent
seedHQ rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool envPool (InputList.seedHQDB now)
  pure NoContent

requireSeedToken :: Maybe Text -> AppM ()
requireSeedToken rawToken = do
  Env{..} <- ask
  let encodeBody = BL.fromStrict . TE.encodeUtf8
      missingHeader = throwError err401 { errBody = encodeBody "Missing X-Seed-Token header" }
      disabled = throwError err403 { errBody = encodeBody "Seeding endpoint disabled" }
      invalid = throwError err403 { errBody = encodeBody "Invalid seed token" }
  secret <- maybe disabled pure (seedTriggerToken envConfig)
  token  <- maybe missingHeader (pure . T.strip) rawToken
  when (T.null token) missingHeader
  when (token /= secret) invalid
  pure ()

getSessionInputList :: Maybe Int -> Maybe Text -> AppM [Entity InputList.InputListEntry]
getSessionInputList mIndex mSessionId = do
  (_session, rows) <- resolveSessionInputData mIndex mSessionId
  pure rows

getSessionInputListPdf
  :: Maybe Int
  -> Maybe Text
  -> AppM (Headers '[Header "Content-Disposition" Text] BL.ByteString)
getSessionInputListPdf mIndex mSessionId = do
  (Entity _ session, rows) <- resolveSessionInputData mIndex mSessionId
  let title = fromMaybe (ME.sessionService session <> " session") (ME.sessionClientPartyRef session)
      latex = InputList.renderInputListLatex title rows
  pdfResult <- liftIO (InputList.generateInputListPdf latex)
  case pdfResult of
    Left errMsg -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 errMsg) }
    Right pdf -> do
      let fileName    = InputList.sanitizeFileName title <> ".pdf"
          disposition = T.concat ["attachment; filename=\"", fileName, "\""]
      pure (addHeader disposition pdf)

resolveSessionInputData
  :: Maybe Int
  -> Maybe Text
  -> AppM (Entity ME.Session, [Entity InputList.InputListEntry])
resolveSessionInputData mIndex mSessionId = do
  Env{..} <- ask
  action <- case mSessionId of
    Just rawId ->
      case fromPathPiece rawId of
        Nothing     -> throwBadRequest "Invalid sessionId"
        Just keyVal -> pure (InputList.fetchSessionInputRowsByKey keyVal)
    Nothing -> do
      idx <- case mIndex of
        Nothing     -> pure 1
        Just n
          | n >= 1    -> pure n
          | otherwise -> throwBadRequest "index must be greater than or equal to 1"
      pure (InputList.fetchSessionInputRowsByIndex idx)
  result <- liftIO $ flip runSqlPool envPool action
  maybe (throwError err404) pure result

fanPublicServer :: ServerT FanPublicAPI AppM
fanPublicServer =
       listFanArtists
  :<|> fanArtistProfile
  :<|> fanArtistReleases
  where
    listFanArtists :: AppM [ArtistProfileDTO]
    listFanArtists = do
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool loadAllArtistProfilesDTO

    fanArtistProfile :: Int64 -> AppM ArtistProfileDTO
    fanArtistProfile artistId = do
      Env pool _ <- ask
      mDto <- liftIO $ flip runSqlPool pool $ loadArtistProfileDTO (toSqlKey artistId)
      maybe (throwError err404) pure mDto

    fanArtistReleases :: Int64 -> AppM [ArtistReleaseDTO]
    fanArtistReleases artistId = do
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool $ do
        releases <- selectList [ArtistReleaseArtistPartyId ==. toSqlKey artistId] [Desc ArtistReleaseCreatedAt]
        pure (map toArtistReleaseDTO releases)

coursesPublicServer :: ServerT CoursesPublicAPI AppM
coursesPublicServer =
       courseMetadataH
  :<|> registrationH
  where
    courseMetadataH slug = loadCourseMetadata slug
    registrationH slug payload = createOrUpdateRegistration slug payload

coursesAdminServer :: AuthedUser -> ServerT Courses.CoursesAdminAPI AppM
coursesAdminServer user =
       upsertCourseH
  :<|> listRegistrationsH
  :<|> getRegistrationH
  :<|> listEmailEventsH
  :<|> updateStatusH
  where
    requireCourseAdmin = unless (hasModuleAccess ModuleAdmin user) $
      throwError err403

    upsertCourseH payload = do
      requireCourseAdmin
      saveCourse payload

    listRegistrationsH mSlug mStatus mLimit = do
      requireCourseAdmin
      listCourseRegistrations mSlug mStatus mLimit

    getRegistrationH slug regId = do
      requireCourseAdmin
      fetchCourseRegistration slug regId

    listEmailEventsH regId mLimit = do
      requireCourseAdmin
      listCourseRegistrationEmailEvents regId mLimit

    updateStatusH slug regId statusPayload = do
      requireCourseAdmin
      updateCourseRegistrationStatus slug regId statusPayload

radioPresencePublicServer :: Int64 -> AppM (Maybe RadioPresenceDTO)
radioPresencePublicServer partyId = do
  when (partyId <= 0) $ throwBadRequest "Invalid party id"
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    mRow <- selectFirst [PartyRadioPresencePartyId ==. toSqlKey partyId] []
    pure (fmap presenceToDTO mRow)
  where
    presenceToDTO (Entity _ PartyRadioPresence{..}) =
      RadioPresenceDTO
        { rpPartyId     = fromIntegral (fromSqlKey partyRadioPresencePartyId)
        , rpStreamUrl   = partyRadioPresenceStreamUrl
        , rpStationName = partyRadioPresenceStationName
        , rpStationId   = partyRadioPresenceStationId
        , rpUpdatedAt   = partyRadioPresenceUpdatedAt
        }

listEngineersPublic :: AppM [PublicEngineerDTO]
listEngineersPublic = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    engineerRoles <- selectList [PartyRoleRole ==. Engineer, PartyRoleActive ==. True] []
    let partyIds = map (partyRolePartyId . entityVal) engineerRoles
    parties <- selectList
      [ PartyId <-. partyIds
      , PartyPrimaryEmail !=. Nothing -- evita cuentas de sistema sin correo (p.ej. "Scheduling")
      ]
      [Asc PartyDisplayName]
    pure [PublicEngineerDTO (fromSqlKey pid) (M.partyDisplayName p) | Entity pid p <- parties]

whatsappWebhookServer :: ServerT WhatsAppWebhookAPI AppM
whatsappWebhookServer =
       verifyHook
  :<|> handleMessages
  where
    verifyHook _ mToken mChallenge = do
      cfg <- liftIO loadWhatsAppEnv
      case waVerifyToken cfg of
        Nothing -> throwError err403 { errBody = "Verify token not configured" }
        Just expected ->
          case mToken of
            Just tok | tok == expected -> pure (fromMaybe "" mChallenge)
            _ -> throwError err403 { errBody = "Verify token mismatch" }

    handleMessages payload = do
      cfg <- liftIO loadWhatsAppEnv
      Env{envConfig, envPool} <- ask
      now <- liftIO getCurrentTime
      let inbound = extractWhatsAppInbound payload
      for_ inbound $ \WAInbound{..} -> do
        let externalId =
              if T.null waInboundExternalId
                then waInboundSenderId <> "-" <> T.pack (show now)
                else waInboundExternalId
        liftIO $ flip runSqlPool envPool $ do
          _ <- upsert (ME.WhatsAppMessage externalId
                         waInboundSenderId
                         Nothing
                         (Just waInboundText)
                         "incoming"
                         waInboundAdExternalId
                         waInboundAdName
                         waInboundCampaignExternalId
                         waInboundCampaignName
                         waInboundMetadata
                         "pending"
                         Nothing
                         Nothing
                         Nothing
                         0
                         Nothing
                         Nothing
                         Nothing
                         now)
               [ ME.WhatsAppMessageText =. Just waInboundText
               , ME.WhatsAppMessageDirection =. "incoming"
               , ME.WhatsAppMessageReplyStatus =. "pending"
               , ME.WhatsAppMessageAdExternalId =. waInboundAdExternalId
               , ME.WhatsAppMessageAdName =. waInboundAdName
               , ME.WhatsAppMessageCampaignExternalId =. waInboundCampaignExternalId
               , ME.WhatsAppMessageCampaignName =. waInboundCampaignName
               , ME.WhatsAppMessageMetadata =. waInboundMetadata
               ]
          pure ()
        let lowerBody = T.toLower (T.strip waInboundText)
        when ("inscribirme" `T.isInfixOf` lowerBody) $ do
          case normalizePhone waInboundSenderId of
            Nothing -> pure ()
            Just phone -> do
              _ <- createOrUpdateRegistration (productionCourseSlug envConfig) CourseRegistrationRequest
                { fullName = Nothing
                , email = Nothing
                , phoneE164 = Just phone
                , source = "whatsapp"
                , howHeard = Just "whatsapp"
                , utm = Nothing
                }
              replyRes <- sendWhatsappReply cfg phone
              case replyRes of
                Left err -> liftIO $ flip runSqlPool envPool $
                  updateWhere
                    [ ME.WhatsAppMessageExternalId ==. externalId ]
                    [ ME.WhatsAppMessageReplyError =. Just err ]
                Right replyTxt -> liftIO $ flip runSqlPool envPool $ do
                  updateWhere
                    [ ME.WhatsAppMessageExternalId ==. externalId ]
                    [ ME.WhatsAppMessageRepliedAt =. Just now
                    , ME.WhatsAppMessageReplyText =. Just replyTxt
                    , ME.WhatsAppMessageReplyError =. Nothing
                    ]
                  _ <- insert_ (ME.WhatsAppMessage (externalId <> "-out-" <> T.pack (show now))
                                  waInboundSenderId
                                  Nothing
                                  (Just replyTxt)
                                  "outgoing"
                                  waInboundAdExternalId
                                  waInboundAdName
                                  waInboundCampaignExternalId
                                  waInboundCampaignName
                                  Nothing
                                  "sent"
                                  Nothing
                                  Nothing
                                  (Just now)
                                  1
                                  Nothing
                                  Nothing
                                  Nothing
                                  now)
                  pure ()
      pure NoContent

whatsappHooksServer :: ServerT WhatsAppHooksAPI AppM
whatsappHooksServer = whatsappWebhookServer

whatsappMessagesServer :: AuthedUser -> ServerT Api.WhatsAppMessagesAPI AppM
whatsappMessagesServer _ mLimit mDirection mRepliedOnly = do
  let limit = normalizeLimit mLimit
  direction <- parseDirectionParam mDirection
  repliedOnly <- parseBoolParam mRepliedOnly
  let filters =
        concat
          [ maybe [] (\dir -> [ME.WhatsAppMessageDirection ==. dir]) direction
          , if repliedOnly then [ME.WhatsAppMessageRepliedAt !=. Nothing] else []
          ]
  rows <- runDB $
    selectList filters [Desc ME.WhatsAppMessageCreatedAt, LimitTo limit]
  let toObj (Entity _ m) = object
        [ "externalId" .= ME.whatsAppMessageExternalId m
        , "senderId"   .= ME.whatsAppMessageSenderId m
        , "senderName" .= ME.whatsAppMessageSenderName m
        , "text"       .= ME.whatsAppMessageText m
        , "metadata"   .= ME.whatsAppMessageMetadata m
        , "direction"  .= ME.whatsAppMessageDirection m
        , "repliedAt"  .= ME.whatsAppMessageRepliedAt m
        , "replyText"  .= ME.whatsAppMessageReplyText m
        , "replyError" .= ME.whatsAppMessageReplyError m
        , "createdAt"  .= ME.whatsAppMessageCreatedAt m
        ]
  pure (toJSON (map toObj rows))

whatsappReplyServer :: AuthedUser -> ServerT Api.WhatsAppReplyAPI AppM
whatsappReplyServer _ WhatsAppReplyReq{..} = do
  now <- liftIO getCurrentTime
  waEnv <- liftIO loadWhatsAppEnv
  let recipient = T.strip wrSenderId
      body = T.strip wrMessage
      mExternalId = wrExternalId >>= (\raw -> let trimmed = T.strip raw in if T.null trimmed then Nothing else Just trimmed)
  when (T.null recipient) $ throwBadRequest "Remitente requerido"
  when (T.null body) $ throwBadRequest "Mensaje vacío"
  sendResult <- sendWhatsAppText waEnv recipient body
  runDB $ do
    insert_ (ME.WhatsAppMessage (recipient <> "-out-" <> T.pack (show now))
              recipient
              Nothing
              (Just body)
              "outgoing"
              Nothing
              Nothing
              Nothing
              Nothing
              Nothing
              "sent"
              Nothing
              Nothing
              (Just now)
              1
              Nothing
              Nothing
              (either Just (const Nothing) sendResult)
              now)
    for_ mExternalId $ \extId -> do
      let baseFilters =
            [ ME.WhatsAppMessageExternalId ==. extId
            , ME.WhatsAppMessageDirection ==. "incoming"
            , ME.WhatsAppMessageRepliedAt ==. Nothing
            ]
      case sendResult of
        Left err ->
          updateWhere baseFilters
            [ ME.WhatsAppMessageReplyError =. Just err
            ]
        Right _ ->
          updateWhere baseFilters
            [ ME.WhatsAppMessageRepliedAt =. Just now
            , ME.WhatsAppMessageReplyText =. Just body
            , ME.WhatsAppMessageReplyError =. Nothing
            ]
  case sendResult of
    Left err ->
      pure (object ["status" .= ("error" :: Text), "message" .= err])
    Right responseBody ->
      pure (object ["status" .= ("ok" :: Text), "message" .= ("sent" :: Text), "response" .= responseBody])

whatsappConsentServer :: AuthedUser -> ServerT Api.WhatsAppConsentAPI AppM
whatsappConsentServer user =
  let requireAdmin = unless (hasRole Admin user) $ throwError err403
  in whatsappConsentRoutes "tdf-hq-ui" requireAdmin

whatsappConsentPublicServer :: ServerT Api.WhatsAppConsentPublicAPI AppM
whatsappConsentPublicServer = whatsappConsentRoutes "public" (pure ())

whatsappConsentRoutes :: Text -> AppM () -> ServerT Api.WhatsAppConsentRoutes AppM
whatsappConsentRoutes defaultSource requireGate =
       createConsent
  :<|> revokeConsent
  :<|> fetchStatus
  where
    normalizePhoneOrFail raw =
      case normalizePhone raw of
        Just val -> pure val
        Nothing -> throwBadRequest "Número de WhatsApp inválido."

    toStatus phoneVal mRow =
      case mRow of
        Nothing ->
          WhatsAppConsentStatus
            { wcsPhone = phoneVal
            , wcsConsent = False
            , wcsConsentedAt = Nothing
            , wcsRevokedAt = Nothing
            , wcsDisplayName = Nothing
            }
        Just (Entity _ row) ->
          WhatsAppConsentStatus
            { wcsPhone = phoneVal
            , wcsConsent = ME.whatsAppConsentConsent row
            , wcsConsentedAt = ME.whatsAppConsentConsentedAt row
            , wcsRevokedAt = ME.whatsAppConsentRevokedAt row
            , wcsDisplayName = ME.whatsAppConsentDisplayName row
            }

    persistConsent phoneVal nameClean sourceClean noteClean now = runDB $ do
      let record =
            ME.WhatsAppConsent
              { ME.whatsAppConsentPhoneE164 = phoneVal
              , ME.whatsAppConsentDisplayName = nameClean
              , ME.whatsAppConsentConsent = True
              , ME.whatsAppConsentSource = sourceClean
              , ME.whatsAppConsentNote = noteClean
              , ME.whatsAppConsentConsentedAt = Just now
              , ME.whatsAppConsentRevokedAt = Nothing
              , ME.whatsAppConsentCreatedAt = now
              , ME.whatsAppConsentUpdatedAt = now
              }
      _ <- upsert record
        [ ME.WhatsAppConsentDisplayName =. nameClean
        , ME.WhatsAppConsentConsent =. True
        , ME.WhatsAppConsentSource =. sourceClean
        , ME.WhatsAppConsentNote =. noteClean
        , ME.WhatsAppConsentConsentedAt =. Just now
        , ME.WhatsAppConsentRevokedAt =. Nothing
        , ME.WhatsAppConsentUpdatedAt =. now
        ]
      getBy (ME.UniqueWhatsAppConsent phoneVal)

    persistOptOut phoneVal reasonClean now = runDB $ do
      let record =
            ME.WhatsAppConsent
              { ME.whatsAppConsentPhoneE164 = phoneVal
              , ME.whatsAppConsentDisplayName = Nothing
              , ME.whatsAppConsentConsent = False
              , ME.whatsAppConsentSource = Just "opt-out"
              , ME.whatsAppConsentNote = reasonClean
              , ME.whatsAppConsentConsentedAt = Nothing
              , ME.whatsAppConsentRevokedAt = Just now
              , ME.whatsAppConsentCreatedAt = now
              , ME.whatsAppConsentUpdatedAt = now
              }
      _ <- upsert record
        [ ME.WhatsAppConsentDisplayName =. Nothing
        , ME.WhatsAppConsentConsent =. False
        , ME.WhatsAppConsentSource =. Just "opt-out"
        , ME.WhatsAppConsentNote =. reasonClean
        , ME.WhatsAppConsentConsentedAt =. Nothing
        , ME.WhatsAppConsentRevokedAt =. Just now
        , ME.WhatsAppConsentUpdatedAt =. now
        ]
      getBy (ME.UniqueWhatsAppConsent phoneVal)

    sendConsentMessage phoneVal nameClean = do
      waEnv <- liftIO loadWhatsAppEnv
      let greeting =
            case nameClean of
              Just nm -> "Hola " <> nm <> "! "
              Nothing -> "Hola! "
          msg = greeting <>
            "Gracias por aceptar recibir mensajes de TDF Records por WhatsApp. " <>
            "Responde STOP si deseas dejar de recibir mensajes."
      sendWhatsAppText waEnv phoneVal msg

    sendOptOutMessage phoneVal = do
      waEnv <- liftIO loadWhatsAppEnv
      let msg = "Listo. No recibirás más mensajes de TDF Records por WhatsApp. " <>
                "Si fue un error, escríbenos y lo reactivamos."
      sendWhatsAppText waEnv phoneVal msg

    createConsent WhatsAppConsentRequest{..} = do
      requireGate
      unless wcrConsent $ throwBadRequest "Debes aceptar el consentimiento para continuar."
      phoneVal <- normalizePhoneOrFail wcrPhone
      now <- liftIO getCurrentTime
      let nameClean = cleanOptional wcrName
          sourceClean = cleanOptional wcrSource <|> Just defaultSource
          noteClean = Just "consent"
          shouldSend = fromMaybe True wcrSendMessage
      _ <- persistConsent phoneVal nameClean sourceClean noteClean now
      (sent, msgText) <- if shouldSend
        then do
          res <- sendConsentMessage phoneVal nameClean
          pure $ case res of
            Left err -> (False, Just err)
            Right msg -> (True, Just msg)
        else pure (False, Nothing)
      status <- runDB $ getBy (ME.UniqueWhatsAppConsent phoneVal)
      pure WhatsAppConsentResponse
        { wcrsStatus = toStatus phoneVal status
        , wcrsMessageSent = sent
        , wcrsMessage = msgText
        }

    revokeConsent WhatsAppOptOutRequest{..} = do
      requireGate
      phoneVal <- normalizePhoneOrFail worPhone
      now <- liftIO getCurrentTime
      let reasonClean = cleanOptional worReason
          shouldSend = fromMaybe True worSendMessage
      _ <- persistOptOut phoneVal reasonClean now
      (sent, msgText) <- if shouldSend
        then do
          res <- sendOptOutMessage phoneVal
          pure $ case res of
            Left err -> (False, Just err)
            Right msg -> (True, Just msg)
        else pure (False, Nothing)
      status <- runDB $ getBy (ME.UniqueWhatsAppConsent phoneVal)
      pure WhatsAppConsentResponse
        { wcrsStatus = toStatus phoneVal status
        , wcrsMessageSent = sent
        , wcrsMessage = msgText
        }

    fetchStatus mPhone = do
      requireGate
      phoneRaw <- maybe (throwBadRequest "phone requerido") pure mPhone
      phoneVal <- normalizePhoneOrFail phoneRaw
      mRow <- runDB $ getBy (ME.UniqueWhatsAppConsent phoneVal)
      pure (toStatus phoneVal mRow)

normalizeLimit :: Maybe Int -> Int
normalizeLimit = max 1 . min 200 . fromMaybe 100

parseBoolParam :: Maybe Text -> AppM Bool
parseBoolParam Nothing = pure False
parseBoolParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "true" -> pure True
    "1" -> pure True
    "yes" -> pure True
    "false" -> pure False
    "0" -> pure False
    "no" -> pure False
    "" -> pure False
    _ -> throwBadRequest "Invalid repliedOnly value"

parseDirectionParam :: Maybe Text -> AppM (Maybe Text)
parseDirectionParam Nothing = pure Nothing
parseDirectionParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "" -> pure Nothing
    "all" -> pure Nothing
    "incoming" -> pure (Just "incoming")
    "outgoing" -> pure (Just "outgoing")
    _ -> throwBadRequest "Invalid direction value"

data MetaChannel = MetaInstagram | MetaFacebook
  deriving (Show, Eq)

data MetaBackfillOptions = MetaBackfillOptions
  { mboPlatform :: Text
  , mboConversationLimit :: Int
  , mboMessagesPerConversation :: Int
  , mboOnlyUnread :: Bool
  , mboDryRun :: Bool
  } deriving (Show, Eq)

parseMetaBackfillOptions :: Value -> Maybe MetaBackfillOptions
parseMetaBackfillOptions =
  parseMaybe $ withObject "MetaBackfillOptions" $ \o -> do
    mPlatform <- o .:? "platform"
    mConversationLimit <- o .:? "conversationLimit"
    mMessagesPerConversation <- o .:? "messagesPerConversation"
    mOnlyUnread <- o .:? "onlyUnread"
    mDryRun <- o .:? "dryRun"
    let platformRaw = fromMaybe "all" (mPlatform :: Maybe Text)
        platformNorm =
          case T.toCaseFold (T.strip platformRaw) of
            "instagram" -> "instagram"
            "facebook" -> "facebook"
            "all" -> "all"
            _ -> "all"
        clamp minV maxV v = max minV (min maxV v)
        convLimit = clamp 1 200 (fromMaybe 50 (mConversationLimit :: Maybe Int))
        msgLimit = clamp 1 200 (fromMaybe 50 (mMessagesPerConversation :: Maybe Int))
    pure MetaBackfillOptions
      { mboPlatform = platformNorm
      , mboConversationLimit = convLimit
      , mboMessagesPerConversation = msgLimit
      , mboOnlyUnread = fromMaybe True (mOnlyUnread :: Maybe Bool)
      , mboDryRun = fromMaybe False (mDryRun :: Maybe Bool)
      }

jsonTextField :: Text -> Value -> Maybe Text
jsonTextField fieldName payload =
  join (parseMaybe (withObject "jsonTextField" (\o -> o .:? AKey.fromText fieldName)) payload)

jsonBoolField :: Text -> Value -> Maybe Bool
jsonBoolField fieldName payload =
  join (parseMaybe (withObject "jsonBoolField" (\o -> o .:? AKey.fromText fieldName)) payload)

jsonValueField :: Text -> Value -> Maybe Value
jsonValueField fieldName payload =
  join (parseMaybe (withObject "jsonValueField" (\o -> o .:? AKey.fromText fieldName)) payload)

jsonArrayField :: Text -> Value -> [Value]
jsonArrayField fieldName payload =
  fromMaybe [] (parseMaybe (withObject "jsonArrayField" (\o -> o .:? AKey.fromText fieldName .!= [])) payload)

jsonNestedArrayField :: Text -> Text -> Value -> [Value]
jsonNestedArrayField outer inner payload =
  case jsonValueField outer payload of
    Nothing -> []
    Just nested -> jsonArrayField inner nested

jsonIntFromValue :: Value -> Maybe Int
jsonIntFromValue (Number n) = Sci.toBoundedInteger n
jsonIntFromValue (String txt) =
  case reads (T.unpack (T.strip txt)) of
    [(v, "")] -> Just v
    _ -> Nothing
jsonIntFromValue _ = Nothing

jsonIntField :: Text -> Value -> Maybe Int
jsonIntField fieldName payload = jsonValueField fieldName payload >>= jsonIntFromValue

parseMetaMessageTime :: Maybe Text -> Maybe UTCTime
parseMetaMessageTime Nothing = Nothing
parseMetaMessageTime (Just txt) = iso8601ParseM (T.unpack (T.strip txt))

serverErrorToText :: ServerError -> Text
serverErrorToText err =
  let bodyTxt = T.strip (TE.decodeUtf8 (BL.toStrict (errBody err)))
  in if T.null bodyTxt then "Server error" else bodyTxt

isMetaGraphTimeoutError :: ServerError -> Bool
isMetaGraphTimeoutError err =
  let bodyTxt = T.toCaseFold (serverErrorToText err)
  in "request timed out" `T.isInfixOf` bodyTxt
      || ("oauthexception" `T.isInfixOf` bodyTxt && "2534084" `T.isInfixOf` bodyTxt)
      || "reduce the amount of data you're asking for" `T.isInfixOf` bodyTxt

tokenSuffix :: Text -> Text
tokenSuffix tok =
  let trimmed = T.strip tok
      suffixLen = min 8 (T.length trimmed)
  in if suffixLen <= 0 then "empty" else T.takeEnd suffixLen trimmed

buildFacebookGraphRequest
  :: AppConfig
  -> Text
  -> [(Text, Text)]
  -> AppM Request
buildFacebookGraphRequest cfg path params = do
  let base = T.dropWhileEnd (== '/') (facebookGraphBase cfg)
      normalizedPath = if "/" `T.isPrefixOf` path then path else "/" <> path
      query = renderSimpleQuery True (map (\(k, v) -> (TE.encodeUtf8 k, TE.encodeUtf8 v)) params)
      url = T.unpack (base <> normalizedPath <> TE.decodeUtf8 query)
  reqE <- liftIO (try (parseRequest url) :: IO (Either SomeException Request))
  case reqE of
    Left err -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack (displayException err))) }
    Right req -> pure req

requestFacebookGraphValue
  :: Manager
  -> AppConfig
  -> Text
  -> [(Text, Text)]
  -> AppM Value
requestFacebookGraphValue manager cfg path params = do
  req <- buildFacebookGraphRequest cfg path params
  respE <- liftIO (try (httpLbs req manager) :: IO (Either SomeException (Response BL.ByteString)))
  resp <- case respE of
    Left err ->
      throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack (displayException err))) }
    Right ok -> pure ok
  let status = statusCode (responseStatus resp)
  when (status >= 400) $ do
    let bodySnippet = take 2000 (BL8.unpack (responseBody resp))
    throwError err502
      { errBody = BL.fromStrict (TE.encodeUtf8 ("Facebook request failed (" <> T.pack (show status) <> ") " <> T.pack bodySnippet)) }
  case eitherDecode (responseBody resp) of
    Left err ->
      throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 ("Facebook parse error: " <> T.pack err)) }
    Right val -> pure val

fetchGraphOwnId :: Manager -> AppConfig -> Text -> AppM Text
fetchGraphOwnId manager cfg accessToken = do
  payload <- requestFacebookGraphValue manager cfg "/me"
    [ ("fields", "id,name")
    , ("access_token", accessToken)
    ]
  case jsonTextField "id" payload of
    Just ownerId | not (T.null (T.strip ownerId)) -> pure (T.strip ownerId)
    _ -> throwError err502 { errBody = "Facebook /me response missing id" }

fetchGraphConversations
  :: Manager
  -> AppConfig
  -> Text
  -> Text
  -> Maybe Text
  -> MetaBackfillOptions
  -> AppM [Value]
fetchGraphConversations manager cfg path accessToken mPlatform MetaBackfillOptions{..} = do
  let fetchConversationStubs conversationLimit = do
        let baseParams =
              [ ("fields", "id,updated_time,unread_count")
              , ("limit", T.pack (show conversationLimit))
              , ("access_token", accessToken)
              ]
            params = maybe baseParams (\platformVal -> ("platform", platformVal) : baseParams) mPlatform
        payload <- requestFacebookGraphValue manager cfg path params
        pure (jsonArrayField "data" payload)

      fetchConversationMessages conversationId = do
        let trimmedId = T.strip conversationId
            msgParams =
              [ ("fields", "id,from{id,name,username},message,text,created_time,is_echo,attachments{mime_type,name,image_data,video_data,file_url}")
              , ("limit", T.pack (show mboMessagesPerConversation))
              , ("access_token", accessToken)
              ]
        payload <- requestFacebookGraphValue manager cfg ("/" <> trimmedId <> "/messages") msgParams
        pure (jsonArrayField "data" payload)

      withMessages :: Value -> [Value] -> Value
      withMessages conversationVal messages =
        object
          [ "id" .= jsonTextField "id" conversationVal
          , "updated_time" .= jsonTextField "updated_time" conversationVal
          , "unread_count" .= jsonIntField "unread_count" conversationVal
          , "messages" .= object [ "data" .= messages ]
          ]

      hydrateConversation conversationVal = do
        let unreadCount = fromMaybe 0 (jsonIntField "unread_count" conversationVal)
            mConversationId = fmap T.strip (jsonTextField "id" conversationVal)
            shouldSkipMessages = mboOnlyUnread && unreadCount <= 0
        case mConversationId of
          Nothing -> pure (withMessages conversationVal [])
          Just conversationId
            | T.null conversationId -> pure (withMessages conversationVal [])
            | shouldSkipMessages -> pure (withMessages conversationVal [])
            | otherwise -> do
                messages <- fetchConversationMessages conversationId
                  `catchError` \err ->
                    if isMetaGraphTimeoutError err
                      then pure []
                      else throwError err
                pure (withMessages conversationVal messages)

      runAttempt conversationLimit = do
        stubs <- fetchConversationStubs conversationLimit
        mapM hydrateConversation stubs

      retryWithLowerLimit conversationLimit = do
        runAttempt conversationLimit
          `catchError` \err ->
            if isMetaGraphTimeoutError err && conversationLimit > 1
              then retryWithLowerLimit (max 1 (conversationLimit `div` 2))
              else throwError err
  retryWithLowerLimit (max 1 mboConversationLimit)

isIncomingMetaMessage :: Text -> Value -> Bool
isIncomingMetaMessage ownId msgVal =
  let mFrom = jsonValueField "from" msgVal
      senderId = mFrom >>= jsonTextField "id"
      isEcho = fromMaybe False (jsonBoolField "is_echo" msgVal)
  in case senderId of
       Nothing -> False
       Just sid ->
         let normalizedSender = T.toCaseFold (T.strip sid)
             normalizedOwn = T.toCaseFold (T.strip ownId)
         in not isEcho && normalizedSender /= normalizedOwn

selectConversationMessages :: MetaBackfillOptions -> Value -> [Value]
selectConversationMessages MetaBackfillOptions{..} conversationVal =
  let allMessages = jsonNestedArrayField "messages" "data" conversationVal
      unreadCount = jsonIntField "unread_count" conversationVal
      sortedByNewest =
        sortOn
          (Down . fromMaybe (UTCTime (fromGregorian 1970 1 1) 0) . parseMetaMessageTime . jsonTextField "created_time")
          allMessages
      toTake =
        if mboOnlyUnread
          then case unreadCount of
            Just n | n > 0 -> n
            Just _ -> 0
            Nothing -> length sortedByNewest
          else length sortedByNewest
  in take (max 0 toTake) sortedByNewest

storeBackfilledMessage
  :: MetaChannel
  -> MetaBackfillOptions
  -> Text
  -> Value
  -> AppM Bool
storeBackfilledMessage channel MetaBackfillOptions{..} conversationId msgVal = do
  now <- liftIO getCurrentTime
  let externalId = jsonTextField "id" msgVal
      mFrom = jsonValueField "from" msgVal
      mAttachments = jsonValueField "attachments" msgVal
      senderId = mFrom >>= jsonTextField "id"
      senderName = (mFrom >>= jsonTextField "name") <|> (mFrom >>= jsonTextField "username")
      bodyRaw = jsonTextField "message" msgVal <|> jsonTextField "text" msgVal
      body =
        case bodyRaw of
          Just txt | not (T.null (T.strip txt)) -> T.strip txt
          _ -> "[attachment]"
      createdAt = fromMaybe now (parseMetaMessageTime (jsonTextField "created_time" msgVal))
      metaPairs =
        [ "backfilled" .= True
        , "conversationId" .= conversationId
        , "source" .= ("meta-graph" :: Text)
        ]
        <> maybe [] (\fromVal -> ["from" .= fromVal]) mFrom
        <> maybe [] (\attVal -> ["attachments" .= attVal]) mAttachments
      metadata = TE.decodeUtf8 (BL.toStrict (encode (object metaPairs)))
  case (externalId, senderId) of
    (Just extId, Just sid) | not (T.null (T.strip extId)) && not (T.null (T.strip sid)) -> do
      if mboDryRun
        then pure True
        else do
          case channel of
            MetaInstagram ->
              runDB $ do
                _ <- upsert (InstagramMessage extId
                              sid
                              senderName
                              (Just body)
                              "incoming"
                              Nothing
                              Nothing
                              Nothing
                              Nothing
                              (Just metadata)
                              "pending"
                              Nothing
                              Nothing
                              Nothing
                              0
                              Nothing
                              Nothing
                              Nothing
                              createdAt)
                     [ InstagramMessageSenderName =. senderName
                     , InstagramMessageText =. Just body
                     , InstagramMessageDirection =. "incoming"
                     , InstagramMessageMetadata =. Just metadata
                     , InstagramMessageReplyStatus =. "pending"
                     ]
                pure ()
            MetaFacebook ->
              runDB $ do
                _ <- upsert (ME.FacebookMessage extId
                              sid
                              senderName
                              (Just body)
                              "incoming"
                              Nothing
                              Nothing
                              Nothing
                              Nothing
                              (Just metadata)
                              "pending"
                              Nothing
                              Nothing
                              Nothing
                              0
                              Nothing
                              Nothing
                              Nothing
                              createdAt)
                     [ ME.FacebookMessageSenderName =. senderName
                     , ME.FacebookMessageText =. Just body
                     , ME.FacebookMessageDirection =. "incoming"
                     , ME.FacebookMessageMetadata =. Just metadata
                     , ME.FacebookMessageReplyStatus =. "pending"
                     ]
                pure ()
          pure True
    _ -> pure False

backfillFacebookToken
  :: Manager
  -> AppConfig
  -> MetaBackfillOptions
  -> Text
  -> AppM Value
backfillFacebookToken manager cfg opts accessToken = do
  ownerId <- fetchGraphOwnId manager cfg accessToken
  conversations <- fetchGraphConversations manager cfg "/me/conversations" accessToken Nothing opts
  (incomingScanned, importedCount) <- foldM
    (\(scannedAcc, importedAcc) conversationVal -> do
      let conversationId = fromMaybe "unknown" (jsonTextField "id" conversationVal)
          incomingMessages = filter (isIncomingMetaMessage ownerId) (selectConversationMessages opts conversationVal)
      stored <- mapM (storeBackfilledMessage MetaFacebook opts conversationId) incomingMessages
      let imported = length (filter (\ok -> ok) stored)
      pure (scannedAcc + length incomingMessages, importedAcc + imported)
    )
    (0, 0)
    conversations
  pure (object
    [ "platform" .= ("facebook" :: Text)
    , "tokenSuffix" .= tokenSuffix accessToken
    , "ownerId" .= ownerId
    , "conversations" .= length conversations
    , "incomingScanned" .= incomingScanned
    , "imported" .= importedCount
    ])

backfillInstagramAccount
  :: Manager
  -> AppConfig
  -> MetaBackfillOptions
  -> (Text, Text, Maybe Text)
  -> AppM Value
backfillInstagramAccount manager cfg opts (igUserId, accessToken, mHandle) = do
  let trimmedIgUserId = T.strip igUserId
      fetchByMe = fetchGraphConversations manager cfg "/me/conversations" accessToken (Just "instagram") opts
  conversations <-
    if T.null trimmedIgUserId
      then fetchByMe
      else
        fetchGraphConversations manager cfg ("/" <> trimmedIgUserId <> "/conversations") accessToken (Just "instagram") opts
          `catchError` \_ -> fetchByMe
  (incomingScanned, importedCount) <- foldM
    (\(scannedAcc, importedAcc) conversationVal -> do
      let conversationId = fromMaybe "unknown" (jsonTextField "id" conversationVal)
          incomingMessages = filter (isIncomingMetaMessage igUserId) (selectConversationMessages opts conversationVal)
      stored <- mapM (storeBackfilledMessage MetaInstagram opts conversationId) incomingMessages
      let imported = length (filter (\ok -> ok) stored)
      pure (scannedAcc + length incomingMessages, importedAcc + imported)
    )
    (0, 0)
    conversations
  pure (object
    [ "platform" .= ("instagram" :: Text)
    , "igUserId" .= igUserId
    , "handle" .= mHandle
    , "tokenSuffix" .= tokenSuffix accessToken
    , "conversations" .= length conversations
    , "incomingScanned" .= incomingScanned
    , "imported" .= importedCount
    ])

metaBackfillServer :: AuthedUser -> Value -> AppM Value
metaBackfillServer user payload = do
  unless (hasRole Admin user) $ throwError err403
  opts <- maybe (throwBadRequest "Invalid meta backfill payload") pure (parseMetaBackfillOptions payload)
  Env{envConfig} <- ask
  manager <- liftIO $ newManager tlsManagerSettings
  rows <- runDB $
    selectList
      [ SocialSyncAccountPlatform ==. "instagram"
      , SocialSyncAccountAccessToken !=. Nothing
      , SocialSyncAccountStatus ==. "connected"
      ]
      [Desc SocialSyncAccountUpdatedAt]
  let sources =
        [ (socialSyncAccountExternalUserId acc, tok, socialSyncAccountHandle acc)
        | Entity _ acc <- rows
        , let extId = T.strip (socialSyncAccountExternalUserId acc)
        , not (T.null extId)
        , Just rawTok <- [socialSyncAccountAccessToken acc]
        , let tok = T.strip rawTok
        , not (T.null tok)
        ]
      configuredSource =
        case fmap T.strip (instagramMessagingToken envConfig) of
          Just tok | not (T.null tok) ->
            let mConfiguredIgId = fmap T.strip (instagramMessagingAccountId envConfig)
                configuredIgId = fromMaybe "" mConfiguredIgId
            in Just (configuredIgId, tok, mConfiguredIgId)
          _ -> Nothing
      configuredFacebookToken =
        case fmap T.strip (facebookMessagingToken envConfig) of
          Just tok | not (T.null tok) -> Just tok
          _ -> Nothing
      instagramSources = nub (sources ++ maybeToList configuredSource)
      facebookTokens = nub ([ tok | (_, tok, _) <- instagramSources ] ++ maybeToList configuredFacebookToken)
      runInstagram = mboPlatform opts == "all" || mboPlatform opts == "instagram"
      runFacebook = mboPlatform opts == "all" || mboPlatform opts == "facebook"
  facebookResults <-
    if runFacebook
      then forM facebookTokens $ \tok ->
        (Right <$> backfillFacebookToken manager envConfig opts tok)
          `catchError` \err ->
            pure (Left (object
              [ "platform" .= ("facebook" :: Text)
              , "tokenSuffix" .= tokenSuffix tok
              , "error" .= serverErrorToText err
              ]))
      else pure []
  instagramResults <-
    if runInstagram
      then forM instagramSources $ \src@(_igUserId, tok, _handle) ->
        (Right <$> backfillInstagramAccount manager envConfig opts src)
          `catchError` \err ->
            pure (Left (object
              [ "platform" .= ("instagram" :: Text)
              , "tokenSuffix" .= tokenSuffix tok
              , "error" .= serverErrorToText err
              ]))
      else pure []
  let successes = [val | Right val <- facebookResults ++ instagramResults]
      failures = [val | Left val <- facebookResults ++ instagramResults]
      importedTotal = sum (map (\v -> fromMaybe 0 (jsonIntField "imported" v)) successes)
      scannedTotal = sum (map (\v -> fromMaybe 0 (jsonIntField "incomingScanned" v)) successes)
  pure (object
    [ "status" .= ("ok" :: Text)
    , "options" .= object
        [ "platform" .= mboPlatform opts
        , "conversationLimit" .= mboConversationLimit opts
        , "messagesPerConversation" .= mboMessagesPerConversation opts
        , "onlyUnread" .= mboOnlyUnread opts
        , "dryRun" .= mboDryRun opts
        ]
    , "sources" .= object
        [ "instagramAccounts" .= length instagramSources
        , "facebookTokens" .= length facebookTokens
        , "configuredMessagingToken" .= isJust configuredSource
        , "configuredFacebookToken" .= isJust configuredFacebookToken
        ]
    , "totals" .= object
        [ "imported" .= importedTotal
        , "incomingScanned" .= scannedTotal
        , "successes" .= length successes
        , "failures" .= length failures
        ]
    , "results" .= successes
    , "errors" .= failures
    ])

fanSecureServer :: AuthedUser -> ServerT FanSecureAPI AppM
fanSecureServer user =
       (fanGetProfile user :<|> fanUpdateProfile user)
  :<|> (fanListFollows user :<|> fanFollowArtist user :<|> fanUnfollowArtist user)
  :<|> (artistGetOwnProfile user :<|> artistUpdateOwnProfile user)

socialServer :: AuthedUser -> ServerT SocialAPI AppM
socialServer user =
       socialListFollowers user
  :<|> socialListFollowing user
  :<|> vcardExchange user
  :<|> socialListFriends user
  :<|> socialAddFriend user
  :<|> socialRemoveFriend user
  :<|> socialListSuggestedFriends user

chatServer :: AuthedUser -> ServerT ChatAPI AppM
chatServer user =
       chatListThreads user
  :<|> chatGetOrCreateDM user
  :<|> chatListMessages user
  :<|> chatSendMessage user

driveServer :: AuthedUser -> ServerT DriveAPI AppM
driveServer user =
  driveUploadServer user
  :<|> driveTokenExchangeServer user
  :<|> driveTokenRefreshServer user

driveUploadServer :: AuthedUser -> Maybe Text -> DriveUploadForm -> AppM DriveUploadDTO
driveUploadServer _ mAccessToken DriveUploadForm{..} = do
  manager <- liftIO $ newManager tlsManagerSettings
  accessToken <- resolveDriveAccessToken manager (fmap T.strip mAccessToken <|> duAccessToken)
  mFolderEnv <- liftIO $ lookupEnvTextNonEmpty "DRIVE_UPLOAD_FOLDER_ID"
  let folder = duFolderId <|> mFolderEnv
      fallbackName =
        let raw = T.strip (fdFileName duFile)
        in if T.null raw then Nothing else Just raw
      nameOverride = duName <|> fallbackName
  dtoOrErr <-
    liftIO
      (try (uploadToDrive manager accessToken duFile nameOverride folder) ::
        IO (Either SomeException DriveUploadDTO))
  case dtoOrErr of
    Right dto -> pure dto
    Left err ->
      throwError err502
        { errBody =
            BL8.pack ("Google Drive upload falló: " <> displayException err)
        }
  where
    resolveDriveAccessToken :: Manager -> Maybe Text -> AppM Text
    resolveDriveAccessToken manager mProvided =
      case mProvided of
        Just tok | not (T.null (T.strip tok)) -> pure (T.strip tok)
        _ -> do
          mRefreshToken <- liftIO $ lookupEnvTextNonEmpty "DRIVE_REFRESH_TOKEN"
          mAccessTokenEnv <- liftIO $ lookupEnvTextNonEmpty "DRIVE_ACCESS_TOKEN"
          case mRefreshToken of
            Just rt -> do
              (cid, secret) <- loadDriveClientCreds
              refreshDriveAccessToken manager cid secret rt `catchError` \err ->
                handleRefreshError err mAccessTokenEnv
            Nothing -> do
              maybe
                (throwError err503
                  { errBody =
                      "Google Drive no configurado (faltan DRIVE_REFRESH_TOKEN o " <>
                      "DRIVE_ACCESS_TOKEN)."
                  })
                pure
                mAccessTokenEnv

    handleRefreshError :: ServerError -> Maybe Text -> AppM Text
    handleRefreshError err mAccessTokenEnv
      | isInvalidGrant err =
          case mAccessTokenEnv of
            Just tok -> pure tok
            Nothing ->
              throwError err401
                { errBody = "Refresh token expirado o revocado. Reautoriza Google Drive." }
      | otherwise = throwError err

    isInvalidGrant :: ServerError -> Bool
    isInvalidGrant err =
      let body = map toLower (BL8.unpack (errBody err))
      in "invalid_grant" `isInfixOf` body

driveTokenExchangeServer :: AuthedUser -> DriveTokenExchangeRequest -> AppM DriveTokenResponse
driveTokenExchangeServer _ DriveTokenExchangeRequest{..} = do
  Env{envConfig} <- ask
  manager <- liftIO $ newManager tlsManagerSettings
  (cid, secret) <- loadDriveClientCreds
  let redirectResolved = resolveDriveRedirectUri envConfig redirectUri
  token <- requestGoogleToken manager
    [ ("client_id", TE.encodeUtf8 cid)
    , ("client_secret", TE.encodeUtf8 secret)
    , ("code", TE.encodeUtf8 code)
    , ("code_verifier", TE.encodeUtf8 codeVerifier)
    , ("redirect_uri", TE.encodeUtf8 redirectResolved)
    , ("grant_type", "authorization_code")
    ]
  pure (driveTokenResponseFrom token Nothing)

driveTokenRefreshServer :: AuthedUser -> DriveTokenRefreshRequest -> AppM DriveTokenResponse
driveTokenRefreshServer _ DriveTokenRefreshRequest{..} = do
  manager <- liftIO $ newManager tlsManagerSettings
  (cid, secret) <- loadDriveClientCreds
  token <- requestGoogleToken manager
    [ ("client_id", TE.encodeUtf8 cid)
    , ("client_secret", TE.encodeUtf8 secret)
    , ("refresh_token", TE.encodeUtf8 refreshToken)
    , ("grant_type", "refresh_token")
    ]
  pure (driveTokenResponseFrom token (Just refreshToken))

resolveDriveRedirectUri :: AppConfig -> Maybe Text -> Text
resolveDriveRedirectUri cfg mProvided =
  fromMaybe (resolveConfiguredAppBase cfg <> "/oauth/google-drive/callback") (mProvided >>= nonEmptyTextLocal)
  where
    nonEmptyTextLocal txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

driveTokenResponseFrom :: GoogleToken -> Maybe Text -> DriveTokenResponse
driveTokenResponseFrom GoogleToken{..} fallbackRefresh =
  DriveTokenResponse
    { accessToken = access_token
    , refreshToken = refresh_token <|> fallbackRefresh
    , expiresIn = fromMaybe 3600 expires_in
    , tokenType = token_type
    }

requestGoogleToken :: Manager -> [(ByteString, ByteString)] -> AppM GoogleToken
requestGoogleToken manager form = do
  req0 <- liftIO $ parseRequest "https://oauth2.googleapis.com/token"
  let payload = renderSimpleQuery False form
      req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (BL.fromStrict payload)
        , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
        }
  respOrErr <-
    liftIO
      (try (httpLbs req manager) ::
        IO (Either SomeException (Response BL.ByteString)))
  resp <- case respOrErr of
    Left err ->
      throwError err502
        { errBody = BL8.pack ("No se pudo contactar Google OAuth: " <> displayException err) }
    Right ok -> pure ok
  let codeStatus = statusCode (responseStatus resp)
  when (codeStatus >= 400) $ do
    let bodySnippet = take 2000 (BL8.unpack (responseBody resp))
        suffix = if null bodySnippet then "" else " " <> bodySnippet
    throwError err502
      { errBody =
          BL8.pack
            ("Solicitud OAuth falló (" <> show codeStatus <> ")." <> suffix)
      }
  token <- case eitherDecode (responseBody resp) of
    Left err ->
      throwError err502 { errBody = BL8.pack ("No se pudo parsear token: " <> err) }
    Right tok -> pure (tok :: GoogleToken)
  pure token

loadDriveClientCreds :: AppM (Text, Text)
loadDriveClientCreds = do
  mCid <- liftIO $ lookupEnvTextNonEmpty "DRIVE_CLIENT_ID"
  mSecret <- liftIO $ lookupEnvTextNonEmpty "DRIVE_CLIENT_SECRET"
  mCidFallback <- liftIO $ lookupEnvTextNonEmpty "GOOGLE_CLIENT_ID"
  mSecretFallback <- liftIO $ lookupEnvTextNonEmpty "GOOGLE_CLIENT_SECRET"
  case (mCid <|> mCidFallback, mSecret <|> mSecretFallback) of
    (Just cid, Just secret) -> pure (cid, secret)
    _ ->
      throwError err503
        { errBody =
            "Google Drive no configurado (faltan DRIVE_CLIENT_ID/DRIVE_CLIENT_SECRET o " <>
            "GOOGLE_CLIENT_ID/GOOGLE_CLIENT_SECRET)."
        }

refreshDriveAccessToken :: Manager -> Text -> Text -> Text -> AppM Text
refreshDriveAccessToken manager cid secret rt = do
  token <- requestGoogleToken manager
    [ ("client_id", TE.encodeUtf8 cid)
    , ("client_secret", TE.encodeUtf8 secret)
    , ("refresh_token", TE.encodeUtf8 rt)
    , ("grant_type", "refresh_token")
    ]
  pure (access_token token)

lookupEnvTextNonEmpty :: String -> IO (Maybe Text)
lookupEnvTextNonEmpty key = do
  mVal <- lookupEnv key
  pure $ case fmap (T.strip . T.pack) mVal of
    Just val | not (T.null val) -> Just val
    _ -> Nothing

countriesServer :: AppM [CountryDTO]
countriesServer = do
  countries <- runDB $ selectList [] [Asc CountryName]
  pure (map toCountryDTO countries)

protectedServer :: AuthedUser -> ServerT ProtectedAPI AppM
protectedServer user =
       partyServer user
  :<|> bookingServer user
  :<|> proposalsServer user
  :<|> serviceCatalogServer user
  :<|> packageServer user
  :<|> invoiceServer user
  :<|> receiptServer user
  :<|> adminServer user
  :<|> userRolesServer user
  :<|> fanSecureServer user
  :<|> inventoryServer user
  :<|> bandsServer user
  :<|> sessionsServer user
  :<|> pipelinesServer user
  :<|> roomsServer user
  :<|> liveSessionsServer user
  :<|> feedbackServer user
  :<|> marketplaceAdminServer user
  :<|> paymentsServer user
  :<|> instagramServer
  :<|> facebookServer
  :<|> instagramOAuthServer user
  :<|> whatsappMessagesServer user
  :<|> whatsappReplyServer user
  :<|> whatsappConsentServer user
  :<|> socialServer user
  :<|> chatServer user
  :<|> chatkitSessionServer user
  :<|> tidalAgentServer user
  :<|> socialSyncServer user
  :<|> metaBackfillServer user
  :<|> socialEventsServer user
  :<|> internshipsServer user
  :<|> adsAdminServer user
  :<|> coursesAdminServer user
  :<|> labelServer user
  :<|> calendarServer user
  :<|> cmsAdminServer user
  :<|> driveServer user
  :<|> radioServer user
  :<|> countriesServer
  :<|> futureServer

calendarServer :: AuthedUser -> ServerT CalAPI.CalendarAPI AppM
calendarServer user =
       calendarAuthUrlH
  :<|> calendarTokenExchangeH
  :<|> calendarConfigH
  :<|> calendarSyncH
  :<|> calendarEventsH
  where
    requireAdmin = unless (hasRole Admin user) $ throwError err403

    loadGoogleEnv :: Maybe Text -> AppM (Text, Text, Text)
    loadGoogleEnv mRedirect = do
      mCid <- liftIO $ lookupEnv "GOOGLE_CLIENT_ID"
      mSecret <- liftIO $ lookupEnv "GOOGLE_CLIENT_SECRET"
      mRedirectEnv <- liftIO $ lookupEnv "GOOGLE_REDIRECT_URI"
      let cid = fmap T.strip (T.pack <$> mCid)
          secret = fmap T.strip (T.pack <$> mSecret)
          redirect = case mRedirect of
            Just txt | not (T.null (T.strip txt)) -> Just (T.strip txt)
            _ -> fmap T.strip (T.pack <$> mRedirectEnv)
      case (cid, secret, redirect) of
        (Just cid', Just sec', Just redir') -> pure (cid', sec', redir')
        _ ->
          throwError err503
            { errBody = "Google Calendar no configurado (faltan GOOGLE_CLIENT_ID / GOOGLE_CLIENT_SECRET / GOOGLE_REDIRECT_URI)." }

    calendarAuthUrlH :: AppM CalAPI.AuthUrlResponse
    calendarAuthUrlH = do
      requireAdmin
      (cid, _sec, redirect) <- loadGoogleEnv Nothing
      let scope = urlEncode True "https://www.googleapis.com/auth/calendar.readonly"
          url = T.concat
            [ "https://accounts.google.com/o/oauth2/v2/auth?response_type=code"
            , "&client_id=", cid
            , "&redirect_uri=", redirect
            , "&scope=", TE.decodeUtf8 scope
            , "&access_type=offline&prompt=consent"
            ]
      pure (CalAPI.AuthUrlResponse url)

    calendarTokenExchangeH :: CalAPI.TokenExchangeIn -> AppM CalAPI.CalendarConfigDTO
    calendarTokenExchangeH CalAPI.TokenExchangeIn{..} = do
      requireAdmin
      (cid, sec, redirect) <- loadGoogleEnv redirectUri
      manager <- liftIO $ newManager tlsManagerSettings
      req0 <- liftIO $ parseRequest "https://oauth2.googleapis.com/token"
      let form =
            renderSimpleQuery False
              [ ("code", TE.encodeUtf8 code)
              , ("client_id", TE.encodeUtf8 cid)
              , ("client_secret", TE.encodeUtf8 sec)
              , ("redirect_uri", TE.encodeUtf8 redirect)
              , ("grant_type", "authorization_code")
              ]
          req = req0
            { method = "POST"
            , requestBody = RequestBodyLBS (BL.fromStrict form)
            , requestHeaders =
                [ ("Content-Type", "application/x-www-form-urlencoded")
                ]
            }
      resp <- liftIO $ httpLbs req manager
      let codeStatus = statusCode (responseStatus resp)
      when (codeStatus >= 400) $
        throwError err502 { errBody = "Intercambio de token falló con Google." }
      token <- case eitherDecode (responseBody resp) of
        Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear token: " <> err) }
        Right tok -> pure tok
      now <- liftIO getCurrentTime
      let GoogleToken
            { access_token = gAccessToken
            , refresh_token = gRefreshToken
            , expires_in = gExpiresIn
            , token_type = gTokenType
            } = token
          expiresAt = addUTCTime . fromIntegral <$> gExpiresIn <*> pure now
      mExisting <- runDB $ getBy (Cal.UniqueCalendar calendarId)
      cfgId <- case mExisting of
        Nothing -> runDB $ insert Cal.GoogleCalendarConfig
          { Cal.googleCalendarConfigOwnerId = Just (auPartyId user)
          , Cal.googleCalendarConfigCalendarId = calendarId
          , Cal.googleCalendarConfigAccessToken = Just gAccessToken
          , Cal.googleCalendarConfigRefreshToken = gRefreshToken
          , Cal.googleCalendarConfigTokenType = gTokenType
          , Cal.googleCalendarConfigTokenExpiresAt = expiresAt
          , Cal.googleCalendarConfigSyncCursor = Nothing
          , Cal.googleCalendarConfigSyncedAt = Nothing
          , Cal.googleCalendarConfigCreatedAt = now
          , Cal.googleCalendarConfigUpdatedAt = now
          }
        Just (Entity eid oldCfg) -> do
          runDB $ update eid
            [ Cal.GoogleCalendarConfigAccessToken =. Just gAccessToken
            , Cal.GoogleCalendarConfigRefreshToken =. (gRefreshToken <|> Cal.googleCalendarConfigRefreshToken oldCfg)
            , Cal.GoogleCalendarConfigTokenType =. gTokenType
            , Cal.GoogleCalendarConfigTokenExpiresAt =. expiresAt
            , Cal.GoogleCalendarConfigUpdatedAt =. now
            ]
          pure eid
      pure CalAPI.CalendarConfigDTO
        { configId = fromIntegral (fromSqlKey cfgId)
        , calendarId = calendarId
        , syncCursor = Nothing
        , syncedAt = Nothing
        }

    calendarConfigH :: Maybe Text -> AppM (Maybe CalAPI.CalendarConfigDTO)
    calendarConfigH mCalendarId = do
      requireAdmin
      let mTrimmed = fmap T.strip mCalendarId
      mCfg <- case mTrimmed of
        Just cid | not (T.null cid) -> runDB $ getBy (Cal.UniqueCalendar cid)
        _ -> listToMaybe <$> runDB (selectList [] [Desc Cal.GoogleCalendarConfigUpdatedAt, LimitTo 1])
      pure (toCfgDTO <$> mCfg)

    calendarSyncH :: CalAPI.SyncRequest -> AppM CalAPI.SyncResult
    calendarSyncH CalAPI.SyncRequest{..} = do
      requireAdmin
      (cidEnv, secEnv, redirectEnv) <- loadGoogleEnv Nothing
      mCfg <- runDB $ getBy (Cal.UniqueCalendar calendarId)
      case mCfg of
        Nothing -> throwError err404
        Just (Entity cfgId cfg) -> do
          now <- liftIO getCurrentTime
          cfgFresh <- ensureAccessToken cidEnv secEnv redirectEnv cfg now
          (createdCount, updatedCount, cancelledCount, newCursor) <-
            syncFromGoogle cfgFresh from to now
          runDB $ update cfgId
            [ Cal.GoogleCalendarConfigSyncedAt =. Just now
            , Cal.GoogleCalendarConfigSyncCursor =. newCursor
            ]
          pure CalAPI.SyncResult
            { created = createdCount
            , updated = updatedCount
            , deleted = cancelledCount
            , cursor = newCursor
            }

    calendarEventsH :: Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> AppM [CalAPI.CalendarEventDTO]
    calendarEventsH mCal fromTs toTs mStatus = do
      requireAdmin
      let baseFilters = maybe [] (\cid -> [Cal.GoogleCalendarEventCalendarId ==. cid]) mCal
          dateFilters =
            maybe [] (\fromVal -> [Cal.GoogleCalendarEventStartAt >=. Just fromVal]) fromTs
            ++ maybe [] (\toVal -> [Cal.GoogleCalendarEventStartAt <=. Just toVal]) toTs
          statusFilters = maybe [] (\st -> [Cal.GoogleCalendarEventStatus ==. st]) mStatus
          filters = baseFilters ++ dateFilters ++ statusFilters
      rows <- runDB $ selectList filters [Desc Cal.GoogleCalendarEventStartAt, Desc Cal.GoogleCalendarEventUpdatedLocal]
      pure (map toEventDTO rows)

    toCfgDTO :: Entity Cal.GoogleCalendarConfig -> CalAPI.CalendarConfigDTO
    toCfgDTO (Entity cfgId cfg) =
      CalAPI.CalendarConfigDTO
        { configId = fromIntegral (fromSqlKey cfgId)
        , calendarId = Cal.googleCalendarConfigCalendarId cfg
        , syncCursor = Cal.googleCalendarConfigSyncCursor cfg
        , syncedAt = Cal.googleCalendarConfigSyncedAt cfg
        }

    toEventDTO :: Entity Cal.GoogleCalendarEvent -> CalAPI.CalendarEventDTO
    toEventDTO (Entity eid e) =
      CalAPI.CalendarEventDTO
        { eventId = entityKeyInt eid
        , googleId = Cal.googleCalendarEventGoogleId e
        , calendarId = Cal.googleCalendarEventCalendarId e
        , status = Cal.googleCalendarEventStatus e
        , summary = Cal.googleCalendarEventSummary e
        , description = Cal.googleCalendarEventDescription e
        , location = Cal.googleCalendarEventLocation e
        , startAt = Cal.googleCalendarEventStartAt e
        , endAt = Cal.googleCalendarEventEndAt e
        , updatedAt = Cal.googleCalendarEventUpdatedAt e
        , htmlLink = Cal.googleCalendarEventHtmlLink e
        , rawPayload = CMS.unAesonValue <$> Cal.googleCalendarEventRawPayload e
        }

    ensureAccessToken :: Text -> Text -> Text -> Cal.GoogleCalendarConfig -> UTCTime -> AppM Cal.GoogleCalendarConfig
    ensureAccessToken cid sec redirect cfg now =
      case Cal.googleCalendarConfigTokenExpiresAt cfg of
        Just expAt | expAt > addUTCTime 60 now -> pure cfg
        _ -> do
          case Cal.googleCalendarConfigRefreshToken cfg of
            Nothing -> throwError err401 { errBody = "No hay refresh_token para Google Calendar." }
            Just rt -> refreshAccessToken cid sec redirect cfg rt now

    refreshAccessToken :: Text -> Text -> Text -> Cal.GoogleCalendarConfig -> Text -> UTCTime -> AppM Cal.GoogleCalendarConfig
    refreshAccessToken cid sec redirect cfg rt now = do
      manager <- liftIO $ newManager tlsManagerSettings
      req0 <- liftIO $ parseRequest "https://oauth2.googleapis.com/token"
      let form =
            renderSimpleQuery False
              [ ("client_id", TE.encodeUtf8 cid)
              , ("client_secret", TE.encodeUtf8 sec)
              , ("refresh_token", TE.encodeUtf8 rt)
              , ("grant_type", "refresh_token")
              , ("redirect_uri", TE.encodeUtf8 redirect)
              ]
          req = req0
            { method = "POST"
            , requestBody = RequestBodyLBS (BL.fromStrict form)
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            }
      resp <- liftIO $ httpLbs req manager
      let codeStatus = statusCode (responseStatus resp)
      when (codeStatus >= 400) $
        throwError err502 { errBody = "Refresh token falló con Google." }
      token <- case eitherDecode (responseBody resp) of
        Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear refresh token: " <> err) }
        Right tok -> pure tok
      let GoogleToken
            { access_token = gAccessToken
            , expires_in = gExpiresIn
            , token_type = gTokenType
            } = token
          expiresAt = addUTCTime . fromIntegral <$> gExpiresIn <*> pure now
          updatedCfg = cfg
            { Cal.googleCalendarConfigAccessToken = Just gAccessToken
            , Cal.googleCalendarConfigTokenType = gTokenType
            , Cal.googleCalendarConfigTokenExpiresAt = expiresAt
            , Cal.googleCalendarConfigUpdatedAt = now
            }
      _ <- runDB $ updateWhere
        [Cal.GoogleCalendarConfigCalendarId ==. Cal.googleCalendarConfigCalendarId cfg]
        [ Cal.GoogleCalendarConfigAccessToken =. Cal.googleCalendarConfigAccessToken updatedCfg
        , Cal.GoogleCalendarConfigTokenType =. Cal.googleCalendarConfigTokenType updatedCfg
        , Cal.GoogleCalendarConfigTokenExpiresAt =. Cal.googleCalendarConfigTokenExpiresAt updatedCfg
        , Cal.GoogleCalendarConfigUpdatedAt =. now
        ]
      pure updatedCfg

    syncFromGoogle :: Cal.GoogleCalendarConfig -> Maybe UTCTime -> Maybe UTCTime -> UTCTime -> AppM (Int, Int, Int, Maybe Text)
    syncFromGoogle cfg fromTs toTs now = do
      manager <- liftIO $ newManager tlsManagerSettings
      let token = Cal.googleCalendarConfigAccessToken cfg
      case token of
        Nothing -> throwError err401 { errBody = "No hay access_token para Google Calendar." }
        Just tok -> do
          (events, cursor) <- fetchAllPages manager tok (Cal.googleCalendarConfigCalendarId cfg) (Cal.googleCalendarConfigSyncCursor cfg) fromTs toTs Nothing []
          results <- mapM (upsertEvent now (Cal.googleCalendarConfigCalendarId cfg)) events
          let createdCount = length (filter (== "created") results)
              updatedCount = length (filter (== "updated") results)
              deletedCount = length (filter (== "cancelled") results)
          pure (createdCount, updatedCount, deletedCount, cursor)

    fetchAllPages
      :: Manager
      -> Text
      -> Text
      -> Maybe Text
      -> Maybe UTCTime
      -> Maybe UTCTime
      -> Maybe Text
      -> [Value]
      -> AppM ([Value], Maybe Text)
    fetchAllPages manager token calendarId mSync mFrom mTo mPage acc = do
      let baseUrl = T.unpack $ "https://www.googleapis.com/calendar/v3/calendars/" <> calendarId <> "/events"
          timeParams =
            case mSync of
              Just _ -> []
              Nothing ->
                [ ("singleEvents", Just "true")
                , ("showDeleted", Just "true")
                , ("maxResults", Just "2000")
                ] ++ maybe [] (\fromVal -> [("timeMin", Just (formatTime' fromVal))]) mFrom
                  ++ maybe [] (\toVal -> [("timeMax", Just (formatTime' toVal))]) mTo
          syncParams = maybe [] (\st -> [("syncToken", Just (TE.encodeUtf8 st))]) mSync
          pageParams = maybe [] (\pt -> [("pageToken", Just (TE.encodeUtf8 pt))]) mPage
          params = timeParams ++ syncParams ++ pageParams
          query = renderQuery True params
      req0 <- liftIO $ parseRequest (baseUrl ++ BL8.unpack (BL.fromStrict query))
      let req = req0 { requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 token)] }
      resp <- liftIO $ httpLbs req manager
      let codeStatus = statusCode (responseStatus resp)
      if codeStatus == 410
        then fetchAllPages manager token calendarId Nothing mFrom mTo Nothing acc
        else do
          when (codeStatus >= 400) $
            throwError err502 { errBody = "Google Calendar devolvió error al sincronizar." }
          page <- case eitherDecode (responseBody resp) of
            Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear eventos: " <> err) }
            Right val -> pure val
          let GoogleEventsPage items nextPage nextSync = page
              acc' = acc <> items
          case nextPage of
            Just p -> fetchAllPages manager token calendarId mSync mFrom mTo (Just p) acc'
            Nothing -> pure (acc', nextSync)

    upsertEvent :: UTCTime -> Text -> Value -> AppM Text
    upsertEvent now calendarId eventVal =
      case parseEvent calendarId eventVal of
        Nothing -> pure "skipped"
        Just ev -> do
          let keyFilter =
                [ Cal.GoogleCalendarEventCalendarId ==. calendarId
                , Cal.GoogleCalendarEventGoogleId ==. peGoogleId ev
                ]
          existing <- runDB $ selectFirst keyFilter []
          case existing of
            Nothing -> do
              _ <- runDB $ insert Cal.GoogleCalendarEvent
                { Cal.googleCalendarEventCalendarId = calendarId
                , Cal.googleCalendarEventGoogleId = peGoogleId ev
                , Cal.googleCalendarEventStatus = peStatus ev
                , Cal.googleCalendarEventSummary = peSummary ev
                , Cal.googleCalendarEventDescription = peDescription ev
                , Cal.googleCalendarEventLocation = peLocation ev
                , Cal.googleCalendarEventStartAt = peStartAt ev
                , Cal.googleCalendarEventEndAt = peEndAt ev
                , Cal.googleCalendarEventUpdatedAt = peUpdatedAt ev
                , Cal.googleCalendarEventHtmlLink = peHtmlLink ev
                , Cal.googleCalendarEventAttendees = peAttendees ev
                , Cal.googleCalendarEventRawPayload = peRawPayload ev
                , Cal.googleCalendarEventCreatedAt = now
                , Cal.googleCalendarEventUpdatedLocal = now
                }
              pure (if peStatus ev == "cancelled" then "cancelled" else "created")
            Just (Entity eid _) -> do
              runDB $ update eid
                [ Cal.GoogleCalendarEventStatus =. peStatus ev
                , Cal.GoogleCalendarEventSummary =. peSummary ev
                , Cal.GoogleCalendarEventDescription =. peDescription ev
                , Cal.GoogleCalendarEventLocation =. peLocation ev
                , Cal.GoogleCalendarEventStartAt =. peStartAt ev
                , Cal.GoogleCalendarEventEndAt =. peEndAt ev
                , Cal.GoogleCalendarEventUpdatedAt =. peUpdatedAt ev
                , Cal.GoogleCalendarEventHtmlLink =. peHtmlLink ev
                , Cal.GoogleCalendarEventAttendees =. peAttendees ev
                , Cal.GoogleCalendarEventRawPayload =. peRawPayload ev
                , Cal.GoogleCalendarEventUpdatedLocal =. now
                ]
              pure (if peStatus ev == "cancelled" then "cancelled" else "updated")

    parseEvent :: Text -> Value -> Maybe ParsedEvent
    parseEvent _ val =
      parseMaybe (withObject "GoogleEvent" $ \o -> do
        gid <- o .: "id"
        status <- o .:? "status" .!= "confirmed"
        summary <- o .:? "summary"
        description <- o .:? "description"
        location <- o .:? "location"
        htmlLink <- o .:? "htmlLink"
        updatedRaw <- o .:? "updated"
        attendeesVal <- o .:? "attendees"
        startObj <- o .:? "start"
        endObj <- o .:? "end"
        let startAt = startObj >>= parseDateFields
            endAt = endObj >>= parseDateFields
            updatedAt = updatedRaw >>= parseIso
        pure ParsedEvent
          { peGoogleId = gid
          , peStatus = status
          , peSummary = summary
          , peDescription = description
          , peLocation = location
          , peStartAt = startAt
          , peEndAt = endAt
          , peUpdatedAt = updatedAt
          , peHtmlLink = htmlLink
          , peAttendees = CMS.AesonValue <$> attendeesVal
          , peRawPayload = Just (CMS.AesonValue val)
          }) val

    parseDateFields :: Value -> Maybe UTCTime
    parseDateFields =
      parseMaybe (withObject "DateTime" $ \o -> do
        dateTime <- o .:? "dateTime"
        dateOnly <- o .:? "date"
        case (dateTime :: Maybe Text, dateOnly :: Maybe Text) of
          (Just dt, _) -> pure dt
          (Nothing, Just d) -> pure (d <> "T00:00:00Z")
          _ -> fail "no date"
      ) >=> parseIso

    parseIso :: Text -> Maybe UTCTime
    parseIso txt = iso8601ParseM (T.unpack txt)

    formatTime' :: UTCTime -> ByteString
    formatTime' t = TE.encodeUtf8 (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t))

productionCourseSlug :: AppConfig -> Text
productionCourseSlug = courseSlugFallback
productionCoursePrice :: Double
productionCoursePrice = 150
productionCourseCapacity :: Int
productionCourseCapacity = 16

-- Backward-compatible helpers used by cron jobs
buildLandingUrl :: AppConfig -> Text
buildLandingUrl cfg = buildLandingUrlFor cfg (productionCourseSlug cfg)

courseMetadataFor :: AppConfig -> Maybe Text -> Text -> Maybe CourseMetadata
courseMetadataFor cfg mWaContact slugVal =
  let fallbackSlug = productionCourseSlug cfg
  in if normalizeSlug slugVal /= fallbackSlug
    then Nothing
    else
      let whatsappUrl = buildWhatsappCtaFor mWaContact "Curso de Producción Musical" (buildLandingUrl cfg)
          sessions =
            [ CourseSession "Sábado 1 · Introducción" (fromGregorian 2026 4 11)
            , CourseSession "Sábado 2 · Grabación" (fromGregorian 2026 4 18)
            , CourseSession "Sábado 3 · Mezcla" (fromGregorian 2026 4 25)
            , CourseSession "Sábado 4 · Masterización" (fromGregorian 2026 5 2)
            ]
          syllabus =
            [ SyllabusItem "Introducción a la producción musical" ["Conceptos básicos", "Herramientas esenciales"]
            , SyllabusItem "Grabación y captura de audio" ["Técnicas de grabación", "Configuración de micrófonos"]
            , SyllabusItem "Mezcla y edición" ["Ecualización y compresión", "Balance y panoramización"]
            , SyllabusItem "Masterización y publicación" ["Mastering", "Distribución digital"]
            ]
      in Just CourseMetadata
        { slug = fallbackSlug
        , title = "Curso de Producción Musical"
        , subtitle = "Presencial · Cuatro sábados · 16 horas en total · Próximo inicio: segundo sábado de abril"
        , format = "Presencial"
        , duration = "Cuatro sábados (16 horas en total)"
        , price = productionCoursePrice
        , currency = "USD"
        , capacity = productionCourseCapacity
        , remaining = productionCourseCapacity
        , sessionStartHour = 15
        , sessionDurationHours = 4
        , locationLabel = "TDF Records – Quito"
        , locationMapUrl = courseMapFallback cfg
        , daws = ["Logic", "Luna"]
        , includes =
            [ "Acceso a grabaciones"
            , "Certificado de participación"
            , "Mentorías"
            , "Grupo de WhatsApp"
            , "Acceso a la plataforma de TDF Records"
            ]
        , instructorName = Just "Esteban Muñoz"
        , instructorBio = Just "Productor e ingeniero residente en TDF Records, mentor del programa de producción."
        , instructorAvatarUrl = Just (courseInstructorAvatarFallback cfg)
        , sessions = sessions
        , syllabus = syllabus
        , whatsappCtaUrl = whatsappUrl
        , landingUrl = buildLandingUrl cfg
        }

data WhatsAppEnv = WhatsAppEnv
  { waToken        :: Maybe Text
  , waPhoneId      :: Maybe Text
  , waVerifyToken  :: Maybe Text
  , waContactNumber :: Maybe Text
  , waApiVersion   :: Maybe Text
  }

loadWhatsAppEnv :: IO WhatsAppEnv
loadWhatsAppEnv = do
  token   <- firstNonEmptyText ["WHATSAPP_TOKEN", "WA_TOKEN"]
  phoneId <- firstNonEmptyText ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]
  verify  <- firstNonEmptyText ["WHATSAPP_VERIFY_TOKEN", "WA_VERIFY_TOKEN"]
  contact <- firstNonEmptyText ["COURSE_WHATSAPP_NUMBER", "WHATSAPP_CONTACT_NUMBER", "WA_CONTACT_NUMBER"]
  apiVersion <- firstNonEmptyText ["WHATSAPP_API_VERSION", "WA_GRAPH_API_VERSION", "WA_API_VERSION"]
  pure WhatsAppEnv
    { waToken = token
    , waPhoneId = phoneId
    , waVerifyToken = verify
    , waContactNumber = contact
    , waApiVersion = apiVersion
    }

firstNonEmptyText :: [String] -> IO (Maybe Text)
firstNonEmptyText names = go names
  where
    go [] = pure Nothing
    go (n:ns) = do
      val <- lookupEnv n
      case fmap (T.strip . T.pack) val of
        Just txt | not (T.null txt) -> pure (Just txt)
        _                           -> go ns

loadCourseMetadata :: Text -> AppM CourseMetadata
loadCourseMetadata rawSlug = do
  Env{..} <- ask
  waEnv <- liftIO loadWhatsAppEnv
  let normalized = normalizeSlug rawSlug
  mDbMeta <- loadCourseMetadataFromDB envConfig waEnv normalized
  let fallbackMeta = courseMetadataFor envConfig (waContactNumber waEnv) normalized
  baseMeta <- maybe (maybe (throwNotFound "Curso no encontrado") pure fallbackMeta) pure mDbMeta
  let Courses.CourseMetadata{ Courses.capacity = baseCapacity } = baseMeta
  countRegs <- runDB $
    count
      [ ME.CourseRegistrationCourseSlug ==. normalized
      , ME.CourseRegistrationStatus !=. "cancelled"
      ]
  let remainingSeats = max 0 (baseCapacity - fromIntegral countRegs)
  pure baseMeta { Courses.remaining = remainingSeats }

loadCourseMetadataFromDB :: AppConfig -> WhatsAppEnv -> Text -> AppM (Maybe CourseMetadata)
loadCourseMetadataFromDB cfg waEnv slugVal = do
  runDB $ do
    mCourse <- getBy (Trials.UniqueCourseSlug slugVal)
    case mCourse of
      Nothing -> pure Nothing
      Just (Entity cid course) -> do
        sessions <- selectList
          [Trials.CourseSessionModelCourseId ==. cid]
          [Asc Trials.CourseSessionModelOrder, Asc Trials.CourseSessionModelDate]
        syllabus <- selectList
          [Trials.CourseSyllabusItemCourseId ==. cid]
          [Asc Trials.CourseSyllabusItemOrder, Asc Trials.CourseSyllabusItemId]
        pure $ Just (toCourseMetadata cfg waEnv course sessions syllabus)

toCourseMetadata
  :: AppConfig
  -> WhatsAppEnv
  -> Trials.Course
  -> [Entity Trials.CourseSessionModel]
  -> [Entity Trials.CourseSyllabusItem]
  -> CourseMetadata
toCourseMetadata cfg waEnv course sessions syllabus =
  let slugVal = Trials.courseSlug course
      landingUrlVal =
        fromMaybe (buildLandingUrlFor cfg slugVal) (cleanOptional (Trials.courseLandingUrl course))
      whatsappUrl =
        fromMaybe
          (buildWhatsappCtaFor (waContactNumber waEnv) (Trials.courseTitle course) landingUrlVal)
          (cleanOptional (Trials.courseWhatsappCtaUrl course))
      dawsList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseDaws course))
      includesList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseIncludes course))
      instructorNameVal = cleanOptional (Trials.courseInstructorName course)
      instructorBioVal = cleanOptional (Trials.courseInstructorBio course)
      instructorAvatarVal = cleanOptional (Trials.courseInstructorAvatarUrl course)
      toSession (Entity _ s) =
        CourseSession
          { label = Trials.courseSessionModelLabel s
          , date = Trials.courseSessionModelDate s
          }
      toSyllabus (Entity _ s) =
        SyllabusItem
          { title = Trials.courseSyllabusItemTitle s
          , topics = filter (not . T.null . T.strip) (Trials.courseSyllabusItemTopics s)
          }
  in CourseMetadata
      { slug = slugVal
      , title = Trials.courseTitle course
      , subtitle = fromMaybe "" (Trials.courseSubtitle course)
      , format = fromMaybe "" (Trials.courseFormat course)
      , duration = fromMaybe "" (Trials.courseDuration course)
      , price = fromIntegral (Trials.coursePriceCents course) / 100
      , currency = Trials.courseCurrency course
      , capacity = Trials.courseCapacity course
      , remaining = Trials.courseCapacity course
      , sessionStartHour = fromMaybe 0 (Trials.courseSessionStartHour course)
      , sessionDurationHours = fromMaybe 0 (Trials.courseSessionDurationHours course)
      , locationLabel = fromMaybe "" (Trials.courseLocationLabel course)
      , locationMapUrl = fromMaybe "" (Trials.courseLocationMapUrl course)
      , daws = dawsList
      , includes = includesList
      , instructorName = instructorNameVal
      , instructorBio = instructorBioVal
      , instructorAvatarUrl = instructorAvatarVal
      , sessions = map toSession sessions
      , syllabus = map toSyllabus syllabus
      , whatsappCtaUrl = whatsappUrl
      , landingUrl = landingUrlVal
      }

buildLandingUrlFor :: AppConfig -> Text -> Text
buildLandingUrlFor cfg slugVal =
  let base = resolveConfiguredAppBase cfg
  in base <> "/curso/" <> slugVal

buildWhatsappCtaFor :: Maybe Text -> Text -> Text -> Text
buildWhatsappCtaFor mNumber courseTitle landingUrl =
  let msg = "INSCRIBIRME " <> courseTitle <> " " <> landingUrl
      encoded = TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 msg))
      cleanNumber txt = T.filter isDigit txt
  in case mNumber >>= nonEmpty of
       Just num -> "https://wa.me/" <> cleanNumber num <> "?text=" <> encoded
       Nothing  -> "https://wa.me/?text=" <> encoded
  where
    nonEmpty txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

saveCourse :: CourseUpsert -> AppM CourseMetadata
saveCourse Courses.CourseUpsert{..} = do
  now <- liftIO getCurrentTime
  Env{..} <- ask
  waEnv <- liftIO loadWhatsAppEnv
  let slugVal = normalizeSlug slug
      titleClean = T.strip title
  when (T.null slugVal) $
    throwBadRequest "slug requerido"
  when (T.null titleClean) $
    throwBadRequest "titulo requerido"
  let
      subtitleClean = cleanOptional subtitle
      formatClean = cleanOptional format
      durationClean = cleanOptional duration
      currencyClean = let cur = T.strip currency in if T.null cur then "USD" else cur
      capacityClean = max 0 capacity
      priceCentsClean = max 0 priceCents
      startHourClean = fmap (max 0) sessionStartHour
      durationHoursClean = fmap (max 0) sessionDurationHours
      locationLabelClean = cleanOptional locationLabel
      locationMapUrlClean = cleanOptional locationMapUrl
      landingUrlClean = cleanOptional landingUrl
      landingResolved = fromMaybe (buildLandingUrlFor envConfig slugVal) landingUrlClean
      whatsappClean = cleanOptional whatsappCtaUrl
      whatsappResolved = fromMaybe (buildWhatsappCtaFor (waContactNumber waEnv) titleClean landingResolved) whatsappClean
      dawsClean = normalizeList daws
      includesClean = normalizeList includes
      instructorNameClean = cleanOptional instructorName
      instructorBioClean = cleanOptional instructorBio
      instructorAvatarClean = cleanOptional instructorAvatarUrl
      normalizeList xs =
        case filter (not . T.null) (map T.strip xs) of
          [] -> Nothing
          ys -> Just ys
      sanitizeTopics = filter (not . T.null . T.strip)
      withOrder fallbackIdx mOrder = fromMaybe fallbackIdx mOrder
  runDB $ do
    mExisting <- getBy (Trials.UniqueCourseSlug slugVal)
    courseId <- case mExisting of
      Nothing -> insert Trials.Course
        { Trials.courseSlug = slugVal
        , Trials.courseTitle = titleClean
        , Trials.courseSubtitle = subtitleClean
        , Trials.courseFormat = formatClean
        , Trials.courseDuration = durationClean
        , Trials.coursePriceCents = priceCentsClean
        , Trials.courseCurrency = currencyClean
        , Trials.courseCapacity = capacityClean
        , Trials.courseSessionStartHour = startHourClean
        , Trials.courseSessionDurationHours = durationHoursClean
        , Trials.courseLocationLabel = locationLabelClean
        , Trials.courseLocationMapUrl = locationMapUrlClean
        , Trials.courseWhatsappCtaUrl = Just whatsappResolved
        , Trials.courseLandingUrl = Just landingResolved
        , Trials.courseDaws = dawsClean
        , Trials.courseIncludes = includesClean
        , Trials.courseInstructorName = instructorNameClean
        , Trials.courseInstructorBio = instructorBioClean
        , Trials.courseInstructorAvatarUrl = instructorAvatarClean
        , Trials.courseCreatedAt = now
        , Trials.courseUpdatedAt = now
        }
      Just (Entity cid _) -> do
        update cid
          [ Trials.CourseSlug =. slugVal
          , Trials.CourseTitle =. titleClean
          , Trials.CourseSubtitle =. subtitleClean
          , Trials.CourseFormat =. formatClean
          , Trials.CourseDuration =. durationClean
          , Trials.CoursePriceCents =. priceCentsClean
          , Trials.CourseCurrency =. currencyClean
          , Trials.CourseCapacity =. capacityClean
          , Trials.CourseSessionStartHour =. startHourClean
          , Trials.CourseSessionDurationHours =. durationHoursClean
          , Trials.CourseLocationLabel =. locationLabelClean
          , Trials.CourseLocationMapUrl =. locationMapUrlClean
          , Trials.CourseWhatsappCtaUrl =. Just whatsappResolved
          , Trials.CourseLandingUrl =. Just landingResolved
          , Trials.CourseDaws =. dawsClean
          , Trials.CourseIncludes =. includesClean
          , Trials.CourseInstructorName =. instructorNameClean
          , Trials.CourseInstructorBio =. instructorBioClean
          , Trials.CourseInstructorAvatarUrl =. instructorAvatarClean
          , Trials.CourseUpdatedAt =. now
          ]
        pure cid

    deleteWhere [Trials.CourseSessionModelCourseId ==. courseId]
    deleteWhere [Trials.CourseSyllabusItemCourseId ==. courseId]

    let sessionPayload = zip [1..] sessions
    for_ sessionPayload $ \(idx, CourseSessionIn{..}) -> do
      let ordVal = withOrder idx order
      insert_ Trials.CourseSessionModel
        { Trials.courseSessionModelCourseId = courseId
        , Trials.courseSessionModelLabel = T.strip label
        , Trials.courseSessionModelDate = date
        , Trials.courseSessionModelOrder = Just ordVal
        }

    let syllabusPayload = zip [1..] syllabus
    for_ syllabusPayload $ \(idx, CourseSyllabusIn{title = syllabusTitle, topics = syllabusTopics, order = syllabusOrder}) -> do
      let ordVal = withOrder idx syllabusOrder
      insert_ Trials.CourseSyllabusItem
        { Trials.courseSyllabusItemCourseId = courseId
        , Trials.courseSyllabusItemTitle = T.strip syllabusTitle
        , Trials.courseSyllabusItemTopics = sanitizeTopics syllabusTopics
        , Trials.courseSyllabusItemOrder = Just ordVal
        }
  loadCourseMetadata slugVal

listCourseRegistrations
  :: Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> AppM [DTO.CourseRegistrationDTO]
listCourseRegistrations mSlug mStatus mLimit = do
  let filters = catMaybes
        [ (\s -> ME.CourseRegistrationCourseSlug ==. normalizeSlug s) <$> cleanOptional mSlug
        , (\s -> ME.CourseRegistrationStatus ==. T.toLower (T.strip s)) <$> cleanOptional mStatus
        ]
      capped = max 1 (min 500 (fromMaybe 200 mLimit))
  rows <- runDB $ selectList filters [Desc ME.CourseRegistrationCreatedAt, LimitTo capped]
  pure (map toCourseRegistrationDTO rows)

fetchCourseRegistration :: Text -> Int64 -> AppM DTO.CourseRegistrationDTO
fetchCourseRegistration rawSlug regId = do
  let slugVal = normalizeSlug rawSlug
      key = toSqlKey regId
  mRow <- runDB $ getEntity key
  case mRow of
    Nothing -> throwNotFound "Registro no encontrado"
    Just ent@(Entity _ reg)
      | ME.courseRegistrationCourseSlug reg /= slugVal -> throwNotFound "Registro no encontrado"
      | otherwise -> pure (toCourseRegistrationDTO ent)

updateCourseRegistrationStatus
  :: Text
  -> Int64
  -> CourseRegistrationStatusUpdate
  -> AppM CourseRegistrationResponse
updateCourseRegistrationStatus rawSlug regId CourseRegistrationStatusUpdate{..} = do
  let slugVal = normalizeSlug rawSlug
      newStatus = T.toLower (T.strip status)
      key = toSqlKey regId
  when (T.null newStatus) $
    throwBadRequest "status requerido"
  now <- liftIO getCurrentTime
  mRow <- runDB $ getEntity key
  case mRow of
    Nothing -> throwNotFound "Registro no encontrado"
    Just (Entity _ reg)
      | ME.courseRegistrationCourseSlug reg /= slugVal -> throwNotFound "Registro no encontrado"
      | otherwise -> do
          runDB $ update key
            [ ME.CourseRegistrationStatus =. newStatus
            , ME.CourseRegistrationUpdatedAt =. now
            ]
          pure CourseRegistrationResponse { id = regId, status = newStatus }

listCourseRegistrationEmailEvents
  :: Int64
  -> Maybe Int
  -> AppM [DTO.CourseEmailEventDTO]
listCourseRegistrationEmailEvents regId mLimit = do
  let regKey = toSqlKey regId :: Key ME.CourseRegistration
      capped = max 1 (min 500 (fromMaybe 100 mLimit))
  mRow <- runDB $ getEntity regKey
  case mRow of
    Nothing -> throwNotFound "Registro no encontrado"
    Just (Entity _ reg) -> do
      let mEmail =
            case ME.courseRegistrationEmail reg of
              Nothing -> Nothing
              Just raw ->
                let normalized = T.toLower (T.strip raw)
                in if T.null normalized then Nothing else Just normalized
          byRegistration = [ME.CourseEmailEventRegistrationId ==. Just regKey]
          rowsQuery =
            case mEmail of
              Nothing -> selectList byRegistration [Desc ME.CourseEmailEventCreatedAt, LimitTo capped]
              Just emailVal ->
                selectList
                  (byRegistration ||.
                    [ ME.CourseEmailEventCourseSlug ==. ME.courseRegistrationCourseSlug reg
                    , ME.CourseEmailEventRecipientEmail ==. emailVal
                    ])
                  [Desc ME.CourseEmailEventCreatedAt, LimitTo capped]
      rows <- runDB rowsQuery
      pure (map toCourseEmailEventDTO rows)

toCourseRegistrationDTO :: Entity ME.CourseRegistration -> DTO.CourseRegistrationDTO
toCourseRegistrationDTO (Entity rid reg) =
  DTO.CourseRegistrationDTO
    { DTO.crId = fromSqlKey rid
    , DTO.crCourseSlug = ME.courseRegistrationCourseSlug reg
    , DTO.crFullName = ME.courseRegistrationFullName reg
    , DTO.crEmail = ME.courseRegistrationEmail reg
    , DTO.crPhoneE164 = ME.courseRegistrationPhoneE164 reg
    , DTO.crSource = ME.courseRegistrationSource reg
    , DTO.crStatus = ME.courseRegistrationStatus reg
    , DTO.crHowHeard = ME.courseRegistrationHowHeard reg
    , DTO.crUtmSource = ME.courseRegistrationUtmSource reg
    , DTO.crUtmMedium = ME.courseRegistrationUtmMedium reg
    , DTO.crUtmCampaign = ME.courseRegistrationUtmCampaign reg
    , DTO.crUtmContent = ME.courseRegistrationUtmContent reg
    , DTO.crCreatedAt = ME.courseRegistrationCreatedAt reg
    , DTO.crUpdatedAt = ME.courseRegistrationUpdatedAt reg
    }

toCourseEmailEventDTO :: Entity ME.CourseEmailEvent -> DTO.CourseEmailEventDTO
toCourseEmailEventDTO (Entity eventId ev) =
  DTO.CourseEmailEventDTO
    { DTO.ceId = fromSqlKey eventId
    , DTO.ceCourseSlug = ME.courseEmailEventCourseSlug ev
    , DTO.ceRegistrationId = fromSqlKey <$> ME.courseEmailEventRegistrationId ev
    , DTO.ceRecipientEmail = ME.courseEmailEventRecipientEmail ev
    , DTO.ceRecipientName = ME.courseEmailEventRecipientName ev
    , DTO.ceEventType = ME.courseEmailEventEventType ev
    , DTO.ceStatus = ME.courseEmailEventStatus ev
    , DTO.ceMessage = ME.courseEmailEventMessage ev
    , DTO.ceCreatedAt = ME.courseEmailEventCreatedAt ev
    }

recordCourseEmailEvent
  :: Text
  -> Maybe ME.CourseRegistrationId
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> Maybe Text
  -> AppM ()
recordCourseEmailEvent rawSlug mRegistrationId rawRecipientEmail mRecipientName rawEventType rawStatus mMessage = do
  let recipientEmail = T.toLower (T.strip rawRecipientEmail)
  unless (T.null recipientEmail) $ do
    now <- liftIO getCurrentTime
    runDB $ insert_ ME.CourseEmailEvent
      { ME.courseEmailEventCourseSlug = normalizeSlug rawSlug
      , ME.courseEmailEventRegistrationId = mRegistrationId
      , ME.courseEmailEventRecipientEmail = recipientEmail
      , ME.courseEmailEventRecipientName = cleanOptional mRecipientName
      , ME.courseEmailEventEventType = T.toLower (T.strip rawEventType)
      , ME.courseEmailEventStatus = T.toLower (T.strip rawStatus)
      , ME.courseEmailEventMessage = cleanOptional mMessage
      , ME.courseEmailEventCreatedAt = now
      }

courseEmailSentWithinLast24Hours :: Text -> AppM Bool
courseEmailSentWithinLast24Hours rawRecipientEmail = do
  let recipientEmail = T.toLower (T.strip rawRecipientEmail)
  if T.null recipientEmail
    then pure False
    else do
      now <- liftIO getCurrentTime
      let cutoff = addUTCTime (negate 86400) now
      mRecent <- runDB $ selectFirst
        [ ME.CourseEmailEventRecipientEmail ==. recipientEmail
        , ME.CourseEmailEventStatus ==. "sent"
        , ME.CourseEmailEventCreatedAt >=. cutoff
        ]
        [Desc ME.CourseEmailEventCreatedAt]
      pure (isJust mRecent)

createOrUpdateRegistration :: Text -> CourseRegistrationRequest -> AppM CourseRegistrationResponse
createOrUpdateRegistration rawSlug CourseRegistrationRequest{..} = do
  metaRaw <- loadCourseMetadata rawSlug
  let Courses.CourseMetadata{ Courses.slug = metaSlug
                            , Courses.sessions = metaSessions
                            , Courses.title = metaTitle
                            , Courses.landingUrl = metaLanding
                            } = metaRaw
  now <- liftIO getCurrentTime
  let slugVal = normalizeSlug metaSlug
      nameClean = cleanOptional fullName
      sourceClean = normalizeSource source
      howHeardClean = cleanOptional howHeard
      phoneClean = phoneE164 >>= normalizePhone
      pendingStatus = "pending_payment"
      (utmSourceVal, utmMediumVal, utmCampaignVal, utmContentVal) = normalizeUtm utm
  normalizedEmail <- case cleanOptional email of
    Nothing -> pure Nothing
    Just e  -> Just <$> requireEmail e
  mNewUser <- case normalizedEmail of
    Nothing -> pure Nothing
    Just addr -> ensureUserAccount nameClean addr
  when (sourceClean == "landing" && isNothing nameClean) $
    throwBadRequest "nombre requerido"
  when (sourceClean == "landing" && isNothing normalizedEmail) $
    throwBadRequest "email requerido"
  existing <- runDB $ findExistingRegistration slugVal normalizedEmail phoneClean
  case existing of
    -- Update in-place only when the existing row is still pending; otherwise create a fresh row.
    Just (Entity regId reg) | ME.courseRegistrationStatus reg == pendingStatus -> do
      runDB $ update regId
        [ ME.CourseRegistrationFullName =. (nameClean <|> ME.courseRegistrationFullName reg)
        , ME.CourseRegistrationEmail =. (normalizedEmail <|> ME.courseRegistrationEmail reg)
        , ME.CourseRegistrationPhoneE164 =. (phoneClean <|> ME.courseRegistrationPhoneE164 reg)
        , ME.CourseRegistrationSource =. sourceClean
        , ME.CourseRegistrationStatus =. pendingStatus
        , ME.CourseRegistrationHowHeard =. (howHeardClean <|> ME.courseRegistrationHowHeard reg)
        , ME.CourseRegistrationUtmSource =. (utmSourceVal <|> ME.courseRegistrationUtmSource reg)
        , ME.CourseRegistrationUtmMedium =. (utmMediumVal <|> ME.courseRegistrationUtmMedium reg)
        , ME.CourseRegistrationUtmCampaign =. (utmCampaignVal <|> ME.courseRegistrationUtmCampaign reg)
        , ME.CourseRegistrationUtmContent =. (utmContentVal <|> ME.courseRegistrationUtmContent reg)
        , ME.CourseRegistrationUpdatedAt =. now
        ]
      sendConfirmation slugVal regId metaTitle metaLanding metaSessions nameClean normalizedEmail mNewUser
      pure CourseRegistrationResponse { id = fromSqlKey regId, status = pendingStatus }
    _ -> do
      regId <- runDB $ insert ME.CourseRegistration
        { ME.courseRegistrationCourseSlug = slugVal
        , ME.courseRegistrationFullName = nameClean
        , ME.courseRegistrationEmail = normalizedEmail
        , ME.courseRegistrationPhoneE164 = phoneClean
        , ME.courseRegistrationSource = sourceClean
        , ME.courseRegistrationStatus = pendingStatus
        , ME.courseRegistrationHowHeard = howHeardClean
        , ME.courseRegistrationUtmSource = utmSourceVal
        , ME.courseRegistrationUtmMedium = utmMediumVal
        , ME.courseRegistrationUtmCampaign = utmCampaignVal
        , ME.courseRegistrationUtmContent = utmContentVal
        , ME.courseRegistrationCreatedAt = now
        , ME.courseRegistrationUpdatedAt = now
        }
      sendConfirmation slugVal regId metaTitle metaLanding metaSessions nameClean normalizedEmail mNewUser
      pure CourseRegistrationResponse { id = fromSqlKey regId, status = pendingStatus }
  where
    sendConfirmation courseSlug regKey courseTitle landing metaSessions nameClean mEmail mNewUser =
      case mEmail of
        Nothing -> pure ()
        Just emailAddr -> do
          Env{..} <- ask
          let emailSvc = EmailSvc.mkEmailService envConfig
              displayName = fromMaybe "" nameClean
              datesSummary =
                let fmt d = T.pack (formatTime defaultTimeLocale "%d %b %Y" d)
                in T.intercalate ", " (map (\Courses.CourseSession{ Courses.date = d } -> fmt d) metaSessions)
          -- Check if email is configured before attempting to send
          case EmailSvc.esConfig emailSvc of
            Nothing -> do
              let msg = "[CourseRegistration] WARNING: SMTP not configured. Email confirmation not sent to " <> emailAddr
              liftIO $ do
                hPutStrLn stderr (T.unpack msg)
                LogBuf.addLog LogBuf.LogWarning msg
              recordCourseEmailEvent
                courseSlug
                (Just regKey)
                emailAddr
                nameClean
                "registration_confirmation"
                "skipped"
                (Just msg)
            Just _ -> do
              alreadySent <- courseEmailSentWithinLast24Hours emailAddr
              if alreadySent
                then do
                  let msg = "[CourseRegistration] Skipped registration confirmation to " <> emailAddr <> ": daily email cap reached (max 1 every 24h)."
                  liftIO $ LogBuf.addLog LogBuf.LogWarning msg
                  recordCourseEmailEvent
                    courseSlug
                    (Just regKey)
                    emailAddr
                    nameClean
                    "registration_confirmation"
                    "skipped"
                    (Just msg)
                else do
                  result <- liftIO $ try $ EmailSvc.sendCourseRegistration emailSvc displayName emailAddr courseTitle landing datesSummary
                  case result of
                    Left err -> do
                      let errorMsg = T.pack (displayException (err :: SomeException))
                          msg = "[CourseRegistration] Failed to send confirmation email to " <> emailAddr <> ": " <> errorMsg
                      liftIO $ do
                        hPutStrLn stderr (T.unpack msg)
                        LogBuf.addLog LogBuf.LogError msg
                      recordCourseEmailEvent
                        courseSlug
                        (Just regKey)
                        emailAddr
                        nameClean
                        "registration_confirmation"
                        "failed"
                        (Just errorMsg)
                    Right () -> do
                      let msg = "[CourseRegistration] Successfully sent confirmation email to " <> emailAddr
                      liftIO $ LogBuf.addLog LogBuf.LogInfo msg
                      recordCourseEmailEvent
                        courseSlug
                        (Just regKey)
                        emailAddr
                        nameClean
                        "registration_confirmation"
                        "sent"
                        Nothing
              -- Send welcome email if we just created credentials.
              -- This is evaluated after registration_confirmation, so a just-sent
              -- confirmation will block welcome in the same request.
              for_ mNewUser $ \(username, tempPassword) -> do
                welcomeAlreadySent <- courseEmailSentWithinLast24Hours emailAddr
                if welcomeAlreadySent
                  then do
                    let msg = "[CourseRegistration] Skipped welcome email to " <> emailAddr <> ": daily email cap reached (max 1 every 24h)."
                    liftIO $ LogBuf.addLog LogBuf.LogWarning msg
                    recordCourseEmailEvent
                      courseSlug
                      (Just regKey)
                      emailAddr
                      nameClean
                      "welcome_credentials"
                      "skipped"
                      (Just msg)
                  else do
                    welcomeResult <- liftIO $ try $ EmailSvc.sendWelcome emailSvc displayName emailAddr username tempPassword
                    case welcomeResult of
                      Left err -> do
                        let errorMsg = T.pack (displayException (err :: SomeException))
                            msg = "[CourseRegistration] Failed to send welcome email to " <> emailAddr <> ": " <> errorMsg
                        liftIO $ do
                          hPutStrLn stderr (T.unpack msg)
                          LogBuf.addLog LogBuf.LogError msg
                        recordCourseEmailEvent
                          courseSlug
                          (Just regKey)
                          emailAddr
                          nameClean
                          "welcome_credentials"
                          "failed"
                          (Just errorMsg)
                      Right () -> do
                        let msg = "[CourseRegistration] Sent welcome credentials to " <> emailAddr
                        liftIO $ LogBuf.addLog LogBuf.LogInfo msg
                        recordCourseEmailEvent
                          courseSlug
                          (Just regKey)
                          emailAddr
                          nameClean
                          "welcome_credentials"
                          "sent"
                          Nothing

-- | Returns messages that include text bodies, paired with the sender phone.
extractTextMessages :: WAMetaWebhook -> [(Text, Text)]
extractTextMessages payload =
  [ (waInboundSenderId, waInboundText)
  | WAInbound{waInboundSenderId, waInboundText} <- extractWhatsAppInbound payload
  ]

data WAInbound = WAInbound
  { waInboundExternalId :: Text
  , waInboundSenderId   :: Text
  , waInboundText       :: Text
  , waInboundAdExternalId :: Maybe Text
  , waInboundAdName     :: Maybe Text
  , waInboundCampaignExternalId :: Maybe Text
  , waInboundCampaignName :: Maybe Text
  , waInboundMetadata   :: Maybe Text
  } deriving (Show)

extractWhatsAppInbound :: WAMetaWebhook -> [WAInbound]
extractWhatsAppInbound WAMetaWebhook{entry} =
  [ WAInbound
      { waInboundExternalId = fromMaybe (WA.from msg <> "-" <> fromMaybe "0" (waTimestamp msg)) (waId msg)
      , waInboundSenderId = WA.from msg
      , waInboundText = body
      , waInboundAdExternalId = adExt
      , waInboundAdName = adName
      , waInboundCampaignExternalId = Nothing
      , waInboundCampaignName = Nothing
      , waInboundMetadata = metaTxt
      }
  | WAEntry{changes} <- entry
  , WAChange{value=WAValue{messages=Just msgs}} <- changes
  , msg@WAMessage{waType, text=Just txtBody} <- msgs
  , waType == "text"
  , let body = WA.body txtBody
        referral = waReferral msg <|> (waContext msg >>= waContextReferral)
        (adExt, adName, metaTxt) = waReferralMeta referral
  ]
  where
    waReferralMeta Nothing = (Nothing, Nothing, Nothing)
    waReferralMeta (Just WAReferral{sourceId, headline, waBody, sourceType, sourceUrl}) =
      let adName = headline <|> waBody
          metaObj = object
            [ "source_id" .= sourceId
            , "source_type" .= sourceType
            , "source_url" .= sourceUrl
            , "headline" .= headline
            , "body" .= waBody
            ]
          metaTxt = Just (TE.decodeUtf8 (BL.toStrict (encode metaObj)))
      in (sourceId, adName, metaTxt)

sendWhatsappReply :: WhatsAppEnv -> Text -> AppM (Either Text Text)
sendWhatsappReply WhatsAppEnv{waToken = Just tok, waPhoneId = Just pid, waApiVersion = mVersion} phone = do
  Env{envConfig} <- ask
  let slugVal = productionCourseSlug envConfig
  metaRaw <- loadCourseMetadata slugVal
  let Courses.CourseMetadata{ Courses.title = metaTitle
                            , Courses.capacity = metaCapacity
                            , Courses.landingUrl = metaLanding
                            } = metaRaw
  manager <- liftIO $ newManager tlsManagerSettings
  let msg = "¡Gracias por tu interés en " <> metaTitle <> "! Aquí tienes el link de inscripción: "
            <> metaLanding <> ". Cupos limitados (" <> T.pack (show metaCapacity) <> ")."
      version = fromMaybe "v20.0" mVersion
  res <- liftIO $ sendText manager version tok pid phone msg
  pure $ case res of
    Left err -> Left (T.pack err)
    Right _ -> Right msg
sendWhatsappReply _ _ = pure (Left "WhatsApp config not available")

sendWhatsAppText :: WhatsAppEnv -> Text -> Text -> AppM (Either Text Text)
sendWhatsAppText WhatsAppEnv{waToken = Just tok, waPhoneId = Just pid, waApiVersion = mVersion} phone msg = do
  manager <- liftIO $ newManager tlsManagerSettings
  let version = fromMaybe "v20.0" mVersion
  res <- liftIO $ sendText manager version tok pid phone msg
  pure $ case res of
    Left err -> Left (T.pack err)
    Right _ -> Right msg
sendWhatsAppText _ _ _ = pure (Left "WhatsApp config not available")

normalizePhone :: Text -> Maybe Text
normalizePhone raw =
  let trimmed = T.filter (not . isSpace) (T.strip raw)
      digits = T.filter (\c -> isDigit c || c == '+') trimmed
      withoutPlus = T.dropWhile (== '+') digits
      onlyDigits = T.filter isDigit withoutPlus
  in if T.null onlyDigits then Nothing else Just ("+" <> onlyDigits)

normalizeSlug :: Text -> Text
normalizeSlug = T.toLower . T.strip

normalizeSource :: Text -> Text
normalizeSource raw =
  let trimmed = T.toLower (T.strip raw)
  in if T.null trimmed then "landing" else trimmed

normalizeUtm :: Maybe UTMTags -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
normalizeUtm Nothing = (Nothing, Nothing, Nothing, Nothing)
normalizeUtm (Just UTMTags{..}) =
  (cleanOptional source, cleanOptional medium, cleanOptional campaign, cleanOptional content)

-- Ensure a Party/UserCredential exists for a registration email. Returns (username, tempPassword) only when a new
-- credential was created.
ensureUserAccount :: Maybe Text -> Text -> AppM (Maybe (Text, Text))
ensureUserAccount mName emailAddr = snd <$> ensurePartyWithAccount mName emailAddr Nothing

ensurePartyWithAccount :: Maybe Text -> Text -> Maybe Text -> AppM (Key Party, Maybe (Text, Text))
ensurePartyWithAccount mName emailAddr mPhone = do
  now <- liftIO getCurrentTime
  let display = case fmap T.strip mName of
        Just nameTxt | not (T.null nameTxt) -> nameTxt
        _                                   -> emailAddr
      phoneClean = mPhone >>= normalizePhone
  partyId <- runDB $ do
    mParty <- selectFirst [PartyPrimaryEmail ==. Just emailAddr] []
    case mParty of
      Just (Entity pid party) -> do
        let updates = catMaybes
              [ if not (T.null (M.partyDisplayName party)) || T.null display
                  then Nothing
                  else Just (PartyDisplayName =. display)
              , case phoneClean of
                  Just phone | isNothing (partyPrimaryPhone party) -> Just (PartyPrimaryPhone =. Just phone)
                  _ -> Nothing
              ]
        unless (null updates) (update pid updates)
        pure pid
      Nothing -> insert Party
        { partyLegalName = Nothing
        , partyDisplayName = display
        , partyIsOrg = False
        , partyTaxId = Nothing
        , partyPrimaryEmail = Just emailAddr
        , partyPrimaryPhone = phoneClean
        , partyWhatsapp = Nothing
        , partyInstagram = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes = Nothing
        , partyCreatedAt = now
        }
  mCred <- runDB $ selectFirst [UserCredentialPartyId ==. partyId] []
  newCred <- case mCred of
    Just _ -> pure Nothing
    Nothing -> do
      username <- runDB (generateUniqueUsername (deriveBaseUsername mName emailAddr) partyId)
      tempPassword <- liftIO Email.generateTempPassword
      hashed <- liftIO (hashPasswordText tempPassword)
      _ <- runDB $ insert UserCredential
        { userCredentialPartyId = partyId
        , userCredentialUsername = username
        , userCredentialPasswordHash = hashed
        , userCredentialActive = True
        }
      -- Assign default roles for new customers/fans
      runDB $ do
        _ <- upsert (PartyRole partyId Customer True) [PartyRoleActive =. True]
        _ <- upsert (PartyRole partyId Fan True) [PartyRoleActive =. True]
        pure ()
      pure (Just (username, tempPassword))
  pure (partyId, newCred)

deriveBaseUsername :: Maybe Text -> Text -> Text
deriveBaseUsername mName emailAddr =
  let emailLocal = T.takeWhile (/= '@') emailAddr
      candidate = fromMaybe emailLocal (slugify <$> mName)
  in if T.null candidate then emailLocal else candidate

generateUniqueUsername :: Text -> PartyId -> SqlPersistT IO Text
generateUniqueUsername base partyId = go (0 :: Int)
  where
    baseClean = T.take 60 (T.filter (\c -> isAlphaNum c || c `elem` (".-_" :: String)) (T.toLower (T.strip base)))
    fallback = "tdf-user-" <> T.pack (show (fromSqlKey partyId))
    root = if T.null baseClean then fallback else baseClean
    go attempt = do
      let suffix = if attempt == 0 then "" else "-" <> T.pack (show attempt)
          candidate = T.take 60 (root <> suffix)
      conflict <- getBy (UniqueCredentialUsername candidate)
      case conflict of
        Nothing -> pure candidate
        Just _  -> go (attempt + 1)

slugify :: Text -> Text
slugify =
  T.take 60 . T.filter (\c -> isAlphaNum c || c `elem` ("._-" :: String)) . T.toLower . T.strip

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

findExistingRegistration
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> SqlPersistT IO (Maybe (Entity ME.CourseRegistration))
findExistingRegistration slugVal mEmail mPhone = do
  byEmail <- case mEmail of
    Nothing -> pure Nothing
    Just e  -> selectFirst
      [ ME.CourseRegistrationCourseSlug ==. slugVal
      , ME.CourseRegistrationEmail ==. Just e
      ]
      [Desc ME.CourseRegistrationCreatedAt]
  case byEmail of
    Just hit -> pure (Just hit)
    Nothing -> case mPhone of
      Nothing -> pure Nothing
      Just p -> selectFirst
        [ ME.CourseRegistrationCourseSlug ==. slugVal
        , ME.CourseRegistrationPhoneE164 ==. Just p
        ]
        [Desc ME.CourseRegistrationCreatedAt]

-- Health
health :: AppM TDF.API.HealthStatus
health = pure (HealthStatus "ok" "ok")

login :: LoginRequest -> AppM LoginResponse
login LoginRequest{..} = do
  Env pool _ <- ask
  result <- liftIO $ flip runSqlPool pool (runLogin username password)
  case result of
    Left msg  -> throwAuthError msg
    Right res -> pure res
  where
    throwAuthError msg = throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

googleLogin :: GoogleLoginRequest -> AppM LoginResponse
googleLogin GoogleLoginRequest{..} = do
  let tokenClean = T.strip idToken
  when (T.null tokenClean) $ throwBadRequest "Google idToken is required"
  Env pool cfg <- ask
  let mClientId = googleClientId cfg
  when (isNothing mClientId) $
    throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Google Sign-In is not configured") }
  manager <- liftIO $ newManager tlsManagerSettings
  verification <- liftIO $ verifyGoogleIdToken manager tokenClean mClientId
  case verification of
    Left msg -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
    Right profile -> do
      result <- liftIO $ flip runSqlPool pool (completeGoogleLogin profile)
      case result of
        Left err -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 err) }
        Right resp -> pure resp

verifyGoogleIdToken :: Manager -> Text -> Maybe Text -> IO (Either Text GoogleProfile)
verifyGoogleIdToken manager rawToken mExpectedClientId = do
  let encoded = BS8.unpack (urlEncode True (TE.encodeUtf8 rawToken))
  req <- parseRequest ("https://oauth2.googleapis.com/tokeninfo?id_token=" <> encoded)
  respResult <- try (httpLbs req manager)
    :: IO (Either SomeException (Response BL.ByteString))
  case respResult of
    Left _ ->
      pure (Left "No pudimos validar tu sesión con Google. Intenta nuevamente.")
    Right resp ->
      let status = statusCode (responseStatus resp)
      in if status /= 200
         then pure (Left "Tu sesión de Google es inválida o expiró.")
         else case eitherDecode (responseBody resp) of
           Left _ -> pure (Left "No pudimos validar tu sesión con Google.")
           Right info -> pure (validate info)
  where
    validate info
      | not (gitEmailVerified info) =
          Left "Tu correo de Google debe estar verificado."
      | Just expected <- mExpectedClientId
      , gitAud info /= expected =
          Left "El token de Google no coincide con el cliente configurado."
      | not (issuerAllowed (gitIss info)) =
          Left "El token de Google proviene de un emisor no permitido."
      | otherwise =
          let normalizedEmail = T.toLower (T.strip (gitEmail info))
              normalizedName  = cleanOptional (gitName info)
              profile = GoogleProfile
                { gpEmail   = normalizedEmail
                , gpName    = normalizedName <|> Just normalizedEmail
                , gpPicture = gitPicture info
                }
          in Right profile

issuerAllowed :: Maybe Text -> Bool
issuerAllowed Nothing = True
issuerAllowed (Just issRaw) =
  let issuer = T.toLower (T.strip issRaw)
  in "accounts.google.com" `T.isInfixOf` issuer

completeGoogleLogin :: GoogleProfile -> SqlPersistT IO (Either Text LoginResponse)
completeGoogleLogin GoogleProfile{..} = do
  mExisting <- lookupByEmail gpEmail
  case mExisting of
    Just (Entity _ cred)
      | not (userCredentialActive cred) ->
          pure (Left "Cuenta deshabilitada. Contacta a soporte.")
      | otherwise -> do
          token <- createTokenWithLabel (userCredentialPartyId cred) (Just ("google-login:" <> gpEmail))
          mUser <- loadAuthedUser token
          case mUser of
            Nothing   -> pure (Left "No pudimos cargar tu perfil.")
            Just user -> pure (Right (toLoginResponse token user))
    Nothing -> do
      now <- liftIO getCurrentTime
      let displayName = fromMaybe gpEmail (cleanOptional gpName)
          partyRecord = Party
            { partyLegalName        = Nothing
            , partyDisplayName      = displayName
            , partyIsOrg            = False
            , partyTaxId            = Nothing
            , partyPrimaryEmail     = Just gpEmail
            , partyPrimaryPhone     = Nothing
            , partyWhatsapp         = Nothing
            , partyInstagram        = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes            = Nothing
            , partyCreatedAt        = now
            }
      pid <- insert partyRecord
      applyRoles pid [Customer, Fan]
      ensureFanProfileIfMissing pid displayName now
      tempPassword <- liftIO generateTemporaryPassword
      hashed <- liftIO (hashPasswordText tempPassword)
      _ <- insert UserCredential
        { userCredentialPartyId      = pid
        , userCredentialUsername     = gpEmail
        , userCredentialPasswordHash = hashed
        , userCredentialActive       = True
        }
      token <- createTokenWithLabel pid (Just ("google-login:" <> gpEmail))
      mUser <- loadAuthedUser token
      case mUser of
        Nothing   -> pure (Left "No pudimos cargar tu perfil.")
        Just user -> pure (Right (toLoginResponse token user))

runLogin :: Text -> Text -> SqlPersistT IO (Either Text LoginResponse)
runLogin identifier pwd = do
  mCred <- lookupCredential identifier
  case mCred of
    Nothing -> pure (Left invalidMsg)
    Just (Entity _ cred)
      | not (userCredentialActive cred) -> pure (Left "Account disabled")
      | otherwise ->
          if validatePassword (TE.encodeUtf8 (userCredentialPasswordHash cred)) (TE.encodeUtf8 pwd)
            then do
              token <- createSessionToken (userCredentialPartyId cred) (userCredentialUsername cred)
              mUser  <- loadAuthedUser token
              case mUser of
                Nothing    -> pure (Left "Failed to load user profile")
                Just user  -> pure (Right (toLoginResponse token user))
            else pure (Left invalidMsg)
  where
    invalidMsg = "Invalid username or password"

lookupCredential :: Text -> SqlPersistT IO (Maybe (Entity UserCredential))
lookupCredential rawIdentifier = do
  let trimmed = T.strip rawIdentifier
  if T.null trimmed
    then pure Nothing
    else do
      byUsername <- getBy (UniqueCredentialUsername trimmed)
      case byUsername of
        Just cred -> pure (Just cred)
        Nothing   -> lookupByEmail trimmed

lookupByEmail :: Text -> SqlPersistT IO (Maybe (Entity UserCredential))
lookupByEmail emailAddress = do
  let query =
        "SELECT ?? FROM user_credential \
        \ JOIN party ON user_credential.party_id = party.id \
        \ WHERE lower(party.primary_email) = lower(?) \
        \ LIMIT 1"
  creds <- rawSql query [PersistText emailAddress]
  pure (listToMaybe creds)

data SignupDbError
  = SignupEmailExists
  | SignupProfileError
  | SignupArtistUnavailable
  deriving (Eq, Show)

signupAllowedRoles :: [RoleEnum]
signupAllowedRoles =
  [ Fan
  , Customer
  , Artist
  , Artista
  , Promotor
  , Promoter
  , Producer
  , Songwriter
  , DJ
  , Publicist
  , TourManager
  , LabelRep
  , StageManager
  , RoadCrew
  , Photographer
  , AandR
  , Intern
  , Student
  , Vendor
  , ReadOnly
  ]

signup :: SignupRequest -> AppM LoginResponse
signup SignupRequest
  { firstName = rawFirst
  , lastName = rawLast
  , email = rawEmail
  , phone = rawPhone
  , password = rawPassword
  , roles = requestedRoles
  , fanArtistIds = requestedFanArtistIds
  , claimArtistId = rawClaimArtistId
  , internshipStartAt = rawInternshipStartAt
  , internshipEndAt = rawInternshipEndAt
  , internshipRequiredHours = rawInternshipRequiredHours
  , internshipSkills = rawInternshipSkills
  , internshipAreas = rawInternshipAreas
  } = do
  let emailClean    = T.strip rawEmail
      passwordClean = T.strip rawPassword
      firstClean    = T.strip rawFirst
      lastClean     = T.strip rawLast
      phoneClean    = fmap T.strip rawPhone
      claimArtistIdClean = rawClaimArtistId >>= (\val -> if val > 0 then Just val else Nothing)
      internshipSkillsClean = cleanOptional rawInternshipSkills
      internshipAreasClean  = cleanOptional rawInternshipAreas
      displayNameText =
        case filter (not . T.null) [firstClean, lastClean] of
          [] -> emailClean
          xs -> T.unwords xs
  when (T.null emailClean) $ throwBadRequest "Email is required"
  when (T.null passwordClean) $ throwBadRequest "Password is required"
  when (T.length passwordClean < 8) $ throwBadRequest "Password must be at least 8 characters"
  when (T.null firstClean && T.null lastClean) $ throwBadRequest "First or last name is required"
  when (maybe False (< 0) rawInternshipRequiredHours) $
    throwBadRequest "Internship required hours must be non-negative"
  now <- liftIO getCurrentTime
  Env pool cfg <- ask
  let emailSvc = EmailSvc.mkEmailService cfg
      allowedRoles = maybe [] (filter (`elem` signupAllowedRoles)) requestedRoles
      sanitizedRoles = nub (Customer : Fan : allowedRoles)
      sanitizedFanArtists = maybe [] (filter (> 0)) requestedFanArtistIds
  result <- liftIO $ flip runSqlPool pool $
    runSignupDb emailClean passwordClean displayNameText phoneClean sanitizedRoles sanitizedFanArtists claimArtistIdClean rawInternshipStartAt rawInternshipEndAt rawInternshipRequiredHours internshipSkillsClean internshipAreasClean now
  case result of
    Left SignupEmailExists ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Account already exists for this email") }
    Left SignupArtistUnavailable ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Artist profile is not available to claim") }
    Left SignupProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Right resp -> do
      liftIO $ EmailSvc.sendWelcome emailSvc displayNameText emailClean emailClean passwordClean
      pure resp
  where
    runSignupDb
      :: Text
      -> Text
      -> Text
      -> Maybe Text
      -> [RoleEnum]
      -> [Int64]
      -> Maybe Int64
      -> Maybe Day
      -> Maybe Day
      -> Maybe Int
      -> Maybe Text
      -> Maybe Text
      -> UTCTime
      -> SqlPersistT IO (Either SignupDbError LoginResponse)
    runSignupDb emailVal passwordVal displayNameText phoneVal rolesVal fanArtistIdsVal mClaimArtistId internStartAt internEndAt internRequiredHours internSkills internAreas nowVal = do
      existing <- getBy (UniqueCredentialUsername emailVal)
      case existing of
        Just _  -> pure (Left SignupEmailExists)
        Nothing -> do
          partyResult <- resolveParty displayNameText mClaimArtistId emailVal phoneVal nowVal
          case partyResult of
            Left err -> pure (Left err)
            Right (pid, partyLabel, existingRoles) -> do
              let rolesWithArtist =
                    if isJust mClaimArtistId && Artist `notElem` rolesVal
                      then Artist : rolesVal
                      else rolesVal
                  rolesToApply = nub (rolesWithArtist ++ existingRoles)
              applyRoles pid rolesToApply
              when (Intern `elem` rolesToApply) $
                upsertInternProfile pid internStartAt internEndAt internRequiredHours internSkills internAreas nowVal
              for_ fanArtistIdsVal $ \artistId -> do
                let artistKey = toSqlKey (fromIntegral artistId) :: Key Party
                when (artistKey /= pid) $
                  void $ insertBy (FanFollow pid artistKey nowVal)
              hashed <- liftIO (hashPasswordText passwordVal)
              _ <- insert UserCredential
                { userCredentialPartyId      = pid
                , userCredentialUsername     = emailVal
                , userCredentialPasswordHash = hashed
                , userCredentialActive       = True
                }
              ensureFanProfileIfMissing pid partyLabel nowVal
              token <- createSessionToken pid emailVal
              mUser <- loadAuthedUser token
              case mUser of
                Nothing   -> pure (Left SignupProfileError)
                Just user -> pure (Right (toLoginResponse token user))
      where
        upsertInternProfile
          :: PartyId
          -> Maybe Day
          -> Maybe Day
          -> Maybe Int
          -> Maybe Text
          -> Maybe Text
          -> UTCTime
          -> SqlPersistT IO ()
        upsertInternProfile pid startAt endAt requiredHours skills areas nowStamp = do
          mProfile <- getBy (ME.UniqueInternProfile pid)
          let updates = catMaybes
                [ fmap (ME.InternProfileStartAt =.) (fmap Just startAt)
                , fmap (ME.InternProfileEndAt =.) (fmap Just endAt)
                , fmap (ME.InternProfileRequiredHours =.) (fmap Just requiredHours)
                , fmap (ME.InternProfileSkills =.) (fmap Just skills)
                , fmap (ME.InternProfileAreas =.) (fmap Just areas)
                ]
          case mProfile of
            Just (Entity key _) ->
              unless (null updates) $
                update key (updates ++ [ME.InternProfileUpdatedAt =. nowStamp])
            Nothing -> do
              _ <- insert ME.InternProfile
                { ME.internProfilePartyId   = pid
                , ME.internProfileStartAt   = startAt
                , ME.internProfileEndAt     = endAt
                , ME.internProfileRequiredHours = requiredHours
                , ME.internProfileSkills    = skills
                , ME.internProfileAreas     = areas
                , ME.internProfileCreatedAt = nowStamp
                , ME.internProfileUpdatedAt = nowStamp
                }
              pure ()

    resolveParty
      :: Text
      -> Maybe Int64
      -> Text
      -> Maybe Text
      -> UTCTime
      -> SqlPersistT IO (Either SignupDbError (PartyId, Text, [RoleEnum]))
    resolveParty displayNameText Nothing emailVal phoneVal nowVal = do
      let partyRecord = Party
            { partyLegalName        = Nothing
            , partyDisplayName      = displayNameText
            , partyIsOrg            = False
            , partyTaxId            = Nothing
            , partyPrimaryEmail     = Just emailVal
            , partyPrimaryPhone     = phoneVal
            , partyWhatsapp         = Nothing
            , partyInstagram        = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes            = Nothing
            , partyCreatedAt        = nowVal
            }
      pid <- insert partyRecord
      pure (Right (pid, displayNameText, []))

    resolveParty _ (Just artistId) emailVal phoneVal _ = do
      let artistKey = toSqlKey (fromIntegral artistId) :: Key Party
      mProfile <- getBy (UniqueArtistProfile artistKey)
      case mProfile of
        Nothing -> pure (Left SignupArtistUnavailable)
        Just _ -> do
          existingAccount <- selectFirst [UserCredentialPartyId ==. artistKey] []
          case existingAccount of
            Just _  -> pure (Left SignupArtistUnavailable)
            Nothing -> do
              mArtistParty <- getEntity artistKey
              case mArtistParty of
                Nothing -> pure (Left SignupArtistUnavailable)
                Just (Entity _ party) -> do
                  let normalizedPhone = cleanOptional phoneVal
                      normalizedEmail = Just emailVal
                      emailMissing = maybe True T.null (M.partyPrimaryEmail party)
                      updates =
                        [PartyPrimaryEmail =. normalizedEmail | emailMissing]
                        ++ [PartyPrimaryPhone =. normalizedPhone | isNothing (M.partyPrimaryPhone party), isJust normalizedPhone]
                  unless (null updates) $
                    update artistKey updates
                  activeRoles <- selectList [PartyRolePartyId ==. artistKey, PartyRoleActive ==. True] []
                  let existingRoles = map (partyRoleRole . entityVal) activeRoles
                      label = M.partyDisplayName party
                  pure (Right (artistKey, label, existingRoles))

data PasswordChangeError
  = PasswordInvalid
  | PasswordAccountDisabled
  | PasswordProfileError
  deriving (Eq, Show)

changePassword :: Maybe Text -> ChangePasswordRequest -> AppM LoginResponse
changePassword mAuthHeader ChangePasswordRequest{..} = do
  let currentPasswordClean = T.strip currentPassword
      newPasswordClean     = T.strip newPassword
      maybeUsernameClean   = T.strip <$> username
  when (T.null currentPasswordClean) $ throwBadRequest "Current password is required"
  when (T.null newPasswordClean) $ throwBadRequest "New password is required"
  when (T.length newPasswordClean < 8) $
    throwBadRequest "New password must be at least 8 characters"
  Env pool _ <- ask
  usernameClean <- resolveUsername pool maybeUsernameClean mAuthHeader
  when (T.null usernameClean) $ throwBadRequest "Username is required"
  result <- liftIO $ flip runSqlPool pool $
    runChangePassword usernameClean currentPasswordClean newPasswordClean
  case result of
    Left PasswordInvalid ->
      throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 "Invalid username or password") }
    Left PasswordAccountDisabled ->
      throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Account disabled") }
    Left PasswordProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Right resp -> pure resp
  where
    resolveUsername pool mUsername header =
      case mUsername of
        Just uname | not (T.null uname) -> pure uname
        _ -> do
          token <- case header >>= parseBearer . T.strip of
            Nothing  -> throwBadRequest "Username is required"
            Just tok -> pure tok
          mResolved <- liftIO $ flip runSqlPool pool (lookupUsernameFromToken token)
          case fmap T.strip mResolved of
            Nothing     -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 "Invalid or inactive session token") }
            Just uname' -> pure uname'

    parseBearer headerText =
      case T.words headerText of
        [scheme, value]
          | T.toLower scheme == "bearer" -> Just value
        [value] -> Just value
        _       -> Nothing

    lookupUsernameFromToken token = do
      mUser <- loadAuthedUser token
      case mUser of
        Nothing -> pure Nothing
        Just AuthedUser{..} -> do
          mCred <- selectFirst [UserCredentialPartyId ==. auPartyId, UserCredentialActive ==. True] []
          pure (fmap (userCredentialUsername . entityVal) mCred)

    runChangePassword
      :: Text
      -> Text
      -> Text
      -> SqlPersistT IO (Either PasswordChangeError LoginResponse)
    runChangePassword uname currentPwd newPwd = do
      mCred <- getBy (UniqueCredentialUsername uname)
      case mCred of
        Nothing -> pure (Left PasswordInvalid)
        Just (Entity credId cred)
          | not (userCredentialActive cred) -> pure (Left PasswordAccountDisabled)
          | not (validatePassword (TE.encodeUtf8 (userCredentialPasswordHash cred)) (TE.encodeUtf8 currentPwd)) ->
              pure (Left PasswordInvalid)
          | otherwise -> do
              hashed <- liftIO (hashPasswordText newPwd)
              update credId [UserCredentialPasswordHash =. hashed]
              deactivatePasswordTokens (userCredentialPartyId cred)
              token <- createSessionToken (userCredentialPartyId cred) uname
              mResolved <- lookupUsernameFromToken token
              case mResolved of
                Nothing -> pure (Left PasswordProfileError)
                Just _  -> do
                  mUser <- loadAuthedUser token
                  case mUser of
                    Nothing   -> pure (Left PasswordProfileError)
                    Just user -> pure (Right (toLoginResponse token user))

data PasswordResetError
  = PasswordResetInvalidToken
  | PasswordResetAccountDisabled
  | PasswordResetProfileError
  deriving (Eq, Show)

passwordReset :: PasswordResetRequest -> AppM NoContent
passwordReset PasswordResetRequest{..} = do
  let emailClean = T.strip email
  when (T.null emailClean) $ throwBadRequest "Email is required"
  Env pool cfg <- ask
  let emailSvc = EmailSvc.mkEmailService cfg
  mPayload <- liftIO $ flip runSqlPool pool (runPasswordReset emailClean)
  for_ mPayload $ \(token, displayName) -> liftIO $
    EmailSvc.sendPasswordReset emailSvc displayName emailClean token
  pure NoContent
  where
    runPasswordReset :: Text -> SqlPersistT IO (Maybe (Text, Text))
    runPasswordReset emailVal = do
      mCred <- getBy (UniqueCredentialUsername emailVal)
      case mCred of
        Nothing -> pure Nothing
        Just (Entity _ cred)
          | not (userCredentialActive cred) -> pure Nothing
          | otherwise -> do
              deactivatePasswordResetTokens (userCredentialPartyId cred)
              token <- createPasswordResetToken (userCredentialPartyId cred) emailVal
              mParty <- get (userCredentialPartyId cred)
              let displayName =
                    maybe emailVal M.partyDisplayName mParty
              pure (Just (token, displayName))

passwordResetConfirm :: PasswordResetConfirmRequest -> AppM LoginResponse
passwordResetConfirm PasswordResetConfirmRequest{..} = do
  let tokenClean       = T.strip token
      newPasswordClean = T.strip newPassword
  when (T.null tokenClean) $ throwBadRequest "Token is required"
  when (T.null newPasswordClean) $ throwBadRequest "New password is required"
  when (T.length newPasswordClean < 8) $
    throwBadRequest "New password must be at least 8 characters"
  Env pool _ <- ask
  result <- liftIO $ flip runSqlPool pool (runPasswordResetConfirm tokenClean newPasswordClean)
  case result of
    Left PasswordResetInvalidToken ->
      throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 "Invalid or expired token") }
    Left PasswordResetAccountDisabled ->
      throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Account disabled") }
    Left PasswordResetProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Right resp -> pure resp
  where
    runPasswordResetConfirm
      :: Text
      -> Text
      -> SqlPersistT IO (Either PasswordResetError LoginResponse)
    runPasswordResetConfirm tokenVal passwordVal = do
      mToken <- getBy (UniqueApiToken tokenVal)
      case mToken of
        Nothing -> pure (Left PasswordResetInvalidToken)
        Just (Entity tokenId apiToken)
          | not (apiTokenActive apiToken) -> pure (Left PasswordResetInvalidToken)
          | not (isResetToken (apiTokenLabel apiToken)) -> pure (Left PasswordResetInvalidToken)
          | otherwise -> do
              let mResetUsername = do
                    labelText <- apiTokenLabel apiToken
                    resetTokenUsername labelText
              case mResetUsername of
                Nothing -> pure (Left PasswordResetInvalidToken)
                Just resetUsername -> do
                  mCred <- getBy (UniqueCredentialUsername resetUsername)
                  case mCred of
                    Nothing -> pure (Left PasswordResetInvalidToken)
                    Just (Entity credId cred)
                      | userCredentialPartyId cred /= apiTokenPartyId apiToken ->
                          pure (Left PasswordResetInvalidToken)
                      | not (userCredentialActive cred) -> pure (Left PasswordResetAccountDisabled)
                      | otherwise -> do
                          hashed <- liftIO (hashPasswordText passwordVal)
                          update credId [UserCredentialPasswordHash =. hashed]
                          update tokenId [ApiTokenActive =. False]
                          deactivatePasswordTokens (userCredentialPartyId cred)
                          deactivatePasswordResetTokens (userCredentialPartyId cred)
                          sessionToken <- createSessionToken (userCredentialPartyId cred) (userCredentialUsername cred)
                          mUser <- loadAuthedUser sessionToken
                          case mUser of
                            Nothing   -> pure (Left PasswordResetProfileError)
                            Just user -> pure (Right (toLoginResponse sessionToken user))

    isResetToken :: Maybe Text -> Bool
    isResetToken Nothing = False
    isResetToken (Just lbl) = "password-reset:" `T.isPrefixOf` lbl

    resetTokenUsername :: Text -> Maybe Text
    resetTokenUsername lbl =
      let prefix = "password-reset:"
      in T.stripPrefix prefix lbl >>= nonEmptyTextLocal

    nonEmptyTextLocal :: Text -> Maybe Text
    nonEmptyTextLocal txt
      | T.null (T.strip txt) = Nothing
      | otherwise = Just (T.strip txt)

fanGetProfile :: AuthedUser -> AppM FanProfileDTO
fanGetProfile user = do
  requireFanAccess user
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ loadFanProfileDTO (auPartyId user)

fanUpdateProfile :: AuthedUser -> FanProfileUpdate -> AppM FanProfileDTO
fanUpdateProfile user FanProfileUpdate{..} = do
  requireFanAccess user
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ do
    let fanId = auPartyId user
    _ <- upsert FanProfile
      { fanProfileFanPartyId     = fanId
      , fanProfileDisplayName    = fpuDisplayName
      , fanProfileAvatarUrl      = fpuAvatarUrl
      , fanProfileFavoriteGenres = fpuFavoriteGenres
      , fanProfileBio            = fpuBio
      , fanProfileCity           = fpuCity
      , fanProfileCreatedAt      = now
      , fanProfileUpdatedAt      = Just now
      }
      [ FanProfileDisplayName    =. fpuDisplayName
      , FanProfileAvatarUrl      =. fpuAvatarUrl
      , FanProfileFavoriteGenres =. fpuFavoriteGenres
      , FanProfileBio            =. fpuBio
      , FanProfileCity           =. fpuCity
      , FanProfileUpdatedAt      =. Just now
      ]
    loadFanProfileDTO fanId

fanListFollows :: AuthedUser -> AppM [FanFollowDTO]
fanListFollows user = do
  requireFanAccess user
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    follows <- selectList [FanFollowFanPartyId ==. auPartyId user] [Desc FanFollowCreatedAt]
    let artistIds = map (fanFollowArtistPartyId . entityVal) follows
    nameMap <- fetchPartyNameMap artistIds
    profileMap <- fetchArtistProfileMap artistIds
    pure (map (fanFollowEntityToDTO nameMap profileMap) follows)

fanFollowArtist :: AuthedUser -> Int64 -> AppM FanFollowDTO
fanFollowArtist user artistId = do
  requireFanAccess user
  when (artistId <= 0) $ throwBadRequest "Invalid artist id"
  let artistKey = toSqlKey artistId :: PartyId
      fanKey    = auPartyId user
  when (artistKey == fanKey) $
    throwBadRequest "No puedes seguirte a ti mismo"
  Env pool _ <- ask
  mDto <- liftIO $ flip runSqlPool pool $ do
    mArtist <- get artistKey
    case mArtist of
      Nothing -> pure Nothing
      Just _ -> do
        now <- liftIO getCurrentTime
        _ <- insertUnique FanFollow
          { fanFollowFanPartyId    = fanKey
          , fanFollowArtistPartyId = artistKey
          , fanFollowCreatedAt     = now
          }
        loadFanFollowDTO fanKey artistKey
  maybe (throwError err404) pure mDto

fanUnfollowArtist :: AuthedUser -> Int64 -> AppM NoContent
fanUnfollowArtist user artistId = do
  requireFanAccess user
  let artistKey = toSqlKey artistId :: PartyId
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $
    deleteBy (UniqueFanFollow (auPartyId user) artistKey)
  pure NoContent

-- Chat (1:1 DM between parties)

normalizeDmPair :: PartyId -> PartyId -> (PartyId, PartyId)
normalizeDmPair a b =
  if fromSqlKey a <= fromSqlKey b
    then (a, b)
    else (b, a)

ensureCanChatWith :: AuthedUser -> PartyId -> AppM ()
ensureCanChatWith user otherPartyId = do
  if hasRole Admin user
    then pure ()
    else do
      let me = auPartyId user
      isMutual <- runDB $ do
        mAB <- getBy (UniquePartyFollow me otherPartyId)
        mBA <- getBy (UniquePartyFollow otherPartyId me)
        pure (isJust mAB && isJust mBA)
      unless isMutual $
        throwError err403
          { errBody = BL.fromStrict (TE.encodeUtf8 ("Solo puedes chatear con amigos mutuos." :: Text)) }

requireChatThreadParticipant :: AuthedUser -> ChatThread -> AppM ()
requireChatThreadParticipant user ChatThread{chatThreadDmPartyA, chatThreadDmPartyB} = do
  let me = auPartyId user
  unless (me == chatThreadDmPartyA || me == chatThreadDmPartyB) $
    throwError err404

chatThreadToDTO
  :: PartyId
  -> ChatThreadId
  -> ChatThread
  -> Maybe Party
  -> Maybe (Entity ChatMessage)
  -> ChatThreadDTO
chatThreadToDTO otherKey threadId ChatThread{chatThreadUpdatedAt} mOtherParty mLastMessage =
  let otherName = maybe "Unknown" M.partyDisplayName mOtherParty
      (lastBody, lastAt) =
        case mLastMessage of
          Nothing -> (Nothing, Nothing)
          Just (Entity _ msg) -> (Just (chatMessageBody msg), Just (chatMessageCreatedAt msg))
  in ChatThreadDTO
      { ctThreadId = fromSqlKey threadId
      , ctOtherPartyId = fromSqlKey otherKey
      , ctOtherDisplayName = otherName
      , ctLastMessage = lastBody
      , ctLastMessageAt = lastAt
      , ctUpdatedAt = chatThreadUpdatedAt
      }

chatMessageToDTO :: Entity ChatMessage -> ChatMessageDTO
chatMessageToDTO (Entity mid ChatMessage{..}) =
  ChatMessageDTO
    { cmId = fromSqlKey mid
    , cmThreadId = fromSqlKey chatMessageThreadId
    , cmSenderPartyId = fromSqlKey chatMessageSenderPartyId
    , cmBody = chatMessageBody
    , cmCreatedAt = chatMessageCreatedAt
    }

chatListThreads :: AuthedUser -> AppM [ChatThreadDTO]
chatListThreads user = do
  let me = auPartyId user
  threads <-
    runDB $
      selectList
        ([ChatThreadDmPartyA ==. me] ||. [ChatThreadDmPartyB ==. me])
        [Desc ChatThreadUpdatedAt, Desc ChatThreadId]
  forM threads $ \(Entity tid thread) -> do
    let otherKey =
          if chatThreadDmPartyA thread == me
            then chatThreadDmPartyB thread
            else chatThreadDmPartyA thread
    mOther <- runDB $ get otherKey
    mLast <- runDB $ selectFirst [ChatMessageThreadId ==. tid] [Desc ChatMessageId]
    pure (chatThreadToDTO otherKey tid thread mOther mLast)

chatGetOrCreateDM :: AuthedUser -> Int64 -> AppM ChatThreadDTO
chatGetOrCreateDM user otherPartyId = do
  when (otherPartyId <= 0) $ throwBadRequest "Invalid party id"
  let me = auPartyId user
      otherKey = toSqlKey otherPartyId :: PartyId
  when (me == otherKey) $
    throwBadRequest "No puedes chatear contigo mismo"
  mOther <- runDB $ get otherKey
  when (isNothing mOther) $ throwError err404
  ensureCanChatWith user otherKey
  let (a, b) = normalizeDmPair me otherKey
  now <- liftIO getCurrentTime
  existing <- runDB $ getBy (UniqueChatThread a b)
  case existing of
    Just (Entity tid thread) -> do
      mLast <- runDB $ selectFirst [ChatMessageThreadId ==. tid] [Desc ChatMessageId]
      pure (chatThreadToDTO otherKey tid thread mOther mLast)
    Nothing -> do
      let thread = ChatThread
            { chatThreadDmPartyA = a
            , chatThreadDmPartyB = b
            , chatThreadCreatedAt = now
            , chatThreadUpdatedAt = now
            }
      tid <- runDB $ insert thread
      pure (chatThreadToDTO otherKey tid thread mOther Nothing)

chatListMessages :: AuthedUser -> Int64 -> Maybe Int -> Maybe Int64 -> Maybe Int64 -> AppM [ChatMessageDTO]
chatListMessages user threadId mLimit mBeforeId mAfterId = do
  let limit = fromMaybe 50 mLimit
  when (limit < 1 || limit > 200) $
    throwBadRequest "limit must be between 1 and 200"
  when (isJust mBeforeId && isJust mAfterId) $
    throwBadRequest "Use either beforeId or afterId"
  let tid = toSqlKey threadId :: ChatThreadId
  mThread <- runDB $ get tid
  thread <-
    case mThread of
      Nothing -> throwError err404
      Just t  -> pure t
  requireChatThreadParticipant user thread
  let baseFilters = [ChatMessageThreadId ==. tid]
  messages <- runDB $
    case (mBeforeId, mAfterId) of
      (Just beforeId, Nothing) -> do
        let beforeKey = toSqlKey beforeId :: ChatMessageId
        rows <- selectList (baseFilters ++ [ChatMessageId <. beforeKey]) [Desc ChatMessageId, LimitTo limit]
        pure (reverse rows)
      (Nothing, Just afterId) -> do
        let afterKey = toSqlKey afterId :: ChatMessageId
        selectList (baseFilters ++ [ChatMessageId >. afterKey]) [Asc ChatMessageId, LimitTo limit]
      _ -> do
        rows <- selectList baseFilters [Desc ChatMessageId, LimitTo limit]
        pure (reverse rows)
  pure (map chatMessageToDTO messages)

chatSendMessage :: AuthedUser -> Int64 -> ChatSendMessageRequest -> AppM ChatMessageDTO
chatSendMessage user threadId ChatSendMessageRequest{..} = do
  let bodyClean = T.strip csmBody
  when (T.null bodyClean) $
    throwBadRequest "Mensaje vacío"
  when (T.length bodyClean > 5000) $
    throwBadRequest "Mensaje demasiado largo (max 5000 caracteres)"
  let tid = toSqlKey threadId :: ChatThreadId
      me  = auPartyId user
  mThread <- runDB $ get tid
  thread <-
    case mThread of
      Nothing -> throwError err404
      Just t  -> pure t
  requireChatThreadParticipant user thread
  let otherKey =
        if chatThreadDmPartyA thread == me
          then chatThreadDmPartyB thread
          else chatThreadDmPartyA thread
  ensureCanChatWith user otherKey
  now <- liftIO getCurrentTime
  mid <- runDB $ do
    mid' <- insert ChatMessage
      { chatMessageThreadId = tid
      , chatMessageSenderPartyId = me
      , chatMessageBody = bodyClean
      , chatMessageCreatedAt = now
      }
    update tid [ChatThreadUpdatedAt =. now]
    pure mid'
  pure
    ChatMessageDTO
      { cmId = fromSqlKey mid
      , cmThreadId = fromSqlKey tid
      , cmSenderPartyId = fromSqlKey me
      , cmBody = bodyClean
      , cmCreatedAt = now
      }

-- Social graph (party <-> party follows)
socialListFollowers :: AuthedUser -> AppM [PartyFollowDTO]
socialListFollowers user = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    rows <- selectList [PartyFollowFollowingPartyId ==. auPartyId user] [Desc PartyFollowCreatedAt]
    pure (map partyFollowEntityToDTO rows)

socialListFollowing :: AuthedUser -> AppM [PartyFollowDTO]
socialListFollowing user = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    rows <- selectList [PartyFollowFollowerPartyId ==. auPartyId user] [Desc PartyFollowCreatedAt]
    pure (map partyFollowEntityToDTO rows)

socialListFriends :: AuthedUser -> AppM [PartyFollowDTO]
socialListFriends user = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    following <- selectList [PartyFollowFollowerPartyId ==. auPartyId user] []
    let otherIds = map (partyFollowFollowingPartyId . entityVal) following
    reverseRows <- if null otherIds
      then pure []
      else selectList
             [ PartyFollowFollowerPartyId <-. otherIds
             , PartyFollowFollowingPartyId ==. auPartyId user
             ] []
    let mutualIds = Set.fromList (map (partyFollowFollowerPartyId . entityVal) reverseRows)
        mutual = filter (\(Entity _ pf) -> partyFollowFollowingPartyId pf `Set.member` mutualIds) following
    pure (map partyFollowEntityToDTO mutual)

-- Suggest friends-of-friends that are not already connected to the user.
socialListSuggestedFriends :: AuthedUser -> AppM [SuggestedFriendDTO]
socialListSuggestedFriends user = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    following <- selectList [PartyFollowFollowerPartyId ==. auPartyId user] []
    followers <- selectList [PartyFollowFollowingPartyId ==. auPartyId user] []
    let followingIds = map (partyFollowFollowingPartyId . entityVal) following
        followerIds  = map (partyFollowFollowerPartyId . entityVal) followers
        directSet    = Set.fromList (auPartyId user : followingIds ++ followerIds)
        seeds        = Set.toList (Set.delete (auPartyId user) directSet)
    if null seeds
      then pure []
      else do
        secondDegree <- selectList [PartyFollowFollowerPartyId <-. seeds] []
        let counts = foldl' (\acc (Entity _ pf) ->
                              let candidate = partyFollowFollowingPartyId pf
                              in if Set.member candidate directSet
                                   then acc
                                   else Map.insertWith (+) candidate 1 acc
                            ) Map.empty secondDegree
            ordered = take 20 $ sortOn (Down . snd) (Map.toList counts)
        pure
          [ SuggestedFriendDTO
              { sfPartyId = fromSqlKey candidateId
              , sfMutualCount = mutuals
              }
          | (candidateId, mutuals) <- ordered
          ]

socialAddFriend :: AuthedUser -> Int64 -> AppM [PartyFollowDTO]
socialAddFriend user targetId = do
  when (targetId <= 0) $ throwBadRequest "Invalid party id"
  let followerKey = auPartyId user
      targetKey   = toSqlKey targetId :: PartyId
  when (followerKey == targetKey) $
    throwBadRequest "No puedes agregarte como amigo"
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    mTarget <- get targetKey
    case mTarget of
      Nothing -> pure []
      Just _ -> do
        now <- liftIO getCurrentTime
        _ <- upsert PartyFollow
          { partyFollowFollowerPartyId  = followerKey
          , partyFollowFollowingPartyId = targetKey
          , partyFollowViaNfc           = False
          , partyFollowCreatedAt        = now
          }
          [ PartyFollowViaNfc =. False ]
        _ <- upsert PartyFollow
          { partyFollowFollowerPartyId  = targetKey
          , partyFollowFollowingPartyId = followerKey
          , partyFollowViaNfc           = False
          , partyFollowCreatedAt        = now
          }
          [ PartyFollowViaNfc =. False ]
        rows <- selectList
          [ PartyFollowFollowerPartyId ==. followerKey
          , PartyFollowFollowingPartyId ==. targetKey
          ] []
        pure (map partyFollowEntityToDTO rows)

socialRemoveFriend :: AuthedUser -> Int64 -> AppM NoContent
socialRemoveFriend user targetId = do
  when (targetId <= 0) $ throwBadRequest "Invalid party id"
  let followerKey = auPartyId user
      targetKey   = toSqlKey targetId :: PartyId
  when (followerKey == targetKey) $
    throwBadRequest "No puedes eliminarte como amigo"
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    deleteBy (UniquePartyFollow followerKey targetKey)
    deleteBy (UniquePartyFollow targetKey followerKey)
  pure NoContent

vcardExchange :: AuthedUser -> VCardExchangeRequest -> AppM [PartyFollowDTO]
vcardExchange user VCardExchangeRequest{..} = do
  when (vcerPartyId <= 0) $ throwBadRequest "Invalid party id"
  let followerKey = auPartyId user
      targetKey   = toSqlKey vcerPartyId :: PartyId
  when (followerKey == targetKey) $
    throwBadRequest "No puedes compartir tu vCard contigo mismo"
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    mTarget <- get targetKey
    case mTarget of
      Nothing -> pure []
      Just _ -> do
        now <- liftIO getCurrentTime
        -- Create mutual follows; mark as NFC-sourced.
        _ <- upsert PartyFollow
          { partyFollowFollowerPartyId  = followerKey
          , partyFollowFollowingPartyId = targetKey
          , partyFollowViaNfc           = True
          , partyFollowCreatedAt        = now
          }
          [ PartyFollowViaNfc =. True ]
        _ <- upsert PartyFollow
          { partyFollowFollowerPartyId  = targetKey
          , partyFollowFollowingPartyId = followerKey
          , partyFollowViaNfc           = True
          , partyFollowCreatedAt        = now
          }
          [ PartyFollowViaNfc =. True ]
        rowsAB <- selectList
          [ PartyFollowFollowerPartyId ==. followerKey
          , PartyFollowFollowingPartyId ==. targetKey
          ] [Desc PartyFollowCreatedAt]
        rowsBA <- selectList
          [ PartyFollowFollowerPartyId ==. targetKey
          , PartyFollowFollowingPartyId ==. followerKey
          ] [Desc PartyFollowCreatedAt]
        pure (map partyFollowEntityToDTO (rowsAB ++ rowsBA))

requireFanAccess :: AuthedUser -> AppM ()
requireFanAccess AuthedUser{..} =
  unless (Fan `elem` auRoles || Customer `elem` auRoles) $
    throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Fan access required") }

requireArtistAccess :: AuthedUser -> AppM ()
requireArtistAccess AuthedUser{..} =
  unless (Artist `elem` auRoles || Artista `elem` auRoles || Admin `elem` auRoles) $
    throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Artist access required") }

artistGetOwnProfile :: AuthedUser -> AppM ArtistProfileDTO
artistGetOwnProfile user = do
  requireArtistAccess user
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ loadOrCreateArtistProfileDTO (auPartyId user)

artistUpdateOwnProfile :: AuthedUser -> ArtistProfileUpsert -> AppM ArtistProfileDTO
artistUpdateOwnProfile user payload = do
  requireArtistAccess user
  let artistKey = auPartyId user
      sanitized = payload { apuArtistId = fromSqlKey artistKey }
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ upsertArtistProfileRecord artistKey sanitized now

loadFanProfileDTO :: PartyId -> SqlPersistT IO FanProfileDTO
loadFanProfileDTO fanId = do
  mProfile <- getBy (UniqueFanProfile fanId)
  party <- get fanId
  now <- liftIO getCurrentTime
  profileEntity <- case mProfile of
    Nothing -> do
      let displayName = M.partyDisplayName <$> party
          record = FanProfile
            { fanProfileFanPartyId     = fanId
            , fanProfileDisplayName    = displayName
            , fanProfileAvatarUrl      = Nothing
            , fanProfileFavoriteGenres = Nothing
            , fanProfileBio            = Nothing
            , fanProfileCity           = Nothing
            , fanProfileCreatedAt      = now
            , fanProfileUpdatedAt      = Nothing
            }
      key <- insert record
      pure (Entity key record)
    Just ent -> pure ent
  let fallbackName = maybe Nothing (Just . M.partyDisplayName) party
  pure (fanProfileToDTO fallbackName (entityVal profileEntity))

partyFollowEntityToDTO :: Entity PartyFollow -> PartyFollowDTO
partyFollowEntityToDTO (Entity _ pf) =
  PartyFollowDTO
    { pfFollowerId  = fromSqlKey (partyFollowFollowerPartyId pf)
    , pfFollowingId = fromSqlKey (partyFollowFollowingPartyId pf)
    , pfViaNfc      = partyFollowViaNfc pf
    , pfStartedAt   = utctDay (partyFollowCreatedAt pf)
    }


toArtistReleaseDTO :: Entity ArtistRelease -> ArtistReleaseDTO
toArtistReleaseDTO (Entity releaseId release) =
  ArtistReleaseDTO
    { arArtistId      = fromSqlKey (artistReleaseArtistPartyId release)
    , arReleaseId     = fromSqlKey releaseId
    , arTitle         = artistReleaseTitle release
    , arReleaseDate   = artistReleaseReleaseDate release
    , arDescription   = artistReleaseDescription release
    , arCoverImageUrl = artistReleaseCoverImageUrl release
    , arSpotifyUrl    = artistReleaseSpotifyUrl release
    , arYoutubeUrl    = artistReleaseYoutubeUrl release
    }

fanProfileToDTO :: Maybe Text -> FanProfile -> FanProfileDTO
fanProfileToDTO fallback FanProfile{..} = FanProfileDTO
  { fpArtistId      = fromSqlKey fanProfileFanPartyId
  , fpDisplayName   = fanProfileDisplayName <|> fallback
  , fpAvatarUrl     = fanProfileAvatarUrl
  , fpFavoriteGenres = fanProfileFavoriteGenres
  , fpBio           = fanProfileBio
  , fpCity          = fanProfileCity
  }

fanFollowEntityToDTO
  :: Map.Map PartyId Text
  -> Map.Map PartyId ArtistProfile
  -> Entity FanFollow
  -> FanFollowDTO
fanFollowEntityToDTO nameMap profileMap (Entity _ follow) =
  let artistId = fanFollowArtistPartyId follow
      profile = Map.lookup artistId profileMap
  in FanFollowDTO
      { ffArtistId     = fromSqlKey artistId
      , ffArtistName   = Map.findWithDefault "Artista" artistId nameMap
      , ffHeroImageUrl = profile >>= artistProfileHeroImageUrl
      , ffSpotifyUrl   = profile >>= artistProfileSpotifyUrl
      , ffYoutubeUrl   = profile >>= artistProfileYoutubeUrl
      , ffStartedAt    = utctDay (fanFollowCreatedAt follow)
      }

loadFanFollowDTO :: PartyId -> PartyId -> SqlPersistT IO (Maybe FanFollowDTO)
loadFanFollowDTO fanId artistId = do
  mFollow <- selectFirst [FanFollowFanPartyId ==. fanId, FanFollowArtistPartyId ==. artistId] []
  case mFollow of
    Nothing -> pure Nothing
    Just followEnt -> do
      nameMap <- fetchPartyNameMap [artistId]
      profileMap <- fetchArtistProfileMap [artistId]
      pure $ Just (fanFollowEntityToDTO nameMap profileMap followEnt)

toLoginResponse :: Text -> AuthedUser -> LoginResponse
toLoginResponse token AuthedUser{..} = LoginResponse
  { token   = token
  , partyId = fromSqlKey auPartyId
  , roles   = auRoles
  , modules = map moduleName (Set.toList auModules)
  }

ensureFanProfileIfMissing :: PartyId -> Text -> UTCTime -> SqlPersistT IO ()
ensureFanProfileIfMissing pid label nowVal = do
  mProfile <- getBy (UniqueFanProfile pid)
  case mProfile of
    Just _ -> pure ()
    Nothing -> insert_ FanProfile
      { fanProfileFanPartyId     = pid
      , fanProfileDisplayName    = Just label
      , fanProfileAvatarUrl      = Nothing
      , fanProfileFavoriteGenres = Nothing
      , fanProfileBio            = Nothing
      , fanProfileCity           = Nothing
      , fanProfileCreatedAt      = nowVal
      , fanProfileUpdatedAt      = Nothing
      }

generateTemporaryPassword :: IO Text
generateTemporaryPassword = do
  randomUuid <- nextRandom
  pure ("google-" <> toText randomUuid)

createSessionToken :: PartyId -> Text -> SqlPersistT IO Text
createSessionToken pid uname =
  createTokenWithLabel pid (Just ("password-login:" <> uname))

createPasswordResetToken :: PartyId -> Text -> SqlPersistT IO Text
createPasswordResetToken pid emailVal =
  createTokenWithLabel pid (Just ("password-reset:" <> emailVal))

createTokenWithLabel :: PartyId -> Maybe Text -> SqlPersistT IO Text
createTokenWithLabel pid label = do
  token <- liftIO (toText <$> nextRandom)
  inserted <- insertUnique (ApiToken token pid label True)
  case inserted of
    Nothing -> createTokenWithLabel pid label
    Just _  -> pure token

deactivatePasswordTokens :: PartyId -> SqlPersistT IO ()
deactivatePasswordTokens pid = do
  tokens <- selectList [ApiTokenPartyId ==. pid, ApiTokenActive ==. True] []
  forM_ tokens $ \(Entity tokenId tok) ->
    case apiTokenLabel tok of
      Just lbl | "password-login:" `T.isPrefixOf` lbl ->
        update tokenId [ApiTokenActive =. False]
      _ -> pure ()

deactivatePasswordResetTokens :: PartyId -> SqlPersistT IO ()
deactivatePasswordResetTokens pid = do
  tokens <- selectList [ApiTokenPartyId ==. pid, ApiTokenActive ==. True] []
  forM_ tokens $ \(Entity tokenId tok) ->
    case apiTokenLabel tok of
      Just lbl | "password-reset:" `T.isPrefixOf` lbl ->
        update tokenId [ApiTokenActive =. False]
      _ -> pure ()

metaServer :: ServerT Meta.MetaAPI AppM
metaServer = hoistServer metaProxy lift Meta.metaServer
  where
    metaProxy = Proxy :: Proxy Meta.MetaAPI

academyServer :: ServerT AcademyAPI AppM
academyServer =
       enrollH
  :<|> microcourseH
  :<|> progressH
  :<|> claimReferralH
  :<|> nextCohortH
  where
    enrollH EnrollReq{..} = do
      normalizedEmail <- requireEmail email
      normalizedRole  <- requireRole role
      let platformClean = cleanOptional platform
      user <- upsertAcademyUser normalizedEmail normalizedRole platformClean
      for_ (cleanOptional referralCode) $ \codeTxt ->
        claimReferral normalizedEmail (Just (entityKey user)) codeTxt
      pure NoContent

    microcourseH rawSlug = do
      slug <- requireSlug rawSlug
      mCourse <- runDB $ getBy (UniqueAcademyMicrocourseSlug slug)
      case mCourse of
        Nothing -> throwNotFound "Microcurso no disponible"
        Just (Entity courseId course) -> do
          lessons <- runDB $ selectList
            [AcademyLessonMicrocourseId ==. courseId]
            [Asc AcademyLessonDay]
          pure MicrocourseDTO
            { mcSlug    = academyMicrocourseSlug course
            , mcTitle   = academyMicrocourseTitle course
            , mcSummary = academyMicrocourseSummary course
            , lessons   = map lessonToDTO lessons
            }

    progressH ProgressReq{..} = do
      normalizedEmail <- requireEmail email
      courseSlug <- requireSlug slug
      dayNumber <- requireDay day
      mUser <- runDB $ getBy (UniqueAcademyUserEmail normalizedEmail)
      user <- maybe (throwNotFound "Usuario no inscrito") pure mUser
      mCourse <- runDB $ getBy (UniqueAcademyMicrocourseSlug courseSlug)
      course <- maybe (throwNotFound "Microcurso no disponible") pure mCourse
      mLesson <- runDB $ selectFirst
        [ AcademyLessonMicrocourseId ==. entityKey course
        , AcademyLessonDay ==. dayNumber
        ]
        []
      lesson <- maybe (throwNotFound "Lección no encontrada") pure mLesson
      now <- liftIO getCurrentTime
      void $ runDB $ upsert
        (AcademyProgress (entityKey user) (entityKey lesson) now)
        [AcademyProgressCompletedAt =. now]
      pure NoContent

    claimReferralH ReferralClaimReq{..} = do
      normalizedEmail <- requireEmail email
      mUser <- runDB $ getBy (UniqueAcademyUserEmail normalizedEmail)
      claimReferral normalizedEmail (entityKey <$> mUser) code
      pure NoContent

    nextCohortH = do
      now <- liftIO getCurrentTime
      mCohort <- runDB $ selectFirst
        [CohortEndsAt >=. now]
        [Asc CohortStartsAt]
      cohortEnt <- maybe (throwNotFound "Sin cohortes activas") pure mCohort
      seatsTaken <- runDB $ count [CohortEnrollmentCohortId ==. entityKey cohortEnt]
      let cohort = entityVal cohortEnt
          remaining = max 0 (cohortSeatCap cohort - seatsTaken)
      pure NextCohortDTO
        { nextSlug      = cohortSlug cohort
        , nextTitle     = cohortTitle cohort
        , nextStartsAt  = cohortStartsAt cohort
        , nextEndsAt    = cohortEndsAt cohort
        , nextSeatCap   = cohortSeatCap cohort
        , nextSeatsLeft = remaining
        }

    lessonToDTO (Entity _ lesson) = LessonDTO
      { lessonDay   = academyLessonDay lesson
      , lessonTitle = academyLessonTitle lesson
      , lessonBody  = academyLessonBody lesson
      }

cleanOptional :: Maybe Text -> Maybe Text
cleanOptional Nothing = Nothing
cleanOptional (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

requireEmail :: Text -> AppM Text
requireEmail raw = do
  let normalized = T.toLower (T.strip raw)
  when (T.null normalized) (throwBadRequest "email requerido")
  pure normalized

requireRole :: Text -> AppM Text
requireRole raw = do
  let normalized = T.toLower (T.strip raw)
  when (T.null normalized) (throwBadRequest "role requerido")
  pure normalized

requireSlug :: Text -> AppM Text
requireSlug raw = do
  let normalized = T.toLower (T.strip raw)
  when (T.null normalized) (throwBadRequest "slug requerido")
  pure normalized

requireDay :: Int -> AppM Int
requireDay n = do
  when (n <= 0) (throwBadRequest "day debe ser positivo")
  pure n

requireReferralCode :: Text -> AppM Text
requireReferralCode raw = do
  let normalized = T.toUpper (T.strip raw)
  when (T.null normalized) (throwBadRequest "code requerido")
  pure normalized

upsertAcademyUser :: Text -> Text -> Maybe Text -> AppM (Entity AcademyUser)
upsertAcademyUser emailVal roleVal platformVal = do
  now <- liftIO getCurrentTime
  runDB $ do
    mExisting <- getBy (UniqueAcademyUserEmail emailVal)
    case mExisting of
      Nothing -> do
        let record = AcademyUser emailVal roleVal platformVal now
        key <- insert record
        pure (Entity key record)
      Just (Entity key _) -> do
        update key
          [ AcademyUserRole =. roleVal
          , AcademyUserPlatform =. platformVal
          ]
        updated <- getJust key
        pure (Entity key updated)

claimReferral :: Text -> Maybe (Key AcademyUser) -> Text -> AppM ()
claimReferral emailVal mUser rawCode = do
  codeTxt <- requireReferralCode rawCode
  let codeKey = ReferralCodeKey codeTxt
  existing <- runDB $ get codeKey
  case existing of
    Nothing -> throwNotFound "Código de referido inválido"
    Just _  -> pure ()
  now <- liftIO getCurrentTime
  void $ runDB $ upsert
    ReferralClaim
      { referralClaimCodeId = codeKey
      , referralClaimClaimantUserId = mUser
      , referralClaimEmail = emailVal
      , referralClaimClaimedAt = now
      }
    [ ReferralClaimClaimantUserId =. mUser
    , ReferralClaimEmail =. emailVal
    , ReferralClaimClaimedAt =. now
    ]

throwNotFound :: Text -> AppM a
throwNotFound msg = throwError err404 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

seedTrigger :: Maybe Text -> AppM NoContent
seedTrigger rawToken = do
  requireSeedToken rawToken
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool seedAll
  pure NoContent

-- Parties
partyServer :: AuthedUser -> ServerT PartyAPI AppM
partyServer user = listParties user :<|> createParty user :<|> partyById
  where
    partyById pid = getParty user pid :<|> updateParty user pid :<|> addRole user pid :<|> partyRelated user pid

listParties :: AuthedUser -> AppM [PartyDTO]
listParties user = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  (entities, accountIds) <- liftIO $ flip runSqlPool pool $ do
    creds <- selectList [] []
    let accountSet = Set.fromList (map (userCredentialPartyId . entityVal) creds)
    parts <- selectList [] [Asc PartyId]
    pure (parts, accountSet)
  pure (map (\ent -> toPartyDTO (Set.member (entityKey ent) accountIds) ent) entities)

createParty :: AuthedUser -> PartyCreate -> AppM PartyDTO
createParty user req = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let p = Party
          { partyLegalName = cLegalName req
          , partyDisplayName = cDisplayName req
          , partyIsOrg = cIsOrg req
          , partyTaxId = cTaxId req
          , partyPrimaryEmail = cPrimaryEmail req
          , partyPrimaryPhone = cPrimaryPhone req
          , partyWhatsapp = cWhatsapp req
          , partyInstagram = cInstagram req
          , partyEmergencyContact = cEmergencyContact req
          , partyNotes = cNotes req
          , partyCreatedAt = now
          }
  pid <- liftIO $ flip runSqlPool pool $ insert p
  liftIO $ flip runSqlPool pool $ mapM_ (\role -> upsert
    (PartyRole pid role True)
    [PartyRoleActive =. True]) (fromMaybe [] (cRoles req))
  pure $ toPartyDTO False (Entity pid p)

getParty :: AuthedUser -> Int64 -> AppM PartyDTO
getParty user pidI = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  mp <- liftIO $ flip runSqlPool pool $ getEntity pid
  case mp of
    Nothing -> throwError err404
    Just ent -> do
      bandDetails <- liftIO $ flip runSqlPool pool $ loadBandForParty (entityKey ent)
      hasAccount <- liftIO $ flip runSqlPool pool $
        fmap isJust (selectFirst [UserCredentialPartyId ==. entityKey ent] [])
      pure (toPartyDTOWithBand bandDetails hasAccount ent)

updateParty :: AuthedUser -> Int64 -> PartyUpdate -> AppM PartyDTO
updateParty user pidI req = do
  requireModule user ModuleCRM
  Env pool _ <- ask
  let pid = toSqlKey pidI :: Key Party
  liftIO $ flip runSqlPool pool $ do
    mp <- get pid
    case mp of
      Nothing -> pure ()
      Just p -> do
        let p' = p
              { partyLegalName        = maybe (partyLegalName p) Just (uLegalName req)
              , partyDisplayName      = fromMaybe (M.partyDisplayName p) (uDisplayName req)
              , partyIsOrg            = fromMaybe (partyIsOrg p)         (uIsOrg req)
              , partyTaxId            = maybe (partyTaxId p) Just       (uTaxId req)
              , partyPrimaryEmail     = maybe (partyPrimaryEmail p) Just (uPrimaryEmail req)
              , partyPrimaryPhone     = maybe (partyPrimaryPhone p) Just (uPrimaryPhone req)
              , partyWhatsapp         = maybe (partyWhatsapp p) Just    (uWhatsapp req)
              , partyInstagram        = maybe (partyInstagram p) Just   (uInstagram req)
              , partyEmergencyContact = maybe (partyEmergencyContact p) Just (uEmergencyContact req)
              , partyNotes            = maybe (partyNotes p) Just       (uNotes req)
              }
        replace pid p'
  getParty user pidI

addRole :: AuthedUser -> Int64 -> RolePayload -> AppM NoContent
addRole user pidI (RolePayload roleTxt) = do
  requireModule user ModuleAdmin
  Env pool _ <- ask
  let pid  = toSqlKey pidI :: Key Party
      role = parseRole roleTxt
  liftIO $ flip runSqlPool pool $ void $ upsert
    (PartyRole pid role True)
    [ PartyRoleActive =. True ]
  pure NoContent
  where
    parseRole t =
      case readMaybe (T.unpack (T.strip t)) of
        Just r  -> r
        Nothing -> ReadOnly

partyRelated :: AuthedUser -> Int64 -> AppM PartyRelatedDTO
partyRelated user pidI = do
  requireModule user ModuleCRM
  now <- liftIO getCurrentTime
  let partyKey = toSqlKey pidI :: Key Party

  (asCustomer, asEngineer) <- runDB $ do
    customerRows <- selectList [BookingPartyId ==. Just partyKey] [Desc BookingStartsAt, LimitTo 50]
    engineerRows <- selectList [BookingEngineerPartyId ==. Just partyKey] [Desc BookingStartsAt, LimitTo 50]
    pure (customerRows, engineerRows)

  (studentSessions, teacherSessions, subjectMap, partyNameMap, bookingMap) <- runDB $ do
    studentRows <- selectList [Trials.ClassSessionStudentId ==. partyKey] [Desc Trials.ClassSessionStartAt, LimitTo 50]
    teacherRows <- selectList [Trials.ClassSessionTeacherId ==. partyKey] [Desc Trials.ClassSessionStartAt, LimitTo 50]
    let sessions = studentRows ++ teacherRows
        subjectIds = Set.toList $ Set.fromList $ map (Trials.classSessionSubjectId . entityVal) sessions
        teacherIds = Set.toList $ Set.fromList $ map (Trials.classSessionTeacherId . entityVal) sessions
        studentIds = Set.toList $ Set.fromList $ map (Trials.classSessionStudentId . entityVal) sessions
        bookingIds = catMaybes $ map (Trials.classSessionBookingId . entityVal) sessions

    subjects <- if null subjectIds
      then pure []
      else selectList [Trials.SubjectId <-. subjectIds] []
    let subjectsById = Map.fromList [ (entityKey e, entityVal e) | e <- subjects ]

    let partyIds = Set.toList (Set.fromList (teacherIds ++ studentIds))
    parties <- if null partyIds
      then pure []
      else selectList [PartyId <-. partyIds] []
    let partyNamesById = Map.fromList [ (entityKey e, M.partyDisplayName (entityVal e)) | e <- parties ]

    bookings <- if null bookingIds
      then pure []
      else selectList [BookingId <-. bookingIds] []
    let bookingsById = Map.fromList [ (entityKey e, entityVal e) | e <- bookings ]

    pure (studentRows, teacherRows, subjectsById, partyNamesById, bookingsById)

  tracks <- runDB $
    selectList [ME.LabelTrackOwnerPartyId ==. Just partyKey] [Desc ME.LabelTrackUpdatedAt, LimitTo 100]

  let toRelatedBooking role (Entity bookingId booking) =
        PartyRelatedBooking
          { prbBookingId  = fromSqlKey bookingId
          , prbRole       = role
          , prbTitle      = bookingTitle booking
          , prbServiceType = bookingServiceType booking
          , prbStartsAt   = bookingStartsAt booking
          , prbEndsAt     = bookingEndsAt booking
          , prbStatus     = T.pack (show (bookingStatus booking))
          }

      classStatusLabel attended startAt mBooking =
        case bookingStatus <$> mBooking of
          Just Cancelled  -> "cancelada"
          Just NoShow     -> "cancelada"
          Just Completed  -> "realizada"
          Just Tentative  -> "por-confirmar"
          Just InProgress -> "programada"
          Just Confirmed  -> "programada"
          _ ->
            if attended
              then "realizada"
              else if startAt > now then "programada" else "por-confirmar"

      toRelatedClass role (Entity classSessionId cs) =
        let teacherId = Trials.classSessionTeacherId cs
            studentId = Trials.classSessionStudentId cs
            subjectId = Trials.classSessionSubjectId cs
            mBooking = Trials.classSessionBookingId cs >>= (`Map.lookup` bookingMap)
        in PartyRelatedClassSession
          { prcClassSessionId = fromSqlKey classSessionId
          , prcRole           = role
          , prcSubjectId      = fromSqlKey subjectId
          , prcSubjectName    = Trials.subjectName <$> Map.lookup subjectId subjectMap
          , prcTeacherId      = fromSqlKey teacherId
          , prcTeacherName    = Map.lookup teacherId partyNameMap
          , prcStudentId      = fromSqlKey studentId
          , prcStudentName    = Map.lookup studentId partyNameMap
          , prcStartAt        = Trials.classSessionStartAt cs
          , prcEndAt          = Trials.classSessionEndAt cs
          , prcStatus         = classStatusLabel (Trials.classSessionAttended cs) (Trials.classSessionStartAt cs) mBooking
          , prcBookingId      = fromSqlKey <$> Trials.classSessionBookingId cs
          }

      toRelatedTrack (Entity key t) =
        PartyRelatedLabelTrack
          { prtId        = toPathPiece key
          , prtTitle     = ME.labelTrackTitle t
          , prtStatus    = ME.labelTrackStatus t
          , prtCreatedAt = ME.labelTrackCreatedAt t
          , prtUpdatedAt = ME.labelTrackUpdatedAt t
          }

      bookingsOut =
        map (toRelatedBooking "cliente") asCustomer
          ++ map (toRelatedBooking "ingeniero") asEngineer

      classSessionsOut =
        map (toRelatedClass "estudiante") studentSessions
          ++ map (toRelatedClass "profesor") teacherSessions

  pure PartyRelatedDTO
    { prPartyId = pidI
    , prBookings = bookingsOut
    , prClassSessions = classSessionsOut
    , prLabelTracks = map toRelatedTrack tracks
    }

-- Bookings
bookingPublicServer :: ServerT Api.BookingPublicAPI AppM
bookingPublicServer = createPublicBooking

inventoryStaticServer :: FilePath -> ServerT Api.AssetsAPI AppM
inventoryStaticServer assetsRoot =
  serveDirectoryFileServer (assetsRoot </> "inventory")

assetsServeServer :: FilePath -> ServerT Api.AssetsServeAPI AppM
assetsServeServer assetsRoot =
  serveDirectoryFileServer assetsRoot

bookingServer :: AuthedUser -> ServerT BookingAPI AppM
bookingServer user =
       listBookings user
  :<|> createBooking user
  :<|> updateBooking user

listBookings :: AuthedUser -> Maybe Int64 -> Maybe Int64 -> Maybe Int64 -> AppM [BookingDTO]
listBookings user mBookingId mPartyId mEngineerPartyId = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  liftIO $ do
    dbBookings <- flip runSqlPool pool $ do
      case mBookingId of
        Just bid | bid > 0 -> do
          let bookingKey = toSqlKey bid :: Key Booking
          mBooking <- getEntity bookingKey
          case mBooking of
            Nothing -> pure []
            Just ent -> buildBookingDTOs [ent]
        _ -> do
          let loadByParty pid = do
                let pidKey = toSqlKey pid :: Key Party
                selectList [BookingPartyId ==. Just pidKey] [Desc BookingStartsAt, LimitTo 500]
              loadByEngineer pid = do
                let pidKey = toSqlKey pid :: Key Party
                selectList [BookingEngineerPartyId ==. Just pidKey] [Desc BookingStartsAt, LimitTo 500]
          case (mPartyId, mEngineerPartyId) of
            (Nothing, Nothing) -> do
              bookings <- selectList [] [Desc BookingId]
              buildBookingDTOs bookings
            _ -> do
              byParty <- maybe (pure []) loadByParty mPartyId
              byEngineer <- maybe (pure []) loadByEngineer mEngineerPartyId
              let merged = dedupeByKey (byParty ++ byEngineer)
              buildBookingDTOs merged
    if isJust mBookingId || isJust mPartyId || isJust mEngineerPartyId
      then pure dbBookings
      else do
        courseSessions <- flip runSqlPool pool courseCalendarBookings
        pure (dbBookings ++ courseSessions)
  where
    dedupeByKey :: [Entity Booking] -> [Entity Booking]
    dedupeByKey = go Set.empty []
      where
        go _ acc [] = reverse acc
        go seen acc (e:es) =
          let key = entityKey e
          in if Set.member key seen
               then go seen acc es
               else go (Set.insert key seen) (e:acc) es

courseCalendarBookings :: SqlPersistT IO [BookingDTO]
courseCalendarBookings = do
  courses <- selectList [] []
  fmap concat $ forM courses $ \(Entity courseId course) -> do
    regCount <- count
      [ ME.CourseRegistrationCourseSlug ==. Trials.courseSlug course
      , ME.CourseRegistrationStatus !=. "cancelled"
      ]
    sessions <- selectList [Trials.CourseSessionModelCourseId ==. courseId] [Asc Trials.CourseSessionModelOrder, Asc Trials.CourseSessionModelDate]
    let metaTitle = Trials.courseTitle course
        slugVal = Trials.courseSlug course
        capacityVal = Trials.courseCapacity course
        remainingVal = max 0 (capacityVal - fromIntegral regCount)
        startHour = fromMaybe 0 (Trials.courseSessionStartHour course)
        durationHours = fromMaybe 0 (Trials.courseSessionDurationHours course)
        coursePriceD = fromIntegral (Trials.coursePriceCents course) / 100
        mkBooking :: Int -> Entity Trials.CourseSessionModel -> BookingDTO
        mkBooking idx (Entity _ s) =
          let dateVal = Trials.courseSessionModelDate s
              startUtc = UTCTime dateVal (secondsToDiffTime (fromIntegral startHour * 60 * 60))
              endUtc   = addUTCTime (fromIntegral durationHours * 60 * 60) startUtc
          in BookingDTO
               { bookingId          = negate (fromIntegral (1000 + idx))
               , title              = "Curso: " <> Trials.courseSessionModelLabel s
               , startsAt           = startUtc
               , endsAt             = endUtc
               , status             = "course"
               , notes              = Just ("Curso: " <> metaTitle)
               , partyId            = Nothing
               , engineerPartyId    = Nothing
               , engineerName       = Nothing
               , serviceType        = Just "Curso"
               , serviceOrderId     = Nothing
               , serviceOrderTitle  = Nothing
               , customerName       = Nothing
               , partyDisplayName   = Nothing
               , resources          = []
               , courseSlug         = Just slugVal
               , coursePrice        = Just coursePriceD
               , courseCapacity     = Just capacityVal
               , courseRemaining    = Just remainingVal
               , courseLocation     = Trials.courseLocationLabel course
               }
    pure (zipWith mkBooking [1 :: Int ..] sessions)

createPublicBooking :: PublicBookingReq -> AppM BookingDTO
createPublicBooking PublicBookingReq{..} = do
  when (T.null (T.strip pbFullName)) (throwBadRequest "nombre requerido")
  when (T.null (T.strip pbEmail)) (throwBadRequest "email requerido")
  let serviceTypeClean = normalizeOptionalInput (Just pbServiceType)
  when (isNothing serviceTypeClean) (throwBadRequest "serviceType requerido")
  let engineerIdClean  = pbEngineerPartyId >>= (\i -> if i > 0 then Just i else Nothing)
      engineerNameClean = normalizeOptionalInput pbEngineerName
  case validateEngineer serviceTypeClean engineerIdClean engineerNameClean of
    Left msg -> throwBadRequest msg
    Right () -> pure ()
  let durationMins = max 30 (fromMaybe 60 pbDurationMinutes)
      endsAt       = addUTCTime (fromIntegral durationMins * 60) pbStartsAt
      notesClean   = normalizeOptionalInput pbNotes
  (partyId, _) <- ensurePartyWithAccount (Just (T.strip pbFullName)) (T.strip pbEmail) pbPhone
  Env pool _ <- ask
  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean (fromMaybe [] pbResourceIds) pbStartsAt endsAt
  mEngineerParty <- case engineerIdClean of
    Nothing -> pure Nothing
    Just pid -> liftIO $ flip runSqlPool pool $ get (toSqlKey (fromIntegral pid) :: Key Party)
  let resolvedEngineerName =
        engineerNameClean
          <|> (M.partyDisplayName <$> mEngineerParty)
  now <- liftIO getCurrentTime
  let bookingRecord = Booking
        { bookingTitle          = buildTitle serviceTypeClean pbFullName
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = Just partyId
        , bookingServiceType    = serviceTypeClean
        , bookingEngineerPartyId = engineerIdClean >>= (Just . toSqlKey . fromIntegral)
        , bookingEngineerName    = resolvedEngineerName
        , bookingStartsAt       = pbStartsAt
        , bookingEndsAt         = endsAt
        , bookingStatus         = Tentative
        , bookingCreatedBy      = Nothing
        , bookingNotes          = notesClean
        , bookingCreatedAt      = now
        }
  dto <- liftIO $ flip runSqlPool pool $ do
    bookingId <- insert bookingRecord
    let uniqueResources = nub resourceKeys
    forM_ (zip [0 :: Int ..] uniqueResources) $ \(idx, key) ->
      insert_ BookingResource
        { bookingResourceBookingId = bookingId
        , bookingResourceResourceId = key
        , bookingResourceRole = if idx == 0 then "primary" else "secondary"
        }
    created <- getJustEntity bookingId
    dtos <- buildBookingDTOs [created]
    let fallback = BookingDTO
          { bookingId          = fromSqlKey bookingId
          , title              = bookingTitle bookingRecord
          , startsAt           = bookingStartsAt bookingRecord
          , endsAt             = bookingEndsAt bookingRecord
          , status             = T.pack (show (bookingStatus bookingRecord))
          , notes              = bookingNotes bookingRecord
          , partyId            = Just (fromSqlKey partyId)
          , engineerPartyId    = fmap fromSqlKey (bookingEngineerPartyId bookingRecord)
          , engineerName       = bookingEngineerName bookingRecord
          , serviceType        = bookingServiceType bookingRecord
          , serviceOrderId     = Nothing
          , serviceOrderTitle  = Nothing
          , customerName       = Just pbFullName
          , partyDisplayName   = Just pbFullName
          , resources          = []
          , courseSlug         = Nothing
          , coursePrice        = Nothing
          , courseCapacity     = Nothing
          , courseRemaining    = Nothing
          , courseLocation     = Nothing
          }
    pure (headDef fallback dtos)
  notifyEngineerIfNeeded dto
  pure dto
  where
    headDef defVal xs =
      case xs of
        (y:_) -> y
        []    -> defVal
    buildTitle (Just svc) nameTxt = svc <> " · " <> nameTxt
    buildTitle Nothing nameTxt    = "Reserva · " <> nameTxt

createBooking :: AuthedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  now <- liftIO getCurrentTime

  let status'          = parseStatusWithDefault Confirmed (cbStatus req)
      serviceTypeClean = normalizeOptionalInput (cbServiceType req)
      engineerIdClean  = cbEngineerPartyId req >>= (\i -> if i > 0 then Just i else Nothing)
      engineerNameClean = normalizeOptionalInput (cbEngineerName req)
      partyKey         = fmap (toSqlKey . fromIntegral) (cbPartyId req)
      requestedRooms   = fromMaybe [] (cbResourceIds req)
  case validateEngineer serviceTypeClean engineerIdClean engineerNameClean of
    Left msg -> throwBadRequest msg
    Right () -> pure ()

  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean requestedRooms (cbStartsAt req) (cbEndsAt req)
  mEngineerParty <- case engineerIdClean of
    Nothing -> pure Nothing
    Just pid -> liftIO $ flip runSqlPool pool $ get (toSqlKey (fromIntegral pid) :: Key Party)
  let resolvedEngineerName =
        engineerNameClean
          <|> (M.partyDisplayName <$> mEngineerParty)

  let bookingRecord = Booking
        { bookingTitle          = cbTitle req
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = partyKey
        , bookingServiceType    = serviceTypeClean
        , bookingEngineerPartyId = engineerIdClean >>= (Just . toSqlKey . fromIntegral)
        , bookingEngineerName    = resolvedEngineerName
        , bookingStartsAt       = cbStartsAt req
        , bookingEndsAt         = cbEndsAt req
        , bookingStatus         = status'
        , bookingCreatedBy      = Nothing
        , bookingNotes          = normalizeOptionalInput (cbNotes req)
        , bookingCreatedAt      = now
        }

  dto <- liftIO $ flip runSqlPool pool $ do
    bookingId <- insert bookingRecord
    let uniqueResources = nub resourceKeys
    forM_ (zip [0 :: Int ..] uniqueResources) $ \(idx, key) ->
      insert_ BookingResource
        { bookingResourceBookingId = bookingId
        , bookingResourceResourceId = key
        , bookingResourceRole = if idx == 0 then "primary" else "secondary"
        }
    created <- getJustEntity bookingId
    dtos <- buildBookingDTOs [created]
    let fallback = BookingDTO
          { bookingId   = fromSqlKey bookingId
          , title       = bookingTitle bookingRecord
          , startsAt    = bookingStartsAt bookingRecord
          , endsAt      = bookingEndsAt bookingRecord
          , status      = T.pack (show (bookingStatus bookingRecord))
          , notes       = bookingNotes bookingRecord
          , partyId     = fmap fromSqlKey partyKey
          , engineerPartyId = fmap fromSqlKey (bookingEngineerPartyId bookingRecord)
          , engineerName = bookingEngineerName bookingRecord
          , serviceType = bookingServiceType bookingRecord
          , serviceOrderId = Nothing
          , serviceOrderTitle = Nothing
          , customerName = Nothing
          , partyDisplayName = Nothing
          , resources   = []
          , courseSlug = Nothing
          , coursePrice = Nothing
          , courseCapacity = Nothing
          , courseRemaining = Nothing
          , courseLocation = Nothing
          }
    pure (headDef fallback dtos)
  notifyEngineerIfNeeded dto
  pure dto
  where
    headDef defVal xs =
      case xs of
        (y:_) -> y
        []    -> defVal

updateBooking :: AuthedUser -> Int64 -> UpdateBookingReq -> AppM BookingDTO
updateBooking user bookingIdI req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  let bookingId = toSqlKey bookingIdI :: Key Booking
  result <- liftIO $ flip runSqlPool pool $ do
    mBooking <- getEntity bookingId
    case mBooking of
      Nothing -> pure (Left err404)
      Just (Entity _ current) -> do
        let applyText fallback Nothing = fallback
            applyText fallback (Just val) =
              let trimmed = T.strip val
              in if T.null trimmed then fallback else trimmed
            applyMaybeText fallback Nothing = fallback
            applyMaybeText _ (Just val) = normalizeOptionalInput (Just val)
            updated = current
              { bookingTitle       = applyText (bookingTitle current) (ubTitle req)
              , bookingServiceType = applyMaybeText (bookingServiceType current) (ubServiceType req)
              , bookingNotes       = applyMaybeText (bookingNotes current) (ubNotes req)
              , bookingStatus      = maybe (bookingStatus current) (parseStatusWithDefault (bookingStatus current)) (ubStatus req)
              , bookingStartsAt    = fromMaybe (bookingStartsAt current) (ubStartsAt req)
              , bookingEndsAt      = fromMaybe (bookingEndsAt current) (ubEndsAt req)
              , bookingEngineerPartyId = maybe (bookingEngineerPartyId current) (Just . toSqlKey . fromIntegral) (ubEngineerPartyId req)
              , bookingEngineerName    = maybe (bookingEngineerName current) (normalizeOptionalInput . Just) (ubEngineerName req)
              }
        case validateEngineer (bookingServiceType updated) (fmap fromSqlKey (bookingEngineerPartyId updated)) (bookingEngineerName updated) of
          Left msg -> pure (Left err400 { errBody = BL8.fromStrict (TE.encodeUtf8 msg) })
          Right () -> do
            replace bookingId updated
            dtos <- buildBookingDTOs [Entity bookingId updated]
            pure (maybe (Left err500) Right (listToMaybe dtos))
  either throwError pure result


loadBookingResourceMap :: [Key Booking] -> SqlPersistT IO (Map.Map (Key Booking) [BookingResourceDTO])
loadBookingResourceMap [] = pure Map.empty
loadBookingResourceMap bookingIds = do
  bookingResources <- selectList [BookingResourceBookingId <-. bookingIds] []
  if null bookingResources
    then pure Map.empty
    else do
      let resourceIds = map (bookingResourceResourceId . entityVal) bookingResources
      resources <- selectList [ResourceId <-. resourceIds] []
      let resourceMap = Map.fromList [ (entityKey resEnt, resEnt) | resEnt <- resources ]
          accumulate acc (Entity _ br) =
            case Map.lookup (bookingResourceResourceId br) resourceMap of
              Nothing      -> acc
              Just resEnt  ->
                let bookingKey = bookingResourceBookingId br
                    dto = BookingResourceDTO
                      { brRoomId   = toPathPiece (bookingResourceResourceId br)
                      , brRoomName = resourceName (entityVal resEnt)
                      , brRole     = bookingResourceRole br
                      }
                in Map.insertWith (++) bookingKey [dto] acc
      pure (foldl' accumulate Map.empty bookingResources)

buildBookingDTOs :: [Entity Booking] -> SqlPersistT IO [BookingDTO]
buildBookingDTOs [] = pure []
buildBookingDTOs bookings = do
  let bookingIds       = map entityKey bookings
      bookingPartyIds  = catMaybes $ map (bookingPartyId . entityVal) bookings
      serviceOrderKeys = catMaybes $ map (bookingServiceOrderId . entityVal) bookings
  resMap <- loadBookingResourceMap bookingIds
  serviceOrderMap <- loadServiceOrderMap serviceOrderKeys
  let servicePartyIds = map (M.serviceOrderCustomerId . entityVal) (Map.elems serviceOrderMap)
      partyIds = nub (bookingPartyIds ++ servicePartyIds)
  partyMap <- loadPartyDisplayMap partyIds
  pure $ map (toDTO resMap partyMap serviceOrderMap) bookings
  where
    toDTO resMap partyMap soMap (Entity bid b) =
      let resources = Map.findWithDefault [] bid resMap
          partyDisplay = bookingPartyId b >>= (`Map.lookup` partyMap)
          serviceOrderEnt = bookingServiceOrderId b >>= (`Map.lookup` soMap)
          serviceOrderTitleText = serviceOrderEnt >>= (M.serviceOrderTitle . entityVal)
          customerNameText = serviceOrderEnt >>= \soEnt ->
            Map.lookup (M.serviceOrderCustomerId (entityVal soEnt)) partyMap
          fallbackOrderTitle = normalizeOptionalInput (Just (bookingTitle b))
          fallbackCustomer = partyDisplay
      in BookingDTO
        { bookingId   = fromSqlKey bid
        , title       = bookingTitle b
        , startsAt    = bookingStartsAt b
        , endsAt      = bookingEndsAt b
        , status      = T.pack (show (bookingStatus b))
        , notes       = bookingNotes b
        , partyId     = fmap fromSqlKey (bookingPartyId b)
        , engineerPartyId = fmap fromSqlKey (bookingEngineerPartyId b)
        , engineerName    = bookingEngineerName b
        , serviceType = bookingServiceType b
        , serviceOrderId    = fmap fromSqlKey (bookingServiceOrderId b)
        , serviceOrderTitle = serviceOrderTitleText <|> fallbackOrderTitle
        , customerName      = customerNameText <|> fallbackCustomer
        , partyDisplayName  = partyDisplay
        , resources   = resources
        , courseSlug        = Nothing
        , coursePrice       = Nothing
        , courseCapacity    = Nothing
        , courseRemaining   = Nothing
        , courseLocation    = Nothing
        }

loadPartyDisplayMap :: [Key Party] -> SqlPersistT IO (Map.Map (Key Party) Text)
loadPartyDisplayMap [] = pure Map.empty
loadPartyDisplayMap partyIds = do
  parties <- selectList [PartyId <-. partyIds] []
  pure $ Map.fromList
    [ (entityKey ent, M.partyDisplayName (entityVal ent))
    | ent <- parties
    ]

loadServiceOrderMap :: [Key ServiceOrder] -> SqlPersistT IO (Map.Map (Key ServiceOrder) (Entity ServiceOrder))
loadServiceOrderMap [] = pure Map.empty
loadServiceOrderMap serviceOrderIds = do
  serviceOrders <- selectList [ServiceOrderId <-. serviceOrderIds] []
  pure $ Map.fromList [ (entityKey ent, ent) | ent <- serviceOrders ]

normalizeOptionalInput :: Maybe Text -> Maybe Text
normalizeOptionalInput Nothing = Nothing
normalizeOptionalInput (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

parseStatusWithDefault :: BookingStatus -> Text -> BookingStatus
parseStatusWithDefault fallback raw =
  let cleaned = T.strip raw
  in case readMaybe (T.unpack cleaned) of
       Just s  -> s
       Nothing -> fallback

requiresEngineer :: Maybe Text -> Bool
requiresEngineer Nothing = False
requiresEngineer (Just svc) =
  let lowered = T.toLower (T.strip svc)
  in any (`T.isInfixOf` lowered) ["graba", "mezcl", "master"]

validateEngineer :: Maybe Text -> Maybe Int64 -> Maybe Text -> Either Text ()
validateEngineer svc mEngineerId mEngineerName
  | requiresEngineer svc && isNothing mEngineerId && maybe True T.null (fmap T.strip mEngineerName) =
      Left "Selecciona un ingeniero para grabación/mezcla/mastering"
  | otherwise = Right ()

notifyEngineerIfNeeded :: BookingDTO -> AppM ()
notifyEngineerIfNeeded BookingDTO{engineerPartyId = Nothing, engineerName = Nothing} = pure ()
notifyEngineerIfNeeded booking = do
  Env pool cfg <- ask
  mEngineer <- liftIO $ flip runSqlPool pool $ case engineerPartyId booking of
    Just pid -> get (toSqlKey (fromIntegral pid) :: Key Party)
    Nothing  -> pure Nothing
  let displayName = DTO.engineerName booking
        <|> (M.partyDisplayName <$> mEngineer)
      emailAddr = mEngineer >>= partyPrimaryEmail
  case emailAddr of
    Nothing -> pure ()
    Just engineerEmail -> do
      let name = fromMaybe engineerEmail displayName
          emailSvc = EmailSvc.mkEmailService cfg
          subjectSvc = fromMaybe "Reserva" (DTO.serviceType booking)
          startTxt = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" (DTO.startsAt booking))
          customer = DTO.customerName booking <|> DTO.partyDisplayName booking
          noteText = case booking of DTO.BookingDTO{DTO.notes = ns} -> ns
      liftIO $
        EmailSvc.sendEngineerBooking
          emailSvc
          name
          engineerEmail
          subjectSvc
          startTxt
          customer
          noteText

resolveResourcesForBooking :: Maybe Text -> [Text] -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
resolveResourcesForBooking service requested start end = do
  explicit <- resolveRequestedResources requested
  if not (null explicit)
    then pure explicit
    else defaultResourcesForService service start end

resolveRequestedResources :: [Text] -> SqlPersistT IO [Key Resource]
resolveRequestedResources rawIds = do
  rooms <- selectList [ResourceResourceType ==. Room, ResourceActive ==. True] [Asc ResourceId]
  let indexById = Map.fromList $ map (\(Entity k room) -> (k, room)) rooms
      indexByName = Map.fromList $ map (\(Entity k room) -> (T.toLower (resourceName room), k)) rooms
      resolveOne rid =
        case fromPathPiece rid of
          Just key | Map.member key indexById -> Just key
          _ ->
            let normalized = T.toLower (T.strip rid)
            in Map.lookup normalized indexByName
      dedupKeys = Set.toList . Set.fromList
  pure $ dedupKeys (mapMaybe resolveOne rawIds)

defaultResourcesForService :: Maybe Text -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
defaultResourcesForService Nothing _ _ = pure []
defaultResourcesForService (Just service) start end = do
  let normalized = T.toLower (T.strip service)
  rooms <- selectList [ResourceResourceType ==. Room, ResourceActive ==. True] [Asc ResourceId]
  let findByName name = find (\(Entity _ room) -> T.toLower (resourceName room) == T.toLower name) rooms
      boothPredicate (Entity _ room) = "booth" `T.isInfixOf` T.toLower (resourceName room)
      controlRoom = findByName "Control Room"
      vocalBooth  =
        findByName "Vocal Booth"
          <|> find (\(Entity _ room) ->
                let lower = T.toLower (resourceName room)
                in "vocal" `T.isInfixOf` lower || "booth" `T.isInfixOf` lower) rooms
      djBooths =
        let candidateNames = ["Booth 1","Booth 2","Booth A","Booth B","DJ Booth 1","DJ Booth 2","DJ Booth"]
            nameMatches = mapMaybe findByName candidateNames
            boothMatches = filter boothPredicate rooms
        in dedupeEntities (nameMatches ++ boothMatches)
  case () of
    _ | normalized `elem` ["grabacion audiovisual live", "grabación audiovisual live"] ->
      pure $ map entityKey $ catMaybes (map findByName ["Live Room", "Control Room"])
    _ | normalized `elem` ["band recording", "grabacion banda", "grabación banda"] ->
      pure $ map entityKey $ catMaybes (map findByName ["Live Room", "Control Room"])
    _ | normalized `elem` ["vocal recording", "grabacion vocal", "grabación vocal"] ->
      pure $ map entityKey $ catMaybes [vocalBooth, controlRoom]
    _ | "mix" `T.isInfixOf` normalized || "mezcla" `T.isInfixOf` normalized ->
      pure $ maybe [] (pure . entityKey) controlRoom
    _ | "master" `T.isInfixOf` normalized ->
      pure $ maybe [] (pure . entityKey) controlRoom
    _ | normalized `elem` ["band rehearsal", "ensayo banda"] ->
      pure $ maybe [] (pure . entityKey) (findByName "Live Room")
    _ | normalized `elem` ["dj booth rental", "practica de dj", "práctica de dj", "dj practice"] ||
        "dj" `T.isInfixOf` normalized -> do
      pickBooth djBooths
    _ -> pure []
  where
    pickBooth [] = pure []
    pickBooth (Entity key _ : rest) = do
      available <- isResourceAvailableDB key start end
      if available
        then pure [key]
        else do
          remaining <- pickBooth rest
          pure (if null remaining then [key] else remaining)

dedupeEntities :: [Entity Resource] -> [Entity Resource]
dedupeEntities = go Set.empty []
  where
    go _ acc [] = reverse acc
    go seen acc (e:es) =
      let key = entityKey e
      in if Set.member key seen
           then go seen acc es
           else go (Set.insert key seen) (e:acc) es

isResourceAvailableDB :: Key Resource -> UTCTime -> UTCTime -> SqlPersistT IO Bool
isResourceAvailableDB resourceKey start end = do
  bookingResources <- selectList [BookingResourceResourceId ==. resourceKey] []
  let bookingIds = map (bookingResourceBookingId . entityVal) bookingResources
  if null bookingIds
    then pure True
    else do
      bookings <- selectList [BookingId <-. bookingIds] []
      let activeBookings = filter (\(Entity _ b) -> bookingStatus b `notElem` [Cancelled, NoShow]) bookings
      pure $ all (noOverlap . entityVal) activeBookings
  where
    noOverlap booking =
      bookingEndsAt booking <= start || bookingStartsAt booking >= end

-- Packages
packageServer :: AuthedUser -> ServerT PackageAPI AppM
packageServer user = listProducts user :<|> createPurchase user

listProducts :: AuthedUser -> AppM [PackageProductDTO]
listProducts user = do
  requireModule user ModulePackages
  Env pool _ <- ask
  ps <- liftIO $ flip runSqlPool pool $ selectList [PackageProductActive ==. True] [Asc PackageProductId]
  pure $ map toDTO ps
  where
    toDTO (Entity pid p) = PackageProductDTO
      { ppId         = fromSqlKey pid
      , ppName       = packageProductName p
      , ppService    = T.pack (show (packageProductServiceKind p))
      , ppUnitsKind  = T.pack (show (packageProductUnitsKind p))
      , ppUnitsQty   = packageProductUnitsQty p
      , ppPriceCents = packageProductPriceCents p
      }

createPurchase :: AuthedUser -> PackagePurchaseReq -> AppM NoContent
createPurchase user req = do
  requireModule user ModulePackages
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let buyer = toSqlKey (buyerId req)   :: Key Party
      prodK = toSqlKey (productId req) :: Key PackageProduct
  liftIO $ flip runSqlPool pool $ do
    mp <- get prodK
    case mp of
      Nothing -> pure ()
      Just p -> do
        let qty    = packageProductUnitsQty p
            priceC = packageProductPriceCents p
        _ <- insert PackagePurchase
              { packagePurchaseBuyerId        = buyer
              , packagePurchaseProductId      = prodK
              , packagePurchasePurchasedAt    = now
              , packagePurchasePriceCents     = priceC
              , packagePurchaseExpiresAt      = Nothing
              , packagePurchaseRemainingUnits = qty
              , packagePurchaseStatus         = "Active"
              }
        pure ()
  pure NoContent

-- Invoices
invoiceServer :: AuthedUser -> ServerT InvoiceAPI AppM
invoiceServer user =
       listInvoices user
  :<|> createInvoice user
  :<|> generateInvoiceForSession user
  :<|> getInvoicesBySession user
  :<|> getInvoiceById user

generateInvoiceForSession :: AuthedUser -> Text -> Value -> AppM Value
generateInvoiceForSession user sessionId _payload = do
  requireModule user ModuleInvoicing
  pure (object ["ok" .= True, "sessionId" .= sessionId])

getInvoicesBySession :: AuthedUser -> Text -> AppM Value
getInvoicesBySession user sessionId = do
  requireModule user ModuleInvoicing
  pure (object ["ok" .= True, "sessionId" .= sessionId])

getInvoiceById :: AuthedUser -> Int64 -> AppM Value
getInvoiceById user invoiceId = do
  requireModule user ModuleInvoicing
  pure (object ["ok" .= True, "invoiceId" .= invoiceId])

listInvoices :: AuthedUser -> AppM [InvoiceDTO]
listInvoices user = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    invoices <- selectList [] [Desc InvoiceId]
    let invoiceIds = map entityKey invoices
    lineEntities <-
      if null invoiceIds
        then pure []
        else selectList [InvoiceLineInvoiceId <-. invoiceIds] [Asc InvoiceLineId]
    receiptEntities <-
      if null invoiceIds
        then pure []
        else selectList [ReceiptInvoiceId <-. invoiceIds] []
    let lineMap = foldr (\ent@(Entity _ line) acc ->
                      Map.insertWith (++) (invoiceLineInvoiceId line) [ent] acc)
                    Map.empty
                    lineEntities
        receiptMap = Map.fromList
          [ (receiptInvoiceId rec, entityKey ent)
          | ent@(Entity _ rec) <- receiptEntities
          ]
    pure
      [ invoiceToDTO invEnt
          (Map.findWithDefault [] (entityKey invEnt) lineMap)
          (Map.lookup (entityKey invEnt) receiptMap)
      | invEnt <- invoices
      ]

createInvoice :: AuthedUser -> CreateInvoiceReq -> AppM InvoiceDTO
createInvoice user CreateInvoiceReq{..} = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  when (null ciLineItems) $ throwBadRequest "Invoice requires at least one line item"
  preparedLines <- case traverse prepareLine ciLineItems of
    Left msg   -> throwBadRequest msg
    Right vals -> pure vals
  now <- liftIO getCurrentTime
  let day      = utctDay now
      cid      = toSqlKey ciCustomerId :: Key Party
      currency = normalizeCurrency ciCurrency
      notes    = normalizeOptionalText ciNotes
      number   = normalizeOptionalText ciNumber
      subtotal = sum (map plSubtotal preparedLines)
      taxTotal = sum (map plTax preparedLines)
      grand    = sum (map plTotal preparedLines)
      invoiceRecord = Invoice
        { invoiceCustomerId    = cid
        , invoiceIssueDate     = day
        , invoiceDueDate       = day
        , invoiceNumber        = number
        , invoiceStatus        = Draft
        , invoiceCurrency      = currency
        , invoiceSubtotalCents = subtotal
        , invoiceTaxCents      = taxTotal
        , invoiceTotalCents    = grand
        , invoiceSriDocumentId = Nothing
        , invoiceNotes         = notes
        , invoiceCreatedAt     = now
        }
  (invoiceEnt, lineEntities, maybeReceiptKey) <- liftIO $ flip runSqlPool pool $ do
    iid <- insert invoiceRecord
    let invEntity = Entity iid invoiceRecord
    invoiceLines <- forM preparedLines $ \pl -> do
      let line = invoiceLineFromPrepared iid pl
      lid <- insert line
      pure (Entity lid line)
    receiptKey <-
      if fromMaybe False ciGenerateReceipt
        then do
          (receiptEnt, _) <- issueReceipt now Nothing Nothing notes Nothing invEntity invoiceLines
          pure (Just (entityKey receiptEnt))
        else pure Nothing
    pure (invEntity, invoiceLines, receiptKey)
  pure $ invoiceToDTO invoiceEnt lineEntities maybeReceiptKey

-- Receipts
receiptServer :: AuthedUser -> ServerT ReceiptAPI AppM
receiptServer user = listReceipts user :<|> createReceipt user :<|> getReceipt user

listReceipts :: AuthedUser -> AppM [ReceiptDTO]
listReceipts user = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    receipts <- selectList [] [Desc ReceiptId]
    let receiptIds = map entityKey receipts
    lineEntities <-
      if null receiptIds
        then pure []
        else selectList [ReceiptLineReceiptId <-. receiptIds] [Asc ReceiptLineId]
    let lineMap = foldr (\ent@(Entity _ line) acc ->
                      Map.insertWith (++) (receiptLineReceiptId line) [ent] acc)
                    Map.empty
                    lineEntities
    pure
      [ receiptToDTO recEnt (Map.findWithDefault [] (entityKey recEnt) lineMap)
      | recEnt <- receipts
      ]

createReceipt :: AuthedUser -> CreateReceiptReq -> AppM ReceiptDTO
createReceipt user CreateReceiptReq{..} = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let iid = toSqlKey crInvoiceId :: Key Invoice
  result <- liftIO $ flip runSqlPool pool $ do
    mInvoice <- getEntity iid
    case mInvoice of
      Nothing      -> pure (Left "invoice-not-found")
      Just invEnt -> do
        existing <- selectFirst [ReceiptInvoiceId ==. iid] []
        case existing of
          Just receiptEnt -> do
            receiptLines <- selectList [ReceiptLineReceiptId ==. entityKey receiptEnt] [Asc ReceiptLineId]
            pure (Right (receiptToDTO receiptEnt receiptLines))
          Nothing -> do
            invoiceLines <- selectList [InvoiceLineInvoiceId ==. iid] [Asc InvoiceLineId]
            if null invoiceLines
              then pure (Left "invoice-empty")
              else do
                (receiptEnt, receiptLines) <-
                  issueReceipt now (normalizeOptionalText crBuyerName) (normalizeOptionalText crBuyerEmail)
                               (normalizeOptionalText crNotes) (normalizeOptionalText crCurrency)
                               invEnt invoiceLines
                pure (Right (receiptToDTO receiptEnt receiptLines))
  case result of
    Left "invoice-not-found" -> throwError err404 { errBody = BL.fromStrict (TE.encodeUtf8 "Invoice not found") }
    Left "invoice-empty"     -> throwBadRequest "Invoice has no line items to receipt"
    Left otherMsg             -> throwBadRequest otherMsg
    Right dto                 -> pure dto

getReceipt :: AuthedUser -> Int64 -> AppM ReceiptDTO
getReceipt user ridParam = do
  requireModule user ModuleInvoicing
  Env pool _ <- ask
  let rid = toSqlKey ridParam :: Key Receipt
  result <- liftIO $ flip runSqlPool pool $ do
    mReceipt <- getEntity rid
    case mReceipt of
      Nothing -> pure Nothing
      Just rec -> do
        receiptLines <- selectList [ReceiptLineReceiptId ==. rid] [Asc ReceiptLineId]
        pure (Just (receiptToDTO rec receiptLines))
  maybe (throwError err404) pure result

data PreparedLine = PreparedLine
  { plDescription       :: Text
  , plQuantity          :: Int
  , plUnitCents         :: Int
  , plTaxBps            :: Int
  , plServiceOrderId    :: Maybe (Key ServiceOrder)
  , plPackagePurchaseId :: Maybe (Key PackagePurchase)
  , plSubtotal          :: Int
  , plTax               :: Int
  , plTotal             :: Int
  }

prepareLine :: CreateInvoiceLineReq -> Either Text PreparedLine
prepareLine CreateInvoiceLineReq{..} = do
  let desc = T.strip cilDescription
  if T.null desc then Left "Line item description is required" else pure ()
  when (cilQuantity <= 0) $ Left "Line item quantity must be greater than zero"
  when (cilUnitCents < 0) $ Left "Line item unit amount must be zero or greater"
  let taxBpsVal = fromMaybe 0 cilTaxBps
  when (taxBpsVal < 0) $ Left "Line item tax basis points must be zero or greater"
  let subtotal = cilQuantity * cilUnitCents
      tax      = (subtotal * taxBpsVal) `div` 10000
      total    = subtotal + tax
      serviceOrderKey = (toSqlKey <$> cilServiceOrderId) :: Maybe (Key ServiceOrder)
      packagePurchaseKey = (toSqlKey <$> cilPackagePurchaseId) :: Maybe (Key PackagePurchase)
  pure PreparedLine
    { plDescription       = desc
    , plQuantity          = cilQuantity
    , plUnitCents         = cilUnitCents
    , plTaxBps            = taxBpsVal
    , plServiceOrderId    = serviceOrderKey
    , plPackagePurchaseId = packagePurchaseKey
    , plSubtotal          = subtotal
    , plTax               = tax
    , plTotal             = total
    }

invoiceLineFromPrepared :: Key Invoice -> PreparedLine -> InvoiceLine
invoiceLineFromPrepared iid PreparedLine{..} = InvoiceLine
  { invoiceLineInvoiceId         = iid
  , invoiceLineServiceOrderId    = plServiceOrderId
  , invoiceLinePackagePurchaseId = plPackagePurchaseId
  , invoiceLineDescription       = plDescription
  , invoiceLineQuantity          = plQuantity
  , invoiceLineUnitCents         = plUnitCents
  , invoiceLineTaxBps            = plTaxBps
  , invoiceLineTotalCents        = plTotal
  }

normalizeCurrency :: Maybe Text -> Text
normalizeCurrency mCur =
  case fmap T.strip mCur of
    Just cur | not (T.null cur) -> T.toUpper cur
    _                           -> "USD"

normalizeOptionalText :: Maybe Text -> Maybe Text
normalizeOptionalText =
  let clean t =
        let trimmed = T.strip t
        in if T.null trimmed then Nothing else Just trimmed
  in (>>= clean)

invoiceToDTO :: Entity Invoice -> [Entity InvoiceLine] -> Maybe (Key Receipt) -> InvoiceDTO
invoiceToDTO (Entity iid inv) invLines mReceiptKey = InvoiceDTO
  { invId      = fromSqlKey iid
  , number     = invoiceNumber inv
  , statusI    = T.pack (show (invoiceStatus inv))
  , subtotalC  = invoiceSubtotalCents inv
  , taxC       = invoiceTaxCents inv
  , totalC     = invoiceTotalCents inv
  , currency   = invoiceCurrency inv
  , customerId = Just (fromSqlKey (invoiceCustomerId inv))
  , notes      = invoiceNotes inv
  , receiptId  = fmap fromSqlKey mReceiptKey
  , lineItems  = map invoiceLineToDTO invLines
  }

invoiceLineToDTO :: Entity InvoiceLine -> InvoiceLineDTO
invoiceLineToDTO (Entity lid line) = InvoiceLineDTO
  { lineId            = fromSqlKey lid
  , description       = invoiceLineDescription line
  , quantity          = invoiceLineQuantity line
  , unitCents         = invoiceLineUnitCents line
  , taxBps            = invoiceLineTaxBps line
  , totalCents        = invoiceLineTotalCents line
  , serviceOrderId    = fromSqlKey <$> invoiceLineServiceOrderId line
  , packagePurchaseId = fromSqlKey <$> invoiceLinePackagePurchaseId line
  }

receiptToDTO :: Entity Receipt -> [Entity ReceiptLine] -> ReceiptDTO
receiptToDTO (Entity rid rec) receiptLines = ReceiptDTO
  { receiptId     = fromSqlKey rid
  , receiptNumber = M.receiptNumber rec
  , issuedAt      = M.receiptIssuedAt rec
  , issueDate     = M.receiptIssueDate rec
  , buyerName     = M.receiptBuyerName rec
  , buyerEmail    = M.receiptBuyerEmail rec
  , currency      = M.receiptCurrency rec
  , subtotalCents = M.receiptSubtotalCents rec
  , taxCents      = M.receiptTaxCents rec
  , totalCents    = M.receiptTotalCents rec
  , notes         = M.receiptNotes rec
  , invoiceId     = fromSqlKey (M.receiptInvoiceId rec)
  , lineItems     = map receiptLineToDTO receiptLines
  }

receiptLineToDTO :: Entity ReceiptLine -> ReceiptLineDTO
receiptLineToDTO (Entity lid line) = ReceiptLineDTO
  { receiptLineId = fromSqlKey lid
  , rlDescription = receiptLineDescription line
  , rlQuantity    = receiptLineQuantity line
  , rlUnitCents   = receiptLineUnitCents line
  , rlTaxBps      = receiptLineTaxBps line
  , rlTotalCents  = receiptLineTotalCents line
  }

issueReceipt
  :: UTCTime
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Entity Invoice
  -> [Entity InvoiceLine]
  -> SqlPersistT IO (Entity Receipt, [Entity ReceiptLine])
issueReceipt now mBuyerName mBuyerEmail mNotes mCurrency (Entity iid inv) lineEntities = do
  let customerId = invoiceCustomerId inv
  party <- get customerId
  let defaultName  = maybe "Cliente" M.partyDisplayName party
      defaultEmail = party >>= partyPrimaryEmail
      buyerName    = fromMaybe defaultName mBuyerName
      buyerEmail   = mBuyerEmail <|> defaultEmail
      currency     = maybe (invoiceCurrency inv) (normalizeCurrency . Just) mCurrency
      notes        = mNotes <|> invoiceNotes inv
      calcTotals (Entity _ line) =
        let lineSubtotal = invoiceLineQuantity line * invoiceLineUnitCents line
            lineTotal    = invoiceLineTotalCents line
        in (lineSubtotal, lineTotal - lineSubtotal, lineTotal)
      subtotals = [ s | ent <- lineEntities, let (s, _, _) = calcTotals ent ]
      taxPieces = [ t | ent <- lineEntities, let (_, t, _) = calcTotals ent ]
      totals    = [ tot | ent <- lineEntities, let (_, _, tot) = calcTotals ent ]
      subtotal  = sum subtotals
      taxTotal  = sum taxPieces
      total     = sum totals
  number <- generateReceiptNumber (utctDay now)
  let receiptRecord = Receipt
        { receiptInvoiceId    = iid
        , receiptNumber       = number
        , receiptIssueDate    = utctDay now
        , receiptIssuedAt     = now
        , receiptBuyerPartyId = Just customerId
        , receiptBuyerName    = buyerName
        , receiptBuyerEmail   = buyerEmail
        , receiptCurrency     = currency
        , receiptSubtotalCents = subtotal
        , receiptTaxCents     = taxTotal
        , receiptTotalCents   = total
        , receiptNotes        = notes
        , receiptCreatedAt    = now
        }
  rid <- insert receiptRecord
  receiptLines <- forM lineEntities $ \(Entity _ line) -> do
    let lineRecord = ReceiptLine
          { receiptLineReceiptId = rid
          , receiptLineDescription = invoiceLineDescription line
          , receiptLineQuantity    = invoiceLineQuantity line
          , receiptLineUnitCents   = invoiceLineUnitCents line
          , receiptLineTaxBps      = Just (invoiceLineTaxBps line)
          , receiptLineTotalCents  = invoiceLineTotalCents line
          }
    rlId <- insert lineRecord
    pure (Entity rlId lineRecord)
  pure (Entity rid receiptRecord, receiptLines)

generateReceiptNumber :: Day -> SqlPersistT IO Text
generateReceiptNumber day = do
  let (year, _, _) = toGregorian day
      start = fromGregorian year 1 1
      next  = fromGregorian (year + 1) 1 1
  countForYear <- count [ReceiptIssueDate >=. start, ReceiptIssueDate <. next]
  let sequenceNumber = countForYear + 1
  pure (T.pack (printf "R-%04d-%04d" year sequenceNumber))

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

hasRole :: RoleEnum -> AuthedUser -> Bool
hasRole roleTag AuthedUser{..} = roleTag `elem` auRoles


requireModule :: AuthedUser -> ModuleAccess -> AppM ()
requireModule user moduleTag
  | hasModuleAccess moduleTag user = pure ()
  | otherwise = throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
  where
    msg = "Missing access to module: " <> moduleName moduleTag
-- User roles API
userRolesServer :: AuthedUser -> ServerT UserRolesAPI AppM
userRolesServer user =
       listUsers
  :<|> userRoutes
  where
    listUsers = do
      requireModule user ModuleAdmin
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool loadUserRoleSummaries

    userRoutes userId =
           getUserRolesH userId
      :<|> updateUserRolesH userId

    getUserRolesH userId = do
      requireModule user ModuleAdmin
      Env pool _ <- ask
      let credKey = toSqlKey userId :: Key UserCredential
      mRoles <- liftIO $ flip runSqlPool pool $ do
        mCred <- getEntity credKey
        case mCred of
          Nothing -> pure Nothing
          Just (Entity _ cred) -> do
            rows <- selectList
              [ PartyRolePartyId ==. userCredentialPartyId cred
              , PartyRoleActive ==. True
              ]
              [Asc PartyRoleRole]
            pure $ Just (map (partyRoleRole . entityVal) rows)
      maybe (throwError err404) pure mRoles

    updateUserRolesH userId UserRoleUpdatePayload{roles = payloadRoles} = do
      requireModule user ModuleAdmin
      Env pool _ <- ask
      let credKey = toSqlKey userId :: Key UserCredential
      updated <- liftIO $ flip runSqlPool pool $ do
        mCred <- getEntity credKey
        case mCred of
          Nothing -> pure False
          Just (Entity _ cred) -> do
            applyRoles (userCredentialPartyId cred) payloadRoles
            pure True
      unless updated $ throwError err404
      pure NoContent

loadUserRoleSummaries :: SqlPersistT IO [UserRoleSummaryDTO]
loadUserRoleSummaries = do
  creds <- selectList [] [Asc UserCredentialId]
  mapM summarize creds
  where
    summarize (Entity credId cred) = do
      party <- getJustEntity (userCredentialPartyId cred)
      roles <- selectList
        [ PartyRolePartyId ==. userCredentialPartyId cred
        , PartyRoleActive ==. True
        ]
        [Asc PartyRoleRole]
      pure UserRoleSummaryDTO
        { id        = fromSqlKey credId
        , name      = M.partyDisplayName (entityVal party)
        , email     = M.partyPrimaryEmail (entityVal party)
        , phone     = M.partyPrimaryPhone (entityVal party)
        , roles     = map (partyRoleRole . entityVal) roles
        , status    = if userCredentialActive cred then AccountStatusActive else AccountStatusInactive
        , createdAt = M.partyCreatedAt (entityVal party)
        }

applyRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO ()
applyRoles partyKey rolesList = do
  existing <- selectList [PartyRolePartyId ==. partyKey] []
  let desired = Set.fromList rolesList
  forM_ (Set.toList desired) $ \role ->
    void $ upsert (PartyRole partyKey role True) [PartyRoleActive =. True]
  forM_ existing $ \(Entity roleId record) ->
    when (partyRoleActive record && Set.notMember (partyRoleRole record) desired) $
      update roleId [PartyRoleActive =. False]

adsPublicServer :: ServerT AdsPublicAPI AppM
adsPublicServer = adsInquiryPublic :<|> adsAssistPublic

adsInquiryPublic :: AdsInquiry -> AppM AdsInquiryOut
adsInquiryPublic inquiry = do
  env <- ask
  now <- liftIO getCurrentTime
  partyId <- runDB $ ensurePartyForInquiry inquiry now
  (mSubjectKey, courseLabel) <- runDB $ resolveSubject (aiCourse inquiry)
  inquiryId <- runDB $ do
    rid <- insert (Trials.LeadInterest
      { Trials.leadInterestPartyId   = partyId
      , Trials.leadInterestInterestType = "ad_inquiry"
      , Trials.leadInterestSubjectId = mSubjectKey
      , Trials.leadInterestDetails   = fmap T.strip (aiMessage inquiry)
      , Trials.leadInterestSource    = T.toLower (fromMaybe "ads" (aiChannel inquiry))
      , Trials.leadInterestDriveLink = Nothing
      , Trials.leadInterestStatus    = "Open"
      , Trials.leadInterestCreatedAt = now
      })
    pure rid
  channels <- liftIO $ sendAutoReplies (envConfig env) inquiry courseLabel
  pure AdsInquiryOut
    { aioOk = True
    , aioInquiryId = entityKeyInt inquiryId
    , aioPartyId = entityKeyInt partyId
    , aioRepliedVia = channels
    }

adsAssistPublic :: AdsAssistRequest -> AppM AdsAssistResponse
adsAssistPublic AdsAssistRequest{aarAdId, aarCampaignId, aarMessage, aarChannel} = do
  let body = T.strip aarMessage
  when (T.null body) $ throwBadRequest "Mensaje vacío"
  env <- ask
  let adKey = fmap toSqlKey aarAdId :: Maybe ME.AdCreativeId
      campaignKey = fmap toSqlKey aarCampaignId :: Maybe ME.CampaignId
      hasScope = isJust adKey || isJust campaignKey
  candidateAds <- runDB $
    case campaignKey of
      Nothing -> pure (maybeToList adKey)
      Just ck -> do
        ads <- selectKeysList [ME.AdCreativeCampaignId ==. Just ck] []
        pure (maybe ads (:ads) adKey)
  examples <- runDB $ loadAdExamples hasScope candidateAds
  kb <- liftIO $ retrieveRagContext (envConfig env) (envPool env) body
  reply <- liftIO $ runRagChatWithStatus (envConfig env) kb examples body aarChannel
  finalReply <-
    case reply of
      Right txt | not (T.null (T.strip txt)) -> pure (T.strip txt)
      Left err
        | shouldRetryWithFallbackModel 0 err ->
            pure (adsAssistNoAiFallback (envConfig env))
      Left err ->
        throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 err) }
      _ ->
        throwError err502 { errBody = "No pude generar una respuesta automática en este momento." }
  pure AdsAssistResponse
    { aasReply = finalReply
    , aasUsedExamples = map adExampleToDTO examples
    , aasKnowledgeUsed = kb
    }

adsAssistNoAiFallback :: AppConfig -> Text
adsAssistNoAiFallback cfg =
  let courseUrl = resolveConfiguredAppBase cfg <> "/curso/" <> courseSlugFallback cfg
  in T.intercalate " "
       [ "Gracias por escribirnos."
       , "Ahora mismo el asistente automático no está disponible,"
       , "pero te comparto lo principal:"
       , "el Curso de Producción Musical es presencial en Quito,"
       , "dura cuatro sábados (16 horas en total), cuesta $150 USD y tiene cupos limitados."
       , "Más info e inscripción aquí:"
       , courseUrl
       ]

adsAdminServer :: AuthedUser -> ServerT AdsAdminAPI AppM
adsAdminServer user =
       adsListInquiries user
  :<|> adsListCampaigns user
  :<|> adsUpsertCampaign user
  :<|> adsGetCampaign user
  :<|> adsUpsertAd user
  :<|> adsListAdsForCampaign user
  :<|> adsListExamples user
  :<|> adsCreateExample user

adsListInquiries :: AuthedUser -> AppM [AdsInquiryDTO]
adsListInquiries user = do
  requireModule user ModuleAdmin
  rows <- runDB $ selectList [Trials.LeadInterestInterestType ==. "ad_inquiry"] [Desc Trials.LeadInterestCreatedAt, LimitTo 200]
  let partyIds = map (Trials.leadInterestPartyId . entityVal) rows
      subjectIds = mapMaybe (Trials.leadInterestSubjectId . entityVal) rows
  parties <- if null partyIds then pure [] else runDB (selectList [M.PartyId <-. partyIds] [])
  subjects <- if null subjectIds then pure [] else runDB (selectList [Trials.SubjectId <-. subjectIds] [])
  let partyMap = Map.fromList [ (entityKey p, entityVal p) | p <- parties ]
      subjectMap = Map.fromList [ (entityKey s, Trials.subjectName (entityVal s)) | s <- subjects ]
  pure
    [ AdsInquiryDTO
        { aidInquiryId = entityKeyInt iid
        , aidCreatedAt = Trials.leadInterestCreatedAt li
        , aidName      = M.partyDisplayName <$> Map.lookup (Trials.leadInterestPartyId li) partyMap
        , aidEmail     = M.partyPrimaryEmail =<< Map.lookup (Trials.leadInterestPartyId li) partyMap
        , aidPhone     = M.partyPrimaryPhone =<< Map.lookup (Trials.leadInterestPartyId li) partyMap
        , aidCourse    = (Trials.leadInterestSubjectId li >>= (`Map.lookup` subjectMap)) <|> Trials.leadInterestDetails li
        , aidMessage   = Trials.leadInterestDetails li
        , aidChannel   = Just (Trials.leadInterestSource li)
        , aidStatus    = Trials.leadInterestStatus li
        }
    | Entity iid li <- rows
    ]

adsListCampaigns :: AuthedUser -> AppM [CampaignDTO]
adsListCampaigns user = do
  requireModule user ModuleAdmin
  rows <- runDB $ selectList [] [Desc ME.CampaignUpdatedAt, LimitTo 200]
  pure (map campaignToDTO rows)

adsGetCampaign :: AuthedUser -> Int64 -> AppM CampaignDTO
adsGetCampaign user cid = do
  requireModule user ModuleAdmin
  let key = toSqlKey cid :: ME.CampaignId
  mRow <- runDB $ get key
  case mRow of
    Nothing -> throwError err404
    Just row -> pure (campaignToDTO (Entity key row))

adsUpsertCampaign :: AuthedUser -> CampaignUpsert -> AppM CampaignDTO
adsUpsertCampaign user CampaignUpsert{..} = do
  requireModule user ModuleAdmin
  let nameClean = T.strip cuName
      statusVal =
        case cuStatus of
          Nothing -> "active"
          Just raw ->
            let trimmed = T.strip raw
            in if T.null trimmed then "active" else trimmed
  when (T.null nameClean) $ throwBadRequest "Nombre requerido"
  now <- liftIO getCurrentTime
  cid <- case cuId of
    Nothing -> runDB $ insert ME.Campaign
      { ME.campaignName = nameClean
      , ME.campaignObjective = T.strip <$> cuObjective
      , ME.campaignPlatform = T.strip <$> cuPlatform
      , ME.campaignStatus = statusVal
      , ME.campaignBudgetCents = cuBudgetCents
      , ME.campaignStartDate = cuStartDate
      , ME.campaignEndDate = cuEndDate
      , ME.campaignCreatedAt = now
      , ME.campaignUpdatedAt = now
      }
    Just rawId -> do
      let key = toSqlKey rawId :: ME.CampaignId
      mCampaign <- runDB $ get key
      when (isNothing mCampaign) $ throwError err404
      runDB $ update key
        [ ME.CampaignName =. nameClean
        , ME.CampaignObjective =. (T.strip <$> cuObjective)
        , ME.CampaignPlatform =. (T.strip <$> cuPlatform)
        , ME.CampaignStatus =. statusVal
        , ME.CampaignBudgetCents =. cuBudgetCents
        , ME.CampaignStartDate =. cuStartDate
        , ME.CampaignEndDate =. cuEndDate
        , ME.CampaignUpdatedAt =. now
        ]
      pure key
  row <- runDB $ getJust cid
  pure (campaignToDTO (Entity cid row))

adsUpsertAd :: AuthedUser -> AdCreativeUpsert -> AppM AdCreativeDTO
adsUpsertAd user AdCreativeUpsert{..} = do
  requireModule user ModuleAdmin
  let nameClean = T.strip acuName
      mCampaign = fmap toSqlKey acuCampaignId :: Maybe ME.CampaignId
      statusVal =
        case acuStatus of
          Nothing -> "active"
          Just raw ->
            let trimmed = T.strip raw
            in if T.null trimmed then "active" else trimmed
  when (T.null nameClean) $ throwBadRequest "Nombre del anuncio requerido"
  case mCampaign of
    Nothing -> pure ()
    Just key -> do
      mExisting <- runDB $ get key
      when (isNothing mExisting) $ throwError err404 { errBody = "Campaign not found" }
  now <- liftIO getCurrentTime
  adId <- case acuId of
    Nothing -> runDB $ insert ME.AdCreative
      { ME.adCreativeCampaignId = mCampaign
      , ME.adCreativeExternalId = fmap T.strip acuExternalId
      , ME.adCreativeName = nameClean
      , ME.adCreativeChannel = T.strip <$> acuChannel
      , ME.adCreativeAudience = T.strip <$> acuAudience
      , ME.adCreativeLandingUrl = T.strip <$> acuLandingUrl
      , ME.adCreativeCta = T.strip <$> acuCta
      , ME.adCreativeStatus = statusVal
      , ME.adCreativeNotes = T.strip <$> acuNotes
      , ME.adCreativeCreatedAt = now
      , ME.adCreativeUpdatedAt = now
      }
    Just rawId -> do
      let key = toSqlKey rawId :: ME.AdCreativeId
      mAd <- runDB $ get key
      when (isNothing mAd) $ throwError err404
      runDB $ update key
        [ ME.AdCreativeCampaignId =. mCampaign
        , ME.AdCreativeExternalId =. (T.strip <$> acuExternalId)
        , ME.AdCreativeName =. nameClean
        , ME.AdCreativeChannel =. (T.strip <$> acuChannel)
        , ME.AdCreativeAudience =. (T.strip <$> acuAudience)
        , ME.AdCreativeLandingUrl =. (T.strip <$> acuLandingUrl)
        , ME.AdCreativeCta =. (T.strip <$> acuCta)
        , ME.AdCreativeStatus =. statusVal
        , ME.AdCreativeNotes =. (T.strip <$> acuNotes)
        , ME.AdCreativeUpdatedAt =. now
        ]
      pure key
  row <- runDB $ getJust adId
  pure (adToDTO (Entity adId row))

adsListAdsForCampaign :: AuthedUser -> Int64 -> AppM [AdCreativeDTO]
adsListAdsForCampaign user cid = do
  requireModule user ModuleAdmin
  let key = toSqlKey cid :: ME.CampaignId
  rows <- runDB $ selectList [ME.AdCreativeCampaignId ==. Just key] [Desc ME.AdCreativeUpdatedAt, LimitTo 200]
  pure (map adToDTO rows)

adsListExamples :: AuthedUser -> Int64 -> AppM [AdConversationExampleDTO]
adsListExamples user adId = do
  requireModule user ModuleAdmin
  let key = toSqlKey adId :: ME.AdCreativeId
  rows <- runDB $ selectList [ME.AdConversationExampleAdId ==. key] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 200]
  pure (map adExampleToDTO rows)

adsCreateExample :: AuthedUser -> Int64 -> AdConversationExampleCreate -> AppM AdConversationExampleDTO
adsCreateExample user adId AdConversationExampleCreate{..} = do
  requireModule user ModuleAdmin
  let userMsg = T.strip aecUserMessage
      assistantMsg = T.strip aecAssistantMessage
      key = toSqlKey adId :: ME.AdCreativeId
  when (T.null userMsg) $ throwBadRequest "Ejemplo sin pregunta"
  when (T.null assistantMsg) $ throwBadRequest "Ejemplo sin respuesta"
  mAd <- runDB $ get key
  when (isNothing mAd) $ throwError err404
  now <- liftIO getCurrentTime
  exId <- runDB $ insert ME.AdConversationExample
    { ME.adConversationExampleAdId = key
    , ME.adConversationExampleUserMessage = userMsg
    , ME.adConversationExampleAssistantMessage = assistantMsg
    , ME.adConversationExampleTags = aecTags
    , ME.adConversationExampleCreatedAt = now
    , ME.adConversationExampleUpdatedAt = now
    }
  row <- runDB $ getJust exId
  pure (adExampleToDTO (Entity exId row))

loadAdExamples :: Bool -> [ME.AdCreativeId] -> SqlPersistT IO [Entity ME.AdConversationExample]
loadAdExamples hasScope adIds
  | hasScope && null adIds = pure []
  | null adIds = selectList [] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 6]
  | otherwise = selectList [ME.AdConversationExampleAdId <-. adIds] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 6]

adExampleToDTO :: Entity ME.AdConversationExample -> AdConversationExampleDTO
adExampleToDTO (Entity eid ex) =
  AdConversationExampleDTO
    { aceId = fromSqlKey eid
    , aceAdId = fromSqlKey (ME.adConversationExampleAdId ex)
    , aceUserMessage = ME.adConversationExampleUserMessage ex
    , aceAssistantMessage = ME.adConversationExampleAssistantMessage ex
    , aceTags = ME.adConversationExampleTags ex
    }

campaignToDTO :: Entity ME.Campaign -> CampaignDTO
campaignToDTO (Entity cid c) =
  CampaignDTO
    { campId = fromSqlKey cid
    , campName = ME.campaignName c
    , campObjective = ME.campaignObjective c
    , campPlatform = ME.campaignPlatform c
    , campStatus = ME.campaignStatus c
    , campBudgetCents = ME.campaignBudgetCents c
    , campStartDate = ME.campaignStartDate c
    , campEndDate = ME.campaignEndDate c
    }

adToDTO :: Entity ME.AdCreative -> AdCreativeDTO
adToDTO (Entity aid a) =
  AdCreativeDTO
    { adId = fromSqlKey aid
    , adCampaignId = fmap fromSqlKey (ME.adCreativeCampaignId a)
    , adExternalId = ME.adCreativeExternalId a
    , adName = ME.adCreativeName a
    , adChannel = ME.adCreativeChannel a
    , adAudience = ME.adCreativeAudience a
    , adLandingUrl = ME.adCreativeLandingUrl a
    , adCta = ME.adCreativeCta a
    , adStatus = ME.adCreativeStatus a
    , adNotes = ME.adCreativeNotes a
    }

data OpenAIChatMessage = OpenAIChatMessage
  { role :: Text
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON OpenAIChatMessage where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance FromJSON OpenAIChatMessage where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatCompletionReq = ChatCompletionReq
  { model :: Text
  , messages :: [OpenAIChatMessage]
  , temperature :: Double
  } deriving (Show, Generic)

instance ToJSON ChatCompletionReq where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatChoice = ChatChoice
  { message :: OpenAIChatMessage
  } deriving (Show, Generic)
instance FromJSON ChatChoice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

data ChatCompletionResp = ChatCompletionResp
  { choices :: [ChatChoice]
  } deriving (Show, Generic)
instance FromJSON ChatCompletionResp where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

runRagChat :: AppConfig -> [Text] -> [Entity ME.AdConversationExample] -> Text -> Maybe Text -> IO Text
runRagChat cfg kb examples userMsg mChannel = do
  res <- runRagChatWithStatus cfg kb examples userMsg mChannel
  pure $
    case res of
      Right txt | not (T.null (T.strip txt)) -> T.strip txt
      _ -> "No pude generar una respuesta automática en este momento."

runRagChatWithStatus
  :: AppConfig
  -> [Text]
  -> [Entity ME.AdConversationExample]
  -> Text
  -> Maybe Text
  -> IO (Either Text Text)
runRagChatWithStatus cfg kb examples userMsg mChannel =
  callOpenAIChat cfg (buildRagMessages kb examples userMsg mChannel)

buildRagMessages
  :: [Text]
  -> [Entity ME.AdConversationExample]
  -> Text
  -> Maybe Text
  -> [OpenAIChatMessage]
buildRagMessages kb examples userMsg mChannel =
  let contextBlock = if null kb then "No hay contexto" else T.intercalate "\n\n" kb
      exampleMsgs = concatMap exampleToMessages examples
      channelNote = maybe "" (\ch -> "[Canal: " <> T.strip ch <> "] ") mChannel
      systemIntro = T.intercalate "\n"
        [ "Eres un asistente de marketing de TDF Records."
        , "Responde en español (Quito, Ecuador), tono cálido y conciso."
        , "Tu objetivo es ayudar al usuario y, cuando sea relevante, promocionar el Curso de Producción Musical (presencial, cuatro sábados, 16 horas en total, $150 USD, cupos limitados) con link: https://tdf-app.pages.dev/curso/produccion-musical-abr-2026"
        , "REGLA CRÍTICA DE FORMATO: responde SOLO con una de estas dos formas:"
        , "1) SEND: <tu respuesta final para enviar al usuario>"
        , "2) HOLD: <por qué no puedes responder todavía>\\nNEED: <lista corta de datos que faltan>"
        , "No incluyas nada fuera de ese formato."
        ]
  in [ mkMsg "system" systemIntro
     , mkMsg "system" ("Contexto de negocio:\n" <> contextBlock)
     ] ++ exampleMsgs ++ [mkMsg "user" (channelNote <> userMsg)]
  where
    mkMsg r c = OpenAIChatMessage { role = r, content = c }
    exampleToMessages (Entity _ ex) =
      [ mkMsg "user" (ME.adConversationExampleUserMessage ex)
      , mkMsg "assistant" (ME.adConversationExampleAssistantMessage ex)
      ]

callOpenAIChat :: AppConfig -> [OpenAIChatMessage] -> IO (Either Text Text)
callOpenAIChat cfg messages =
  case openAiApiKey cfg of
    Nothing -> pure (Left "OPENAI_API_KEY no configurada")
    Just key -> do
      manager <- newManager tlsManagerSettings
      reqBase <- parseRequest "https://api.openai.com/v1/chat/completions"
      let models = openAIChatModelCandidates (openAiModel cfg)
      tryModels manager reqBase key models Nothing
  where
    tryModels :: Manager -> Request -> Text -> [Text] -> Maybe Text -> IO (Either Text Text)
    tryModels _ _ _ [] lastErr =
      pure (Left (fromMaybe "No pude generar una respuesta automática en este momento." lastErr))
    tryModels manager reqBase key (modelName:remaining) _ = do
      attempt <- requestOpenAIChat manager reqBase key modelName messages
      case attempt of
        Right reply -> pure (Right reply)
        Left (status, msg)
          | shouldRetryWithFallbackModel status msg && not (null remaining) -> do
              hPutStrLn stderr
                ("[openai] Modelo " <> T.unpack modelName <> " no disponible (" <> show status <> "). Intentando fallback.")
              tryModels manager reqBase key remaining (Just msg)
          | otherwise ->
              pure (Left msg)

requestOpenAIChat
  :: Manager
  -> Request
  -> Text
  -> Text
  -> [OpenAIChatMessage]
  -> IO (Either (Int, Text) Text)
requestOpenAIChat manager reqBase key modelName messages = do
  let body = encode ChatCompletionReq
        { model = modelName
        , messages = messages
        , temperature = 0.3
        }
      req =
        reqBase
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 key)
              ]
          , requestBody = RequestBodyLBS body
          }
  resp <- httpLbs req manager
  let status = statusCode (responseStatus resp)
      raw = responseBody resp
  if status >= 200 && status < 300
    then case (eitherDecode raw :: Either String Value) of
      Left err ->
        pure (Left (status, T.pack err))
      Right payload ->
        case extractModelReplyText payload of
          Just reply -> pure (Right reply)
          Nothing ->
            pure (Left (status, fromMaybe "Sin respuesta de modelo" (extractApiErrorMessage payload)))
    else do
      let baseMsg = "Error al generar respuesta (HTTP " <> T.pack (show status) <> ")"
          msg = case (eitherDecode raw :: Either String Value) of
            Right payload -> fromMaybe baseMsg (extractApiErrorMessage payload)
            Left _ -> baseMsg
      pure (Left (status, msg))

openAIChatModelCandidates :: Text -> [Text]
openAIChatModelCandidates primaryModel =
  nub $
    filter (not . T.null)
      [ T.strip primaryModel
      , "gpt-4.1-mini"
      , "gpt-4.1-nano"
      , "gpt-4o-mini"
      , "gpt-3.5-turbo"
      , "gpt-4-turbo"
      , "gpt-4.1"
      ]

shouldRetryWithFallbackModel :: Int -> Text -> Bool
shouldRetryWithFallbackModel status rawMessage =
  hasMarker || (status `elem` [400, 401, 403, 404] && "error al generar respuesta (http " `T.isPrefixOf` msg)
  where
    msg = T.toLower (T.strip rawMessage)
    hasMarker = any (`T.isInfixOf` msg) markers
    markers =
      [ "does not have access to model"
      , "do not have access to model"
      , "access to model"
      , "model not found"
      , "unknown model"
      , "invalid model"
      , "not a valid model"
      , "model_not_found"
      ]

tidalSystemPrompt :: Text
tidalSystemPrompt = T.intercalate "\n"
  [ "You are a TidalCycles code generator."
  , "- Reply ONLY with TidalCycles code, no prose or markdown."
  , "- Keep patterns short, loopable, and safe to evaluate."
  , "- Use d1/d2/d3/d4, stack, sound, n, note, cps/bps, hush. Avoid file I/O or shell commands."
  , "- Prefer concise percussive or melodic patterns; avoid very long sequences."
  ]

tidalAgentServer :: AuthedUser -> TidalAgentRequest -> AppM TidalAgentResponse
tidalAgentServer _ TidalAgentRequest{..} = do
  Env{..} <- ask
  let prompt = T.strip taPrompt
  when (T.null prompt) $ throwBadRequest "Prompt requerido"
  when (T.length prompt > 2000) $ throwBadRequest "Prompt demasiado largo (max 2000 caracteres)"
  apiKey <- case openAiApiKey envConfig of
    Nothing -> throwError err503 { errBody = "OPENAI_API_KEY no configurada" }
    Just key -> pure key
  let model = fromMaybe (openAiModel envConfig) (taModel >>= nonEmptyText)
  manager <- liftIO $ newManager tlsManagerSettings
  reqBase <- liftIO $ parseRequest "https://api.openai.com/v1/chat/completions"
  let body = encode $ object
        [ "model" .= model
        , "messages" .=
            [ object ["role" .= ("system" :: Text), "content" .= tidalSystemPrompt]
            , object ["role" .= ("user" :: Text), "content" .= prompt]
            ]
        , "temperature" .= (0.6 :: Double)
        , "max_tokens" .= (300 :: Int)
        ]
      req =
        reqBase
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
              ]
          , requestBody = RequestBodyLBS body
          }
  resp <- liftIO $ httpLbs req manager
  let status = statusCode (responseStatus resp)
      raw = responseBody resp
  if status >= 200 && status < 300
    then case (eitherDecode raw :: Either String Value) of
      Left err ->
        throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack err)) }
      Right payload ->
        case extractModelReplyText payload of
          Just reply ->
            pure (TidalAgentResponse reply)
          Nothing -> do
            let msg = fromMaybe "Sin respuesta de modelo" (extractApiErrorMessage payload)
            throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
    else do
      let baseMsg = "Error al generar respuesta (HTTP " <> T.pack (show status) <> ")"
          msg = case (eitherDecode raw :: Either String Value) of
            Right payload -> fromMaybe baseMsg (extractApiErrorMessage payload)
            Left _ -> baseMsg
      throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

chatkitSessionServer :: AuthedUser -> ChatKitSessionRequest -> AppM ChatKitSessionResponse
chatkitSessionServer user ChatKitSessionRequest{..} = do
  Env{..} <- ask
  let cfg = envConfig
  workflowId <- case resolveWorkflowId cksWorkflowId (chatKitWorkflowId cfg) of
    Nothing -> throwBadRequest "workflowId requerido"
    Just val -> pure val
  apiKey <- case openAiApiKey cfg of
    Nothing -> throwError err503 { errBody = "OPENAI_API_KEY no configurada" }
    Just key -> pure key
  let userId = T.pack (show (fromSqlKey (auPartyId user)))
      apiBase = normalizeChatKitBase (chatKitApiBase cfg)
  manager <- liftIO $ newManager tlsManagerSettings
  reqBase <- liftIO $ parseRequest (T.unpack (apiBase <> "/v1/chatkit/sessions"))
  let body = encode $ object
        [ "workflow" .= object ["id" .= workflowId]
        , "user" .= userId
        ]
      req =
        reqBase
          { method = "POST"
          , requestHeaders =
              [ ("Content-Type", "application/json")
              , ("Authorization", "Bearer " <> TE.encodeUtf8 apiKey)
              , ("OpenAI-Beta", "chatkit_beta=v1")
              ]
          , requestBody = RequestBodyLBS body
          }
  resp <- liftIO $ httpLbs req manager
  let status = statusCode (responseStatus resp)
      raw = responseBody resp
  case eitherDecode raw of
    Left err ->
      throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack err)) }
    Right payload ->
      if status >= 200 && status < 300
        then case extractChatKitSession payload of
          Just (secret, expiresAfter) ->
            pure ChatKitSessionResponse
              { ckrClientSecret = secret
              , ckrExpiresAfter = expiresAfter
              }
          Nothing ->
            throwError err502 { errBody = "ChatKit respondió sin client_secret" }
        else do
          let baseMsg = "Error al crear sesión ChatKit (HTTP " <> T.pack (show status) <> ")"
              msg = fromMaybe baseMsg (extractApiErrorMessage payload)
          throwError err502 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

resolveWorkflowId :: Maybe Text -> Maybe Text -> Maybe Text
resolveWorkflowId primary fallback =
  (primary <|> fallback) >>= nonEmptyText

normalizeChatKitBase :: Text -> Text
normalizeChatKitBase base =
  let trimmed = T.dropWhileEnd (== '/') (T.strip base)
  in if T.null trimmed then "https://api.openai.com" else trimmed

nonEmptyText :: Text -> Maybe Text
nonEmptyText txt =
  let trimmed = T.strip txt
  in if T.null trimmed then Nothing else Just trimmed

extractChatKitSession :: Value -> Maybe (Text, Maybe Value)
extractChatKitSession = parseMaybe $ withObject "ChatKitSession" $ \o -> do
  secret <- o .: "client_secret"
  expiresAfter <- o .:? "expires_after"
  pure (secret, expiresAfter)

extractApiErrorMessage :: Value -> Maybe Text
extractApiErrorMessage = parseMaybe $ withObject "ApiError" $ \o -> do
  mErr <- o .:? "error"
  case mErr of
    Just (String msg) -> pure msg
    Just (Object errObj) -> requireText =<< errObj .:? "message"
    _ -> requireText =<< o .:? "message"
  where
    requireText mVal =
      case mVal of
        Just txt -> pure txt
        Nothing -> fail "message missing"

extractModelReplyText :: Value -> Maybe Text
extractModelReplyText payload =
  extractChatCompletionText payload
    <|> extractResponsesOutputText payload

extractChatCompletionText :: Value -> Maybe Text
extractChatCompletionText payload = do
  ChatCompletionResp{choices = (ChatChoice OpenAIChatMessage{content = reply} : _)} <- parseMaybe parseJSON payload
  nonEmptyText reply

extractResponsesOutputText :: Value -> Maybe Text
extractResponsesOutputText payload = parseMaybe parsePayload payload
  where
    parsePayload = withObject "ResponsesPayload" $ \o -> do
      mOutputText <- o .:? "output_text"
      case mOutputText >>= nonEmptyText of
        Just txt -> pure txt
        Nothing -> do
          outputs <- o .:? "output" .!= ([] :: [Value])
          let fragments = concatMap extractOutputFragments outputs
          case nonEmptyText (T.intercalate "\n" fragments) of
            Just txt -> pure txt
            Nothing -> fail "output text missing"

extractOutputFragments :: Value -> [Text]
extractOutputFragments value =
  let directText =
        catMaybes
          [ join (parseMaybe (withObject "OutputItemText" (\o -> o .:? "text")) value) >>= nonEmptyText
          , join (parseMaybe (withObject "OutputItemContentText" (\o -> o .:? "content")) value) >>= nonEmptyText
          ]
      contentParts =
        fromMaybe []
          (parseMaybe (withObject "OutputItemContent" (\o -> o .:? "content" .!= ([] :: [Value]))) value)
      partText =
        concatMap
          (\part ->
            catMaybes
              [ join (parseMaybe (withObject "OutputPartText" (\o -> o .:? "text")) part) >>= nonEmptyText
              , join (parseMaybe (withObject "OutputPartContentText" (\o -> o .:? "content")) part) >>= nonEmptyText
              ]
          )
          contentParts
  in directText <> partText

ensurePartyForInquiry :: AdsInquiry -> UTCTime -> SqlPersistT IO PartyId
ensurePartyForInquiry AdsInquiry{..} now = do
  let emailClean = T.strip <$> aiEmail
      phoneClean = aiPhone >>= normalizePhone
      display = fromMaybe "Contacto Ads" (T.strip <$> aiName)
  mExisting <- case emailClean of
    Just e  -> selectFirst [M.PartyPrimaryEmail ==. Just e] []
    Nothing -> case phoneClean of
      Just p  -> selectFirst [M.PartyPrimaryPhone ==. Just p] []
      Nothing -> pure Nothing
  case mExisting of
    Just (Entity pid party) -> do
      let updates = catMaybes
            [ if isJust (M.partyPrimaryEmail party) || isNothing emailClean then Nothing else Just (M.PartyPrimaryEmail =. emailClean)
            , if isJust (M.partyPrimaryPhone party) || isNothing phoneClean then Nothing else Just (M.PartyPrimaryPhone =. phoneClean)
            , if isJust (M.partyWhatsapp party) || isNothing phoneClean then Nothing else Just (M.PartyWhatsapp =. phoneClean)
            , if T.null (M.partyDisplayName party) && not (T.null display) then Just (M.PartyDisplayName =. display) else Nothing
            ]
      unless (null updates) $
        update pid updates
      ensureStudentRole pid
      pure pid
    Nothing -> do
      pid <- insert M.Party
        { M.partyLegalName        = Nothing
        , M.partyDisplayName      = display
        , M.partyIsOrg            = False
        , M.partyTaxId            = Nothing
        , M.partyPrimaryEmail     = emailClean
        , M.partyPrimaryPhone     = phoneClean
        , M.partyWhatsapp         = phoneClean
        , M.partyInstagram        = Nothing
        , M.partyEmergencyContact = Nothing
        , M.partyNotes            = Nothing
        , M.partyCreatedAt        = now
        }
      ensureStudentRole pid
      pure pid
  where
    ensureStudentRole pid = void $ upsert (M.PartyRole pid Student True) [M.PartyRoleActive =. True]

resolveSubject :: Maybe Text -> SqlPersistT IO (Maybe (Key Trials.Subject), Maybe Text)
resolveSubject mCourse =
  case fmap (T.toLower . T.strip) mCourse of
    Nothing -> pure (Nothing, Nothing)
    Just raw -> do
      mSubject <- selectFirst [Trials.SubjectName ==. canonicalCourse raw] []
      pure (entityKey <$> mSubject, Just (canonicalCourse raw))
  where
    canonicalCourse txt
      | "dj" `T.isInfixOf` txt = "DJ"
      | "ableton" `T.isInfixOf` txt = "Producción Música Electrónica (Ableton Live)"
      | "logic" `T.isInfixOf` txt || "luna" `T.isInfixOf` txt = "Producción Musical (Logic/Luna)"
      | "bajo" `T.isInfixOf` txt = "Bajo"
      | "guitarra" `T.isInfixOf` txt = "Guitarra"
      | "teclado" `T.isInfixOf` txt || "piano" `T.isInfixOf` txt = "Teclado"
      | "bateria" `T.isInfixOf` txt = "Batería"
      | "sint" `T.isInfixOf` txt = "Síntesis Modular"
      | otherwise = txt

sendAutoReplies :: AppConfig -> AdsInquiry -> Maybe Text -> IO [Text]
sendAutoReplies cfg AdsInquiry{..} mCourse = do
  mgr <- newManager tlsManagerSettings
  wa <- loadWhatsAppEnv
  let ctaBase = resolveConfiguredAppBase cfg
      cta = ctaBase <> "/trials"
      courseLanding = buildLandingUrl cfg
      courseLabel = fromMaybe "las clases 1:1" mCourse
      body = T.intercalate "\n"
        [ "Hola " <> fromMaybe "" aiName <> " 🙌"
        , "Gracias por tu interés en " <> courseLabel <> "."
        , "Paquete 1:1 (16 horas): $480."
        , "Grupo pequeño: más info y fechas en " <> courseLanding
        , "¿Te agendo una clase de prueba gratis para ver horarios y profesor ideal?"
        , "Confírmame tu disponibilidad y ciudad. Agendamos aquí: " <> cta
        ]
  channels <- fmap catMaybes . sequence $
    [ case (waToken wa, waPhoneId wa, aiPhone >>= normalizePhone) of
        (Just tok, Just pid, Just ph) ->
          do res <- sendText mgr (fromMaybe "v20.0" (waApiVersion wa)) tok pid ph body
             pure (either (const Nothing) (const (Just "whatsapp")) res)
        _ -> pure Nothing
    , case aiEmail of
        Just e -> do
          let svc = EmailSvc.mkEmailService cfg
          EmailSvc.sendTestEmail svc (fromMaybe "Amigo TDF" aiName) e "Gracias por tu interés en TDF"
            [ "Paquete 1:1 (16 horas): $480."
            , "Grupo pequeño: detalles y fechas en " <> courseLanding
            , "Agenda tu clase de prueba gratis aquí: " <> cta <> "/trials"
            ] (Just (cta <> "/trials"))
          pure (Just "email")
        Nothing -> pure Nothing
    ]
  pure channels

cmsPublicServer :: ServerT CmsPublicAPI AppM
cmsPublicServer = cmsGet :<|> cmsList
  where
    fallbackContent slug locale = do
      now <- liftIO getCurrentTime
      pure CmsContentDTO
        { ccdId          = 0
        , ccdSlug        = slug
        , ccdLocale      = locale
        , ccdVersion     = 0
        , ccdStatus      = "missing"
        , ccdTitle       = Nothing
        , ccdPayload     = Nothing
        , ccdCreatedAt   = now
        , ccdPublishedAt = Nothing
        }

    cmsGet mSlug mLocale = do
      slug <- maybe (throwError err400 { errBody = "slug requerido" }) pure mSlug
      let locale = fromMaybe "es" mLocale
      mPublished <- runDB $ selectFirst
        [ CMS.CmsContentSlug ==. slug
        , CMS.CmsContentLocale ==. locale
        , CMS.CmsContentStatus ==. "published"
        ] [Desc CMS.CmsContentVersion]
      contentDTO <- case mPublished of
        Just ent -> pure (toCmsDTO ent)
        Nothing -> do
          mDraft <- runDB $ selectFirst
            [ CMS.CmsContentSlug ==. slug
            , CMS.CmsContentLocale ==. locale
            ]
            [Desc CMS.CmsContentVersion]
          maybe (fallbackContent slug locale) (pure . toCmsDTO) mDraft
      pure contentDTO

    cmsList mLocale mPrefix = do
      let locale = fromMaybe "es" mLocale
      entries <- runDB $
        selectList
          [ CMS.CmsContentLocale ==. locale
          , CMS.CmsContentStatus ==. "published"
          ]
          [ Desc CMS.CmsContentPublishedAt
          , Desc CMS.CmsContentVersion
          ]
      let filtered = case mPrefix of
            Nothing -> entries
            Just prefix -> filter (\(Entity _ c) -> prefix `T.isPrefixOf` CMS.cmsContentSlug c) entries
      pure (map toCmsDTO filtered)

marketplacePublicServer :: ServerT MarketplaceAPI AppM
marketplacePublicServer =
       listMarketplace
  :<|> getMarketplaceItem
  :<|> createCart
  :<|> getCart
  :<|> upsertCartItem
  :<|> checkoutCart
  :<|> createDatafastCheckout
  :<|> confirmDatafastPayment
  :<|> createPaypalOrder
  :<|> capturePaypalOrder
  :<|> getOrder

marketplaceAdminServer :: AuthedUser -> ServerT MarketplaceAdminAPI AppM
marketplaceAdminServer user =
       listMarketplaceOrders user
  :<|> updateMarketplaceOrder user

labelServer :: AuthedUser -> ServerT LabelAPI AppM
labelServer user =
       listLabelTracks user
  :<|> createLabelTrack user
  :<|> updateLabelTrack user
  :<|> deleteLabelTrack user

contractsServer :: ServerT ContractsAPI AppM
contractsServer = hoistServer contractsProxy lift Contracts.server
  where
    contractsProxy = Proxy :: Proxy ContractsAPI

listMarketplace :: AppM [MarketplaceItemDTO]
listMarketplace = do
  Env{..} <- ask
  let assetsBase = resolveConfiguredAssetsBase envConfig
  let loadListings = do
        listings <-
          selectList
            [ME.MarketplaceListingActive ==. True]
            [Asc ME.MarketplaceListingTitle]
        forM listings $ \(Entity lid listing) -> do
          mAsset <- get (ME.marketplaceListingAssetId listing)
          pure (lid, listing, mAsset)
  rows <- liftIO $ flip runSqlPool envPool loadListings
  if not (null rows)
    then do
      dtos <- forM rows (toMarketplaceDTO assetsBase)
      pure (catMaybes dtos)
    else do
      -- Auto-publish demo inventory so the public marketplace is never empty.
      liftIO $ flip runSqlPool envPool $ do
        seedInventoryAssets
        seedMarketplaceListings
      seeded <- liftIO $ flip runSqlPool envPool loadListings
      dtos <- forM seeded (toMarketplaceDTO assetsBase)
      pure (catMaybes dtos)

getMarketplaceItem :: Text -> AppM MarketplaceItemDTO
getMarketplaceItem rawId = do
  listingKey <- case fromPathPiece rawId of
    Nothing -> throwBadRequest "Invalid listing id"
    Just k  -> pure k
  Env{..} <- ask
  let assetsBase = resolveConfiguredAssetsBase envConfig
  mRow <- liftIO $ flip runSqlPool envPool $ do
    mListing <- get listingKey
    case mListing of
      Nothing -> pure Nothing
      Just listing -> do
        mAsset <- get (ME.marketplaceListingAssetId listing)
        pure (Just (listingKey, listing, mAsset))
  case mRow of
    Nothing -> throwError err404
    Just row -> do
      mDto <- toMarketplaceDTO assetsBase row
      maybe (throwError err404) pure mDto

toMarketplaceDTO
  :: Text
  -> (Key ME.MarketplaceListing, ME.MarketplaceListing, Maybe ME.Asset)
  -> AppM (Maybe MarketplaceItemDTO)
toMarketplaceDTO _ (_, _, Nothing) = pure Nothing
toMarketplaceDTO assetsBase (lid, listing, Just asset) = do
  mPhoto <- liftIO $ resolveMarketplacePhotoUrl assetsBase (ME.assetPhotoUrl asset)
  pure $ Just MarketplaceItemDTO
    { miListingId      = toPathPiece lid
    , miAssetId        = toPathPiece (ME.marketplaceListingAssetId listing)
    , miTitle          = ME.marketplaceListingTitle listing
    , miPurpose        = ME.marketplaceListingPurpose listing
    , miCategory       = ME.assetCategory asset
    , miBrand          = ME.assetBrand asset
    , miModel          = ME.assetModel asset
    , miPhotoUrl       = mPhoto
    , miStatus         = Just (assetStatusLabel (ME.assetStatus asset))
    , miCondition      = Just (assetConditionLabel (ME.assetCondition asset))
    , miPriceUsdCents  = ME.marketplaceListingPriceUsdCents listing
    , miPriceDisplay   =
        formatUsd
          (ME.marketplaceListingPriceUsdCents listing)
          (ME.marketplaceListingCurrency listing)
    , miMarkupPct      = ME.marketplaceListingMarkupPct listing
    , miCurrency       = ME.marketplaceListingCurrency listing
    }

resolveMarketplacePhotoUrl :: Text -> Maybe Text -> IO (Maybe Text)
resolveMarketplacePhotoUrl _ Nothing = pure Nothing
resolveMarketplacePhotoUrl assetsBase (Just raw0) = do
  let trimmed = T.strip raw0
      path = T.dropWhile (== '/') trimmed
  if "inventory/" `T.isPrefixOf` path
    then pure (Just (normalizePhoto assetsBase path))
    else pure (Just (normalizePhoto assetsBase trimmed))

normalizePhoto :: Text -> Text -> Text
normalizePhoto assetsBase raw =
  let trimmed = T.strip raw
      base    = T.dropWhileEnd (== '/') assetsBase
  in if "http://" `T.isPrefixOf` trimmed || "https://" `T.isPrefixOf` trimmed
        then trimmed
        else
          let path0 = T.dropWhile (== '/') trimmed
              path
                | "assets/serve/" `T.isPrefixOf` path0 = T.drop (T.length ("assets/serve/" :: Text)) path0
                | "assets/inventory/" `T.isPrefixOf` path0 = T.drop (T.length ("assets/" :: Text)) path0
                | otherwise = path0
          in base <> "/" <> path

createCart :: AppM MarketplaceCartDTO
createCart = do
  now <- liftIO getCurrentTime
  Env{..} <- ask
  cartId <- liftIO $ flip runSqlPool envPool $ insert $ ME.MarketplaceCart now now
  cartDto <- liftIO $ flip runSqlPool envPool $ loadCartDTO cartId
  maybe (throwError err500) pure cartDto

getCart :: Text -> AppM MarketplaceCartDTO
getCart rawId = do
  cartKey <- parseCartId rawId
  Env{..} <- ask
  mCart <- liftIO $ flip runSqlPool envPool $ loadCartDTO cartKey
  maybe (throwError err404) pure mCart

upsertCartItem :: Text -> MarketplaceCartItemUpdate -> AppM MarketplaceCartDTO
upsertCartItem rawId MarketplaceCartItemUpdate{..} = do
  cartKey <- parseCartId rawId
  listingKey <- parseListingId mciuListingId
  when (mciuQuantity < 0) $ throwBadRequest "quantity must be non-negative"
  now <- liftIO getCurrentTime
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ do
    mListing <- get listingKey
    case mListing of
      Nothing -> pure Nothing
      Just _ -> do
        existing <- selectFirst [ME.MarketplaceCartItemCartId ==. cartKey, ME.MarketplaceCartItemListingId ==. listingKey] []
        case existing of
          Nothing ->
            if mciuQuantity == 0
              then pure ()
              else void $ insert ME.MarketplaceCartItem
                     { ME.marketplaceCartItemCartId = cartKey
                     , ME.marketplaceCartItemListingId = listingKey
                     , ME.marketplaceCartItemQuantity = mciuQuantity
                     }
          Just (Entity itemId _) ->
            if mciuQuantity == 0
              then delete itemId
              else update itemId [ME.MarketplaceCartItemQuantity =. mciuQuantity]
        update cartKey [ME.MarketplaceCartUpdatedAt =. now]
        loadCartDTO cartKey
  maybe (throwError err404) pure mDto

checkoutCart :: Text -> MarketplaceCheckoutReq -> AppM MarketplaceOrderDTO
checkoutCart rawId MarketplaceCheckoutReq{..} = do
  when (T.null (T.strip mcrBuyerName)) $ throwBadRequest "buyerName requerido"
  when (T.null (T.strip mcrBuyerEmail)) $ throwBadRequest "buyerEmail requerido"
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  mOrder <- liftIO $ flip runSqlPool envPool $ do
    mCart <- get cartKey
    case mCart of
      Nothing -> pure Nothing
      Just _ -> do
        mTotals <- loadCartTotals cartKey
        case mTotals of
          Nothing -> pure Nothing
          Just (cartItems, totalCents, currency) -> do
            let statusTxt = if totalCents > 0 then "pending" else "contact"
            orderId <- insert ME.MarketplaceOrder
              { ME.marketplaceOrderCartId        = Just cartKey
              , ME.marketplaceOrderBuyerName     = T.strip mcrBuyerName
              , ME.marketplaceOrderBuyerEmail    = T.strip mcrBuyerEmail
              , ME.marketplaceOrderBuyerPhone    = fmap T.strip mcrBuyerPhone
              , ME.marketplaceOrderTotalUsdCents = totalCents
              , ME.marketplaceOrderCurrency      = currency
              , ME.marketplaceOrderStatus        = statusTxt
              , ME.marketplaceOrderPaymentProvider = Nothing
              , ME.marketplaceOrderPaypalOrderId = Nothing
              , ME.marketplaceOrderPaypalPayerEmail = Nothing
              , ME.marketplaceOrderDatafastCheckoutId = Nothing
              , ME.marketplaceOrderDatafastResourcePath = Nothing
              , ME.marketplaceOrderDatafastPaymentId = Nothing
              , ME.marketplaceOrderDatafastResultCode = Nothing
              , ME.marketplaceOrderDatafastResultDescription = Nothing
              , ME.marketplaceOrderDatafastPaymentBrand = Nothing
              , ME.marketplaceOrderDatafastAuthCode = Nothing
              , ME.marketplaceOrderDatafastAcquirerCode = Nothing
              , ME.marketplaceOrderPaidAt        = Nothing
              , ME.marketplaceOrderCreatedAt     = now
              , ME.marketplaceOrderUpdatedAt     = now
              }
            forM_ cartItems $ \(_, listingEnt, _, qty) -> do
              let listing   = entityVal listingEnt
                  unitPrice = ME.marketplaceListingPriceUsdCents listing
                  subtotal  = unitPrice * qty
              void $ insert ME.MarketplaceOrderItem
                { ME.marketplaceOrderItemOrderId           = orderId
                , ME.marketplaceOrderItemListingId         = entityKey listingEnt
                , ME.marketplaceOrderItemQuantity          = qty
                , ME.marketplaceOrderItemUnitPriceUsdCents = unitPrice
                , ME.marketplaceOrderItemSubtotalUsdCents  = subtotal
                }
            loadOrderDTO orderId
  orderDto <- maybe (throwError err404) pure mOrder
  env <- ask
  -- fire-and-forget email confirmation
  let emailSvc = EmailSvc.mkEmailService (envConfig env)
      buyerNameTxt = T.strip mcrBuyerName
      buyerEmailTxt = T.strip mcrBuyerEmail
      itemsSummary = map (\oi -> T.pack (show (moiQuantity oi)) <> " × " <> moiTitle oi <> " — " <> moiSubtotalDisplay oi) (moItems orderDto)
  liftIO $ void $ forkIO $ do
    _ <- (try $
      EmailSvc.sendMarketplaceOrder
        emailSvc
        buyerNameTxt
        buyerEmailTxt
        (moOrderId orderDto)
        (moTotalDisplay orderDto)
        itemsSummary) :: IO (Either SomeException ())
    pure ()
  pure orderDto

createDatafastCheckout :: Text -> MarketplaceCheckoutReq -> AppM DatafastCheckoutDTO
createDatafastCheckout rawId payload = do
  let nameTxt = T.strip (mcrBuyerName payload)
      emailTxt = T.strip (mcrBuyerEmail payload)
      phoneTxt = fmap T.strip (mcrBuyerPhone payload)
  when (T.null nameTxt) $ throwBadRequest "buyerName requerido"
  when (T.null emailTxt) $ throwBadRequest "buyerEmail requerido"
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  mTotals <- liftIO $ flip runSqlPool envPool $ loadCartTotals cartKey
  case mTotals of
    Nothing -> throwError err404
    Just (cartItems, totalCents, currency) -> do
      when (totalCents <= 0) $ throwBadRequest "El carrito está vacío."
      orderKey <- liftIO $ flip runSqlPool envPool $ do
        oid <- insert ME.MarketplaceOrder
          { ME.marketplaceOrderCartId        = Just cartKey
          , ME.marketplaceOrderBuyerName     = nameTxt
          , ME.marketplaceOrderBuyerEmail    = emailTxt
          , ME.marketplaceOrderBuyerPhone    = phoneTxt
          , ME.marketplaceOrderTotalUsdCents = totalCents
          , ME.marketplaceOrderCurrency      = currency
          , ME.marketplaceOrderStatus        = "datafast_init"
          , ME.marketplaceOrderPaymentProvider = Just "datafast"
          , ME.marketplaceOrderPaypalOrderId = Nothing
          , ME.marketplaceOrderPaypalPayerEmail = Nothing
          , ME.marketplaceOrderDatafastCheckoutId = Nothing
          , ME.marketplaceOrderDatafastResourcePath = Nothing
          , ME.marketplaceOrderDatafastPaymentId = Nothing
          , ME.marketplaceOrderDatafastResultCode = Nothing
          , ME.marketplaceOrderDatafastResultDescription = Nothing
          , ME.marketplaceOrderDatafastPaymentBrand = Nothing
          , ME.marketplaceOrderDatafastAuthCode = Nothing
          , ME.marketplaceOrderDatafastAcquirerCode = Nothing
          , ME.marketplaceOrderPaidAt        = Nothing
          , ME.marketplaceOrderCreatedAt     = now
          , ME.marketplaceOrderUpdatedAt     = now
          }
        forM_ cartItems $ \(_, listingEnt, _, qty) -> do
          let listing   = entityVal listingEnt
              unitPrice = ME.marketplaceListingPriceUsdCents listing
              subtotal  = unitPrice * qty
          void $ insert ME.MarketplaceOrderItem
            { ME.marketplaceOrderItemOrderId           = oid
            , ME.marketplaceOrderItemListingId         = entityKey listingEnt
            , ME.marketplaceOrderItemQuantity          = qty
            , ME.marketplaceOrderItemUnitPriceUsdCents = unitPrice
            , ME.marketplaceOrderItemSubtotalUsdCents  = subtotal
            }
        pure oid
      (checkoutId, widgetUrl) <- requestDatafastCheckout orderKey totalCents currency nameTxt emailTxt phoneTxt
      now2 <- liftIO getCurrentTime
      liftIO $ flip runSqlPool envPool $
        update orderKey
          [ ME.MarketplaceOrderStatus =. "datafast_pending"
          , ME.MarketplaceOrderPaymentProvider =. Just "datafast"
          , ME.MarketplaceOrderDatafastCheckoutId =. Just checkoutId
          , ME.MarketplaceOrderUpdatedAt =. now2
          ]
      pure DatafastCheckoutDTO
        { dcOrderId    = toPathPiece orderKey
        , dcCheckoutId = checkoutId
        , dcWidgetUrl  = T.pack widgetUrl
        , dcAmount     = formatUsd totalCents currency
        , dcCurrency   = currency
        }

confirmDatafastPayment :: Maybe Text -> Maybe Text -> AppM MarketplaceOrderDTO
confirmDatafastPayment mOrderId mResourcePath = do
  resourcePathTxt <- case fmap T.strip mResourcePath of
    Just rp | not (T.null rp) -> pure rp
    _ -> throwBadRequest "resourcePath requerido"
  orderKey <- case mOrderId of
    Just oid -> parseOrderId oid
    Nothing  -> throwBadRequest "orderId requerido"
  Env{ envPool } <- ask
  mOrder <- liftIO $ flip runSqlPool envPool $ get orderKey
  order <- maybe (throwError err404) pure mOrder
  dfEnv <- loadDatafastEnv
  statusResp <- datafastPaymentStatus dfEnv resourcePathTxt
  now <- liftIO getCurrentTime
  let code = dfrCode (dfpResult statusResp)
      success = isDfPaymentSuccess code
      pending = isDfPaymentPending code
      nextStatus
        | success = "paid"
        | pending = "datafast_pending"
        | otherwise = "datafast_failed"
      paidAtVal = if success then Just now else ME.marketplaceOrderPaidAt order
      updateFields =
        [ ME.MarketplaceOrderStatus =. nextStatus
        , ME.MarketplaceOrderPaymentProvider =. Just "datafast"
        , ME.MarketplaceOrderPaidAt =. paidAtVal
        , ME.MarketplaceOrderUpdatedAt =. now
        , ME.MarketplaceOrderDatafastResourcePath =. Just resourcePathTxt
        , ME.MarketplaceOrderDatafastPaymentId =. dfpId statusResp
        , ME.MarketplaceOrderDatafastResultCode =. Just code
        , ME.MarketplaceOrderDatafastResultDescription =. dfrDescription (dfpResult statusResp)
        , ME.MarketplaceOrderDatafastPaymentBrand =. dfpPaymentBrand statusResp
        , ME.MarketplaceOrderDatafastAuthCode =. (dfpResultDetails statusResp >>= dfrdAuthCode)
        , ME.MarketplaceOrderDatafastAcquirerCode =. (dfpResultDetails statusResp >>= dfrdAcquirerCode)
        ]
      finalUpdates =
        if isJust (ME.marketplaceOrderDatafastCheckoutId order)
          then updateFields
          else (ME.MarketplaceOrderDatafastCheckoutId =. dfpId statusResp) : updateFields
  liftIO $ flip runSqlPool envPool $ update orderKey finalUpdates
  mDto <- liftIO $ flip runSqlPool envPool $ loadOrderDTO orderKey
  maybe (throwError err500) pure mDto

createPaypalOrder :: Text -> MarketplaceCheckoutReq -> AppM PaypalCreateDTO
createPaypalOrder rawId MarketplaceCheckoutReq{..} = do
  let nameTxt = T.strip mcrBuyerName
      emailTxt = T.strip mcrBuyerEmail
  when (T.null nameTxt) $ throwBadRequest "buyerName requerido"
  when (T.null emailTxt) $ throwBadRequest "buyerEmail requerido"
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  mTotals <- liftIO $ flip runSqlPool envPool $ loadCartTotals cartKey
  case mTotals of
    Nothing -> throwError err404
    Just (cartItems, totalCents, currency) -> do
      (cid, sec, baseUrl) <- loadPaypalEnv
      manager <- liftIO $ newManager tlsManagerSettings
      (ppOrderId, approvalUrl) <- createPaypalOrderRemote manager cid sec baseUrl totalCents currency nameTxt emailTxt
      orderId <- liftIO $ flip runSqlPool envPool $ do
        oid <- insert ME.MarketplaceOrder
          { ME.marketplaceOrderCartId        = Just cartKey
          , ME.marketplaceOrderBuyerName     = nameTxt
          , ME.marketplaceOrderBuyerEmail    = emailTxt
          , ME.marketplaceOrderBuyerPhone    = fmap T.strip mcrBuyerPhone
          , ME.marketplaceOrderTotalUsdCents = totalCents
          , ME.marketplaceOrderCurrency      = currency
          , ME.marketplaceOrderStatus        = "paypal_pending"
          , ME.marketplaceOrderPaymentProvider = Just "paypal"
          , ME.marketplaceOrderPaypalOrderId = Just ppOrderId
          , ME.marketplaceOrderPaypalPayerEmail = Nothing
          , ME.marketplaceOrderDatafastCheckoutId = Nothing
          , ME.marketplaceOrderDatafastResourcePath = Nothing
          , ME.marketplaceOrderDatafastPaymentId = Nothing
          , ME.marketplaceOrderDatafastResultCode = Nothing
          , ME.marketplaceOrderDatafastResultDescription = Nothing
          , ME.marketplaceOrderDatafastPaymentBrand = Nothing
          , ME.marketplaceOrderDatafastAuthCode = Nothing
          , ME.marketplaceOrderDatafastAcquirerCode = Nothing
          , ME.marketplaceOrderPaidAt        = Nothing
          , ME.marketplaceOrderCreatedAt     = now
          , ME.marketplaceOrderUpdatedAt     = now
          }
        forM_ cartItems $ \(_, listingEnt, _, qty) -> do
          let listing   = entityVal listingEnt
              unitPrice = ME.marketplaceListingPriceUsdCents listing
              subtotal  = unitPrice * qty
          void $ insert ME.MarketplaceOrderItem
            { ME.marketplaceOrderItemOrderId           = oid
            , ME.marketplaceOrderItemListingId         = entityKey listingEnt
            , ME.marketplaceOrderItemQuantity          = qty
            , ME.marketplaceOrderItemUnitPriceUsdCents = unitPrice
            , ME.marketplaceOrderItemSubtotalUsdCents  = subtotal
            }
        pure oid
      pure PaypalCreateDTO
        { pcOrderId = toPathPiece orderId
        , pcPaypalOrderId = ppOrderId
        , pcApprovalUrl = approvalUrl
        }

capturePaypalOrder :: PaypalCaptureReq -> AppM MarketplaceOrderDTO
capturePaypalOrder PaypalCaptureReq{..} = do
  orderKey <- parseOrderId pcCaptureOrderId
  Env{ envPool } <- ask
  mOrder <- liftIO $ flip runSqlPool envPool $ get orderKey
  case mOrder of
    Nothing -> throwError err404
    Just order -> do
      (cid, sec, baseUrl) <- loadPaypalEnv
      manager <- liftIO $ newManager tlsManagerSettings
      PayPalCaptureOutcome statusTxt payerEmail <- capturePaypalOrderRemote manager cid sec baseUrl pcCapturePaypalId
      now <- liftIO getCurrentTime
      let nextStatus = if T.toUpper statusTxt == "COMPLETED" then "paid" else T.toLower statusTxt
          paidAtVal  = if T.toUpper statusTxt == "COMPLETED" then Just now else ME.marketplaceOrderPaidAt order
      liftIO $ flip runSqlPool envPool $ update orderKey
        [ ME.MarketplaceOrderStatus =. nextStatus
        , ME.MarketplaceOrderPaypalOrderId =. Just pcCapturePaypalId
        , ME.MarketplaceOrderPaymentProvider =. Just "paypal"
        , ME.MarketplaceOrderPaypalPayerEmail =. (payerEmail <|> ME.marketplaceOrderPaypalPayerEmail order)
        , ME.MarketplaceOrderPaidAt =. paidAtVal
        , ME.MarketplaceOrderUpdatedAt =. now
        ]
      mDto <- liftIO $ flip runSqlPool envPool $ loadOrderDTO orderKey
      maybe (throwError err500) pure mDto

requestDatafastCheckout :: Key ME.MarketplaceOrder -> Int -> Text -> Text -> Text -> Maybe Text -> AppM (Text, String)
requestDatafastCheckout orderKey totalCents currency name email mPhone = do
  dfEnv <- loadDatafastEnv
  manager <- liftIO $ newManager tlsManagerSettings
  let amountTxt = T.pack (printf "%.2f" (fromIntegral totalCents / 100 :: Double))
      currencyTxt = T.toUpper (T.strip currency)
      (givenName, surname) = splitName name
      baseParams =
        [ ("entityId", TE.encodeUtf8 (dfEntityId dfEnv))
        , ("amount", TE.encodeUtf8 amountTxt)
        , ("currency", TE.encodeUtf8 currencyTxt)
        , ("paymentType", "DB")
        , ("merchantTransactionId", TE.encodeUtf8 (toPathPiece orderKey))
        , ("customer.givenName", TE.encodeUtf8 givenName)
        , ("customer.surname", TE.encodeUtf8 surname)
        , ("customer.email", TE.encodeUtf8 email)
        ]
      phoneParam = maybe [] (\p -> [("customer.phone", TE.encodeUtf8 p)]) mPhone
      testModeParam = maybe [] (\tm -> [("testMode", TE.encodeUtf8 tm)]) (dfTestMode dfEnv)
      allParams = baseParams <> phoneParam <> testModeParam <> dfExtraParams dfEnv
      body = RequestBodyBS (renderSimpleQuery False allParams)
      baseUrlClean = normalizeBaseUrl (dfBaseUrl dfEnv)
  req0 <- liftIO $ parseRequest (baseUrlClean ++ "/v1/checkouts")
  let req = req0
        { method = "POST"
        , requestBody = body
        , requestHeaders =
            [ ("Authorization", "Bearer " <> TE.encodeUtf8 (dfBearerToken dfEnv))
            , ("Content-Type", "application/x-www-form-urlencoded")
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "Datafast checkout request failed." }
  case eitherDecode (responseBody resp) of
    Left _ -> throwError err502 { errBody = "No pudimos interpretar la respuesta de Datafast." }
    Right dfResp -> do
      let code = dfrCode (dfcResult dfResp)
      unless (isDfCheckoutSuccess code) $
        throwError err502 { errBody = "Datafast rechazó la solicitud de pago." }
      let checkoutId = dfcId dfResp
          widgetUrl = baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack checkoutId
      pure (checkoutId, widgetUrl)

datafastPaymentStatus :: DatafastEnv -> Text -> AppM DFPaymentStatus
datafastPaymentStatus dfEnv resourcePathTxt = do
  manager <- liftIO $ newManager tlsManagerSettings
  let rp = T.unpack (T.strip resourcePathTxt)
      baseUrlClean = normalizeBaseUrl (dfBaseUrl dfEnv)
      basePath =
        if "http" `isPrefixOf` rp
          then rp
          else baseUrlClean ++ rp
      sep = if '?' `elem` basePath then "&" else "?"
      fullUrl = basePath ++ sep ++ "entityId=" ++ T.unpack (dfEntityId dfEnv)
  req0 <- liftIO $ parseRequest fullUrl
  let req = req0
        { method = "GET"
        , requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 (dfBearerToken dfEnv))]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "Datafast status request failed." }
  case eitherDecode (responseBody resp) of
    Left _ -> throwError err502 { errBody = "No pudimos leer el estado del pago de Datafast." }
    Right statusResp -> pure statusResp

splitName :: Text -> (Text, Text)
splitName raw =
  let parts = T.words raw
  in case parts of
    []       -> ("Cliente", "TDF")
    [x]      -> (x, x)
    (x:rest) -> (x, T.unwords rest)

normalizeBaseUrl :: String -> String
normalizeBaseUrl url =
  case reverse url of
    ('/':rest) -> reverse rest
    _          -> url

isDfCheckoutSuccess :: Text -> Bool
isDfCheckoutSuccess code = "000." `T.isPrefixOf` code

isDfPaymentSuccess :: Text -> Bool
isDfPaymentSuccess code =
  "000.000" `T.isPrefixOf` code || "000.100" `T.isPrefixOf` code

isDfPaymentPending :: Text -> Bool
isDfPaymentPending code = code == "000.200.000"

listMarketplaceOrders :: AuthedUser -> Maybe Text -> Maybe Int -> Maybe Int -> AppM [MarketplaceOrderDTO]
listMarketplaceOrders user mStatus mLimit mOffset = do
  requireMarketplaceAccess user
  let normalizedStatus = mStatus >>= nonEmpty
      limitCount = maybe 50 (max 1 . min 200) mLimit
      offsetCount = max 0 (fromMaybe 0 mOffset)
      filters = maybe [] (\st -> [ME.MarketplaceOrderStatus ==. st]) normalizedStatus
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool $ do
    orders <- selectList filters [Desc ME.MarketplaceOrderCreatedAt, OffsetBy offsetCount, LimitTo limitCount]
    catMaybes <$> mapM (loadOrderDTO . entityKey) orders
  where
    nonEmpty t =
      let trimmed = T.strip t
      in if T.null trimmed then Nothing else Just trimmed

updateMarketplaceOrder :: AuthedUser -> Text -> MarketplaceOrderUpdate -> AppM MarketplaceOrderDTO
updateMarketplaceOrder user rawId MarketplaceOrderUpdate{..} = do
  requireMarketplaceAccess user
  orderKey <- parseOrderId rawId
  now <- liftIO getCurrentTime
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ do
    mOrder <- get orderKey
    case mOrder of
      Nothing -> pure Nothing
      Just order -> do
        let cleanTxt txt =
              let trimmed = T.strip txt
              in if T.null trimmed then Nothing else Just trimmed
            nextStatus   = mouStatus >>= cleanTxt
            nextProvider = mouPaymentProvider >>= \mp -> pure (mp >>= cleanTxt)
            paidAtInput  = mouPaidAt
            paidAtBase   = case paidAtInput of
              Nothing -> ME.marketplaceOrderPaidAt order
              Just v  -> v
            paidAtFinal  =
              if isNothing paidAtInput
                 && maybe False (\s -> T.toLower s == "paid") nextStatus
                 && isNothing (ME.marketplaceOrderPaidAt order)
                then Just now
                else paidAtBase
            updates = catMaybes
              [ fmap (ME.MarketplaceOrderStatus =.) nextStatus
              , fmap (ME.MarketplaceOrderPaymentProvider =.) nextProvider
              , if paidAtFinal /= ME.marketplaceOrderPaidAt order
                  then Just (ME.MarketplaceOrderPaidAt =. paidAtFinal)
                  else Nothing
              ]
            updatesWithTimestamp =
              if null updates
                then []
                else updates ++ [ME.MarketplaceOrderUpdatedAt =. now]
        unless (null updatesWithTimestamp) $ update orderKey updatesWithTimestamp
        loadOrderDTO orderKey
  maybe (throwError err404) pure mDto

getOrder :: Text -> AppM MarketplaceOrderDTO
getOrder rawId = do
  orderKey <- parseOrderId rawId
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ loadOrderDTO orderKey
  maybe (throwError err404) pure mDto

parseOrderId :: Text -> AppM (Key ME.MarketplaceOrder)
parseOrderId rawId =
  case fromPathPiece rawId of
    Nothing -> throwBadRequest "Invalid order id"
    Just k  -> pure k

parseCartId :: Text -> AppM (Key ME.MarketplaceCart)
parseCartId rawId =
  case fromPathPiece rawId of
    Nothing -> throwBadRequest "Invalid cart id"
    Just k  -> pure k

parseListingId :: Text -> AppM (Key ME.MarketplaceListing)
parseListingId rawId =
  case fromPathPiece rawId of
    Nothing -> throwBadRequest "Invalid listing id"
    Just k  -> pure k

requireMarketplaceAccess :: AuthedUser -> AppM ()
requireMarketplaceAccess user =
  unless (hasModuleAccess ModuleAdmin user || hasModuleAccess ModulePackages user) $
    throwError err403

loadCartDTO :: Key ME.MarketplaceCart -> SqlPersistT IO (Maybe MarketplaceCartDTO)
loadCartDTO cartId = do
  mCart <- get cartId
  case mCart of
    Nothing -> pure Nothing
    Just _ -> do
      items <- loadCartLines cartId
      pure (Just (cartToDTO cartId items))

loadCartTotals
  :: Key ME.MarketplaceCart
  -> SqlPersistT IO (Maybe ([(Entity ME.MarketplaceCartItem, Entity ME.MarketplaceListing, Entity ME.Asset, Int)], Int, Text))
loadCartTotals cartId = do
  items <- loadCartLines cartId
  if null items
    then pure Nothing
    else do
      let totalCents = sum [ qty * ME.marketplaceListingPriceUsdCents (entityVal listing)
                           | (_, listing, _, qty) <- items
                           ]
          currency = maybe "USD" (ME.marketplaceListingCurrency . entityVal) (listToMaybe [listing | (_, listing, _, _) <- items])
      pure (Just (items, totalCents, currency))

loadCartLines
  :: Key ME.MarketplaceCart
  -> SqlPersistT IO [(Entity ME.MarketplaceCartItem, Entity ME.MarketplaceListing, Entity ME.Asset, Int)]
loadCartLines cartId = do
  cartItems <- selectList [ME.MarketplaceCartItemCartId ==. cartId] [Asc ME.MarketplaceCartItemId]
  forM cartItems $ \ent@(Entity _ ci) -> do
    listing <- getJustEntity (ME.marketplaceCartItemListingId ci)
    asset   <- getJustEntity (ME.marketplaceListingAssetId (entityVal listing))
    let qty = ME.marketplaceCartItemQuantity ci
    pure (ent, listing, asset, qty)

cartToDTO
  :: Key ME.MarketplaceCart
  -> [(Entity ME.MarketplaceCartItem, Entity ME.MarketplaceListing, Entity ME.Asset, Int)]
  -> MarketplaceCartDTO
cartToDTO cartId items =
  let currency = maybe "USD" (ME.marketplaceListingCurrency . entityVal) (listToMaybe [listing | (_, listing, _, _) <- items])
      subtotal = sum [ ME.marketplaceListingPriceUsdCents (entityVal listing) * qty
                     | (_, listing, _, qty) <- items
                     ]
      itemDtos = flip map items $ \(_, listingEnt, assetEnt, qty) ->
        let listing = entityVal listingEnt
            asset   = entityVal assetEnt
            unitPrice = ME.marketplaceListingPriceUsdCents listing
            subtotalC = unitPrice * qty
        in MarketplaceCartItemDTO
            { mciListingId         = toPathPiece (entityKey listingEnt)
            , mciTitle             = ME.marketplaceListingTitle listing
            , mciCategory          = ME.assetCategory asset
            , mciBrand             = ME.assetBrand asset
            , mciModel             = ME.assetModel asset
            , mciQuantity          = qty
            , mciUnitPriceUsdCents = unitPrice
            , mciSubtotalCents     = subtotalC
            , mciUnitPriceDisplay  = formatUsd unitPrice currency
            , mciSubtotalDisplay   = formatUsd subtotalC currency
            }
  in MarketplaceCartDTO
      { mcCartId          = toPathPiece cartId
      , mcItems           = itemDtos
      , mcCurrency        = currency
      , mcSubtotalCents   = subtotal
      , mcSubtotalDisplay = formatUsd subtotal currency
      }

loadOrderDTO :: Key ME.MarketplaceOrder -> SqlPersistT IO (Maybe MarketplaceOrderDTO)
loadOrderDTO orderId = do
  mOrder <- getEntity orderId
  case mOrder of
    Nothing -> pure Nothing
    Just orderEnt -> do
      orderItems <- selectList [ME.MarketplaceOrderItemOrderId ==. orderId] [Asc ME.MarketplaceOrderItemId]
      listings <- forM orderItems $ \(Entity _ oi) -> do
        mListing <- get (ME.marketplaceOrderItemListingId oi)
        pure (oi, mListing)
      pure (Just (orderToDTO orderEnt listings))

orderToDTO
  :: Entity ME.MarketplaceOrder
  -> [(ME.MarketplaceOrderItem, Maybe ME.MarketplaceListing)]
  -> MarketplaceOrderDTO
orderToDTO (Entity oid order) items =
  let currency = ME.marketplaceOrderCurrency order
      itemDtos = flip map items $ \(oi, mListing) ->
        let titleTxt = maybe "Listado" ME.marketplaceListingTitle mListing
        in MarketplaceOrderItemDTO
              { moiListingId         = toPathPiece (ME.marketplaceOrderItemListingId oi)
              , moiTitle             = titleTxt
              , moiQuantity          = ME.marketplaceOrderItemQuantity oi
              , moiUnitPriceUsdCents = ME.marketplaceOrderItemUnitPriceUsdCents oi
              , moiSubtotalCents     = ME.marketplaceOrderItemSubtotalUsdCents oi
              , moiUnitPriceDisplay  = formatUsd (ME.marketplaceOrderItemUnitPriceUsdCents oi) currency
              , moiSubtotalDisplay   = formatUsd (ME.marketplaceOrderItemSubtotalUsdCents oi) currency
              }
  in MarketplaceOrderDTO
      { moOrderId         = toPathPiece oid
      , moCartId          = toPathPiece <$> ME.marketplaceOrderCartId order
      , moCurrency        = currency
      , moTotalUsdCents   = ME.marketplaceOrderTotalUsdCents order
      , moTotalDisplay    = formatUsd (ME.marketplaceOrderTotalUsdCents order) currency
      , moStatus          = ME.marketplaceOrderStatus order
      , moStatusHistory   = [ (ME.marketplaceOrderStatus order, ME.marketplaceOrderUpdatedAt order) ]
      , moBuyerName       = ME.marketplaceOrderBuyerName order
    , moBuyerEmail      = ME.marketplaceOrderBuyerEmail order
    , moBuyerPhone      = ME.marketplaceOrderBuyerPhone order
    , moPaymentProvider = ME.marketplaceOrderPaymentProvider order
    , moPaypalOrderId   = ME.marketplaceOrderPaypalOrderId order
    , moPaypalPayerEmail = ME.marketplaceOrderPaypalPayerEmail order
    , moPaidAt          = ME.marketplaceOrderPaidAt order
    , moCreatedAt       = ME.marketplaceOrderCreatedAt order
    , moUpdatedAt       = ME.marketplaceOrderUpdatedAt order
    , moItems           = itemDtos
    }

formatUsd :: Int -> Text -> Text
formatUsd cents currency =
  let amount = (fromIntegral cents :: Double) / 100
  in T.pack (printf "%s $%.2f" (T.unpack (T.toUpper currency)) amount)

-- Datafast / OPPWA types and helpers
data DFResult = DFResult
  { dfrCode        :: Text
  , dfrDescription :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DFResult where
  parseJSON = withObject "DFResult" $ \o ->
    DFResult <$> o .: "code" <*> o .:? "description"

data DFCheckoutRes = DFCheckoutRes
  { dfcId     :: Text
  , dfcResult :: DFResult
  } deriving (Show, Generic)
instance FromJSON DFCheckoutRes where
  parseJSON = withObject "DFCheckoutRes" $ \o ->
    DFCheckoutRes <$> o .: "id" <*> o .: "result"

data DFResultDetails = DFResultDetails
  { dfrdAuthCode     :: Maybe Text
  , dfrdAcquirerCode :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON DFResultDetails where
  parseJSON = withObject "DFResultDetails" $ \o ->
    DFResultDetails
      <$> o .:? "AuthCode"
      <*> o .:? "AcquirerCode"

data DFPaymentStatus = DFPaymentStatus
  { dfpId            :: Maybe Text
  , dfpPaymentType   :: Maybe Text
  , dfpPaymentBrand  :: Maybe Text
  , dfpAmount        :: Maybe Text
  , dfpCurrency      :: Maybe Text
  , dfpResult        :: DFResult
  , dfpResultDetails :: Maybe DFResultDetails
  } deriving (Show, Generic)
instance FromJSON DFPaymentStatus where
  parseJSON = withObject "DFPaymentStatus" $ \o ->
    DFPaymentStatus
      <$> o .:? "id"
      <*> o .:? "paymentType"
      <*> o .:? "paymentBrand"
      <*> o .:? "amount"
      <*> o .:? "currency"
      <*> o .:  "result"
      <*> o .:? "resultDetails"

data DatafastEnv = DatafastEnv
  { dfEntityId     :: Text
  , dfBearerToken  :: Text
  , dfBaseUrl      :: String
  , dfTestMode     :: Maybe Text
  , dfExtraParams  :: [(ByteString, ByteString)]
  } deriving (Show)

loadDatafastEnv :: AppM DatafastEnv
loadDatafastEnv = do
  mEntity <- liftIO $ lookupEnv "DATAFAST_ENTITY_ID"
  mBearer <- liftIO $ lookupEnv "DATAFAST_BEARER_TOKEN"
  mBase   <- liftIO $ lookupEnv "DATAFAST_BASE_URL"
  mTest   <- liftIO $ lookupEnv "DATAFAST_TEST_MODE"
  mMid    <- liftIO $ lookupEnv "DATAFAST_MID"
  mTid    <- liftIO $ lookupEnv "DATAFAST_TID"
  mPserv  <- liftIO $ lookupEnv "DATAFAST_PSERV"
  mUserData2 <- liftIO $ lookupEnv "DATAFAST_USER_DATA2"
  mVersionDf <- liftIO $ lookupEnv "DATAFAST_VERSIONDF"
  let entityId = fmap (T.strip . T.pack) mEntity
      bearer   = fmap (T.strip . T.pack) mBearer
      baseUrl  = fromMaybe "https://test.oppwa.com" mBase
      testModeVal = mTest >>= (\v -> let t = T.strip (T.pack v) in if T.null t then Nothing else Just t)
      optPair k mv = (\v -> (k, TE.encodeUtf8 v)) <$> mv
      extras =
        catMaybes
          [ optPair "customParameters[SHOPPER_MID]" (T.strip . T.pack <$> mMid)
          , optPair "customParameters[SHOPPER_TID]" (T.strip . T.pack <$> mTid)
          , optPair "customParameters[SHOPPER_PSERV]" (T.strip . T.pack <$> mPserv)
          , optPair "risk.parameters[USER_DATA2]" (T.strip . T.pack <$> mUserData2)
          , optPair "customParameters[SHOPPER_VERSIONDF]" (Just (maybe "2" (T.pack) mVersionDf))
          ]
  case (entityId, bearer) of
    (Just eid, Just tok) ->
      pure DatafastEnv
        { dfEntityId = eid
        , dfBearerToken = tok
        , dfBaseUrl = baseUrl
        , dfTestMode = testModeVal
        , dfExtraParams = extras
        }
    _ -> throwError err500 { errBody = "Faltan DATAFAST_ENTITY_ID / DATAFAST_BEARER_TOKEN" }

data PayPalLink = PayPalLink
  { pplRel  :: Text
  , pplHref :: Text
  } deriving (Show, Generic)
instance FromJSON PayPalLink where
  parseJSON = withObject "PayPalLink" $ \o ->
    PayPalLink <$> o .: "rel" <*> o .: "href"

data PayPalCreateResponse = PayPalCreateResponse
  { pcrId    :: Text
  , pcrLinks :: [PayPalLink]
  } deriving (Show, Generic)
instance FromJSON PayPalCreateResponse where
  parseJSON = withObject "PayPalCreateResponse" $ \o ->
    PayPalCreateResponse
      <$> o .: "id"
      <*> o .:? "links" .!= []

data PayPalToken = PayPalToken
  { payPalAccessToken :: Text
  } deriving (Show, Generic)
instance FromJSON PayPalToken where
  parseJSON = withObject "PayPalToken" $ \o ->
    PayPalToken <$> o .: "access_token"

data PayPalCaptureOutcome = PayPalCaptureOutcome
  { pcoStatus :: Text
  , pcoPayerEmail :: Maybe Text
  } deriving (Show, Generic)

loadPaypalEnv :: AppM (Text, Text, String)
loadPaypalEnv = do
  mCid <- liftIO $ lookupEnv "PAYPAL_CLIENT_ID"
  mSecret <- liftIO $ lookupEnv "PAYPAL_CLIENT_SECRET"
  mEnv <- liftIO $ lookupEnv "PAYPAL_ENV"
  let cid = fmap (T.strip . T.pack) mCid
      secret = fmap (T.strip . T.pack) mSecret
      baseUrl = case fmap (map toLower) mEnv of
        Just envTxt | envTxt == "live" || envTxt == "prod" -> "https://api-m.paypal.com"
        _ -> "https://api-m.sandbox.paypal.com"
  case (cid, secret) of
    (Just cid', Just sec') -> pure (cid', sec', baseUrl)
    _ -> throwError err500 { errBody = "Faltan PAYPAL_CLIENT_ID / PAYPAL_CLIENT_SECRET" }

paypalAccessToken :: Manager -> Text -> Text -> String -> AppM Text
paypalAccessToken manager cid sec baseUrl = do
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v1/oauth2/token")
  let authVal = BS8.pack "Basic " <> B64.encode (BS8.pack (T.unpack cid <> ":" <> T.unpack sec))
      req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (BL8.pack "grant_type=client_credentials")
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            , ("Authorization", authVal)
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal token request failed." }
  token <- case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear token PayPal: " <> err) }
    Right tok -> pure tok
  pure (payPalAccessToken token)

createPaypalOrderRemote
  :: Manager
  -> Text
  -> Text
  -> String
  -> Int
  -> Text
  -> Text
  -> Text
  -> AppM (Text, Maybe Text)
createPaypalOrderRemote manager cid sec baseUrl totalCents currency buyerName buyerEmail = do
  token <- paypalAccessToken manager cid sec baseUrl
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v2/checkout/orders")
  let amountStr = T.pack (printf "%.2f" (fromIntegral totalCents / 100 :: Double))
      body = object
        [ "intent" .= ("CAPTURE" :: Text)
        , "purchase_units" .=
            [ object
                [ "amount" .= object ["currency_code" .= T.toUpper currency, "value" .= amountStr]
                ]
            ]
        , "payer" .= object
            [ "name" .= object ["given_name" .= buyerName]
            , "email_address" .= buyerEmail
            ]
        , "application_context" .= object ["shipping_preference" .= ("NO_SHIPPING" :: Text)]
        ]
      req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS (encode body)
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal create order falló." }
  resObj <- case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear respuesta PayPal: " <> err) }
    Right val -> pure (val :: PayPalCreateResponse)
  let approval = fmap pplHref . find (\lnk -> pplRel lnk == "approve") $ pcrLinks resObj
  pure (pcrId resObj, approval)

capturePaypalOrderRemote
  :: Manager
  -> Text
  -> Text
  -> String
  -> Text
  -> AppM PayPalCaptureOutcome
capturePaypalOrderRemote manager cid sec baseUrl paypalOrderId = do
  token <- paypalAccessToken manager cid sec baseUrl
  req0 <- liftIO $ parseRequest (baseUrl ++ "/v2/checkout/orders/" ++ T.unpack paypalOrderId ++ "/capture")
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS "{}"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 token)
            ]
        }
  resp <- liftIO $ httpLbs req manager
  when (statusCode (responseStatus resp) >= 400) $
    throwError err502 { errBody = "PayPal capture falló." }
  parsed <- case eitherDecode (responseBody resp) of
    Left err -> throwError err502 { errBody = BL8.pack ("No se pudo parsear captura PayPal: " <> err) }
    Right val -> pure val
  let statusTxt = fromMaybe "unknown" . join $ parseMaybe (withObject "PayPalCapture" (\o -> o .:? "status")) parsed
      payerEmail = parseMaybe (withObject "PayPalCapture" $ \o -> do
        payerObj <- o .:? "payer"
        case payerObj of
          Nothing -> pure Nothing
          Just po -> po .:? "email_address") parsed
  pure PayPalCaptureOutcome
    { pcoStatus = statusTxt
    , pcoPayerEmail = join payerEmail
    }


assetStatusLabel :: ME.AssetStatus -> Text
assetStatusLabel st =
  case st of
    ME.Active            -> "En stock"
    ME.Booked            -> "Reservado"
    ME.OutForMaintenance -> "Mantenimiento"
    ME.Retired           -> "No disponible"

assetConditionLabel :: ME.AssetCondition -> Text
assetConditionLabel cond =
  case cond of
    ME.NewC -> "Nuevo"
    ME.Good -> "Bueno"
    ME.Fair -> "Regular"
    ME.Poor -> "Usado"

data TrackScope = TrackScope
  { tsOwner    :: Maybe PartyId
  , tsAllowAny :: Bool
  }

resolveTrackScope :: AuthedUser -> Maybe Int64 -> AppM TrackScope
resolveTrackScope user mOwnerId = do
  let isAdmin  = hasRole Admin user
      isArtist = hasRole Artist user || hasRole Artista user
  unless (isAdmin || isArtist) $
    throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "Solo artistas o admins pueden gestionar operaciones.") }
  let owner = if isAdmin then fmap toSqlKey mOwnerId else Just (auPartyId user)
  pure TrackScope { tsOwner = owner, tsAllowAny = isAdmin }

ownerFilter :: TrackScope -> [Filter ME.LabelTrack]
ownerFilter TrackScope{..} =
  case tsOwner of
    Nothing    -> [ME.LabelTrackOwnerPartyId ==. Nothing]
    Just owner -> [ME.LabelTrackOwnerPartyId ==. Just owner]

canManageTrack :: TrackScope -> ME.LabelTrack -> Bool
canManageTrack TrackScope{..} track =
  tsAllowAny || ME.labelTrackOwnerPartyId track == tsOwner

ensureTrackAccess :: TrackScope -> ME.LabelTrack -> AppM ()
ensureTrackAccess scope track =
  unless (canManageTrack scope track) $
    throwError err403 { errBody = BL.fromStrict (TE.encodeUtf8 "No puedes modificar operaciones de otro artista.") }

loadTrackOwnerNames :: [Entity ME.LabelTrack] -> AppM (Map.Map PartyId Text)
loadTrackOwnerNames rows = do
  let owners = mapMaybe (ME.labelTrackOwnerPartyId . entityVal) rows
  if null owners
    then pure Map.empty
    else runDB (fetchPartyNameMap owners)

listLabelTracks :: AuthedUser -> Maybe Int64 -> AppM [LabelTrackDTO]
listLabelTracks user mOwnerId = do
  scope <- resolveTrackScope user mOwnerId
  rows <- runDB $ selectList (ownerFilter scope) [Desc ME.LabelTrackCreatedAt]
  nameMap <- loadTrackOwnerNames rows
  pure (map (toLabelTrackDTO nameMap) rows)

createLabelTrack :: AuthedUser -> LabelTrackCreate -> AppM LabelTrackDTO
createLabelTrack user LabelTrackCreate{..} = do
  scope <- resolveTrackScope user ltcOwnerId
  when (T.null (T.strip ltcTitle)) $ throwBadRequest "Título requerido"
  now <- liftIO getCurrentTime
  let record = ME.LabelTrack
        { ME.labelTrackTitle      = T.strip ltcTitle
        , ME.labelTrackNote       = T.strip <$> ltcNote
        , ME.labelTrackStatus     = "open"
        , ME.labelTrackOwnerPartyId = tsOwner scope
        , ME.labelTrackCreatedAt  = now
        , ME.labelTrackUpdatedAt  = now
        }
  entity <- runDB $ do
    key <- insert record
    pure (Entity key record)
  nameMap <- loadTrackOwnerNames [entity]
  pure (toLabelTrackDTO nameMap entity)

updateLabelTrack :: AuthedUser -> Text -> LabelTrackUpdate -> AppM LabelTrackDTO
updateLabelTrack user rawId LabelTrackUpdate{..} = do
  key <- case (fromPathPiece rawId :: Maybe (Key ME.LabelTrack)) of
    Nothing -> throwBadRequest "Invalid track id"
    Just k  -> pure k
  scope <- resolveTrackScope user Nothing
  now <- liftIO getCurrentTime
  mTrack <- runDB $ getEntity key
  case mTrack of
    Nothing -> throwError err404
    Just (Entity _ track) -> do
      ensureTrackAccess scope track
      let updates = catMaybes
            [ (ME.LabelTrackTitle =.) . T.strip <$> ltuTitle
            , case ltuNote of
                Nothing -> Nothing
                Just n  -> Just (ME.LabelTrackNote =. if T.null (T.strip n) then Nothing else Just (T.strip n))
            , (ME.LabelTrackStatus =.) <$> ltuStatus
            , Just (ME.LabelTrackUpdatedAt =. now)
            ]
      unless (null updates) (runDB $ update key updates)
      updated <- runDB $ getJustEntity key
      nameMap <- loadTrackOwnerNames [updated]
      pure (toLabelTrackDTO nameMap updated)

deleteLabelTrack :: AuthedUser -> Text -> AppM NoContent
deleteLabelTrack user rawId = do
  key <- case (fromPathPiece rawId :: Maybe (Key ME.LabelTrack)) of
    Nothing -> throwBadRequest "Invalid track id"
    Just k  -> pure k
  scope <- resolveTrackScope user Nothing
  mTrack <- runDB $ getEntity key
  case mTrack of
    Nothing -> throwError err404
    Just (Entity _ track) -> do
      ensureTrackAccess scope track
      runDB $ delete key
      pure NoContent

toLabelTrackDTO :: Map.Map PartyId Text -> Entity ME.LabelTrack -> LabelTrackDTO
toLabelTrackDTO nameMap (Entity key t) =
  LabelTrackDTO
    { ltId        = toPathPiece key
    , ltTitle     = ME.labelTrackTitle t
    , ltNote      = ME.labelTrackNote t
    , ltStatus    = ME.labelTrackStatus t
    , ltOwnerId   = fromSqlKey <$> ME.labelTrackOwnerPartyId t
    , ltOwnerName = ME.labelTrackOwnerPartyId t >>= (`Map.lookup` nameMap)
    , ltCreatedAt = ME.labelTrackCreatedAt t
    , ltUpdatedAt = ME.labelTrackUpdatedAt t
    }

cmsAdminServer :: AuthedUser -> ServerT CmsAdminAPI AppM
cmsAdminServer user =
       cmsListH
  :<|> cmsCreateH
  :<|> cmsPublishH
  :<|> cmsDeleteH
  where
    requireWebmaster = unless (hasRole Admin user || hasRole Webmaster user) $
      throwError err403

    cmsListH mSlug mLocale = do
      requireWebmaster
      let filters = catMaybes
            [ (CMS.CmsContentSlug ==.) <$> mSlug
            , (CMS.CmsContentLocale ==.) <$> mLocale
            ]
      rows <- runDB $ selectList filters [Desc CMS.CmsContentCreatedAt]
      pure (map toCmsDTO rows)

    cmsCreateH CmsContentIn{..} = do
      requireWebmaster
      now <- liftIO getCurrentTime
      let slug = cciSlug
          locale = cciLocale
          statusVal = fromMaybe "draft" cciStatus
      nextVersion <- runDB $ do
        mLatest <- selectFirst
          [ CMS.CmsContentSlug ==. slug
          , CMS.CmsContentLocale ==. locale
          ]
          [Desc CMS.CmsContentVersion]
        pure $ maybe 1 ((+1) . CMS.cmsContentVersion . entityVal) mLatest
      let publishedAt = if statusVal == "published" then Just now else Nothing
      cid <- runDB $ insert CMS.CmsContent
        { CMS.cmsContentSlug = slug
        , CMS.cmsContentLocale = locale
        , CMS.cmsContentVersion = nextVersion
        , CMS.cmsContentStatus = statusVal
        , CMS.cmsContentTitle = cciTitle
        , CMS.cmsContentPayload = fmap CMS.AesonValue cciPayload
        , CMS.cmsContentCreatedBy = Just (auPartyId user)
        , CMS.cmsContentCreatedAt = now
        , CMS.cmsContentUpdatedAt = now
        , CMS.cmsContentPublishedAt = publishedAt
        }
      ent <- runDB $ getJustEntity cid
      pure (toCmsDTO ent)

    cmsPublishH cid = do
      requireWebmaster
      let contentKey = toSqlKey (fromIntegral cid) :: Key CMS.CmsContent
      now <- liftIO getCurrentTime
      mEnt <- runDB $ get contentKey
      case mEnt of
        Nothing -> throwError err404
        Just ent -> do
          runDB $ do
            updateWhere
              [ CMS.CmsContentSlug ==. CMS.cmsContentSlug ent
              , CMS.CmsContentLocale ==. CMS.cmsContentLocale ent
              ]
              [ CMS.CmsContentStatus =. "archived" ]
            update contentKey
              [ CMS.CmsContentStatus =. "published"
              , CMS.CmsContentPublishedAt =. Just now
              , CMS.CmsContentUpdatedAt =. now
              ]
          ent' <- runDB $ getJustEntity contentKey
          pure (toCmsDTO ent')

    cmsDeleteH cid = do
      requireWebmaster
      let contentKey = toSqlKey (fromIntegral cid) :: Key CMS.CmsContent
      runDB $ delete contentKey
      pure NoContent

toCmsDTO :: Entity CMS.CmsContent -> CmsContentDTO
toCmsDTO (Entity cid c) =
  CmsContentDTO
    { ccdId = entityKeyInt cid
    , ccdSlug = CMS.cmsContentSlug c
    , ccdLocale = CMS.cmsContentLocale c
    , ccdVersion = CMS.cmsContentVersion c
    , ccdStatus = CMS.cmsContentStatus c
    , ccdTitle = CMS.cmsContentTitle c
    , ccdPayload = CMS.unAesonValue <$> CMS.cmsContentPayload c
    , ccdCreatedAt = CMS.cmsContentCreatedAt c
    , ccdPublishedAt = CMS.cmsContentPublishedAt c
    }

entityKeyInt :: ToBackendKey SqlBackend record => Key record -> Int
entityKeyInt = fromIntegral . fromSqlKey

-- Google Drive upload (proxied)
data DriveApiResp = DriveApiResp
  { darId             :: Text
  , darWebViewLink    :: Maybe Text
  , darWebContentLink :: Maybe Text
  , darResourceKey    :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON DriveApiResp where
  parseJSON = withObject "DriveApiResp" $ \o ->
    DriveApiResp <$> o .: "id"
                 <*> o .:? "webViewLink"
                 <*> o .:? "webContentLink"
                 <*> o .:? "resourceKey"

data DriveMetaResp = DriveMetaResp
  { dmrResourceKey :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON DriveMetaResp where
  parseJSON = withObject "DriveMetaResp" $ \o ->
    DriveMetaResp <$> o .:? "resourceKey"

uploadToDrive
  :: Manager
  -> Text            -- ^ Google access token (user or service)
  -> FileData Tmp    -- ^ Uploaded file from client
  -> Maybe Text      -- ^ Optional override name
  -> Maybe Text      -- ^ Optional folder id
  -> IO DriveUploadDTO
uploadToDrive manager accessToken file mName mFolder = do
  uuid <- nextRandom
  let boundary = "tdf-boundary-" <> T.replace "-" "" (toText uuid)
      dashBoundary = "--" <> boundary
      fileName = fromMaybe (fdFileName file) mName
      mimeTypeTxt =
        let raw = T.strip (fdFileCType file)
        in if T.null raw then "application/octet-stream" else raw
      mimeTypeBS = TE.encodeUtf8 mimeTypeTxt
      meta = object $
        [ "name" .= fileName
        , "mimeType" .= mimeTypeTxt
        ] <> maybe [] (\f -> ["parents" .= [f]]) mFolder

  fileBytes <- BL.readFile (fdPayload file)
  let metaPart = BL.intercalate "\r\n"
        [ BL.fromStrict (TE.encodeUtf8 dashBoundary)
        , "Content-Type: application/json; charset=UTF-8"
        , ""
        , encode meta
        ]
      filePart = BL.intercalate "\r\n"
        [ BL.fromStrict (TE.encodeUtf8 dashBoundary)
        , BL.fromStrict $ "Content-Type: " <> mimeTypeBS
        , ""
        , fileBytes
        ]
      closing = BL.fromStrict (TE.encodeUtf8 (dashBoundary <> "--"))
      body = BL.intercalate "\r\n" [metaPart, filePart, closing, ""]

  req0 <- parseRequest $
    "https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart" <>
    "&fields=id,webViewLink,webContentLink,resourceKey&supportsAllDrives=true"
  let bearer = "Bearer " <> TE.encodeUtf8 accessToken
      req = req0
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", bearer)
            , ("Content-Type", BS8.pack ("multipart/related; boundary=" <> T.unpack boundary))
            ]
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req manager
  let uploadStatus = statusCode (responseStatus resp)
  when (uploadStatus >= 400) $ do
    let bodySnippet = take 2000 (BL8.unpack (responseBody resp))
        suffix = if null bodySnippet then "" else " " <> bodySnippet
    fail ("Drive upload failed with status " <> show uploadStatus <> "." <> suffix)
  driveResp <- case eitherDecode (responseBody resp) of
    Left err -> fail ("No pudimos interpretar la respuesta de Drive: " <> err)
    Right ok -> pure (ok :: DriveApiResp)

  -- Best-effort: make the file public.
  let permBody = encode (object ["role" .= ("reader" :: Text), "type" .= ("anyone" :: Text)])
  permReq0 <- parseRequest $
    "https://www.googleapis.com/drive/v3/files/" <>
    T.unpack (darId driveResp) <>
    "/permissions?supportsAllDrives=true"
  let permReq = permReq0
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", bearer)
            , ("Content-Type", "application/json")
            ]
        , requestBody = RequestBodyLBS permBody
        }
  _ <- (try (httpLbs permReq manager) :: IO (Either SomeException (Response BL.ByteString)))

  metaReq0 <- parseRequest $
    "https://www.googleapis.com/drive/v3/files/" <>
    T.unpack (darId driveResp) <>
    "?fields=resourceKey&supportsAllDrives=true"
  let metaReq = metaReq0
        { requestHeaders =
            [ ("Authorization", bearer)
            ]
        }
  metaResp <- (try (httpLbs metaReq manager) :: IO (Either SomeException (Response BL.ByteString)))
  let metaResourceKey =
        case metaResp of
          Right respMeta ->
            case eitherDecode (responseBody respMeta) of
              Right (DriveMetaResp key) -> key
              Left _ -> Nothing
          Left _ -> Nothing
      resolvedResourceKey = darResourceKey driveResp <|> metaResourceKey
      fallbackPublicUrl = "https://drive.google.com/uc?export=download&id=" <> darId driveResp
      appendResourceKey url =
        case resolvedResourceKey of
          Just key
            | not (T.null (T.strip key)) && "resourcekey=" `T.isInfixOf` url == False ->
                url <> "&resourcekey=" <> T.strip key
          _ -> url
      publicUrl = Just $ appendResourceKey $ fromMaybe fallbackPublicUrl (darWebContentLink driveResp)
  pure DriveUploadDTO
    { duFileId = darId driveResp
    , duWebViewLink = darWebViewLink driveResp
    , duWebContentLink = darWebContentLink driveResp
    , duPublicUrl = publicUrl
    }
