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
import           Control.Exception (SomeException, displayException, throwIO, try)
import           Control.Concurrent (forkIO)
import           Control.Monad (foldM, forM, forM_, void, when, unless, (>=>), join)
import           Control.Monad.Except (catchError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import           Data.Int (Int64)
import           Data.List (find, foldl', nub, isInfixOf, sortOn)
import           Data.Ord (Down(..))
import           Data.Foldable (for_)
import           Data.Char (isControl, isDigit, isAlphaNum, isAsciiLower, isAsciiUpper, isSpace, toLower)
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import           Data.Aeson (ToJSON(..), Value(..), defaultOptions, object, (.=), eitherDecode, FromJSON(..), Result(..), encode, fromJSON, genericParseJSON, genericToJSON)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import           Data.Aeson.Types (Parser, camelTo2, fieldLabelModifier, parseEither, parseMaybe, withObject, (.:), (.:?), (.!=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Scientific as Sci
import           Data.Time
  ( Day, UTCTime (..), addUTCTime, diffTimeToPicoseconds, fromGregorian
  , getCurrentTime, secondsToDiffTime, toGregorian, utctDay
  )
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
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
import           Database.Persist.Sql (SqlBackend, SqlPersistT, Single(..), fromSqlKey, rawExecute, rawSql, runSqlPool, toSqlKey)
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.API.Types (RolePayload(..), UserRoleSummaryDTO(..), UserRoleUpdatePayload(..), AccountStatusDTO(..), MarketplaceItemDTO(..), MarketplaceCartDTO(..), MarketplaceCartItemUpdate(..), MarketplaceCartItemDTO(..), MarketplaceOrderDTO(..), MarketplaceOrderItemDTO(..), MarketplaceOrderUpdate(..), MarketplaceCheckoutReq(..), DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..), LabelTrackDTO(..), LabelTrackCreate(..), LabelTrackUpdate(..), DriveUploadDTO(..), DriveTokenExchangeRequest(..), DriveTokenRefreshRequest(..), DriveTokenResponse(..), PartyRelatedDTO(..), PartyRelatedBooking(..), PartyRelatedClassSession(..), PartyRelatedLabelTrack(..))
import           TDF.API.Types (maxMarketplaceCartItemQuantity)
import           TDF.API.WhatsApp (validateHookVerifyRequest)
import qualified TDF.API      as Api
import           TDF.API.Marketplace (MarketplaceAPI, MarketplaceAdminAPI)
import           TDF.API.Label (LabelAPI)
import           TDF.API.Drive (DriveAPI, DriveUploadForm(..))
import           TDF.Contracts.API (ContractsAPI)
import           TDF.Config ( AppConfig(..)
                            , courseInstructorAvatarFallback
                            , courseMapFallback
                            , courseSlugFallback
                            , normalizeConfiguredGraphNodeId
                            , normalizeConfiguredBaseUrl
                            , resolveConfiguredAppBase
                            , resolveConfiguredAssetsBase
                            )
import           TDF.DB
import qualified TDF.Invoice.SRI as Sri
import           TDF.Models
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import           TDF.DTO
import qualified TDF.DTO as DTO
import           TDF.Auth (AuthedUser(..), ModuleAccess(..), authContext, hasAiToolingAccess, hasModuleAccess, hasOperationsAccess, hasSocialInboxAccess, hasStrictAdminAccess, moduleName)
import           TDF.Seed       (seedAll, seedInventoryAssets, seedMarketplaceListings)
import           TDF.ServerAdmin (adminServer)
import qualified TDF.LogBuffer as LogBuf
import           TDF.Server.SocialEventsHandlers (socialEventsServer)
import           TDF.ServerExtra (bandsServer, facebookServer, facebookWebhookServer, instagramServer, instagramWebhookServer, inventoryServer, loadBandForParty, paymentsServer, pipelinesServer, roomsPublicServer, roomsServer, serviceCatalogPublicServer, serviceCatalogServer, sessionsServer)
import qualified TDF.ServerExtra as ServerExtra
import qualified TDF.ServerAuth as AuthServer
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
import qualified TDF.Trials.Server as TrialsServer (isValidHttpUrl, trialsServer)
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
                                    , CourseRegistrationNotesUpdate(..)
                                    , CourseRegistrationReceiptCreate(..)
                                    , CourseRegistrationReceiptUpdate(..)
                                    , CourseRegistrationFollowUpCreate(..)
                                    , CourseRegistrationFollowUpUpdate(..)
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
import           TDF.WhatsApp.Client (SendTextResult)
import           TDF.WhatsApp.History ( IncomingWhatsAppRecord(..)
                                      , OutgoingWhatsAppRecord(..)
                                      , WhatsAppDeliveryUpdate(..)
                                      , applyWhatsAppDeliveryUpdate
                                      , normalizeWhatsAppPhone
                                      , recordIncomingWhatsAppMessage
                                      , recordOutgoingWhatsAppMessage
                                      )
import           TDF.WhatsApp.Transport (WhatsAppEnv(..), loadWhatsAppEnv, sendWhatsAppTextIO)
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
instance FromJSON GoogleToken where
  parseJSON = withObject "GoogleToken" $ \o -> do
    accessToken <- o .: "access_token" >>= parseGoogleTokenField "access_token"
    refreshToken <- o .:? "refresh_token" >>= traverse (parseGoogleTokenField "refresh_token")
    expiresIn <- o .: "expires_in"
    when (expiresIn <= (0 :: Int)) $
      fail "expires_in must be positive"
    tokenType <- o .:? "token_type" >>= traverse parseGoogleTokenType
    pure GoogleToken
      { access_token = accessToken
      , refresh_token = refreshToken
      , expires_in = Just expiresIn
      , token_type = tokenType
      }

parseGoogleTokenField :: Text -> Text -> Parser Text
parseGoogleTokenField fieldName raw =
  let clean = T.strip raw
  in if T.null clean
       then fail (T.unpack fieldName <> " must not be blank")
       else if T.any (\ch -> isSpace ch || isControl ch) clean
         then fail (T.unpack fieldName <> " must not contain whitespace or control characters")
         else pure clean

parseGoogleTokenType :: Text -> Parser Text
parseGoogleTokenType raw = do
  tokenTypeVal <- parseGoogleTokenField "token_type" raw
  if T.toLower tokenTypeVal == "bearer"
    then pure "Bearer"
    else fail "token_type must be Bearer"

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
      trials   = TrialsServer.trialsServer (envPool env)
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
  :<|> AuthServer.sessionServer
  :<|> AuthServer.login
  :<|> AuthServer.googleLogin
  :<|> AuthServer.signup
  :<|> AuthServer.changePassword
  :<|> AuthServer.authV1Server
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
parseMcpRequest = parseMaybe $ withObject "McpRequest" $ \o -> do
  rejectUnexpectedObjectFields "McpRequest" ["jsonrpc", "id", "method", "params"] o
  version <- o .: "jsonrpc"
  when (version /= ("2.0" :: Text)) $
    fail "jsonrpc must be 2.0"
  reqId <- o .:? "id"
  case reqId of
    Just (Object _) -> fail "id must be a string, number, or null"
    Just (Array _)  -> fail "id must be a string, number, or null"
    Just (Bool _)   -> fail "id must be a string, number, or null"
    Just (Number n)
      | not (Sci.isInteger n) -> fail "id number must be integral"
    _               -> pure ()
  method <- o .: "method"
  let methodClean = T.strip method
  when (T.null methodClean) $
    fail "method is required"
  when (method /= methodClean) $
    fail "method must not include surrounding whitespace"
  mParams <- o .:? "params"
  case mParams of
    Nothing -> pure (McpRequest reqId method Nothing)
    Just value@(Object _) -> pure (McpRequest reqId method (Just value))
    Just _ -> fail "params must be an object"

parseToolCallParams :: Value -> Maybe (Text, Value)
parseToolCallParams = parseMaybe $ withObject "ToolCallParams" $ \o -> do
  rejectUnexpectedObjectFields "ToolCallParams" ["name", "arguments"] o
  toolName <- o .: "name"
  let toolNameClean = T.strip toolName
  when (T.null toolNameClean) $
    fail "name is required"
  when (toolName /= toolNameClean) $
    fail "name must not include surrounding whitespace"
  mArgs <- o .:? "arguments"
  args <- case mArgs of
    Nothing -> pure (object [])
    Just value@(Object _) -> pure value
    Just _ -> fail "arguments must be an object"
  pure (toolName, args)

validateMcpToolArguments :: Text -> Value -> Either Text ()
validateMcpToolArguments "tdf_health_check" (Object args)
  | AKeyMap.null args = Right ()
  | otherwise = Left "tdf_health_check does not accept arguments"
validateMcpToolArguments "tdf_health_check" _ =
  Left "tdf_health_check arguments must be an object"
validateMcpToolArguments _ _ = Right ()

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
    Just (toolName, args) ->
      case toolName of
        "tdf_health_check" ->
          case validateMcpToolArguments toolName args of
            Left msg -> pure (mcpErrorValue (mcpReqId req) (-32602) msg Nothing)
            Right () -> do
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
  either throwError pure $
    validateInputListInventoryFilters parsedField sessionKey mChannelParam
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

data SessionInputLookup
  = SessionInputByIndex Int
  | SessionInputByKey ME.SessionId
  deriving (Eq, Show)

validateSessionInputLookup :: Maybe Int -> Maybe Text -> Either ServerError SessionInputLookup
validateSessionInputLookup (Just _) (Just _) =
  Left err400 { errBody = "Provide either index or sessionId, not both" }
validateSessionInputLookup _ (Just rawId) =
  case fromPathPiece rawId of
    Nothing     -> Left err400 { errBody = "Invalid sessionId" }
    Just keyVal -> Right (SessionInputByKey keyVal)
validateSessionInputLookup mIndex Nothing =
  case mIndex of
    Nothing -> Right (SessionInputByIndex 1)
    Just n
      | n >= 1    -> Right (SessionInputByIndex n)
      | otherwise -> Left err400 { errBody = "index must be greater than or equal to 1" }

validateInputListInventoryFilters
  :: Maybe InputList.AssetField
  -> Maybe ME.SessionId
  -> Maybe Int
  -> Either ServerError ()
validateInputListInventoryFilters mField mSession mChannel = do
  when (maybe False (< 1) mChannel) $
    Left err400 { errBody = "channel must be greater than or equal to 1" }
  when (isJust mChannel && isNothing mField) $
    Left err400 { errBody = "channel requires field" }
  when (isJust mSession && isNothing mField) $
    Left err400 { errBody = "sessionId requires field" }
  when (isJust mChannel && isNothing mSession) $
    Left err400 { errBody = "channel requires sessionId" }

resolveSessionInputData
  :: Maybe Int
  -> Maybe Text
  -> AppM (Entity ME.Session, [Entity InputList.InputListEntry])
resolveSessionInputData mIndex mSessionId = do
  Env{..} <- ask
  lookupMode <- either throwError pure (validateSessionInputLookup mIndex mSessionId)
  let action =
        case lookupMode of
          SessionInputByKey keyVal -> InputList.fetchSessionInputRowsByKey keyVal
          SessionInputByIndex idx  -> InputList.fetchSessionInputRowsByIndex idx
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
      artistIdValid <- either throwError pure (validatePositiveIdField "artistId" artistId)
      Env pool _ <- ask
      mDto <- liftIO $ flip runSqlPool pool $ loadArtistProfileDTO (toSqlKey artistIdValid)
      maybe (throwError err404) pure mDto

    fanArtistReleases :: Int64 -> AppM [ArtistReleaseDTO]
    fanArtistReleases artistId = do
      artistIdValid <- either throwError pure (validatePositiveIdField "artistId" artistId)
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool $ do
        releases <- selectList [ArtistReleaseArtistPartyId ==. toSqlKey artistIdValid] [Desc ArtistReleaseCreatedAt]
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
  :<|> listCourseCohortsH
  :<|> listRegistrationsH
  :<|> getRegistrationH
  :<|> getRegistrationDossierH
  :<|> listEmailEventsH
  :<|> updateStatusH
  :<|> updateNotesH
  :<|> createReceiptH
  :<|> updateReceiptH
  :<|> deleteReceiptH
  :<|> createFollowUpH
  :<|> updateFollowUpH
  :<|> deleteFollowUpH
  where
    requireCourseAdmin = unless (hasModuleAccess ModuleAdmin user) $
      throwError err403

    upsertCourseH payload = do
      requireCourseAdmin
      saveCourse payload

    listCourseCohortsH = do
      requireCourseAdmin
      listCourseCohorts

    listRegistrationsH mSlug mStatus mLimit = do
      requireCourseAdmin
      listCourseRegistrations mSlug mStatus mLimit

    getRegistrationH slug regId = do
      requireCourseAdmin
      fetchCourseRegistration slug regId

    getRegistrationDossierH slug regId = do
      requireCourseAdmin
      fetchCourseRegistrationDossier slug regId

    listEmailEventsH regId mLimit = do
      requireCourseAdmin
      listCourseRegistrationEmailEvents regId mLimit

    updateStatusH slug regId statusPayload = do
      requireCourseAdmin
      updateCourseRegistrationStatus user slug regId statusPayload

    updateNotesH slug regId payload = do
      requireCourseAdmin
      updateCourseRegistrationNotes slug regId payload

    createReceiptH slug regId payload = do
      requireCourseAdmin
      createCourseRegistrationReceipt user slug regId payload

    updateReceiptH slug regId receiptId payload = do
      requireCourseAdmin
      updateCourseRegistrationReceipt user slug regId receiptId payload

    deleteReceiptH slug regId receiptId = do
      requireCourseAdmin
      deleteCourseRegistrationReceipt user slug regId receiptId

    createFollowUpH slug regId payload = do
      requireCourseAdmin
      createCourseRegistrationFollowUp user slug regId payload

    updateFollowUpH slug regId followUpId payload = do
      requireCourseAdmin
      updateCourseRegistrationFollowUp user slug regId followUpId payload

    deleteFollowUpH slug regId followUpId = do
      requireCourseAdmin
      deleteCourseRegistrationFollowUp user slug regId followUpId

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
    verifyHook mMode mToken mChallenge = do
      cfg <- liftIO loadWhatsAppEnv
      either throwError pure $
        validateHookVerifyRequest mMode mChallenge mToken (waVerifyToken cfg)

    handleMessages payload = do
      cfg <- liftIO loadWhatsAppEnv
      Env{envConfig, envPool} <- ask
      now <- liftIO getCurrentTime
      let deliveryUpdates = extractWhatsAppDeliveryUpdates payload
      let inbound = extractWhatsAppInbound payload
      liftIO $ flip runSqlPool envPool $
        for_ deliveryUpdates $ \WADeliveryStatus{..} -> do
          _ <- applyWhatsAppDeliveryUpdate now WhatsAppDeliveryUpdate
            { wduExternalId = waDeliveryExternalId
            , wduStatus = waDeliveryStatus
            , wduRecipientId = waDeliveryRecipientId
            , wduOccurredAt = waDeliveryOccurredAt
            , wduDeliveryError = waDeliveryError
            , wduStatusPayload = waDeliveryPayload
            }
          pure ()
      for_ inbound $ \WAInbound{..} -> do
        incomingEntity <- liftIO $ flip runSqlPool envPool $
          recordIncomingWhatsAppMessage now IncomingWhatsAppRecord
            { iwrExternalId = waInboundExternalId
            , iwrSenderId = waInboundSenderId
            , iwrSenderName = waInboundSenderName
            , iwrText = waInboundText
            , iwrAdExternalId = waInboundAdExternalId
            , iwrAdName = waInboundAdName
            , iwrCampaignExternalId = waInboundCampaignExternalId
            , iwrCampaignName = waInboundCampaignName
            , iwrMetadata = waInboundMetadata
            , iwrTransportPayload = Nothing
            , iwrSource = Just "whatsapp_webhook"
            }
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
              let incomingMsg = entityVal incomingEntity
              (replyTxt, replyRes) <- sendWhatsappReply cfg phone
              liftIO $ flip runSqlPool envPool $ do
                _ <- recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
                  { owrRecipientPhone = phone
                  , owrRecipientPartyId = ME.whatsAppMessagePartyId incomingMsg
                  , owrRecipientName = waInboundSenderName <|> ME.whatsAppMessageSenderName incomingMsg
                  , owrRecipientEmail = ME.whatsAppMessageContactEmail incomingMsg
                  , owrActorPartyId = Nothing
                  , owrBody = replyTxt
                  , owrSource = Just "course_enrollment_auto_reply"
                  , owrReplyToMessageId = Just (entityKey incomingEntity)
                  , owrReplyToExternalId = Just (ME.whatsAppMessageExternalId incomingMsg)
                  , owrResendOfMessageId = Nothing
                  , owrMetadata = Nothing
                  }
                  replyRes
                pure ()
      pure NoContent

whatsappHooksServer :: ServerT WhatsAppHooksAPI AppM
whatsappHooksServer = whatsappWebhookServer

whatsappMessagesServer :: AuthedUser -> ServerT Api.WhatsAppMessagesAPI AppM
whatsappMessagesServer user mLimit mDirection mRepliedOnly = do
  unless (hasSocialInboxAccess user) $
    throwError err403 { errBody = "Missing required module access" }
  limit <- either throwError pure (validateWhatsAppMessagesLimit mLimit)
  direction <- either throwError pure (parseDirectionParam mDirection)
  repliedOnly <- either throwError pure (parseBoolParam mRepliedOnly)
  let filters =
        concat
          [ maybe [] (\dir -> [ME.WhatsAppMessageDirection ==. dir]) direction
          , if repliedOnly then [ME.WhatsAppMessageRepliedAt !=. Nothing] else []
          ]
  rows <- runDB $
    selectList filters [Desc ME.WhatsAppMessageCreatedAt, LimitTo limit]
  let toObj (Entity msgKey m) = object
        [ "id" .= fromSqlKey msgKey
        , "externalId" .= ME.whatsAppMessageExternalId m
        , "partyId"    .= fmap fromSqlKey (ME.whatsAppMessagePartyId m)
        , "actorPartyId" .= fmap fromSqlKey (ME.whatsAppMessageActorPartyId m)
        , "senderId"   .= ME.whatsAppMessageSenderId m
        , "senderName" .= ME.whatsAppMessageSenderName m
        , "phoneE164"  .= ME.whatsAppMessagePhoneE164 m
        , "contactEmail" .= ME.whatsAppMessageContactEmail m
        , "text"       .= ME.whatsAppMessageText m
        , "metadata"   .= ME.whatsAppMessageMetadata m
        , "direction"  .= ME.whatsAppMessageDirection m
        , "replyStatus" .= ME.whatsAppMessageReplyStatus m
        , "repliedAt"  .= ME.whatsAppMessageRepliedAt m
        , "replyText"  .= ME.whatsAppMessageReplyText m
        , "replyError" .= ME.whatsAppMessageReplyError m
        , "deliveryStatus" .= ME.whatsAppMessageDeliveryStatus m
        , "deliveryUpdatedAt" .= ME.whatsAppMessageDeliveryUpdatedAt m
        , "deliveryError" .= ME.whatsAppMessageDeliveryError m
        , "source" .= ME.whatsAppMessageSource m
        , "resendOfMessageId" .= fmap fromSqlKey (ME.whatsAppMessageResendOfMessageId m)
        , "createdAt"  .= ME.whatsAppMessageCreatedAt m
        ]
  pure (toJSON (map toObj rows))

whatsappReplyServer :: AuthedUser -> ServerT Api.WhatsAppReplyAPI AppM
whatsappReplyServer user WhatsAppReplyReq{..} = do
  unless (hasSocialInboxAccess user) $
    throwError err403 { errBody = "Missing required module access" }
  let recipientRaw = T.strip wrSenderId
  when (T.null recipientRaw) $ throwBadRequest "Remitente requerido"
  recipient <- either throwError pure (validateWhatsAppPhoneInput recipientRaw)
  mExternalId <- either throwError pure (validateWhatsAppReplyExternalId wrExternalId)
  body <- either throwError pure (validateWhatsAppReplyBody wrMessage)
  mReplyTargetRaw <- case mExternalId of
    Nothing -> pure Nothing
    Just extId -> runDB $ getBy (ME.UniqueWhatsAppMessage extId)
  mReplyTarget <- either throwError pure $
    validateWhatsAppReplyTarget recipient mExternalId mReplyTargetRaw
  now <- liftIO getCurrentTime
  waEnv <- liftIO loadWhatsAppEnv
  sendResult <- sendWhatsAppText waEnv recipient body
  sentEntity <- runDB $
    recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
      { owrRecipientPhone = recipient
      , owrRecipientPartyId = mReplyTarget >>= (ME.whatsAppMessagePartyId . entityVal)
      , owrRecipientName = mReplyTarget >>= (ME.whatsAppMessageSenderName . entityVal)
      , owrRecipientEmail = mReplyTarget >>= (ME.whatsAppMessageContactEmail . entityVal)
      , owrActorPartyId = Just (auPartyId user)
      , owrBody = body
      , owrSource = Just "manual_reply"
      , owrReplyToMessageId = entityKey <$> mReplyTarget
      , owrReplyToExternalId = mExternalId
      , owrResendOfMessageId = Nothing
      , owrMetadata = Nothing
      }
      sendResult
  case sendResult of
    Left err ->
      pure (object
        [ "status" .= ("error" :: Text)
        , "message" .= err
        , "messageId" .= fromSqlKey (entityKey sentEntity)
        , "deliveryStatus" .= ME.whatsAppMessageDeliveryStatus (entityVal sentEntity)
        ])
    Right _ ->
      pure (object
        [ "status" .= ("ok" :: Text)
        , "message" .= ("sent" :: Text)
        , "messageId" .= fromSqlKey (entityKey sentEntity)
        , "deliveryStatus" .= ME.whatsAppMessageDeliveryStatus (entityVal sentEntity)
        ])

whatsappConsentServer :: AuthedUser -> ServerT Api.WhatsAppConsentAPI AppM
whatsappConsentServer user =
  let requireAdmin = unless (hasRole Admin user) $ throwError err403
  in whatsappConsentRoutes "tdf-hq-ui" True requireAdmin

whatsappConsentPublicServer :: ServerT Api.WhatsAppConsentPublicAPI AppM
whatsappConsentPublicServer = whatsappConsentRoutes "public" False (pure ())

whatsAppConsentStatusFromRow
  :: Bool
  -> Text
  -> Maybe (Entity ME.WhatsAppConsent)
  -> WhatsAppConsentStatus
whatsAppConsentStatusFromRow exposeDisplayName phoneVal mRow =
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
        , wcsDisplayName =
            if exposeDisplayName
              then ME.whatsAppConsentDisplayName row
              else Nothing
        }

whatsappConsentRoutes :: Text -> Bool -> AppM () -> ServerT Api.WhatsAppConsentRoutes AppM
whatsappConsentRoutes defaultSource exposeDisplayName requireGate =
       createConsent
  :<|> revokeConsent
  :<|> fetchStatus
  where
    normalizePhoneOrFail raw =
      either throwError pure (validateWhatsAppPhoneInput raw)

    toStatus = whatsAppConsentStatusFromRow exposeDisplayName

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
      result <- sendWhatsAppText waEnv phoneVal msg
      pure (msg, result)

    sendOptOutMessage phoneVal = do
      waEnv <- liftIO loadWhatsAppEnv
      let msg = "Listo. No recibirás más mensajes de TDF Records por WhatsApp. " <>
                "Si fue un error, escríbenos y lo reactivamos."
      result <- sendWhatsAppText waEnv phoneVal msg
      pure (msg, result)

    createConsent WhatsAppConsentRequest{..} = do
      requireGate
      unless wcrConsent $ throwBadRequest "Debes aceptar el consentimiento para continuar."
      phoneVal <- normalizePhoneOrFail wcrPhone
      nameClean <- either throwError pure (validateWhatsAppConsentDisplayName wcrName)
      sourceClean <- either throwError pure (validateWhatsAppConsentSource defaultSource wcrSource)
      now <- liftIO getCurrentTime
      let noteClean = Just "consent"
          shouldSend = fromMaybe True wcrSendMessage
      _ <- persistConsent phoneVal nameClean sourceClean noteClean now
      (sent, msgText) <- if shouldSend
        then do
          (msg, res) <- sendConsentMessage phoneVal nameClean
          _ <- runDB $
            recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
              { owrRecipientPhone = phoneVal
              , owrRecipientPartyId = Nothing
              , owrRecipientName = nameClean
              , owrRecipientEmail = Nothing
              , owrActorPartyId = Nothing
              , owrBody = msg
              , owrSource = Just "consent_confirmation"
              , owrReplyToMessageId = Nothing
              , owrReplyToExternalId = Nothing
              , owrResendOfMessageId = Nothing
              , owrMetadata = Nothing
              }
              res
          pure $ case res of
            Left err -> (False, Just err)
            Right _ -> (True, Just msg)
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
      reasonClean <- either throwError pure (validateWhatsAppOptOutReason worReason)
      now <- liftIO getCurrentTime
      let shouldSend = fromMaybe True worSendMessage
      _ <- persistOptOut phoneVal reasonClean now
      (sent, msgText) <- if shouldSend
        then do
          (msg, res) <- sendOptOutMessage phoneVal
          _ <- runDB $
            recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
              { owrRecipientPhone = phoneVal
              , owrRecipientPartyId = Nothing
              , owrRecipientName = Nothing
              , owrRecipientEmail = Nothing
              , owrActorPartyId = Nothing
              , owrBody = msg
              , owrSource = Just "opt_out_confirmation"
              , owrReplyToMessageId = Nothing
              , owrReplyToExternalId = Nothing
              , owrResendOfMessageId = Nothing
              , owrMetadata = Nothing
              }
              res
          pure $ case res of
            Left err -> (False, Just err)
            Right _ -> (True, Just msg)
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

validateWhatsAppConsentDisplayName :: Maybe Text -> Either ServerError (Maybe Text)
validateWhatsAppConsentDisplayName =
  validateOptionalWhatsAppConsentText "name" maxWhatsAppConsentDisplayNameChars

validateWhatsAppConsentSource :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateWhatsAppConsentSource defaultSource rawSource =
  fmap (<|> Just defaultSource) $
    validateOptionalWhatsAppConsentText "source" maxWhatsAppConsentSourceChars rawSource

validateWhatsAppOptOutReason :: Maybe Text -> Either ServerError (Maybe Text)
validateWhatsAppOptOutReason =
  validateOptionalWhatsAppConsentText "reason" maxWhatsAppOptOutReasonChars

validateOptionalWhatsAppConsentText
  :: Text
  -> Int
  -> Maybe Text
  -> Either ServerError (Maybe Text)
validateOptionalWhatsAppConsentText fieldName maxLength rawValue =
  case cleanOptional rawValue of
    Nothing -> Right Nothing
    Just value
      | T.length value > maxLength ->
          Left err400
            { errBody =
                BL.fromStrict $
                  TE.encodeUtf8 $
                    fieldName <> " is too long (max " <> T.pack (show maxLength) <> " characters)"
            }
      | T.any isControl value ->
          Left err400
            { errBody =
                BL.fromStrict $
                  TE.encodeUtf8 $
                    fieldName <> " must not contain control characters"
            }
      | otherwise -> Right (Just value)

maxWhatsAppConsentDisplayNameChars :: Int
maxWhatsAppConsentDisplayNameChars = 120

maxWhatsAppConsentSourceChars :: Int
maxWhatsAppConsentSourceChars = 80

maxWhatsAppOptOutReasonChars :: Int
maxWhatsAppOptOutReasonChars = 500

validateWhatsAppMessagesLimit :: Maybe Int -> Either ServerError Int
validateWhatsAppMessagesLimit Nothing = Right 100
validateWhatsAppMessagesLimit (Just rawLimit)
  | rawLimit < 1 || rawLimit > 200 =
      Left err400 { errBody = "limit must be between 1 and 200" }
  | otherwise = Right rawLimit

validateWhatsAppReplyBody :: Text -> Either ServerError Text
validateWhatsAppReplyBody rawBody
  | T.null body =
      Left err400 { errBody = "Mensaje vacío" }
  | T.length body > maxWhatsAppReplyBodyChars =
      Left err400 { errBody = "Mensaje demasiado largo (max 4096 caracteres)" }
  | otherwise =
      Right body
  where
    body = T.strip rawBody

maxWhatsAppReplyBodyChars :: Int
maxWhatsAppReplyBodyChars = 4096

validateWhatsAppReplyExternalId :: Maybe Text -> Either ServerError (Maybe Text)
validateWhatsAppReplyExternalId = ServerExtra.validateSocialReplyExternalId

parseBoolParam :: Maybe Text -> Either ServerError Bool
parseBoolParam Nothing = Right False
parseBoolParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "true" -> Right True
    "1" -> Right True
    "yes" -> Right True
    "false" -> Right False
    "0" -> Right False
    "no" -> Right False
    "" -> invalidRepliedOnly
    _ -> invalidRepliedOnly
  where
    invalidRepliedOnly =
      Left err400 { errBody = "repliedOnly must be omitted or one of: true, false, 1, 0, yes, no" }

parseDirectionParam :: Maybe Text -> Either ServerError (Maybe Text)
parseDirectionParam Nothing = Right Nothing
parseDirectionParam (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "" -> invalidDirection
    "all" -> Right Nothing
    "incoming" -> Right (Just "incoming")
    "outgoing" -> Right (Just "outgoing")
    _ -> invalidDirection
  where
    invalidDirection =
      Left err400 { errBody = "direction must be omitted or one of: all, incoming, outgoing" }

validateWhatsAppReplyTarget
  :: Text
  -> Maybe Text
  -> Maybe (Entity ME.WhatsAppMessage)
  -> Either ServerError (Maybe (Entity ME.WhatsAppMessage))
validateWhatsAppReplyTarget _ Nothing _ = Right Nothing
validateWhatsAppReplyTarget _ (Just _) Nothing =
  Left err404 { errBody = "WhatsApp reply target not found" }
validateWhatsAppReplyTarget recipient (Just _) (Just target@(Entity _ row))
  | ME.whatsAppMessageDirection row /= "incoming" =
      Left err400 { errBody = "WhatsApp reply target must be an incoming message" }
  | not (replyTargetMatchesRecipient recipient row) =
      Left err400 { errBody = "WhatsApp reply target does not match recipient" }
  | otherwise = Right (Just target)

replyTargetMatchesRecipient :: Text -> ME.WhatsAppMessage -> Bool
replyTargetMatchesRecipient recipient row =
  recipient `elem` normalizedCandidates
  where
    normalizedCandidates =
      mapMaybe normalizeWhatsAppPhone
        [ ME.whatsAppMessageSenderId row
        , fromMaybe "" (ME.whatsAppMessagePhoneE164 row)
        ]

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
parseMetaBackfillOptions = either (const Nothing) Just . validateMetaBackfillOptions

validateMetaBackfillOptions :: Value -> Either ServerError MetaBackfillOptions
validateMetaBackfillOptions raw = do
  case raw of
    Object metaBackfillFields
      | Just unknownKey <- listToMaybe (unknownMetaBackfillKeys metaBackfillFields) ->
          metaBackfillBadRequest ("Unexpected meta backfill field: " <> AKey.toText unknownKey)
    _ -> pure ()
  case parseMaybe (withObject "MetaBackfillOptions" parseOptions) raw of
    Nothing -> metaBackfillBadRequest "Invalid meta backfill payload"
    Just (mPlatform, mConversationLimit, mMessagesPerConversation, mOnlyUnread, mDryRun) -> do
      platformNorm <- validateMetaBackfillPlatform mPlatform
      convLimit <- validateMetaBackfillLimit "conversationLimit" 50 mConversationLimit
      msgLimit <- validateMetaBackfillLimit "messagesPerConversation" 50 mMessagesPerConversation
      pure MetaBackfillOptions
        { mboPlatform = platformNorm
        , mboConversationLimit = convLimit
        , mboMessagesPerConversation = msgLimit
        , mboOnlyUnread = fromMaybe True mOnlyUnread
        , mboDryRun = fromMaybe False mDryRun
        }
  where
    parseOptions o = do
      mPlatform <- o .:? "platform"
      mConversationLimit <- o .:? "conversationLimit"
      mMessagesPerConversation <- o .:? "messagesPerConversation"
      mOnlyUnread <- o .:? "onlyUnread"
      mDryRun <- o .:? "dryRun"
      pure ( mPlatform
           , mConversationLimit
           , mMessagesPerConversation
           , mOnlyUnread
           , mDryRun
           )

allowedMetaBackfillKeys :: Set.Set AKey.Key
allowedMetaBackfillKeys =
  Set.fromList (map AKey.fromText
    [ "platform"
    , "conversationLimit"
    , "messagesPerConversation"
    , "onlyUnread"
    , "dryRun"
    ])

unknownMetaBackfillKeys :: AKeyMap.KeyMap Value -> [AKey.Key]
unknownMetaBackfillKeys =
  filter (`Set.notMember` allowedMetaBackfillKeys) . AKeyMap.keys

validateMetaBackfillPlatform :: Maybe Text -> Either ServerError Text
validateMetaBackfillPlatform Nothing = pure "all"
validateMetaBackfillPlatform (Just raw) =
  case T.toCaseFold (T.strip raw) of
    "instagram" -> pure "instagram"
    "facebook" -> pure "facebook"
    "all" -> pure "all"
    _ -> metaBackfillBadRequest "platform must be one of: all, instagram, facebook"

validateMetaBackfillLimit :: Text -> Int -> Maybe Int -> Either ServerError Int
validateMetaBackfillLimit _ defaultValue Nothing = pure defaultValue
validateMetaBackfillLimit fieldName _ (Just rawValue)
  | rawValue < 1 || rawValue > 200 =
      metaBackfillBadRequest (fieldName <> " must be between 1 and 200")
  | otherwise = pure rawValue

metaBackfillBadRequest :: Text -> Either ServerError a
metaBackfillBadRequest msg =
  Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

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
  (conversationPath, ownerId) <-
    either throwError pure (resolveInstagramBackfillTarget igUserId)
  conversations <-
    fetchGraphConversations manager cfg conversationPath accessToken (Just "instagram") opts
  (incomingScanned, importedCount) <- foldM
    (\(scannedAcc, importedAcc) conversationVal -> do
      let conversationId = fromMaybe "unknown" (jsonTextField "id" conversationVal)
          incomingMessages = filter (isIncomingMetaMessage ownerId) (selectConversationMessages opts conversationVal)
      stored <- mapM (storeBackfilledMessage MetaInstagram opts conversationId) incomingMessages
      let imported = length (filter (\ok -> ok) stored)
      pure (scannedAcc + length incomingMessages, importedAcc + imported)
    )
    (0, 0)
    conversations
  pure (object
    [ "platform" .= ("instagram" :: Text)
    , "igUserId" .= ownerId
    , "handle" .= mHandle
    , "tokenSuffix" .= tokenSuffix accessToken
    , "conversations" .= length conversations
    , "incomingScanned" .= incomingScanned
    , "imported" .= importedCount
    ])

resolveInstagramBackfillTarget :: Text -> Either ServerError (Text, Text)
resolveInstagramBackfillTarget rawIgUserId =
  case normalizeConfiguredGraphNodeId "Instagram backfill account id" (T.unpack rawIgUserId) of
    Left msg ->
      Left err400 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack msg)) }
    Right Nothing ->
      Right ("/me/conversations", "")
    Right (Just igUserId) ->
      Right ("/" <> igUserId <> "/conversations", igUserId)

metaBackfillServer :: AuthedUser -> Value -> AppM Value
metaBackfillServer user payload = do
  unless (hasRole Admin user) $ throwError err403
  opts <- either throwError pure (validateMetaBackfillOptions payload)
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
  :<|> socialListProfiles user
  :<|> socialGetProfile user
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

validateDriveAccess :: AuthedUser -> Either ServerError ()
validateDriveAccess user
  | hasOperationsAccess user || any (`elem` auRoles user) [Artist, Artista] =
      Right ()
  | otherwise =
      Left err403
        { errBody = "Google Drive access requires operations or artist role"
        }

driveUploadServer :: AuthedUser -> Maybe Text -> DriveUploadForm -> AppM DriveUploadDTO
driveUploadServer user mAccessToken DriveUploadForm{..} = do
  either throwError pure (validateDriveAccess user)
  manager <- liftIO $ newManager tlsManagerSettings
  providedToken <-
    either throwError pure (resolveProvidedDriveAccessToken mAccessToken duAccessToken)
  accessToken <- resolveDriveAccessToken manager providedToken
  mFolderEnv <- liftIO $ lookupEnvTextNonEmpty "DRIVE_UPLOAD_FOLDER_ID"
  folder <- either throwError pure (resolveDriveUploadFolderId duFolderId mFolderEnv)
  nameOverride <- either throwError pure (resolveDriveUploadName duName (fdFileName duFile))
  dtoOrErr <-
    liftIO
      (try (uploadToDrive manager accessToken duFile (Just nameOverride) folder) ::
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

resolveProvidedDriveAccessToken :: Maybe Text -> Maybe Text -> Either ServerError (Maybe Text)
resolveProvidedDriveAccessToken mHeaderToken mFormToken = do
  mHeaderTokenClean <- traverse validateDriveAccessToken (cleanOptional mHeaderToken)
  mFormTokenClean <- traverse validateDriveAccessToken (cleanOptional mFormToken)
  case (mHeaderTokenClean, mFormTokenClean) of
    (Just headerToken, Just formToken)
      | headerToken == formToken -> Right (Just headerToken)
      | otherwise ->
          Left err400
            { errBody = "Conflicting Google Drive access tokens" }
    (Just headerToken, Nothing) -> Right (Just headerToken)
    (Nothing, Just formToken) -> Right (Just formToken)
    (Nothing, Nothing) -> Right Nothing

resolveDriveUploadFolderId :: Maybe Text -> Maybe Text -> Either ServerError (Maybe Text)
resolveDriveUploadFolderId mProvidedFolderId mFallbackFolderId =
  case cleanOptional mProvidedFolderId of
    Just provided ->
      Just <$> validateDriveUploadFolderId err400 "folderId" provided
    Nothing ->
      traverse
        (validateDriveUploadFolderId err500 "DRIVE_UPLOAD_FOLDER_ID")
        (cleanOptional mFallbackFolderId)

validateDriveUploadFolderId :: ServerError -> Text -> Text -> Either ServerError Text
validateDriveUploadFolderId baseError fieldName folderId
  | T.null folderId = invalid
  | T.length folderId > 256 = invalid
  | not (T.all isDriveFolderIdChar folderId) = invalid
  | otherwise = Right folderId
  where
    invalid =
      Left baseError
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (driveUploadFolderIdMessage fieldName))
        }

driveUploadFolderIdMessage :: Text -> Text
driveUploadFolderIdMessage fieldName =
  fieldName <> " must be a Google Drive folder id (1-256 ASCII letters, digits, '-' or '_')"

isDriveFolderIdChar :: Char -> Bool
isDriveFolderIdChar ch =
  isAsciiLower ch || isAsciiUpper ch || isDigit ch || ch == '-' || ch == '_'

resolveDriveUploadName :: Maybe Text -> Text -> Either ServerError Text
resolveDriveUploadName mProvidedName rawFileName =
  case cleanOptional mProvidedName of
    Just provided -> validateDriveUploadName "name" provided
    Nothing ->
      maybe
        (Right "upload")
        (validateDriveUploadName "fileName")
        (cleanOptional (Just rawFileName))

validateDriveUploadName :: Text -> Text -> Either ServerError Text
validateDriveUploadName fieldName uploadName
  | T.length uploadName > 240 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be 240 characters or fewer"))
        }
  | T.any isControl uploadName =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " must not contain control characters"))
        }
  | otherwise = Right uploadName

validateDriveAccessToken :: Text -> Either ServerError Text
validateDriveAccessToken token
  | T.length token > 4096 =
      Left err400 { errBody = "Google Drive access token must be 4096 characters or fewer" }
  | T.any isSpace token =
      Left err400 { errBody = "Google Drive access token must not contain whitespace" }
  | T.any isControl token =
      Left err400 { errBody = "Google Drive access token must not contain control characters" }
  | otherwise = Right token

driveTokenExchangeServer :: AuthedUser -> DriveTokenExchangeRequest -> AppM DriveTokenResponse
driveTokenExchangeServer user payload = do
  either throwError pure (validateDriveAccess user)
  Env{envConfig} <- ask
  manager <- liftIO $ newManager tlsManagerSettings
  (cid, secret) <- loadDriveClientCreds
  (codeVal, codeVerifierVal, redirectResolved) <-
    either throwError pure (validateDriveTokenExchangeRequest envConfig payload)
  token <- requestGoogleToken manager
    [ ("client_id", TE.encodeUtf8 cid)
    , ("client_secret", TE.encodeUtf8 secret)
    , ("code", TE.encodeUtf8 codeVal)
    , ("code_verifier", TE.encodeUtf8 codeVerifierVal)
    , ("redirect_uri", TE.encodeUtf8 redirectResolved)
    , ("grant_type", "authorization_code")
    ]
  pure (driveTokenResponseFrom token Nothing)

driveTokenRefreshServer :: AuthedUser -> DriveTokenRefreshRequest -> AppM DriveTokenResponse
driveTokenRefreshServer user payload = do
  either throwError pure (validateDriveAccess user)
  refreshTokenVal <- either throwError pure (validateDriveTokenRefreshRequest payload)
  manager <- liftIO $ newManager tlsManagerSettings
  (cid, secret) <- loadDriveClientCreds
  token <- requestGoogleToken manager
    [ ("client_id", TE.encodeUtf8 cid)
    , ("client_secret", TE.encodeUtf8 secret)
    , ("refresh_token", TE.encodeUtf8 refreshTokenVal)
    , ("grant_type", "refresh_token")
    ]
  pure (driveTokenResponseFrom token (Just refreshTokenVal))

validateDriveTokenExchangeRequest
  :: AppConfig
  -> DriveTokenExchangeRequest
  -> Either ServerError (Text, Text, Text)
validateDriveTokenExchangeRequest cfg DriveTokenExchangeRequest{..} = do
  codeVal <- validateDriveAuthorizationCode code
  codeVerifierVal <- validateDriveCodeVerifier codeVerifier
  redirectResolved <- resolveDriveRedirectUri cfg redirectUri
  pure (codeVal, codeVerifierVal, redirectResolved)

validateDriveTokenRefreshRequest :: DriveTokenRefreshRequest -> Either ServerError Text
validateDriveTokenRefreshRequest DriveTokenRefreshRequest{refreshToken = rawToken} =
  validateDriveRefreshToken rawToken

validateDriveRefreshToken :: Text -> Either ServerError Text
validateDriveRefreshToken rawToken =
  let tokenVal = T.strip rawToken
  in if T.null tokenVal
       then Left err400 { errBody = "refreshToken is required" }
       else
         if T.any isControl tokenVal
           then Left err400 { errBody = "refreshToken must not contain control characters" }
           else if T.any isSpace tokenVal
           then Left err400 { errBody = "refreshToken must not contain whitespace" }
           else Right tokenVal

validateDriveAuthorizationCode :: Text -> Either ServerError Text
validateDriveAuthorizationCode rawCode =
  let codeVal = T.strip rawCode
  in if T.null codeVal
       then Left err400 { errBody = "code is required" }
       else
         if T.any isControl codeVal
           then Left err400 { errBody = "code must not contain control characters" }
           else if T.any isSpace codeVal
           then Left err400 { errBody = "code must not contain whitespace" }
           else Right codeVal

validateDriveCodeVerifier :: Text -> Either ServerError Text
validateDriveCodeVerifier rawVerifier =
  let verifier = T.strip rawVerifier
      verifierLen = T.length verifier
  in if verifierLen < 43 || verifierLen > 128 || not (T.all isPkceVerifierChar verifier)
       then
         Left err400
           { errBody =
               "codeVerifier must be a PKCE verifier (43-128 chars: A-Z a-z 0-9 - . _ ~)"
           }
       else Right verifier
  where
    isPkceVerifierChar ch =
      isAsciiLower ch
        || isAsciiUpper ch
        || isDigit ch
        || ch `elem` ("-._~" :: String)

resolveDriveRedirectUri :: AppConfig -> Maybe Text -> Either ServerError Text
resolveDriveRedirectUri cfg mProvided =
  maybe
    (Right (resolveConfiguredAppBase cfg <> "/oauth/google-drive/callback"))
    validateDriveRedirectUri
    (cleanOptional mProvided)

validateDriveRedirectUri :: Text -> Either ServerError Text
validateDriveRedirectUri rawRedirect =
  case normalizeConfiguredBaseUrl "redirectUri" (T.unpack rawRedirect) of
    Right (Just uri)
      | googleDriveOAuthCallbackPath `T.isSuffixOf` uri -> Right uri
      | otherwise -> invalid
    _ -> invalid
  where
    googleDriveOAuthCallbackPath = "/oauth/google-drive/callback"
    invalid =
      Left err400
        { errBody =
            "redirectUri must be an absolute http(s) Google Drive OAuth callback URL without query or fragment"
        }

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
  either throwError pure $
    resolveDriveClientCreds mCid mSecret mCidFallback mSecretFallback

resolveDriveClientCreds
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Either ServerError (Text, Text)
resolveDriveClientCreds
  rawDriveClientId
  rawDriveClientSecret
  rawGoogleClientId
  rawGoogleClientSecret =
  case (mDriveClientId, mDriveClientSecret) of
    (Just cid, Just secret) -> Right (cid, secret)
    (Just _, Nothing) -> Left partialDriveCredsError
    (Nothing, Just _) -> Left partialDriveCredsError
    (Nothing, Nothing) ->
      case (mGoogleClientId, mGoogleClientSecret) of
        (Just cid, Just secret) -> Right (cid, secret)
        (Just _, Nothing) -> Left partialGoogleCredsError
        (Nothing, Just _) -> Left partialGoogleCredsError
        (Nothing, Nothing) -> Left missingDriveCredsError
  where
    mDriveClientId = cleanOptional rawDriveClientId
    mDriveClientSecret = cleanOptional rawDriveClientSecret
    mGoogleClientId = cleanOptional rawGoogleClientId
    mGoogleClientSecret = cleanOptional rawGoogleClientSecret

    partialDriveCredsError =
      err503
        { errBody =
            "Google Drive no configurado: set both DRIVE_CLIENT_ID and DRIVE_CLIENT_SECRET, " <>
            "or unset both to use GOOGLE_CLIENT_ID/GOOGLE_CLIENT_SECRET fallback."
        }
    partialGoogleCredsError =
      err503
        { errBody =
            "Google Drive no configurado: GOOGLE_CLIENT_ID and GOOGLE_CLIENT_SECRET " <>
            "must both be set for fallback credentials."
        }
    missingDriveCredsError =
      err503
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
  :<|> serviceMarketplaceServer user
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
  :<|> instagramServer user
  :<|> facebookServer user
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
  :<|> futureServer user

validateCalendarRedirectUri :: Text -> Either ServerError Text
validateCalendarRedirectUri rawRedirect =
  case normalizeConfiguredBaseUrl "redirectUri" (T.unpack rawRedirect) of
    Right (Just uri) -> Right uri
    _ ->
      Left err400
        { errBody =
            "redirectUri must be an absolute http(s) Google Calendar OAuth callback URL without query or fragment"
        }

validateConfiguredCalendarRedirectUri :: Text -> Either ServerError Text
validateConfiguredCalendarRedirectUri rawRedirect =
  case normalizeConfiguredBaseUrl "GOOGLE_REDIRECT_URI" (T.unpack rawRedirect) of
    Right (Just uri) -> Right uri
    _ ->
      Left err503
        { errBody =
            "GOOGLE_REDIRECT_URI must be an absolute http(s) URL without query or fragment"
        }

validateOptionalCalendarIdQuery :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCalendarIdQuery Nothing = Right Nothing
validateOptionalCalendarIdQuery (Just rawCalendarId) =
  Just <$> validateCalendarQueryText "calendarId" rawCalendarId

validateCalendarEventListQuery
  :: Maybe Text
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Maybe Text
  -> Either ServerError (Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe Text)
validateCalendarEventListQuery mCalendarId mFrom mTo mStatus = do
  calendarIdVal <- validateOptionalCalendarIdQuery mCalendarId
  statusVal <- validateCalendarEventStatusQuery mStatus
  case (mFrom, mTo) of
    (Just fromVal, Just toVal) | toVal < fromVal ->
      Left err400 { errBody = "from must be on or before to" }
    _ -> Right (calendarIdVal, mFrom, mTo, statusVal)

validateCalendarEventStatusQuery :: Maybe Text -> Either ServerError (Maybe Text)
validateCalendarEventStatusQuery Nothing = Right Nothing
validateCalendarEventStatusQuery (Just rawStatus) = do
  statusVal <- T.toLower <$> validateCalendarQueryText "status" rawStatus
  if statusVal `elem` calendarEventStatuses
    then Right (Just statusVal)
    else
      Left err400
        { errBody = "status must be one of: confirmed, tentative, cancelled" }

validateCalendarQueryText :: Text -> Text -> Either ServerError Text
validateCalendarQueryText fieldName rawValue =
  let trimmed = T.strip rawValue
  in if T.null trimmed
       then Left (calendarQueryError (fieldName <> " must not be blank"))
       else if T.any isControl trimmed
         then Left (calendarQueryError (fieldName <> " must not contain control characters"))
         else Right trimmed

calendarQueryError :: Text -> ServerError
calendarQueryError message =
  err400 { errBody = BL.fromStrict (TE.encodeUtf8 message) }

calendarEventStatuses :: [Text]
calendarEventStatuses = ["confirmed", "tentative", "cancelled"]

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
      mCid <- liftIO $ lookupEnvTextNonEmpty "GOOGLE_CLIENT_ID"
      mSecret <- liftIO $ lookupEnvTextNonEmpty "GOOGLE_CLIENT_SECRET"
      mRedirectEnv <- liftIO $ lookupEnvTextNonEmpty "GOOGLE_REDIRECT_URI"
      let mProvidedRedirect = cleanOptional mRedirect
          mRedirectRaw = mProvidedRedirect <|> mRedirectEnv
      case (mCid, mSecret, mRedirectRaw) of
        (Just cid', Just sec', Just redirRaw) -> do
          redir' <-
            either throwError pure $
              case mProvidedRedirect of
                Just _ -> validateCalendarRedirectUri redirRaw
                Nothing -> validateConfiguredCalendarRedirectUri redirRaw
          pure (cid', sec', redir')
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
      token <- requestGoogleToken manager
        [ ("code", TE.encodeUtf8 code)
        , ("client_id", TE.encodeUtf8 cid)
        , ("client_secret", TE.encodeUtf8 sec)
        , ("redirect_uri", TE.encodeUtf8 redirect)
        , ("grant_type", "authorization_code")
        ]
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
      calendarIdFilter <- either throwError pure (validateOptionalCalendarIdQuery mCalendarId)
      mCfg <- case calendarIdFilter of
        Just cid -> runDB $ getBy (Cal.UniqueCalendar cid)
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
      (calendarIdFilter, fromFilter, toFilter, statusFilter) <-
        either throwError pure (validateCalendarEventListQuery mCal fromTs toTs mStatus)
      let baseFilters =
            maybe [] (\cid -> [Cal.GoogleCalendarEventCalendarId ==. cid]) calendarIdFilter
          dateFilters =
            maybe [] (\fromVal -> [Cal.GoogleCalendarEventStartAt >=. Just fromVal]) fromFilter
            ++ maybe [] (\toVal -> [Cal.GoogleCalendarEventStartAt <=. Just toVal]) toFilter
          statusFilters = maybe [] (\st -> [Cal.GoogleCalendarEventStatus ==. st]) statusFilter
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
      token <- requestGoogleToken manager
        [ ("client_id", TE.encodeUtf8 cid)
        , ("client_secret", TE.encodeUtf8 sec)
        , ("refresh_token", TE.encodeUtf8 rt)
        , ("grant_type", "refresh_token")
        , ("redirect_uri", TE.encodeUtf8 redirect)
        ]
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
            [ CourseSession "Sábado 1 · Introducción" (fromGregorian 2026 5 2)
            , CourseSession "Sábado 2 · Grabación" (fromGregorian 2026 5 9)
            , CourseSession "Sábado 3 · Mezcla" (fromGregorian 2026 5 16)
            , CourseSession "Sábado 4 · Masterización" (fromGregorian 2026 5 23)
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
        , subtitle = "Presencial · Cuatro sábados · 16 horas en total · Próximo inicio: sábado 2 de mayo"
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

loadCourseMetadata :: Text -> AppM CourseMetadata
loadCourseMetadata rawSlug = do
  normalized <- either throwError pure (validateCourseSlug rawSlug)
  Env{..} <- ask
  waEnv <- liftIO loadWhatsAppEnv
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
        fromMaybe
          (buildLandingUrlFor cfg slugVal)
          (sanitizeStoredCoursePublicUrl "landingUrl" (Trials.courseLandingUrl course))
      whatsappUrl =
        fromMaybe
          (buildWhatsappCtaFor (waContactNumber waEnv) (Trials.courseTitle course) landingUrlVal)
          (sanitizeStoredCoursePublicUrl "whatsappCtaUrl" (Trials.courseWhatsappCtaUrl course))
      dawsList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseDaws course))
      includesList = filter (not . T.null) (maybe [] (map T.strip) (Trials.courseIncludes course))
      instructorNameVal = cleanOptional (Trials.courseInstructorName course)
      instructorBioVal = cleanOptional (Trials.courseInstructorBio course)
      instructorAvatarVal =
        sanitizeStoredCoursePublicUrl "instructorAvatarUrl" (Trials.courseInstructorAvatarUrl course)
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
      , locationMapUrl =
          fromMaybe
            (courseMapFallback cfg)
            (sanitizeStoredCoursePublicUrl "locationMapUrl" (Trials.courseLocationMapUrl course))
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
  in case resolveWhatsappCtaNumber mNumber of
       Just num -> "https://wa.me/" <> num <> "?text=" <> encoded
       Nothing  -> "https://wa.me/?text=" <> encoded

resolveWhatsappCtaNumber :: Maybe Text -> Maybe Text
resolveWhatsappCtaNumber mNumber =
  T.filter isDigit <$> (mNumber >>= normalizeCourseRegistrationPhoneInput)

saveCourse :: CourseUpsert -> AppM CourseMetadata
saveCourse Courses.CourseUpsert{..} = do
  now <- liftIO getCurrentTime
  Env{..} <- ask
  waEnv <- liftIO loadWhatsAppEnv
  slugVal <- either throwError pure (validateCourseSlug slug)
  titleClean <- either throwError pure (validateRequiredCourseTextField "title" 160 title)
  capacityClean <- either throwError pure (validateCoursePositiveField "capacity" capacity)
  priceCentsClean <- either throwError pure (validateCourseNonNegativeField "priceCents" priceCents)
  startHourClean <- either throwError pure (validateOptionalCourseSessionStartHour sessionStartHour)
  durationHoursClean <- either throwError pure (validateOptionalCourseSessionDurationHours sessionDurationHours)
  either throwError pure (validateCourseSessionScheduleWindow startHourClean durationHoursClean)
  sessionsClean <- either throwError pure (validateCourseSessionInputs sessions)
  syllabusClean <- either throwError pure (validateCourseSyllabusInputs syllabus)
  dawsClean <- either throwError pure (validateCourseTextListField "daws" 160 daws)
  includesClean <- either throwError pure (validateCourseTextListField "includes" 160 includes)
  locationMapUrlClean <- either throwError pure (validateCoursePublicUrlField "locationMapUrl" locationMapUrl)
  landingUrlClean <- either throwError pure (validateCoursePublicUrlField "landingUrl" landingUrl)
  whatsappClean <- either throwError pure (validateCoursePublicUrlField "whatsappCtaUrl" whatsappCtaUrl)
  instructorAvatarClean <- either throwError pure (validateCoursePublicUrlField "instructorAvatarUrl" instructorAvatarUrl)
  currencyClean <- either throwError pure (validateCourseCurrency currency)
  let
      subtitleClean = cleanOptional subtitle
      formatClean = cleanOptional format
      durationClean = cleanOptional duration
      locationLabelClean = cleanOptional locationLabel
      landingResolved = fromMaybe (buildLandingUrlFor envConfig slugVal) landingUrlClean
      whatsappResolved = fromMaybe (buildWhatsappCtaFor (waContactNumber waEnv) titleClean landingResolved) whatsappClean
      instructorNameClean = cleanOptional instructorName
      instructorBioClean = cleanOptional instructorBio
      withOrder fallbackIdx mOrder = fromMaybe fallbackIdx mOrder
      -- Persistent is misencoding text[] here, so store course arrays through SQL.
      persistTextArrayField courseId columnName Nothing =
        rawExecute
          ("UPDATE course SET " <> columnName <> " = NULL WHERE id = ?")
          [PersistInt64 (fromSqlKey courseId)]
      persistTextArrayField courseId columnName (Just values) =
        rawExecute
          ("UPDATE course SET " <> columnName <> " = ARRAY(SELECT jsonb_array_elements_text(?::jsonb)) WHERE id = ?")
          [ PersistText (TE.decodeUtf8 (BL.toStrict (encode values)))
          , PersistInt64 (fromSqlKey courseId)
          ]
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
        , Trials.courseDaws = Nothing
        , Trials.courseIncludes = Nothing
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
          , Trials.CourseDaws =. Nothing
          , Trials.CourseIncludes =. Nothing
          , Trials.CourseInstructorName =. instructorNameClean
          , Trials.CourseInstructorBio =. instructorBioClean
          , Trials.CourseInstructorAvatarUrl =. instructorAvatarClean
          , Trials.CourseUpdatedAt =. now
          ]
        pure cid

    persistTextArrayField courseId "daws" dawsClean
    persistTextArrayField courseId "includes" includesClean

    deleteWhere [Trials.CourseSessionModelCourseId ==. courseId]
    deleteWhere [Trials.CourseSyllabusItemCourseId ==. courseId]

    let sessionPayload = zip [1..] sessionsClean
    for_ sessionPayload $ \(idx, CourseSessionIn{..}) -> do
      let ordVal = withOrder idx order
      insert_ Trials.CourseSessionModel
        { Trials.courseSessionModelCourseId = courseId
        , Trials.courseSessionModelLabel = label
        , Trials.courseSessionModelDate = date
        , Trials.courseSessionModelOrder = Just ordVal
        }

    let syllabusPayload = zip [1..] syllabusClean
    for_ syllabusPayload $ \(idx, CourseSyllabusIn{title = syllabusTitle, topics = syllabusTopics, order = syllabusOrder}) -> do
      let ordVal = withOrder idx syllabusOrder
      insert_ Trials.CourseSyllabusItem
        { Trials.courseSyllabusItemCourseId = courseId
        , Trials.courseSyllabusItemTitle = syllabusTitle
        , Trials.courseSyllabusItemTopics = syllabusTopics
        , Trials.courseSyllabusItemOrder = Just ordVal
        }
  loadCourseMetadata slugVal

listCourseCohorts :: AppM [DTO.CourseCohortOptionDTO]
listCourseCohorts = do
  courses <- runDB $ selectList [] [Desc Trials.CourseUpdatedAt]
  registrationSlugs <- runDB $
    rawSql
      "SELECT DISTINCT course_slug FROM course_registration WHERE course_slug IS NOT NULL AND course_slug <> '' ORDER BY course_slug"
      []
  let fromCourse (Entity _ course) =
        DTO.CourseCohortOptionDTO
          { DTO.ccSlug = Trials.courseSlug course
          , DTO.ccTitle = Just (Trials.courseTitle course)
          }
      courseOptions = map fromCourse courses
      knownSlugs = Set.fromList (map DTO.ccSlug courseOptions)
      extraOptions =
        [ DTO.CourseCohortOptionDTO
            { DTO.ccSlug = slugVal
            , DTO.ccTitle = Nothing
            }
        | Single slugVal <- registrationSlugs
        , not (Set.member slugVal knownSlugs)
        ]
  pure (courseOptions <> extraOptions)

listCourseRegistrations
  :: Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> AppM [DTO.CourseRegistrationDTO]
listCourseRegistrations mSlug mStatus mLimit = do
  normalizedSlug <- either throwError pure (validateOptionalCourseSlugFilter mSlug)
  normalizedStatus <- either throwError pure (validateOptionalCourseRegistrationStatusFilter mStatus)
  limit <- either throwError pure (validateCourseRegistrationListLimit 200 mLimit)
  let filters = catMaybes
        [ (ME.CourseRegistrationCourseSlug ==.) <$> normalizedSlug
        , (ME.CourseRegistrationStatus ==.) <$> normalizedStatus
        ]
  rows <- runDB $ selectList filters [Desc ME.CourseRegistrationCreatedAt, LimitTo limit]
  pure (map toCourseRegistrationDTO rows)

fetchCourseRegistrationEntity :: Text -> Int64 -> AppM (Entity ME.CourseRegistration)
fetchCourseRegistrationEntity rawSlug regId = do
  regIdValid <- either throwError pure (validateCourseRegistrationId regId)
  slugVal <- either throwError pure (validateCourseSlug rawSlug)
  let key = toSqlKey regIdValid
  mRow <- runDB $ getEntity key
  case mRow of
    Nothing -> throwNotFound "Registro no encontrado"
    Just ent@(Entity _ reg)
      | ME.courseRegistrationCourseSlug reg /= slugVal -> throwNotFound "Registro no encontrado"
      | otherwise -> pure ent

fetchCourseRegistration :: Text -> Int64 -> AppM DTO.CourseRegistrationDTO
fetchCourseRegistration rawSlug regId =
  toCourseRegistrationDTO <$> fetchCourseRegistrationEntity rawSlug regId

ensureCourseRegistrationPartyRoles :: PartyId -> SqlPersistT IO ()
ensureCourseRegistrationPartyRoles pid = do
  void $ upsert (PartyRole pid Student True) [PartyRoleActive =. True]
  void $ upsert (PartyRole pid Customer True) [PartyRoleActive =. True]

ensurePartyForCourseRegistrationDb
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO PartyId
ensurePartyForCourseRegistrationDb mName mEmail mPhone now = do
  let display = fromMaybe "Alumno / cliente" (cleanOptional mName <|> mEmail <|> mPhone)
  mExisting <- case mEmail of
    Just addr -> selectFirst [PartyPrimaryEmail ==. Just addr] []
    Nothing -> case mPhone of
      Just phone -> selectFirst [PartyPrimaryPhone ==. Just phone] []
      Nothing -> pure Nothing
  pid <- case mExisting of
    Just (Entity partyId party) -> do
      let updates = catMaybes
            [ if isJust (partyPrimaryEmail party) || isNothing mEmail then Nothing else Just (PartyPrimaryEmail =. mEmail)
            , if isJust (partyPrimaryPhone party) || isNothing mPhone then Nothing else Just (PartyPrimaryPhone =. mPhone)
            , if isJust (partyWhatsapp party) || isNothing mPhone then Nothing else Just (PartyWhatsapp =. mPhone)
            , if T.null (M.partyDisplayName party) then Just (PartyDisplayName =. display) else Nothing
            ]
      unless (null updates) $
        update partyId updates
      pure partyId
    Nothing -> insert Party
      { partyLegalName = Nothing
      , partyDisplayName = display
      , partyIsOrg = False
      , partyTaxId = Nothing
      , partyPrimaryEmail = mEmail
      , partyPrimaryPhone = mPhone
      , partyWhatsapp = mPhone
      , partyInstagram = Nothing
      , partyEmergencyContact = Nothing
      , partyNotes = Nothing
      , partyCreatedAt = now
      }
  ensureCourseRegistrationPartyRoles pid
  pure pid

ensureCourseRegistrationParty
  :: Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> UTCTime
  -> AppM (Maybe PartyId, Maybe (Text, Text))
ensureCourseRegistrationParty mName mEmail mPhone now =
  case mEmail of
    Just emailAddr -> do
      (partyId, mNewUser) <- ensurePartyWithAccount mName emailAddr mPhone
      runDB $ ensureCourseRegistrationPartyRoles partyId
      pure (Just partyId, mNewUser)
    Nothing -> case mPhone of
      Nothing -> pure (Nothing, Nothing)
      Just _ -> do
        partyId <- runDB $ ensurePartyForCourseRegistrationDb mName Nothing mPhone now
        pure (Just partyId, Nothing)

ensureCourseRegistrationPartyLink
  :: Entity ME.CourseRegistration
  -> AppM (Entity ME.CourseRegistration)
ensureCourseRegistrationPartyLink ent@(Entity regId reg) =
  case ME.courseRegistrationPartyId reg of
    Just _ -> pure ent
    Nothing -> do
      now <- liftIO getCurrentTime
      (mPartyId, _) <- ensureCourseRegistrationParty
        (ME.courseRegistrationFullName reg)
        (ME.courseRegistrationEmail reg)
        (ME.courseRegistrationPhoneE164 reg)
        now
      case mPartyId of
        Nothing -> pure ent
        Just partyId -> do
          runDB $ update regId
            [ ME.CourseRegistrationPartyId =. Just partyId
            , ME.CourseRegistrationUpdatedAt =. now
            ]
          pure (Entity regId reg
            { ME.courseRegistrationPartyId = Just partyId
            , ME.courseRegistrationUpdatedAt = now
            })

parseOptionalUtcText :: Text -> Maybe Text -> AppM (Maybe UTCTime)
parseOptionalUtcText fieldName mValue =
  case cleanOptional mValue of
    Nothing -> pure Nothing
    Just value ->
      case iso8601ParseM (T.unpack value) of
        Just ts -> pure (Just ts)
        Nothing -> throwBadRequest (fieldName <> " inválido")

courseFollowUpTypeOptions :: [Text]
courseFollowUpTypeOptions =
  [ "note"
  , "call"
  , "whatsapp"
  , "email"
  , "payment_receipt"
  , "status_change"
  , "registration"
  ]

normalizeCourseFollowUpTypeToken :: Text -> Maybe Text
normalizeCourseFollowUpTypeToken raw =
  case normalizeBookingStatusToken raw of
    "note" -> Just "note"
    "call" -> Just "call"
    "whatsapp" -> Just "whatsapp"
    "email" -> Just "email"
    "paymentreceipt" -> Just "payment_receipt"
    "statuschange" -> Just "status_change"
    "registration" -> Just "registration"
    _ -> Nothing

normalizeCourseFollowUpType :: Maybe Text -> Text
normalizeCourseFollowUpType =
  fromMaybe "note" . (>>= normalizeCourseFollowUpTypeToken) . cleanOptional

parseCourseFollowUpType :: Maybe Text -> Either ServerError Text
parseCourseFollowUpType Nothing = Right "note"
parseCourseFollowUpType (Just raw) =
  case cleanOptional (Just raw) >>= normalizeCourseFollowUpTypeToken of
    Just entryTypeVal -> Right entryTypeVal
    Nothing ->
      Left err400
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              "Invalid course follow-up entryType. Allowed values: "
                <> T.intercalate ", " courseFollowUpTypeOptions
        }

registrationHasReceipts :: Key ME.CourseRegistration -> AppM Bool
registrationHasReceipts regKey = do
  mReceipt <- runDB $ selectFirst
    [ME.CourseRegistrationReceiptRegistrationId ==. regKey]
    [Desc ME.CourseRegistrationReceiptCreatedAt]
  pure (isJust mReceipt)

validateCourseRegistrationReceiptDeletion :: Text -> Int -> Either ServerError ()
validateCourseRegistrationReceiptDeletion rawStatus receiptCount
  | normalizedStatus == Just "paid" && receiptCount <= 1 =
      Left err409
        { errBody =
            "No puedes eliminar el unico comprobante de una inscripcion pagada. Cambia el estado antes de borrarlo."
        }
  | otherwise =
      Right ()
  where
    normalizedStatus = normalizeCourseRegistrationStatus rawStatus

insertCourseRegistrationFollowUp
  :: Key ME.CourseRegistration
  -> Maybe PartyId
  -> Maybe PartyId
  -> Text
  -> Maybe Text
  -> Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe UTCTime
  -> UTCTime
  -> SqlPersistT IO (Entity ME.CourseRegistrationFollowUp)
insertCourseRegistrationFollowUp regKey mPartyId mCreatedBy rawEntryType mSubject rawNotes mAttachmentUrl mAttachmentName mNextFollowUpAt now = do
  let notesVal = T.strip rawNotes
      subjectVal = cleanOptional mSubject
      attachmentUrlVal = cleanOptional mAttachmentUrl
      attachmentNameVal = cleanOptional mAttachmentName
      entryTypeVal = normalizeCourseFollowUpType (Just rawEntryType)
  followUpId <- insert ME.CourseRegistrationFollowUp
    { ME.courseRegistrationFollowUpRegistrationId = regKey
    , ME.courseRegistrationFollowUpPartyId = mPartyId
    , ME.courseRegistrationFollowUpEntryType = entryTypeVal
    , ME.courseRegistrationFollowUpSubject = subjectVal
    , ME.courseRegistrationFollowUpNotes = notesVal
    , ME.courseRegistrationFollowUpAttachmentUrl = attachmentUrlVal
    , ME.courseRegistrationFollowUpAttachmentName = attachmentNameVal
    , ME.courseRegistrationFollowUpNextFollowUpAt = mNextFollowUpAt
    , ME.courseRegistrationFollowUpCreatedBy = mCreatedBy
    , ME.courseRegistrationFollowUpCreatedAt = now
    , ME.courseRegistrationFollowUpUpdatedAt = now
    }
  pure $ Entity followUpId ME.CourseRegistrationFollowUp
    { ME.courseRegistrationFollowUpRegistrationId = regKey
    , ME.courseRegistrationFollowUpPartyId = mPartyId
    , ME.courseRegistrationFollowUpEntryType = entryTypeVal
    , ME.courseRegistrationFollowUpSubject = subjectVal
    , ME.courseRegistrationFollowUpNotes = notesVal
    , ME.courseRegistrationFollowUpAttachmentUrl = attachmentUrlVal
    , ME.courseRegistrationFollowUpAttachmentName = attachmentNameVal
    , ME.courseRegistrationFollowUpNextFollowUpAt = mNextFollowUpAt
    , ME.courseRegistrationFollowUpCreatedBy = mCreatedBy
    , ME.courseRegistrationFollowUpCreatedAt = now
    , ME.courseRegistrationFollowUpUpdatedAt = now
    }

recordCourseRegistrationStatusChange
  :: AuthedUser
  -> Entity ME.CourseRegistration
  -> Text
  -> Text
  -> UTCTime
  -> AppM ()
recordCourseRegistrationStatusChange user (Entity regId reg) oldStatus newStatus now = do
  let oldLabel = T.toLower (T.strip oldStatus)
      newLabel = T.toLower (T.strip newStatus)
  unless (oldLabel == newLabel) $
    void $ runDB $ insertCourseRegistrationFollowUp
      regId
      (ME.courseRegistrationPartyId reg)
      (Just (auPartyId user))
      "status_change"
      (Just ("Estado: " <> newLabel))
      ("Cambio de estado automático de " <> oldLabel <> " a " <> newLabel <> ".")
      Nothing
      Nothing
      Nothing
      now

fetchCourseRegistrationDossier :: Text -> Int64 -> AppM DTO.CourseRegistrationDossierDTO
fetchCourseRegistrationDossier rawSlug regId = do
  ent <- fetchCourseRegistrationEntity rawSlug regId >>= ensureCourseRegistrationPartyLink
  let regKey = entityKey ent
  receipts <- runDB $ selectList
    [ME.CourseRegistrationReceiptRegistrationId ==. regKey]
    [Desc ME.CourseRegistrationReceiptCreatedAt]
  followUps <- runDB $ selectList
    [ME.CourseRegistrationFollowUpRegistrationId ==. regKey]
    [Desc ME.CourseRegistrationFollowUpCreatedAt]
  pure DTO.CourseRegistrationDossierDTO
    { DTO.crdRegistration = toCourseRegistrationDTO ent
    , DTO.crdReceipts = map toCourseRegistrationReceiptDTO receipts
    , DTO.crdFollowUps = map toCourseRegistrationFollowUpDTO followUps
    , DTO.crdCanMarkPaid = not (null receipts)
    }

updateCourseRegistrationStatus
  :: AuthedUser
  -> Text
  -> Int64
  -> CourseRegistrationStatusUpdate
  -> AppM CourseRegistrationResponse
updateCourseRegistrationStatus user rawSlug regId CourseRegistrationStatusUpdate{..} = do
  let rawStatus = T.strip status
  when (T.null rawStatus) $
    throwBadRequest "status requerido"
  newStatus <- either throwError pure (parseCourseRegistrationStatus rawStatus)
  ent <- fetchCourseRegistrationEntity rawSlug regId >>= ensureCourseRegistrationPartyLink
  let regKey = entityKey ent
      reg = entityVal ent
  when (newStatus == "paid") $ do
    hasReceipt <- registrationHasReceipts regKey
    unless hasReceipt $
      throwBadRequest "Debes subir un comprobante de pago antes de marcar esta inscripción como pagada."
  now <- liftIO getCurrentTime
  runDB $ update regKey
    [ ME.CourseRegistrationStatus =. newStatus
    , ME.CourseRegistrationUpdatedAt =. now
    ]
  recordCourseRegistrationStatusChange user ent (ME.courseRegistrationStatus reg) newStatus now
  pure CourseRegistrationResponse { id = regId, status = newStatus }

updateCourseRegistrationNotes
  :: Text
  -> Int64
  -> CourseRegistrationNotesUpdate
  -> AppM DTO.CourseRegistrationDTO
updateCourseRegistrationNotes rawSlug regId CourseRegistrationNotesUpdate{..} = do
  ent <- fetchCourseRegistrationEntity rawSlug regId
  now <- liftIO getCurrentTime
  let regKey = entityKey ent
      reg = entityVal ent
      notesVal = cleanOptional notes
      updated = reg
        { ME.courseRegistrationAdminNotes = notesVal
        , ME.courseRegistrationUpdatedAt = now
        }
  runDB $ update regKey
    [ ME.CourseRegistrationAdminNotes =. notesVal
    , ME.CourseRegistrationUpdatedAt =. now
    ]
  pure (toCourseRegistrationDTO (Entity regKey updated))

createCourseRegistrationReceipt
  :: AuthedUser
  -> Text
  -> Int64
  -> CourseRegistrationReceiptCreate
  -> AppM DTO.CourseRegistrationReceiptDTO
createCourseRegistrationReceipt user rawSlug regId CourseRegistrationReceiptCreate{..} = do
  ent <- fetchCourseRegistrationEntity rawSlug regId >>= ensureCourseRegistrationPartyLink
  fileUrlVal <- do
    normalized <- either throwError pure (validateCourseRegistrationUrlField "fileUrl" (Just fileUrl))
    maybe (throwBadRequest "fileUrl requerido") pure normalized
  now <- liftIO getCurrentTime
  let regKey = entityKey ent
      reg = entityVal ent
      receipt = ME.CourseRegistrationReceipt
        { ME.courseRegistrationReceiptRegistrationId = regKey
        , ME.courseRegistrationReceiptPartyId = ME.courseRegistrationPartyId reg
        , ME.courseRegistrationReceiptFileUrl = fileUrlVal
        , ME.courseRegistrationReceiptFileName = cleanOptional fileName
        , ME.courseRegistrationReceiptMimeType = cleanOptional mimeType
        , ME.courseRegistrationReceiptNotes = cleanOptional notes
        , ME.courseRegistrationReceiptUploadedBy = Just (auPartyId user)
        , ME.courseRegistrationReceiptCreatedAt = now
        , ME.courseRegistrationReceiptUpdatedAt = now
        }
  receiptId <- runDB $ insert receipt
  void $ runDB $ insertCourseRegistrationFollowUp
    regKey
    (ME.courseRegistrationPartyId reg)
    (Just (auPartyId user))
    "payment_receipt"
    (Just "Comprobante de pago agregado")
    (fromMaybe "Se agregó un comprobante de pago." (cleanOptional notes <|> cleanOptional fileName))
    (Just fileUrlVal)
    (cleanOptional fileName)
    Nothing
    now
  pure (toCourseRegistrationReceiptDTO (Entity receiptId receipt))

fetchCourseRegistrationReceiptEntity
  :: Text
  -> Int64
  -> Int64
  -> AppM (Entity ME.CourseRegistration, Entity ME.CourseRegistrationReceipt)
fetchCourseRegistrationReceiptEntity rawSlug regId receiptId = do
  receiptIdValid <- either throwError pure (validateCourseRegistrationReceiptId receiptId)
  ent <- fetchCourseRegistrationEntity rawSlug regId
  let regKey = entityKey ent
      receiptKey = toSqlKey receiptIdValid
  mReceipt <- runDB $ getEntity receiptKey
  case mReceipt of
    Nothing -> throwNotFound "Comprobante no encontrado"
    Just receiptEnt@(Entity _ receipt)
      | ME.courseRegistrationReceiptRegistrationId receipt /= regKey -> throwNotFound "Comprobante no encontrado"
      | otherwise -> pure (ent, receiptEnt)

updateCourseRegistrationReceipt
  :: AuthedUser
  -> Text
  -> Int64
  -> Int64
  -> CourseRegistrationReceiptUpdate
  -> AppM DTO.CourseRegistrationReceiptDTO
updateCourseRegistrationReceipt _ rawSlug regId receiptId CourseRegistrationReceiptUpdate{..} = do
  (ent, Entity receiptKey receipt) <- fetchCourseRegistrationReceiptEntity rawSlug regId receiptId
  now <- liftIO getCurrentTime
  fileUrlVal <- case fileUrl of
    Nothing -> pure (ME.courseRegistrationReceiptFileUrl receipt)
    Just rawUrl -> do
      normalized <- either throwError pure (validateCourseRegistrationUrlField "fileUrl" (Just rawUrl))
      maybe (throwBadRequest "fileUrl requerido") pure normalized
  let updated =
        receipt
          { ME.courseRegistrationReceiptPartyId = ME.courseRegistrationPartyId (entityVal ent)
          , ME.courseRegistrationReceiptFileUrl = fileUrlVal
          , ME.courseRegistrationReceiptFileName =
              maybe (ME.courseRegistrationReceiptFileName receipt) (cleanOptional . Just) fileName
          , ME.courseRegistrationReceiptMimeType =
              maybe (ME.courseRegistrationReceiptMimeType receipt) (cleanOptional . Just) mimeType
          , ME.courseRegistrationReceiptNotes =
              maybe (ME.courseRegistrationReceiptNotes receipt) (cleanOptional . Just) notes
          , ME.courseRegistrationReceiptUpdatedAt = now
          }
  runDB $ update receiptKey
    [ ME.CourseRegistrationReceiptPartyId =. ME.courseRegistrationReceiptPartyId updated
    , ME.CourseRegistrationReceiptFileUrl =. ME.courseRegistrationReceiptFileUrl updated
    , ME.CourseRegistrationReceiptFileName =. ME.courseRegistrationReceiptFileName updated
    , ME.CourseRegistrationReceiptMimeType =. ME.courseRegistrationReceiptMimeType updated
    , ME.CourseRegistrationReceiptNotes =. ME.courseRegistrationReceiptNotes updated
    , ME.CourseRegistrationReceiptUpdatedAt =. now
    ]
  pure (toCourseRegistrationReceiptDTO (Entity receiptKey updated))

deleteCourseRegistrationReceipt
  :: AuthedUser
  -> Text
  -> Int64
  -> Int64
  -> AppM NoContent
deleteCourseRegistrationReceipt _ rawSlug regId receiptId = do
  (Entity regKey reg, Entity receiptKey _) <- fetchCourseRegistrationReceiptEntity rawSlug regId receiptId
  receiptCount <- runDB $ count [ME.CourseRegistrationReceiptRegistrationId ==. regKey]
  either throwError pure $
    validateCourseRegistrationReceiptDeletion (ME.courseRegistrationStatus reg) receiptCount
  runDB $ delete receiptKey
  pure NoContent

fetchCourseRegistrationFollowUpEntity
  :: Text
  -> Int64
  -> Int64
  -> AppM (Entity ME.CourseRegistration, Entity ME.CourseRegistrationFollowUp)
fetchCourseRegistrationFollowUpEntity rawSlug regId followUpId = do
  followUpIdValid <- either throwError pure (validateCourseRegistrationFollowUpId followUpId)
  ent <- fetchCourseRegistrationEntity rawSlug regId
  let regKey = entityKey ent
      followUpKey = toSqlKey followUpIdValid
  mFollowUp <- runDB $ getEntity followUpKey
  case mFollowUp of
    Nothing -> throwNotFound "Seguimiento no encontrado"
    Just followUpEnt@(Entity _ followUp)
      | ME.courseRegistrationFollowUpRegistrationId followUp /= regKey -> throwNotFound "Seguimiento no encontrado"
      | otherwise -> pure (ent, followUpEnt)

createCourseRegistrationFollowUp
  :: AuthedUser
  -> Text
  -> Int64
  -> CourseRegistrationFollowUpCreate
  -> AppM DTO.CourseRegistrationFollowUpDTO
createCourseRegistrationFollowUp user rawSlug regId CourseRegistrationFollowUpCreate{..} = do
  ent <- fetchCourseRegistrationEntity rawSlug regId >>= ensureCourseRegistrationPartyLink
  let notesVal = T.strip notes
  when (T.null notesVal) $
    throwBadRequest "notes requerido"
  entryTypeVal <- either throwError pure (parseCourseFollowUpType entryType)
  attachmentUrlVal <- either throwError pure (validateCourseRegistrationUrlField "attachmentUrl" attachmentUrl)
  mNextFollowUpAt <- parseOptionalUtcText "nextFollowUpAt" nextFollowUpAt
  now <- liftIO getCurrentTime
  followUp <- runDB $ insertCourseRegistrationFollowUp
    (entityKey ent)
    (ME.courseRegistrationPartyId (entityVal ent))
    (Just (auPartyId user))
    entryTypeVal
    subject
    notesVal
    attachmentUrlVal
    attachmentName
    mNextFollowUpAt
    now
  pure (toCourseRegistrationFollowUpDTO followUp)

updateCourseRegistrationFollowUp
  :: AuthedUser
  -> Text
  -> Int64
  -> Int64
  -> CourseRegistrationFollowUpUpdate
  -> AppM DTO.CourseRegistrationFollowUpDTO
updateCourseRegistrationFollowUp _ rawSlug regId followUpId CourseRegistrationFollowUpUpdate{..} = do
  (ent, Entity followUpKey followUp) <- fetchCourseRegistrationFollowUpEntity rawSlug regId followUpId
  notesVal <- case notes of
    Nothing -> pure (ME.courseRegistrationFollowUpNotes followUp)
    Just raw ->
      let trimmed = T.strip raw
      in if T.null trimmed
        then throwBadRequest "notes requerido"
        else pure trimmed
  entryTypeVal <- case entryType of
    Nothing -> pure (ME.courseRegistrationFollowUpEntryType followUp)
    Just raw -> either throwError pure (parseCourseFollowUpType (Just raw))
  attachmentUrlVal <- case attachmentUrl of
    Nothing -> pure (ME.courseRegistrationFollowUpAttachmentUrl followUp)
    Just rawUrl -> either throwError pure (validateCourseRegistrationUrlField "attachmentUrl" (Just rawUrl))
  mNextFollowUpAt <- case nextFollowUpAt of
    Nothing -> pure (ME.courseRegistrationFollowUpNextFollowUpAt followUp)
    Just raw -> parseOptionalUtcText "nextFollowUpAt" raw
  now <- liftIO getCurrentTime
  let updated =
        followUp
          { ME.courseRegistrationFollowUpPartyId = ME.courseRegistrationPartyId (entityVal ent)
          , ME.courseRegistrationFollowUpEntryType = entryTypeVal
          , ME.courseRegistrationFollowUpSubject =
              maybe (ME.courseRegistrationFollowUpSubject followUp) (cleanOptional . Just) subject
          , ME.courseRegistrationFollowUpNotes = notesVal
          , ME.courseRegistrationFollowUpAttachmentUrl = attachmentUrlVal
          , ME.courseRegistrationFollowUpAttachmentName =
              maybe (ME.courseRegistrationFollowUpAttachmentName followUp) (cleanOptional . Just) attachmentName
          , ME.courseRegistrationFollowUpNextFollowUpAt = mNextFollowUpAt
          , ME.courseRegistrationFollowUpUpdatedAt = now
          }
  runDB $ update followUpKey
    [ ME.CourseRegistrationFollowUpPartyId =. ME.courseRegistrationFollowUpPartyId updated
    , ME.CourseRegistrationFollowUpEntryType =. ME.courseRegistrationFollowUpEntryType updated
    , ME.CourseRegistrationFollowUpSubject =. ME.courseRegistrationFollowUpSubject updated
    , ME.CourseRegistrationFollowUpNotes =. ME.courseRegistrationFollowUpNotes updated
    , ME.CourseRegistrationFollowUpAttachmentUrl =. ME.courseRegistrationFollowUpAttachmentUrl updated
    , ME.CourseRegistrationFollowUpAttachmentName =. ME.courseRegistrationFollowUpAttachmentName updated
    , ME.CourseRegistrationFollowUpNextFollowUpAt =. ME.courseRegistrationFollowUpNextFollowUpAt updated
    , ME.CourseRegistrationFollowUpUpdatedAt =. now
    ]
  pure (toCourseRegistrationFollowUpDTO (Entity followUpKey updated))

deleteCourseRegistrationFollowUp
  :: AuthedUser
  -> Text
  -> Int64
  -> Int64
  -> AppM NoContent
deleteCourseRegistrationFollowUp _ rawSlug regId followUpId = do
  (_, Entity followUpKey _) <- fetchCourseRegistrationFollowUpEntity rawSlug regId followUpId
  runDB $ delete followUpKey
  pure NoContent

listCourseRegistrationEmailEvents
  :: Int64
  -> Maybe Int
  -> AppM [DTO.CourseEmailEventDTO]
listCourseRegistrationEmailEvents regId mLimit = do
  regIdValid <- either throwError pure (validateCourseRegistrationId regId)
  let regKey = toSqlKey regIdValid :: Key ME.CourseRegistration
  capped <- either throwError pure (validateCourseRegistrationEmailEventListLimit mLimit)
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
    , DTO.crPartyId = fromSqlKey <$> ME.courseRegistrationPartyId reg
    , DTO.crFullName = ME.courseRegistrationFullName reg
    , DTO.crEmail = ME.courseRegistrationEmail reg
    , DTO.crPhoneE164 = ME.courseRegistrationPhoneE164 reg
    , DTO.crSource = ME.courseRegistrationSource reg
    , DTO.crStatus = ME.courseRegistrationStatus reg
    , DTO.crAdminNotes = ME.courseRegistrationAdminNotes reg
    , DTO.crHowHeard = ME.courseRegistrationHowHeard reg
    , DTO.crUtmSource = ME.courseRegistrationUtmSource reg
    , DTO.crUtmMedium = ME.courseRegistrationUtmMedium reg
    , DTO.crUtmCampaign = ME.courseRegistrationUtmCampaign reg
    , DTO.crUtmContent = ME.courseRegistrationUtmContent reg
    , DTO.crCreatedAt = ME.courseRegistrationCreatedAt reg
    , DTO.crUpdatedAt = ME.courseRegistrationUpdatedAt reg
    }

toCourseRegistrationReceiptDTO :: Entity ME.CourseRegistrationReceipt -> DTO.CourseRegistrationReceiptDTO
toCourseRegistrationReceiptDTO (Entity receiptId receipt) =
  DTO.CourseRegistrationReceiptDTO
    { DTO.crrId = fromSqlKey receiptId
    , DTO.crrRegistrationId = fromSqlKey (ME.courseRegistrationReceiptRegistrationId receipt)
    , DTO.crrPartyId = fromSqlKey <$> ME.courseRegistrationReceiptPartyId receipt
    , DTO.crrFileUrl = ME.courseRegistrationReceiptFileUrl receipt
    , DTO.crrFileName = ME.courseRegistrationReceiptFileName receipt
    , DTO.crrMimeType = ME.courseRegistrationReceiptMimeType receipt
    , DTO.crrNotes = ME.courseRegistrationReceiptNotes receipt
    , DTO.crrUploadedBy = fromSqlKey <$> ME.courseRegistrationReceiptUploadedBy receipt
    , DTO.crrCreatedAt = ME.courseRegistrationReceiptCreatedAt receipt
    , DTO.crrUpdatedAt = ME.courseRegistrationReceiptUpdatedAt receipt
    }

toCourseRegistrationFollowUpDTO :: Entity ME.CourseRegistrationFollowUp -> DTO.CourseRegistrationFollowUpDTO
toCourseRegistrationFollowUpDTO (Entity followUpId followUp) =
  DTO.CourseRegistrationFollowUpDTO
    { DTO.crfId = fromSqlKey followUpId
    , DTO.crfRegistrationId = fromSqlKey (ME.courseRegistrationFollowUpRegistrationId followUp)
    , DTO.crfPartyId = fromSqlKey <$> ME.courseRegistrationFollowUpPartyId followUp
    , DTO.crfEntryType = ME.courseRegistrationFollowUpEntryType followUp
    , DTO.crfSubject = ME.courseRegistrationFollowUpSubject followUp
    , DTO.crfNotes = ME.courseRegistrationFollowUpNotes followUp
    , DTO.crfAttachmentUrl = ME.courseRegistrationFollowUpAttachmentUrl followUp
    , DTO.crfAttachmentName = ME.courseRegistrationFollowUpAttachmentName followUp
    , DTO.crfNextFollowUpAt = ME.courseRegistrationFollowUpNextFollowUpAt followUp
    , DTO.crfCreatedBy = fromSqlKey <$> ME.courseRegistrationFollowUpCreatedBy followUp
    , DTO.crfCreatedAt = ME.courseRegistrationFollowUpCreatedAt followUp
    , DTO.crfUpdatedAt = ME.courseRegistrationFollowUpUpdatedAt followUp
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
                            , Courses.remaining = metaRemaining
                            } = metaRaw
  now <- liftIO getCurrentTime
  sourceClean <- either throwError pure (validateCourseRegistrationSource source)
  nameClean <- either throwError pure (validateOptionalCourseRegistrationTextField "fullName" 160 fullName)
  howHeardClean <- either throwError pure (validateOptionalCourseRegistrationTextField "howHeard" 256 howHeard)
  (utmSourceVal, utmMediumVal, utmCampaignVal, utmContentVal) <-
    either throwError pure (validateCourseRegistrationUtm utm)
  let slugVal = normalizeSlug metaSlug
      pendingStatus = "pending_payment"
  normalizedEmail <- either throwError pure (validateCourseRegistrationEmail email)
  phoneClean <- either throwError pure (validateCourseRegistrationPhoneE164 phoneE164)
  either throwError pure (validateCourseRegistrationContactChannels normalizedEmail phoneClean)
  when (sourceClean == "landing" && isNothing nameClean) $
    throwBadRequest "nombre requerido"
  when (sourceClean == "landing" && isNothing normalizedEmail) $
    throwBadRequest "email requerido"
  existing <- runDB $ findExistingRegistration slugVal normalizedEmail phoneClean
  either throwError pure $
    validateCourseRegistrationSeatAvailability
      metaRemaining
      (ME.courseRegistrationStatus . entityVal <$> existing)
  (mPartyId, mNewUser) <- ensureCourseRegistrationParty nameClean normalizedEmail phoneClean now
  case existing of
    -- Update in-place only when the existing row is still pending; otherwise create a fresh row.
    Just (Entity regId reg) | isPendingCourseRegistrationStatus (ME.courseRegistrationStatus reg) -> do
      let resolvedPartyId = ME.courseRegistrationPartyId reg <|> mPartyId
      runDB $ update regId
        [ ME.CourseRegistrationFullName =. (nameClean <|> ME.courseRegistrationFullName reg)
        , ME.CourseRegistrationEmail =. (normalizedEmail <|> ME.courseRegistrationEmail reg)
        , ME.CourseRegistrationPhoneE164 =. (phoneClean <|> ME.courseRegistrationPhoneE164 reg)
        , ME.CourseRegistrationPartyId =. resolvedPartyId
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
        , ME.courseRegistrationPartyId = mPartyId
        , ME.courseRegistrationFullName = nameClean
        , ME.courseRegistrationEmail = normalizedEmail
        , ME.courseRegistrationPhoneE164 = phoneClean
        , ME.courseRegistrationSource = sourceClean
        , ME.courseRegistrationStatus = pendingStatus
        , ME.courseRegistrationAdminNotes = Nothing
        , ME.courseRegistrationHowHeard = howHeardClean
        , ME.courseRegistrationUtmSource = utmSourceVal
        , ME.courseRegistrationUtmMedium = utmMediumVal
        , ME.courseRegistrationUtmCampaign = utmCampaignVal
        , ME.courseRegistrationUtmContent = utmContentVal
        , ME.courseRegistrationCreatedAt = now
        , ME.courseRegistrationUpdatedAt = now
        }
      void $ runDB $ insertCourseRegistrationFollowUp
        regId
        mPartyId
        Nothing
        "registration"
        (Just "Inscripción recibida")
        ("Nueva inscripción capturada desde " <> sourceClean <> ".")
        Nothing
        Nothing
        Nothing
        now
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
  , waInboundSenderName :: Maybe Text
  , waInboundText       :: Text
  , waInboundAdExternalId :: Maybe Text
  , waInboundAdName     :: Maybe Text
  , waInboundCampaignExternalId :: Maybe Text
  , waInboundCampaignName :: Maybe Text
  , waInboundMetadata   :: Maybe Text
  } deriving (Show)

extractWhatsAppInbound :: WAMetaWebhook -> [WAInbound]
extractWhatsAppInbound WAMetaWebhook{entry} =
  concatMap extractEntry entry
  where
    extractEntry WAEntry{changes} = concatMap extractChange changes

    extractChange WAChange{value=WAValue{messages=Just msgs, contacts=mContacts}} =
      let contactMap = Map.fromList
            [ (waIdVal, cleanOptional (WA.name =<< WA.waIdProfile contact))
            | contact <- fromMaybe [] mContacts
            , Just waIdVal <- [cleanOptional (WA.waIdValue contact)]
            ]
      in
        [ WAInbound
            { waInboundExternalId = resolvedExternalId senderId msg
            , waInboundSenderId = senderId
            , waInboundSenderName = join (Map.lookup senderId contactMap)
            , waInboundText = body
            , waInboundAdExternalId = adExt
            , waInboundAdName = adName
            , waInboundCampaignExternalId = Nothing
            , waInboundCampaignName = Nothing
            , waInboundMetadata = metaTxt
            }
        | msg@WAMessage{waType, text=Just txtBody} <- msgs
        , waType == "text"
        , Just senderId <- [cleanOptional (Just (WA.from msg))]
        , Just body <- [cleanOptional (Just (WA.body txtBody))]
        , let referral = waReferral msg <|> (waContext msg >>= waContextReferral)
              (adExt, adName, metaTxt) = waReferralMeta referral
        ]
    extractChange _ = []

    resolvedExternalId senderId msg =
      fromMaybe
        (senderId <> "-" <> fromMaybe "0" (cleanOptional (waTimestamp msg)))
        (cleanWhatsAppWebhookExternalId (waId msg))

    cleanWhatsAppWebhookExternalId rawExternalId =
      cleanOptional rawExternalId >>= \externalId ->
        if T.length externalId <= 256
            && not (T.any isSpace externalId)
            && not (T.any isControl externalId)
          then Just externalId
          else Nothing

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

data WADeliveryStatus = WADeliveryStatus
  { waDeliveryExternalId :: Text
  , waDeliveryStatus     :: Text
  , waDeliveryRecipientId :: Maybe Text
  , waDeliveryOccurredAt :: Maybe UTCTime
  , waDeliveryError      :: Maybe Text
  , waDeliveryPayload    :: Maybe Text
  } deriving (Show)

extractWhatsAppDeliveryUpdates :: WAMetaWebhook -> [WADeliveryStatus]
extractWhatsAppDeliveryUpdates WAMetaWebhook{entry} =
  concatMap extractEntry entry
  where
    extractEntry WAEntry{changes} = concatMap extractChange changes

    extractChange WAChange{value=WAValue{statuses=Just rows}} =
      [ WADeliveryStatus
          { waDeliveryExternalId = externalId
          , waDeliveryStatus = fromMaybe "unknown" (cleanOptional (WA.waStatus statusRow))
          , waDeliveryRecipientId = cleanOptional (WA.waRecipientId statusRow)
          , waDeliveryOccurredAt = WA.waStatusTimestamp statusRow >>= parseWhatsAppEpoch
          , waDeliveryError = foldMap summarizeErrors (WA.waStatusErrors statusRow)
          , waDeliveryPayload = Just (TE.decodeUtf8 (BL.toStrict (encode payload)))
          }
      | statusRow <- rows
      , let externalId = fromMaybe "" (cleanOptional (WA.waStatusId statusRow))
            payload = object
              [ "id" .= WA.waStatusId statusRow
              , "status" .= WA.waStatus statusRow
              , "recipient_id" .= WA.waRecipientId statusRow
              , "timestamp" .= WA.waStatusTimestamp statusRow
              , "errors" .= WA.waStatusErrors statusRow
              ]
      , not (T.null externalId)
      ]
    extractChange _ = []

    summarizeErrors errs =
      let rendered = TE.decodeUtf8 (BL.toStrict (encode errs))
      in cleanOptional (Just rendered)

    parseWhatsAppEpoch raw = do
      value <- cleanOptional (Just raw)
      seconds <- readMaybe (T.unpack value) :: Maybe Integer
      pure (addUTCTime (fromInteger seconds) (UTCTime (fromGregorian 1970 1 1) 0))

sendWhatsappReply :: WhatsAppEnv -> Text -> AppM (Text, Either Text SendTextResult)
sendWhatsappReply waEnv phone = do
  Env{envConfig} <- ask
  let slugVal = productionCourseSlug envConfig
  metaRaw <- loadCourseMetadata slugVal
  let Courses.CourseMetadata{ Courses.title = metaTitle
                            , Courses.capacity = metaCapacity
                            , Courses.landingUrl = metaLanding
                            } = metaRaw
  let msg = "¡Gracias por tu interés en " <> metaTitle <> "! Aquí tienes el link de inscripción: "
            <> metaLanding <> ". Cupos limitados (" <> T.pack (show metaCapacity) <> ")."
  res <- liftIO $ sendWhatsAppTextIO waEnv phone msg
  pure (msg, res)

sendWhatsAppText :: WhatsAppEnv -> Text -> Text -> AppM (Either Text SendTextResult)
sendWhatsAppText waEnv phone msg = liftIO $ sendWhatsAppTextIO waEnv phone msg

normalizePhone :: Text -> Maybe Text
normalizePhone = normalizeWhatsAppPhone

normalizeCourseRegistrationPhoneInput :: Text -> Maybe Text
normalizeCourseRegistrationPhoneInput raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isDigit trimmed
      allowedPhoneChar ch =
        isDigit ch || isSpace ch || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
  in
    if T.null onlyDigits
         || digitCount < 8
         || digitCount > 15
         || hasInvalidChars
         || not plusIsValid
      then Nothing
      else Just ("+" <> onlyDigits)

normalizeSlug :: Text -> Text
normalizeSlug = T.toLower . T.strip

validateCourseSlug :: Text -> Either ServerError Text
validateCourseSlug rawSlug =
  let slugVal = normalizeSlug rawSlug
      isSlugAtom ch = isAsciiLower ch || isDigit ch
      isSlugChar ch = isSlugAtom ch || ch == '-'
  in
    if T.null slugVal
      then Left err400 { errBody = "slug requerido" }
      else
        if T.length slugVal <= courseSlugMaxLength
            && T.all isSlugChar slugVal
            && T.any isSlugAtom slugVal
          then Right slugVal
          else Left invalidCourseSlug
  where
    courseSlugMaxLength = 96
    invalidCourseSlug =
      err400
        { errBody =
            "slug must contain only ASCII letters, numbers, and hyphens, "
              <> "include at least one letter or number, and be 96 characters or fewer"
        }

validateOptionalCourseSlugFilter :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCourseSlugFilter Nothing = Right Nothing
validateOptionalCourseSlugFilter (Just rawSlug)
  | T.null (T.strip rawSlug) =
      Left invalidOptionalCourseSlug
  | otherwise =
      case validateCourseSlug rawSlug of
        Right slugVal -> Right (Just slugVal)
        Left _ -> Left invalidOptionalCourseSlug
  where
    invalidOptionalCourseSlug =
      err400
        { errBody =
            "slug must be omitted or use only ASCII letters, numbers, and hyphens"
        }

validateCourseRegistrationSource :: Text -> Either ServerError Text
validateCourseRegistrationSource raw =
  let sourceVal = T.toLower (T.strip raw)
      isSourceChar ch = isAsciiLower ch || isDigit ch || ch == '_' || ch == '-'
      isSourceAtom ch = isAsciiLower ch || isDigit ch
  in if T.null sourceVal
    then Left err400 { errBody = "source requerido" }
    else
      if T.length sourceVal <= 64
          && T.all isSourceChar sourceVal
          && T.any isSourceAtom sourceVal
        then Right sourceVal
        else
          Left err400
            { errBody =
                "source must be an ASCII keyword using letters, numbers, underscores, "
                  <> "or hyphens (64 chars max)"
            }

validateCourseRegistrationPhoneE164 :: Maybe Text -> Either ServerError (Maybe Text)
validateCourseRegistrationPhoneE164 Nothing = Right Nothing
validateCourseRegistrationPhoneE164 (Just rawPhone) =
  case cleanOptional (Just rawPhone) of
    Nothing -> Right Nothing
    Just _ ->
      case normalizeCourseRegistrationPhoneInput rawPhone of
        Just phoneClean -> Right (Just phoneClean)
        Nothing -> Left err400 { errBody = "phoneE164 inválido" }

validateAdsInquiryPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateAdsInquiryPhone Nothing = Right Nothing
validateAdsInquiryPhone (Just rawPhone) =
  case normalizeOptionalInput (Just rawPhone) of
    Nothing -> Right Nothing
    Just _ ->
      case normalizeCourseRegistrationPhoneInput rawPhone of
        Just phoneClean -> Right (Just phoneClean)
        Nothing -> Left err400 { errBody = "phone inválido" }

validateAdsInquiryContactChannels :: Maybe Text -> Maybe Text -> Either ServerError ()
validateAdsInquiryContactChannels mEmail mPhone
  | isNothing mEmail && isNothing mPhone =
      Left err400 { errBody = "email o phone requerido" }
  | otherwise =
      Right ()

validateAdsInquiryChannel :: Maybe Text -> Either ServerError (Maybe Text)
validateAdsInquiryChannel Nothing = Right Nothing
validateAdsInquiryChannel (Just rawChannel) =
  case normalizeOptionalInput (Just rawChannel) of
    Nothing -> Right Nothing
    Just channel ->
      let channelVal = T.toLower channel
      in if T.length channelVal <= 64
            && T.all isChannelChar channelVal
            && T.any isChannelAtom channelVal
           then Right (Just channelVal)
           else
             Left err400
               { errBody =
                   "channel must be an ASCII keyword using letters, numbers, underscores, "
                     <> "or hyphens (64 chars max)"
               }
  where
    isChannelChar ch = isAsciiLower ch || isDigit ch || ch == '_' || ch == '-'
    isChannelAtom ch = isAsciiLower ch || isDigit ch

validateAdsInquiryMessage :: Maybe Text -> Either ServerError (Maybe Text)
validateAdsInquiryMessage Nothing = Right Nothing
validateAdsInquiryMessage (Just rawMessage) =
  case normalizeOptionalInput (Just rawMessage) of
    Nothing -> Right Nothing
    Just message
      | T.length message > adsInquiryMessageMaxLength ->
          Left err400 { errBody = "message must be 2000 characters or fewer" }
      | T.any isUnsafeAdsInquiryMessageControl message ->
          Left err400
            { errBody =
                "message must not contain control characters "
                  <> "other than tabs or line breaks"
            }
      | otherwise ->
          Right (Just message)
  where
    isUnsafeAdsInquiryMessageControl ch =
      isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

adsInquiryMessageMaxLength :: Int
adsInquiryMessageMaxLength = 2000

validateWhatsAppPhoneInput :: Text -> Either ServerError Text
validateWhatsAppPhoneInput rawPhone =
  case cleanOptional (Just rawPhone) of
    Nothing -> invalidWhatsAppPhone
    Just _ ->
      case normalizeCourseRegistrationPhoneInput rawPhone of
        Just phoneClean -> Right phoneClean
        Nothing -> invalidWhatsAppPhone
  where
    invalidWhatsAppPhone =
      Left err400 { errBody = "Número de WhatsApp inválido." }

validatePublicBookingContactDetails :: Text -> Maybe Text -> Either ServerError (Text, Maybe Text)
validatePublicBookingContactDetails rawEmail rawPhone = do
  emailClean <-
    case cleanOptional (Just rawEmail) of
      Nothing ->
        Left err400 { errBody = "email requerido" }
      Just _ -> do
        normalizedEmail <- validateCourseRegistrationEmail (Just rawEmail)
        case normalizedEmail of
          Just emailVal -> Right emailVal
          Nothing -> Left err400 { errBody = "email requerido" }
  phoneClean <- validateCourseRegistrationPhoneE164 rawPhone
  pure (emailClean, phoneClean)

validatePublicBookingFullName :: Text -> Either ServerError Text
validatePublicBookingFullName rawName =
  case cleanOptional (Just rawName) of
    Nothing -> Left err400 { errBody = "nombre requerido" }
    Just nameVal
      | T.length nameVal > 160 ->
          Left err400 { errBody = "nombre debe tener 160 caracteres o menos" }
      | T.any isControl nameVal ->
          Left err400 { errBody = "nombre no debe contener caracteres de control" }
      | otherwise ->
          Right nameVal

validatePublicBookingServiceType :: Text -> Either ServerError Text
validatePublicBookingServiceType rawServiceType =
  case cleanOptional (Just rawServiceType) of
    Nothing -> Left err400 { errBody = "serviceType requerido" }
    Just serviceTypeVal
      | T.length serviceTypeVal > 120 ->
          Left err400 { errBody = "serviceType debe tener 120 caracteres o menos" }
      | T.any isControl serviceTypeVal ->
          Left err400 { errBody = "serviceType no debe contener caracteres de control" }
      | otherwise ->
          Right serviceTypeVal

validatePublicBookingNotes :: Maybe Text -> Either ServerError (Maybe Text)
validatePublicBookingNotes Nothing = Right Nothing
validatePublicBookingNotes (Just rawNotes) =
  case normalizeOptionalInput (Just rawNotes) of
    Nothing -> Right Nothing
    Just notesVal
      | T.length notesVal > publicBookingNotesMaxLength ->
          Left err400 { errBody = "notes must be 1000 characters or fewer" }
      | T.any isUnsafeNoteControl notesVal ->
          Left err400
            { errBody =
                "notes must not contain control characters "
                  <> "other than tabs or line breaks"
            }
      | otherwise ->
          Right (Just notesVal)
  where
    isUnsafeNoteControl ch =
      isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

publicBookingNotesMaxLength :: Int
publicBookingNotesMaxLength = 1000

validatePublicBookingDurationMinutes :: Maybe Int -> Either ServerError Int
validatePublicBookingDurationMinutes Nothing = Right 60
validatePublicBookingDurationMinutes (Just durationMinutes)
  | durationMinutes < publicBookingMinDurationMinutes
      || durationMinutes > publicBookingMaxDurationMinutes =
      Left err400 { errBody = "durationMinutes must be between 30 and 480" }
  | durationMinutes `mod` publicBookingDurationStepMinutes /= 0 =
      Left err400 { errBody = "durationMinutes must be a multiple of 15" }
  | otherwise =
      Right durationMinutes

publicBookingMinDurationMinutes :: Int
publicBookingMinDurationMinutes = 30

publicBookingMaxDurationMinutes :: Int
publicBookingMaxDurationMinutes = 480

publicBookingDurationStepMinutes :: Int
publicBookingDurationStepMinutes = 15

validatePublicBookingStartAt :: UTCTime -> UTCTime -> Either ServerError UTCTime
validatePublicBookingStartAt now startsAt
  | startsAt <= now = Left err400 { errBody = "startsAt must be in the future" }
  | startsAt > addUTCTime (fromIntegral publicBookingMaxLeadDays * 86400) now =
      Left err400 { errBody = "startsAt must be within 365 days" }
  | not (isPublicBookingStartOnGrid startsAt) =
      Left err400 { errBody = "startsAt must align to a 15-minute slot" }
  | otherwise = Right startsAt

publicBookingMaxLeadDays :: Int
publicBookingMaxLeadDays = 365

isPublicBookingStartOnGrid :: UTCTime -> Bool
isPublicBookingStartOnGrid startsAt =
  let picoseconds = diffTimeToPicoseconds (utctDayTime startsAt)
      stepPicoseconds =
        fromIntegral (publicBookingDurationStepMinutes * 60) * picosecondsPerSecond
  in picoseconds `mod` stepPicoseconds == 0

picosecondsPerSecond :: Integer
picosecondsPerSecond = 1000000000000

validateBookingTimeRange :: UTCTime -> UTCTime -> Either ServerError ()
validateBookingTimeRange startsAt endsAt
  | endsAt > startsAt = Right ()
  | otherwise = Left err400 { errBody = "endsAt must be after startsAt" }

validateCourseRegistrationEmail :: Maybe Text -> Either ServerError (Maybe Text)
validateCourseRegistrationEmail Nothing = Right Nothing
validateCourseRegistrationEmail (Just rawEmail) =
  case cleanOptional (Just rawEmail) of
    Nothing -> Right Nothing
    Just emailVal ->
      let normalized = T.toLower emailVal
      in if isValidCourseRegistrationEmail normalized
        then Right (Just normalized)
        else Left err400 { errBody = "email inválido" }

validateMarketplaceBuyerEmail :: Text -> Either ServerError Text
validateMarketplaceBuyerEmail rawEmail =
  case validateCourseRegistrationEmail (Just rawEmail) of
    Right (Just emailVal) -> Right emailVal
    Right Nothing -> Left err400 { errBody = "buyerEmail requerido" }
    Left _ -> Left err400 { errBody = "buyerEmail inválido" }

validateMarketplaceBuyerPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateMarketplaceBuyerPhone rawPhone =
  case validateCourseRegistrationPhoneE164 rawPhone of
    Right phoneVal -> Right phoneVal
    Left _ -> Left err400 { errBody = "buyerPhone inválido" }

validateMarketplaceBuyerName :: Text -> Either ServerError Text
validateMarketplaceBuyerName rawName =
  case cleanOptional (Just rawName) of
    Nothing -> Left err400 { errBody = "buyerName requerido" }
    Just nameVal
      | T.length nameVal > 160 ->
          Left err400 { errBody = "buyerName must be 160 characters or fewer" }
      | T.any isControl nameVal ->
          Left err400 { errBody = "buyerName must not contain control characters" }
      | otherwise ->
          Right nameVal

validateCourseRegistrationUrlField :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateCourseRegistrationUrlField _ Nothing = Right Nothing
validateCourseRegistrationUrlField fieldName (Just rawUrl) =
  case cleanOptional (Just rawUrl) of
    Nothing -> Right Nothing
    Just urlVal ->
      if "https://" `T.isPrefixOf` T.toLower urlVal
          && TrialsServer.isValidHttpUrl urlVal
        then Right (Just urlVal)
        else
          Left err400
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  fieldName <> " must be an absolute https URL"
            }

validateCoursePublicUrlField :: Text -> Maybe Text -> Either ServerError (Maybe Text)
validateCoursePublicUrlField _ Nothing = Right Nothing
validateCoursePublicUrlField fieldName (Just rawUrl) =
  case cleanOptional (Just rawUrl) of
    Nothing -> Right Nothing
    Just urlVal ->
      if "https://" `T.isPrefixOf` T.toLower urlVal
          && TrialsServer.isValidHttpUrl urlVal
        then
          if fieldName == "whatsappCtaUrl" && not (isAllowedWhatsAppCtaUrl urlVal)
            then
              Left err400
                { errBody =
                    "whatsappCtaUrl must use wa.me, api.whatsapp.com, or web.whatsapp.com"
                }
            else Right (Just urlVal)
        else
          Left err400
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  fieldName <> " must be an absolute https URL"
            }

isAllowedWhatsAppCtaUrl :: Text -> Bool
isAllowedWhatsAppCtaUrl rawUrl =
  maybe False (`elem` allowedHosts) (extractHttpUrlHost rawUrl)
  where
    allowedHosts =
      [ "wa.me"
      , "api.whatsapp.com"
      , "web.whatsapp.com"
      ]

extractHttpUrlHost :: Text -> Maybe Text
extractHttpUrlHost rawUrl = do
  withoutScheme <-
    T.stripPrefix "https://" lowerUrl <|> T.stripPrefix "http://" lowerUrl
  let authority = T.takeWhile (`notElem` ("/?#" :: String)) withoutScheme
      host = T.takeWhile (/= ':') authority
  if T.null host then Nothing else Just host
  where
    lowerUrl = T.toLower (T.strip rawUrl)

sanitizeStoredCoursePublicUrl :: Text -> Maybe Text -> Maybe Text
sanitizeStoredCoursePublicUrl fieldName =
  either (const Nothing) (\urlVal -> urlVal) . validateCoursePublicUrlField fieldName

isValidCourseRegistrationEmail :: Text -> Bool
isValidCourseRegistrationEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      T.length candidate <= 254
        && isValidEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any isSpace candidate)
        && T.isInfixOf "." domain
        && all isValidEmailDomainLabel (T.splitOn "." domain)
    _ -> False

isValidEmailLocalPart :: Text -> Bool
isValidEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= 64
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (T.isInfixOf ".." localPart)
    && T.all isValidEmailLocalChar localPart

isValidEmailLocalChar :: Char -> Bool
isValidEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidEmailDomainLabel :: Text -> Bool
isValidEmailDomainLabel label =
  not (T.null label)
    && T.length label <= 63
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidEmailDomainChar label

isValidEmailDomainChar :: Char -> Bool
isValidEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

validateCourseRegistrationContactChannels :: Maybe Text -> Maybe Text -> Either ServerError ()
validateCourseRegistrationContactChannels mEmail mPhone
  | isNothing mEmail && isNothing mPhone =
      Left err400 { errBody = "email o phoneE164 requerido" }
  | otherwise =
      Right ()

validateCourseRegistrationSeatAvailability :: Int -> Maybe Text -> Either ServerError ()
validateCourseRegistrationSeatAvailability remainingSeats mExistingStatus
  | remainingSeats > 0 =
      Right ()
  | maybe False isPendingCourseRegistrationStatus mExistingStatus =
      Right ()
  | otherwise =
      Left err409 { errBody = "course has no remaining seats" }

isPendingCourseRegistrationStatus :: Text -> Bool
isPendingCourseRegistrationStatus rawStatus =
  normalizeCourseRegistrationStatus rawStatus == Just "pending_payment"

validateRequiredCourseTextField :: Text -> Int -> Text -> Either ServerError Text
validateRequiredCourseTextField fieldName maxLength rawValue =
  case validateOptionalCourseTextField fieldName maxLength (Just rawValue) of
    Right (Just value) -> Right value
    Right Nothing ->
      Left err400
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              fieldName <> " is required"
        }
    Left err -> Left err

validateCourseTextListField :: Text -> Int -> [Text] -> Either ServerError (Maybe [Text])
validateCourseTextListField _ _ [] = Right Nothing
validateCourseTextListField fieldName maxLength rawValues =
  Just <$> traverse validateIndexed (zip [1 :: Int ..] rawValues)
  where
    validateIndexed (idx, rawValue) =
      validateRequiredCourseTextField
        (fieldName <> "[" <> T.pack (show idx) <> "]")
        maxLength
        rawValue

validateOptionalCourseTextField :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCourseTextField _ _ Nothing = Right Nothing
validateOptionalCourseTextField fieldName maxLength (Just rawValue) =
  case cleanOptional (Just rawValue) of
    Nothing -> Right Nothing
    Just value
      | T.length value > maxLength ->
          Left err400
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  fieldName <> " must be " <> T.pack (show maxLength) <> " characters or fewer"
            }
      | T.any isControl value ->
          Left err400
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  fieldName <> " must not contain control characters"
            }
      | otherwise -> Right (Just value)

validateOptionalCourseRegistrationTextField :: Text -> Int -> Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCourseRegistrationTextField = validateOptionalCourseTextField

validateCourseRegistrationUtm :: Maybe UTMTags -> Either ServerError (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
validateCourseRegistrationUtm Nothing = Right (Nothing, Nothing, Nothing, Nothing)
validateCourseRegistrationUtm (Just UTMTags{..}) = do
  sourceVal <- validateOptionalCourseRegistrationTextField "utm.source" 256 source
  mediumVal <- validateOptionalCourseRegistrationTextField "utm.medium" 256 medium
  campaignVal <- validateOptionalCourseRegistrationTextField "utm.campaign" 256 campaign
  contentVal <- validateOptionalCourseRegistrationTextField "utm.content" 256 content
  pure (sourceVal, mediumVal, campaignVal, contentVal)

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
  when (artistId <= 0) $ throwBadRequest "Invalid artist id"
  let artistKey = toSqlKey artistId :: PartyId
  when (artistKey == auPartyId user) $
    throwBadRequest "No puedes dejar de seguirte a ti mismo"
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
  (threadIdValid, mBeforeIdValid, mAfterIdValid) <-
    either throwError pure (validateChatMessageListLookup threadId mBeforeId mAfterId)
  let tid = toSqlKey threadIdValid :: ChatThreadId
  mThread <- runDB $ get tid
  thread <-
    case mThread of
      Nothing -> throwError err404
      Just t  -> pure t
  requireChatThreadParticipant user thread
  beforeCursorKey <- resolveChatMessageCursorInThread tid "beforeId" mBeforeIdValid
  afterCursorKey <- resolveChatMessageCursorInThread tid "afterId" mAfterIdValid
  let baseFilters = [ChatMessageThreadId ==. tid]
  messages <- runDB $
    case (beforeCursorKey, afterCursorKey) of
      (Just beforeKey, Nothing) -> do
        rows <- selectList (baseFilters ++ [ChatMessageId <. beforeKey]) [Desc ChatMessageId, LimitTo limit]
        pure (reverse rows)
      (Nothing, Just afterKey) -> do
        selectList (baseFilters ++ [ChatMessageId >. afterKey]) [Asc ChatMessageId, LimitTo limit]
      _ -> do
        rows <- selectList baseFilters [Desc ChatMessageId, LimitTo limit]
        pure (reverse rows)
  pure (map chatMessageToDTO messages)

resolveChatMessageCursorInThread
  :: ChatThreadId
  -> Text
  -> Maybe Int64
  -> AppM (Maybe ChatMessageId)
resolveChatMessageCursorInThread _ _ Nothing = pure Nothing
resolveChatMessageCursorInThread threadKey fieldName (Just rawCursorId) = do
  let cursorKey = toSqlKey rawCursorId :: ChatMessageId
  mCursor <- runDB $ get cursorKey
  case mCursor of
    Just ChatMessage{chatMessageThreadId}
      | chatMessageThreadId == threadKey -> pure (Just cursorKey)
    _ ->
      throwError err404
        { errBody = BL.fromStrict (TE.encodeUtf8 (fieldName <> " not found in this thread"))
        }

chatSendMessage :: AuthedUser -> Int64 -> ChatSendMessageRequest -> AppM ChatMessageDTO
chatSendMessage user threadId ChatSendMessageRequest{..} = do
  bodyClean <- either throwError pure (validateChatSendMessageBody csmBody)
  threadIdValid <- either throwError pure (validatePositiveIdField "threadId" threadId)
  let tid = toSqlKey threadIdValid :: ChatThreadId
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
  let followerKey = auPartyId user
  targetKey <- runDB (resolveSocialTargetPartyId targetId) >>= either throwError pure
  when (followerKey == targetKey) $
    throwBadRequest "No puedes agregarte como amigo"
  now <- liftIO getCurrentTime
  rows <- runDB $ do
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
    selectList
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
  let followerKey = auPartyId user
  targetKey <- runDB (resolveSocialTargetPartyId vcerPartyId) >>= either throwError pure
  when (followerKey == targetKey) $
    throwBadRequest "No puedes compartir tu vCard contigo mismo"
  now <- liftIO getCurrentTime
  (rowsAB, rowsBA) <- runDB $ do
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
    pure (rowsAB, rowsBA)
  pure (map partyFollowEntityToDTO (rowsAB ++ rowsBA))

resolveSocialTargetPartyId :: Int64 -> SqlPersistT IO (Either ServerError PartyId)
resolveSocialTargetPartyId rawPartyId =
  case validatePositiveIdField "partyId" rawPartyId of
    Left err -> pure (Left err)
    Right partyId -> do
      let targetKey = toSqlKey partyId :: PartyId
      mTarget <- get targetKey
      pure $
        case mTarget of
          Nothing -> Left err404 { errBody = "Party not found" }
          Just _ -> Right targetKey

socialListProfiles :: AuthedUser -> [Int64] -> AppM [SocialPartyProfileDTO]
socialListProfiles _ rawPartyIds = do
  when (any (<= 0) rawPartyIds) $ throwBadRequest "Invalid party id"
  let partyIds = nub rawPartyIds
  if null partyIds
    then pure []
    else do
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool $ loadSocialPartyProfilesDTO partyIds

socialGetProfile :: AuthedUser -> Int64 -> AppM SocialPartyProfileDTO
socialGetProfile _ partyId = do
  when (partyId <= 0) $ throwBadRequest "Invalid party id"
  Env pool _ <- ask
  mProfile <- liftIO $ flip runSqlPool pool $ loadSocialPartyProfileDTO (toSqlKey partyId)
  maybe (throwError err404) pure mProfile

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

loadSocialPartyProfilesDTO :: [Int64] -> SqlPersistT IO [SocialPartyProfileDTO]
loadSocialPartyProfilesDTO rawIds = do
  let uniqueIds = nub rawIds
      partyKeys = map toSqlKey uniqueIds :: [PartyId]
  parties <- if null partyKeys then pure [] else selectList [PartyId <-. partyKeys] []
  profiles <- if null partyKeys then pure [] else selectList [FanProfileFanPartyId <-. partyKeys] []
  let partyMap = Map.fromList [(entityKey ent, ent) | ent <- parties]
      profileMap = Map.fromList [(fanProfileFanPartyId profile, profile) | Entity _ profile <- profiles]
  pure $ mapMaybe
    (\rawId -> do
      let partyKey = toSqlKey rawId :: PartyId
      partyEnt <- Map.lookup partyKey partyMap
      pure (toSocialPartyProfileDTO (Map.lookup partyKey profileMap) partyEnt)
    )
    uniqueIds

loadSocialPartyProfileDTO :: PartyId -> SqlPersistT IO (Maybe SocialPartyProfileDTO)
loadSocialPartyProfileDTO partyId = do
  mParty <- getEntity partyId
  case mParty of
    Nothing -> pure Nothing
    Just partyEnt -> do
      mProfile <- getBy (UniqueFanProfile partyId)
      pure (Just (toSocialPartyProfileDTO (entityVal <$> mProfile) partyEnt))

toSocialPartyProfileDTO :: Maybe FanProfile -> Entity Party -> SocialPartyProfileDTO
toSocialPartyProfileDTO mProfile (Entity partyId party) =
  SocialPartyProfileDTO
    { sppPartyId = fromSqlKey partyId
    , sppDisplayName = fromMaybe (M.partyDisplayName party) (mProfile >>= fanProfileDisplayName)
    , sppAvatarUrl = mProfile >>= fanProfileAvatarUrl
    , sppBio = mProfile >>= fanProfileBio
    , sppCity = mProfile >>= fanProfileCity
    }

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
  role <- either throwError pure (validateRolePayload roleTxt)
  pid <- runDB (resolvePartyRoleAssignmentTarget pidI) >>= either throwError pure
  runDB $ void $ upsert
    (PartyRole pid role True)
    [ PartyRoleActive =. True ]
  pure NoContent

resolvePartyRoleAssignmentTarget :: Int64 -> SqlPersistT IO (Either ServerError PartyId)
resolvePartyRoleAssignmentTarget rawPartyId =
  case validatePositiveIdField "partyId" rawPartyId of
    Left err -> pure (Left err)
    Right partyId -> do
      let pid = toSqlKey partyId :: PartyId
      mParty <- get pid
      pure $
        case mParty of
          Nothing -> Left err404 { errBody = "Party not found" }
          Just _ -> Right pid

validateRolePayload :: Text -> Either ServerError RoleEnum
validateRolePayload raw =
  case roleFromText raw of
    Just role -> Right role
    Nothing ->
      Left err400
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              "role must be one of: "
                <> T.intercalate ", " (map roleToText ([minBound .. maxBound] :: [RoleEnum]))
        }

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

-- Service marketplace
serviceMarketplaceServer :: AuthedUser -> ServerT Api.ServiceMarketplaceAPI AppM
serviceMarketplaceServer user =
       listServiceAds
  :<|> createServiceAd user
  :<|> listServiceAdSlots
  :<|> createServiceAdSlot user
  :<|> createServiceMarketplaceBooking user
  :<|> completeServiceMarketplaceBooking user
  :<|> releaseServiceMarketplaceEscrow user

listServiceAds :: AppM [Api.ServiceAdDTO]
listServiceAds = do
  pool <- asks envPool
  liftIO $ flip runSqlPool pool $ do
    ads <- selectList [ServiceAdActive ==. True] [Desc ServiceAdCreatedAt]
    let providerIds = map (serviceAdProviderPartyId . entityVal) ads
    providers <- if null providerIds then pure [] else selectList [PartyId <-. providerIds] []
    let providerMap =
          Map.fromList
            [ ( entityKey p
              , normalizeOptionalInput (Just (M.partyDisplayName (entityVal p))) <|> M.partyLegalName (entityVal p)
              )
            | p <- providers
            ]
    pure $ map (toServiceAdDTO providerMap) ads

resolveServiceAdEntity :: Int64 -> SqlPersistT IO (Entity ServiceAd)
resolveServiceAdEntity rawAdId = do
  adId <- case validatePositiveIdField "adId" rawAdId of
    Left serverErr -> liftIO $ throwIO serverErr
    Right validAdId -> pure validAdId
  let adKey = toSqlKey adId :: Key ServiceAd
  mAd <- getEntity adKey
  maybe
    (liftIO $ throwIO err404 { errBody = "Service ad not found" })
    pure
    mAd

resolveServiceAdSlotEntity :: Int64 -> SqlPersistT IO (Entity ServiceAdSlot)
resolveServiceAdSlotEntity rawSlotId = do
  slotId <- case validatePositiveIdField "slotId" rawSlotId of
    Left serverErr -> liftIO $ throwIO serverErr
    Right validSlotId -> pure validSlotId
  let slotKey = toSqlKey slotId :: Key ServiceAdSlot
  mSlot <- getEntity slotKey
  maybe
    (liftIO $ throwIO err404 { errBody = "Service ad slot not found" })
    pure
    mSlot

resolveServiceMarketplaceBookingEntity :: Int64 -> SqlPersistT IO (Entity Booking)
resolveServiceMarketplaceBookingEntity rawBookingId = do
  bookingId <- case validatePositiveIdField "bookingId" rawBookingId of
    Left serverErr -> liftIO $ throwIO serverErr
    Right validBookingId -> pure validBookingId
  let bookingKey = toSqlKey bookingId :: Key Booking
  mBooking <- getEntity bookingKey
  maybe
    (liftIO $ throwIO err404 { errBody = "Service marketplace booking not found" })
    pure
    mBooking

createServiceAd :: AuthedUser -> Api.ServiceAdCreateReq -> AppM Api.ServiceAdDTO
createServiceAd user Api.ServiceAdCreateReq{..} = do
  when (T.null (T.strip sacRoleTag) || T.null (T.strip sacHeadline)) $
    throwError err400 { errBody = "roleTag and headline are required" }
  when (sacFeeCents <= 0) $ throwError err400 { errBody = "feeCents must be > 0" }
  currency <- either throwError pure (validateServiceAdCurrency sacCurrency)
  slotMinutes <- either throwError pure (validateServiceAdSlotMinutes sacSlotMinutes)
  now <- liftIO getCurrentTime
  pool <- asks envPool
  when (isNothing sacServiceCatalogId) $ throwError err400 { errBody = "serviceCatalogId is required" }
  let catalogKey = toSqlKey <$> sacServiceCatalogId
  liftIO $ flip runSqlPool pool $ do
    mCatalog <- maybe (pure Nothing) get catalogKey
    case validateServiceMarketplaceCatalog mCatalog of
      Left serverErr -> liftIO $ throwIO serverErr
      Right _ -> pure ()
  let record = ServiceAd
        { serviceAdProviderPartyId = auPartyId user
        , serviceAdServiceCatalogId = catalogKey
        , serviceAdRoleTag = T.strip sacRoleTag
        , serviceAdHeadline = T.strip sacHeadline
        , serviceAdDescription = normalizeOptionalInput sacDescription
        , serviceAdFeeCents = sacFeeCents
        , serviceAdCurrency = currency
        , serviceAdSlotMinutes = slotMinutes
        , serviceAdActive = True
        , serviceAdCreatedAt = now
        }
  dto <- liftIO $ flip runSqlPool pool $ do
    adId <- insert record
    ent <- getJustEntity adId
    mProvider <- get (auPartyId user)
    let providerName =
          mProvider >>= (\p -> normalizeOptionalInput (Just (M.partyDisplayName p)) <|> M.partyLegalName p)
    pure (toServiceAdDTO (Map.singleton (auPartyId user) providerName) ent)
  pure dto

listServiceAdSlots :: Int64 -> AppM [Api.ServiceAdSlotDTO]
listServiceAdSlots adId = do
  pool <- asks envPool
  liftIO $ flip runSqlPool pool $ do
    Entity adKey _ <- resolveServiceAdEntity adId
    slots <- selectList [ServiceAdSlotAdId ==. adKey] [Asc ServiceAdSlotStartsAt]
    pure (map toServiceAdSlotDTO slots)

createServiceAdSlot :: AuthedUser -> Int64 -> Api.ServiceAdSlotCreateReq -> AppM Api.ServiceAdSlotDTO
createServiceAdSlot user adId Api.ServiceAdSlotCreateReq{..} = do
  when (sascEndsAt <= sascStartsAt) $ throwError err400 { errBody = "Invalid slot range" }
  pool <- asks envPool
  liftIO $ flip runSqlPool pool $ do
    ad@(Entity adKey _) <- resolveServiceAdEntity adId
    when (serviceAdProviderPartyId (entityVal ad) /= auPartyId user) $
      liftIO $ throwIO err403
    now <- liftIO getCurrentTime
    slotId <- insert ServiceAdSlot
      { serviceAdSlotAdId = adKey
      , serviceAdSlotStartsAt = sascStartsAt
      , serviceAdSlotEndsAt = sascEndsAt
      , serviceAdSlotStatus = "open"
      , serviceAdSlotCreatedAt = now
      }
    slot <- getJustEntity slotId
    pure (toServiceAdSlotDTO slot)

createServiceMarketplaceBooking :: AuthedUser -> Api.ServiceMarketplaceBookingReq -> AppM Api.ServiceMarketplaceBookingDTO
createServiceMarketplaceBooking user Api.ServiceMarketplaceBookingReq{..} = do
  paymentMethodVal <- either throwError pure (parsePaymentMethodText smbPaymentMethod)
  (adId, slotId) <- either throwError pure (validateServiceMarketplaceBookingRefs smbAdId smbSlotId)
  pool <- asks envPool
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ do
    adEnt@(Entity adKey _) <- resolveServiceAdEntity adId
    slotEnt@(Entity slotKey _) <- resolveServiceAdSlotEntity slotId
    let ad = entityVal adEnt
        slot = entityVal slotEnt
        providerId = serviceAdProviderPartyId ad
    when (not (serviceAdActive ad)) $ liftIO $ throwIO err409 { errBody = "Service ad is inactive" }
    case validateServiceMarketplaceBookingSlot adKey slot of
      Left serverErr -> liftIO $ throwIO serverErr
      Right () -> pure ()
    when (providerId == auPartyId user) $ liftIO $ throwIO err400 { errBody = "Cannot book your own service ad" }
    let orderTitle = fromMaybe (serviceAdHeadline ad) (normalizeOptionalInput smbTitle)
    catalogId <- maybe (liftIO $ throwIO err409 { errBody = "Service ad is missing catalogId" }) pure (serviceAdServiceCatalogId ad)
    catalog <- get catalogId
    catalogKind <- case validateServiceMarketplaceCatalog catalog of
      Left serverErr -> liftIO $ throwIO serverErr
      Right kind -> pure kind
    serviceOrderId <- insert ServiceOrder
      { serviceOrderCustomerId = auPartyId user
      , serviceOrderArtistId = Just providerId
      , serviceOrderCatalogId = catalogId
      , serviceOrderServiceKind = catalogKind
      , serviceOrderTitle = Just orderTitle
      , serviceOrderDescription = normalizeOptionalInput smbNotes
      , serviceOrderStatus = "escrow_held"
      , serviceOrderPriceQuotedCents = Just (serviceAdFeeCents ad)
      , serviceOrderQuoteSentAt = Just now
      , serviceOrderScheduledStart = Just (serviceAdSlotStartsAt slot)
      , serviceOrderScheduledEnd = Just (serviceAdSlotEndsAt slot)
      , serviceOrderCreatedAt = now
      }
    bookingId <- insert Booking
      { bookingTitle = orderTitle
      , bookingServiceOrderId = Just serviceOrderId
      , bookingPartyId = Just (auPartyId user)
      , bookingServiceType = Just (serviceAdRoleTag ad)
      , bookingEngineerPartyId = Just providerId
      , bookingEngineerName = Nothing
      , bookingStartsAt = serviceAdSlotStartsAt slot
      , bookingEndsAt = serviceAdSlotEndsAt slot
      , bookingStatus = Confirmed
      , bookingCreatedBy = Just (auPartyId user)
      , bookingNotes = normalizeOptionalInput smbNotes
      , bookingCreatedAt = now
      }
    update slotKey [ServiceAdSlotStatus =. "booked"]
    paymentId <- insert Payment
      { paymentInvoiceId = Nothing
      , paymentOrderId = Just serviceOrderId
      , paymentPartyId = auPartyId user
      , paymentMethod = paymentMethodVal
      , paymentAmountCents = serviceAdFeeCents ad
      , paymentReceivedAt = now
      , paymentReference = Nothing
      , paymentConcept = Just "escrow_hold"
      , paymentPeriod = Nothing
      , paymentAttachment = Nothing
      , paymentCreatedBy = Just (auPartyId user)
      , paymentCreatedAt = Just now
      }
    escrowId <- insert ServiceEscrow
      { serviceEscrowBookingId = bookingId
      , serviceEscrowServiceOrderId = serviceOrderId
      , serviceEscrowAdId = adKey
      , serviceEscrowPatronPartyId = auPartyId user
      , serviceEscrowProviderPartyId = providerId
      , serviceEscrowAmountCents = serviceAdFeeCents ad
      , serviceEscrowCurrency = serviceAdCurrency ad
      , serviceEscrowStatus = "held"
      , serviceEscrowHeldPaymentId = Just paymentId
      , serviceEscrowReleasedPaymentId = Nothing
      , serviceEscrowHeldAt = now
      , serviceEscrowReleasedAt = Nothing
      }
    pure $ Api.ServiceMarketplaceBookingDTO
      { Api.smbBookingId = fromSqlKey bookingId
      , Api.smbServiceOrderId = fromSqlKey serviceOrderId
      , Api.smbEscrowId = fromSqlKey escrowId
      , Api.smbEscrowStatus = "held"
      , Api.smbEscrowAmountCents = serviceAdFeeCents ad
      , Api.smbEscrowCurrency = serviceAdCurrency ad
      }

completeServiceMarketplaceBooking :: AuthedUser -> Int64 -> AppM Api.ServiceMarketplaceBookingDTO
completeServiceMarketplaceBooking user rawBookingId = do
  pool <- asks envPool
  liftIO $ flip runSqlPool pool $ do
    Entity bookingKey _ <- resolveServiceMarketplaceBookingEntity rawBookingId
    escrowEnt <- getBy (UniqueServiceEscrowBooking bookingKey)
    escrow <- maybe (liftIO $ throwIO err404) pure escrowEnt
    let canComplete = serviceEscrowProviderPartyId (entityVal escrow) == auPartyId user || hasRole Admin user
    when (not canComplete) $ liftIO $ throwIO err403
    update bookingKey [BookingStatus =. Completed]
    update (serviceEscrowServiceOrderId (entityVal escrow)) [ServiceOrderStatus =. "performed"]
    pure (mkEscrowBookingDTO escrow)

releaseServiceMarketplaceEscrow :: AuthedUser -> Int64 -> AppM Api.ServiceMarketplaceBookingDTO
releaseServiceMarketplaceEscrow user rawBookingId = do
  pool <- asks envPool
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ do
    booking@(Entity bookingKey _) <- resolveServiceMarketplaceBookingEntity rawBookingId
    Entity escrowKey escrow <- maybe (liftIO $ throwIO err404) pure =<< getBy (UniqueServiceEscrowBooking bookingKey)
    let canRelease = serviceEscrowPatronPartyId escrow == auPartyId user || hasRole Admin user
    when (not canRelease) $ liftIO $ throwIO err403
    when (bookingStatus (entityVal booking) /= Completed) $
      liftIO $ throwIO err409 { errBody = "Escrow can only be released after booking completion" }
    when (not (escrowTransitionAllowed (serviceEscrowStatus escrow) "released")) $
      liftIO $ throwIO err409 { errBody = "Escrow state transition not allowed" }
    releasePaymentId <- insert Payment
      { paymentInvoiceId = Nothing
      , paymentOrderId = Just (serviceEscrowServiceOrderId escrow)
      , paymentPartyId = serviceEscrowProviderPartyId escrow
      , paymentMethod = BankTransferM
      , paymentAmountCents = serviceEscrowAmountCents escrow
      , paymentReceivedAt = now
      , paymentReference = Nothing
      , paymentConcept = Just "escrow_release"
      , paymentPeriod = Nothing
      , paymentAttachment = Nothing
      , paymentCreatedBy = Just (auPartyId user)
      , paymentCreatedAt = Just now
      }
    update escrowKey
      [ ServiceEscrowStatus =. "released"
      , ServiceEscrowReleasedPaymentId =. Just releasePaymentId
      , ServiceEscrowReleasedAt =. Just now
      ]
    update (serviceEscrowServiceOrderId escrow) [ServiceOrderStatus =. "paid_out"]
    refreshed <- getJustEntity escrowKey
    pure (mkEscrowBookingDTO refreshed)

escrowTransitionAllowed :: Text -> Text -> Bool
escrowTransitionAllowed "held" "released" = True
escrowTransitionAllowed "held" "refunded" = True
escrowTransitionAllowed fromState toState = fromState == toState

toServiceAdDTO :: Map.Map (Key Party) (Maybe Text) -> Entity ServiceAd -> Api.ServiceAdDTO
toServiceAdDTO providerMap (Entity adId ad) = Api.ServiceAdDTO
  { Api.sadId = fromSqlKey adId
  , Api.sadProviderPartyId = fromSqlKey (serviceAdProviderPartyId ad)
  , Api.sadProviderName = join (Map.lookup (serviceAdProviderPartyId ad) providerMap)
  , Api.sadServiceCatalogId = fromSqlKey <$> serviceAdServiceCatalogId ad
  , Api.sadRoleTag = serviceAdRoleTag ad
  , Api.sadHeadline = serviceAdHeadline ad
  , Api.sadDescription = serviceAdDescription ad
  , Api.sadFeeCents = serviceAdFeeCents ad
  , Api.sadCurrency = serviceAdCurrency ad
  , Api.sadSlotMinutes = serviceAdSlotMinutes ad
  , Api.sadActive = serviceAdActive ad
  , Api.sadCreatedAt = serviceAdCreatedAt ad
  }

toServiceAdSlotDTO :: Entity ServiceAdSlot -> Api.ServiceAdSlotDTO
toServiceAdSlotDTO (Entity slotId slot) = Api.ServiceAdSlotDTO
  { Api.sasId = fromSqlKey slotId
  , Api.sasAdId = fromSqlKey (serviceAdSlotAdId slot)
  , Api.sasStartsAt = serviceAdSlotStartsAt slot
  , Api.sasEndsAt = serviceAdSlotEndsAt slot
  , Api.sasStatus = serviceAdSlotStatus slot
  }

mkEscrowBookingDTO :: Entity ServiceEscrow -> Api.ServiceMarketplaceBookingDTO
mkEscrowBookingDTO (Entity escrowId escrow) = Api.ServiceMarketplaceBookingDTO
  { Api.smbBookingId = fromSqlKey (serviceEscrowBookingId escrow)
  , Api.smbServiceOrderId = fromSqlKey (serviceEscrowServiceOrderId escrow)
  , Api.smbEscrowId = fromSqlKey escrowId
  , Api.smbEscrowStatus = serviceEscrowStatus escrow
  , Api.smbEscrowAmountCents = serviceEscrowAmountCents escrow
  , Api.smbEscrowCurrency = serviceEscrowCurrency escrow
  }

parsePaymentMethodText :: Maybe Text -> Either ServerError PaymentMethod
parsePaymentMethodText mTxt =
  case T.toLower . T.strip <$> mTxt of
    Nothing -> Right OtherM
    Just "" -> Right OtherM
    Just "cash" -> Right CashM
    Just "bank_transfer" -> Right BankTransferM
    Just "bank" -> Right BankTransferM
    Just "card" -> Right CardPOSM
    Just "paypal" -> Right PayPalM
    Just "crypto" -> Right CryptoM
    Just "stripe" -> Right StripeM
    Just "wompi" -> Right WompiM
    Just "payphone" -> Right PayPhoneM
    Just "other" -> Right OtherM
    _ ->
      Left err400
        { errBody =
            "paymentMethod must be one of: cash, bank_transfer, bank, card, paypal, crypto, stripe, wompi, payphone, other"
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
  (bookingIdFilter, partyIdFilter, engineerPartyIdFilter) <-
    either throwError pure $
      validateBookingListFilters mBookingId mPartyId mEngineerPartyId
  Env pool _ <- ask
  liftIO $ do
    dbBookings <- flip runSqlPool pool $ do
      case bookingIdFilter of
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
          case (partyIdFilter, engineerPartyIdFilter) of
            (Nothing, Nothing) -> do
              bookings <- selectList [] [Desc BookingId]
              buildBookingDTOs bookings
            _ -> do
              byParty <- maybe (pure []) loadByParty partyIdFilter
              byEngineer <- maybe (pure []) loadByEngineer engineerPartyIdFilter
              let merged = dedupeByKey (byParty ++ byEngineer)
              buildBookingDTOs merged
    if isJust bookingIdFilter || isJust partyIdFilter || isJust engineerPartyIdFilter
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
  fullNameClean <- either throwError pure (validatePublicBookingFullName pbFullName)
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  (emailClean, phoneClean) <-
    either throwError pure $
      validatePublicBookingContactDetails pbEmail pbPhone
  durationMins <-
    either throwError pure $
      validatePublicBookingDurationMinutes pbDurationMinutes
  startsAtClean <-
    either throwError pure $
      validatePublicBookingStartAt now pbStartsAt
  serviceTypeValue <-
    either throwError pure $
      validatePublicBookingServiceType pbServiceType
  let serviceTypeClean = Just serviceTypeValue
  engineerIdClean <-
    either throwError pure $
      validateOptionalPositiveIdField "engineerPartyId" pbEngineerPartyId
  let engineerNameClean = normalizeOptionalInput pbEngineerName
  case validateEngineer serviceTypeClean engineerIdClean engineerNameClean of
    Left msg -> throwBadRequest msg
    Right () -> pure ()
  mEngineerParty <-
    liftIO (flip runSqlPool pool (resolveOptionalBookingEngineerReference engineerIdClean))
      >>= either throwError pure
  let endsAt       = addUTCTime (fromIntegral durationMins * 60) startsAtClean
  notesClean <-
    either throwError pure $
      validatePublicBookingNotes pbNotes
  (partyId, _) <- ensurePartyWithAccount (Just fullNameClean) emailClean phoneClean
  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean (fromMaybe [] pbResourceIds) startsAtClean endsAt
  let resolvedEngineerName =
        engineerNameClean
          <|> (M.partyDisplayName . entityVal <$> mEngineerParty)
  let bookingRecord = Booking
        { bookingTitle          = buildTitle serviceTypeClean fullNameClean
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = Just partyId
        , bookingServiceType    = serviceTypeClean
        , bookingEngineerPartyId = entityKey <$> mEngineerParty
        , bookingEngineerName    = resolvedEngineerName
        , bookingStartsAt       = startsAtClean
        , bookingEndsAt         = endsAt
        , bookingStatus         = Tentative
        , bookingCreatedBy      = Nothing
        , bookingNotes          = notesClean
        , bookingCreatedAt      = now
        }
  dtoResult <- liftIO $ flip runSqlPool pool $ do
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
    pure (requirePersistedBookingDTO dtos)
  dto <- either throwError pure dtoResult
  notifyEngineerIfNeeded dto
  pure dto
  where
    buildTitle (Just svc) nameTxt = svc <> " · " <> nameTxt
    buildTitle Nothing nameTxt    = "Reserva · " <> nameTxt

createBooking :: AuthedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  now <- liftIO getCurrentTime

  status' <- either throwBadRequest pure (parseBookingStatus (cbStatus req))
  either throwError pure $
    validateBookingTimeRange (cbStartsAt req) (cbEndsAt req)
  engineerIdClean <-
    either throwError pure $
      validateOptionalPositiveIdField "engineerPartyId" (cbEngineerPartyId req)
  partyIdClean <-
    either throwError pure $
      validateOptionalPositiveIdField "partyId" (cbPartyId req)
  mParty <-
    liftIO (flip runSqlPool pool (resolveOptionalBookingPartyReference "partyId" partyIdClean))
      >>= either throwError pure
  mEngineerParty <-
    liftIO (flip runSqlPool pool (resolveOptionalBookingEngineerReference engineerIdClean))
      >>= either throwError pure
  let serviceTypeClean = normalizeOptionalInput (cbServiceType req)
      engineerNameClean = normalizeOptionalInput (cbEngineerName req)
      partyKey         = entityKey <$> mParty
      requestedRooms   = fromMaybe [] (cbResourceIds req)
  case validateEngineer serviceTypeClean engineerIdClean engineerNameClean of
    Left msg -> throwBadRequest msg
    Right () -> pure ()

  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean requestedRooms (cbStartsAt req) (cbEndsAt req)
  let resolvedEngineerName =
        engineerNameClean
          <|> (M.partyDisplayName . entityVal <$> mEngineerParty)

  let bookingRecord = Booking
        { bookingTitle          = cbTitle req
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = partyKey
        , bookingServiceType    = serviceTypeClean
        , bookingEngineerPartyId = entityKey <$> mEngineerParty
        , bookingEngineerName    = resolvedEngineerName
        , bookingStartsAt       = cbStartsAt req
        , bookingEndsAt         = cbEndsAt req
        , bookingStatus         = status'
        , bookingCreatedBy      = Nothing
        , bookingNotes          = normalizeOptionalInput (cbNotes req)
        , bookingCreatedAt      = now
        }

  dtoResult <- liftIO $ flip runSqlPool pool $ do
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
    pure (requirePersistedBookingDTO dtos)
  dto <- either throwError pure dtoResult
  notifyEngineerIfNeeded dto
  pure dto

updateBooking :: AuthedUser -> Int64 -> UpdateBookingReq -> AppM BookingDTO
updateBooking user bookingIdI req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  requestedEngineerId <-
    either throwError pure $
      validateOptionalPositiveIdField "engineerPartyId" (ubEngineerPartyId req)
  let bookingId = toSqlKey bookingIdI :: Key Booking
  result <- liftIO $ flip runSqlPool pool $ do
    mBooking <- getEntity bookingId
    case mBooking of
      Nothing -> pure (Left err404)
      Just (Entity _ current) -> do
        requestedEngineerRef <- resolveOptionalBookingEngineerReference requestedEngineerId
        case requestedEngineerRef of
          Left refErr -> pure (Left refErr)
          Right requestedEngineerParty ->
            case traverse parseBookingStatus (ubStatus req) of
              Left msg -> pure (Left err400 { errBody = BL8.fromStrict (TE.encodeUtf8 msg) })
              Right requestedStatus -> do
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
                      , bookingStatus      = fromMaybe (bookingStatus current) requestedStatus
                      , bookingStartsAt    = fromMaybe (bookingStartsAt current) (ubStartsAt req)
                      , bookingEndsAt      = fromMaybe (bookingEndsAt current) (ubEndsAt req)
                      , bookingEngineerPartyId =
                          maybe
                            (bookingEngineerPartyId current)
                            (Just . entityKey)
                            requestedEngineerParty
                      , bookingEngineerName    = maybe (bookingEngineerName current) (normalizeOptionalInput . Just) (ubEngineerName req)
                      }
                case validateBookingTimeRange (bookingStartsAt updated) (bookingEndsAt updated) of
                  Left bookingErr -> pure (Left bookingErr)
                  Right () ->
                    case validateEngineer (bookingServiceType updated) (fmap fromSqlKey (bookingEngineerPartyId updated)) (bookingEngineerName updated) of
                      Left msg -> pure (Left err400 { errBody = BL8.fromStrict (TE.encodeUtf8 msg) })
                      Right () -> do
                        replace bookingId updated
                        dtos <- buildBookingDTOs [Entity bookingId updated]
                        pure (maybe (Left err500) Right (listToMaybe dtos))
  either throwError pure result

resolveOptionalBookingPartyReference
  :: Text
  -> Maybe Int64
  -> SqlPersistT IO (Either ServerError (Maybe (Entity Party)))
resolveOptionalBookingPartyReference _ Nothing = pure (Right Nothing)
resolveOptionalBookingPartyReference fieldName (Just rawId) = do
  let partyKey = toSqlKey (fromIntegral rawId) :: Key Party
  mParty <- getEntity partyKey
  pure $
    case mParty of
      Nothing ->
        Left err422
          { errBody =
              BL.fromStrict (TE.encodeUtf8 (fieldName <> " references an unknown party"))
          }
      Just partyEnt -> Right (Just partyEnt)


resolveOptionalBookingEngineerReference
  :: Maybe Int64
  -> SqlPersistT IO (Either ServerError (Maybe (Entity Party)))
resolveOptionalBookingEngineerReference Nothing = pure (Right Nothing)
resolveOptionalBookingEngineerReference engineerId = do
  resolved <- resolveOptionalBookingPartyReference "engineerPartyId" engineerId
  case resolved of
    Left serverErr -> pure (Left serverErr)
    Right Nothing -> pure (Right Nothing)
    Right (Just partyEnt) -> do
      activeEngineer <- selectFirst
        [ PartyRolePartyId ==. entityKey partyEnt
        , PartyRoleRole ==. Engineer
        , PartyRoleActive ==. True
        ]
        []
      pure $ case activeEngineer of
        Nothing ->
          Left err422
            { errBody = "engineerPartyId must reference an active engineer" }
        Just _ ->
          Right (Just partyEnt)


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

requirePersistedBookingDTO :: [BookingDTO] -> Either ServerError BookingDTO
requirePersistedBookingDTO (dto:_) = Right dto
requirePersistedBookingDTO [] =
  Left err500
    { errBody = "Booking DTO projection returned no rows after persistence"
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

validatePositiveIdField :: Text -> Int64 -> Either ServerError Int64
validatePositiveIdField fieldName rawId
  | rawId <= 0 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be a positive integer"))
        }
  | otherwise = Right rawId

validateOptionalPositiveIdField :: Text -> Maybe Int64 -> Either ServerError (Maybe Int64)
validateOptionalPositiveIdField _ Nothing = Right Nothing
validateOptionalPositiveIdField fieldName (Just rawId)
  | otherwise = Just <$> validatePositiveIdField fieldName rawId

validateCourseRegistrationId :: Int64 -> Either ServerError Int64
validateCourseRegistrationId = validatePositiveIdField "registrationId"

validateCourseRegistrationReceiptId :: Int64 -> Either ServerError Int64
validateCourseRegistrationReceiptId = validatePositiveIdField "receiptId"

validateCourseRegistrationFollowUpId :: Int64 -> Either ServerError Int64
validateCourseRegistrationFollowUpId = validatePositiveIdField "followUpId"

validateChatMessageListLookup
  :: Int64
  -> Maybe Int64
  -> Maybe Int64
  -> Either ServerError (Int64, Maybe Int64, Maybe Int64)
validateChatMessageListLookup threadId mBeforeId mAfterId = do
  threadIdValid <- validatePositiveIdField "threadId" threadId
  beforeIdValid <- validateOptionalPositiveIdField "beforeId" mBeforeId
  afterIdValid <- validateOptionalPositiveIdField "afterId" mAfterId
  when (isJust beforeIdValid && isJust afterIdValid) $
    Left err400 { errBody = "Use either beforeId or afterId" }
  pure (threadIdValid, beforeIdValid, afterIdValid)

validateChatSendMessageBody :: Text -> Either ServerError Text
validateChatSendMessageBody rawBody
  | T.null body =
      Left err400 { errBody = "Mensaje vacío" }
  | T.length body > 5000 =
      Left err400 { errBody = "Mensaje demasiado largo (max 5000 caracteres)" }
  | T.any isUnsafeChatMessageControl body =
      Left err400 { errBody = "message must not contain control characters" }
  | otherwise =
      Right body
  where
    body = T.strip rawBody
    isUnsafeChatMessageControl ch =
      isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

validateBookingListFilters :: Maybe Int64 -> Maybe Int64 -> Maybe Int64 -> Either ServerError (Maybe Int64, Maybe Int64, Maybe Int64)
validateBookingListFilters mBookingId mPartyId mEngineerPartyId = do
  bookingIdFilter <- validateOptionalPositiveIdField "bookingId" mBookingId
  partyIdFilter <- validateOptionalPositiveIdField "partyId" mPartyId
  engineerPartyIdFilter <- validateOptionalPositiveIdField "engineerPartyId" mEngineerPartyId
  when
    ( isJust bookingIdFilter
        && (isJust partyIdFilter || isJust engineerPartyIdFilter)
    )
    (Left err400 { errBody = "bookingId cannot be combined with partyId or engineerPartyId" })
  pure (bookingIdFilter, partyIdFilter, engineerPartyIdFilter)

validateServiceMarketplaceBookingRefs :: Int64 -> Int64 -> Either ServerError (Int64, Int64)
validateServiceMarketplaceBookingRefs rawAdId rawSlotId = do
  adId <- validatePositiveIdField "adId" rawAdId
  slotId <- validatePositiveIdField "slotId" rawSlotId
  pure (adId, slotId)

validateServiceMarketplaceBookingSlot
  :: Key ServiceAd
  -> ServiceAdSlot
  -> Either ServerError ()
validateServiceMarketplaceBookingSlot adKey slot
  | serviceAdSlotAdId slot /= adKey =
      Left err400 { errBody = "slotId does not belong to adId" }
  | T.toCaseFold (T.strip (serviceAdSlotStatus slot)) /= "open" =
      Left err409 { errBody = "Slot is not available" }
  | otherwise =
      Right ()

normalizeOptionalCmsFilter :: Maybe Text -> Maybe Text
normalizeOptionalCmsFilter = normalizeOptionalInput

validateCmsLocaleFilter :: Maybe Text -> Either ServerError Text
validateCmsLocaleFilter rawLocale =
  case normalizeOptionalCmsFilter rawLocale of
    Nothing -> Right "es"
    Just locale
      | isValidCmsLocale locale -> Right (canonicalizeCmsLocale locale)
      | otherwise ->
          Left err400
            { errBody =
                "locale must be omitted or a BCP-47 language tag such as es, en, or es-EC"
            }
  where
    isValidCmsLocale locale =
      T.length locale <= 35
        && case T.splitOn "-" locale of
          [] -> False
          primary : subtags ->
            isValidPrimarySubtag primary && all isValidLocaleSubtag subtags

    isValidPrimarySubtag subtag =
      let len = T.length subtag
      in len >= 2 && len <= 8 && T.all isAsciiLetter subtag

    isValidLocaleSubtag subtag =
      let len = T.length subtag
      in len >= 2 && len <= 8 && T.all isAsciiAlphaNum subtag

    isAsciiLetter ch = isAsciiLower ch || isAsciiUpper ch
    isAsciiAlphaNum ch = isAsciiLetter ch || isDigit ch

canonicalizeCmsLocale :: Text -> Text
canonicalizeCmsLocale rawLocale =
  case T.splitOn "-" rawLocale of
    [] -> rawLocale
    primary : subtags ->
      T.intercalate "-" (T.toLower primary : map canonicalizeSubtag subtags)
  where
    canonicalizeSubtag subtag
      | T.length subtag == 2 && T.all isAsciiLetter subtag = T.toUpper subtag
      | otherwise = T.toLower subtag

    isAsciiLetter ch = isAsciiLower ch || isAsciiUpper ch

validateOptionalCmsLocaleFilter :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCmsLocaleFilter Nothing = Right Nothing
validateOptionalCmsLocaleFilter (Just rawLocale)
  | T.null (T.strip rawLocale) =
      Left err400
        { errBody =
            "locale must be omitted or a BCP-47 language tag such as es, en, or es-EC"
        }
  | otherwise = Just <$> validateCmsLocaleFilter (Just rawLocale)

validateRequiredCmsLocale :: Text -> Either ServerError Text
validateRequiredCmsLocale rawLocale =
  case normalizeOptionalCmsFilter (Just rawLocale) of
    Nothing ->
      Left err400 { errBody = "locale requerido" }
    Just _ ->
      validateCmsLocaleFilter (Just rawLocale)

validateRequiredCmsSlug :: Text -> Either ServerError Text
validateRequiredCmsSlug rawSlug =
  let slugVal = normalizeSlug rawSlug
      isSlugAtom ch = isAsciiLower ch || isDigit ch
      isSlugChar ch = isSlugAtom ch || ch == '-'
  in if T.null slugVal
    then Left err400 { errBody = "slug requerido" }
    else
      if T.length slugVal <= cmsSlugMaxLength
          && T.all isSlugChar slugVal
          && T.any isSlugAtom slugVal
        then Right slugVal
        else Left invalidCmsSlug
  where
    cmsSlugMaxLength = 96
    invalidCmsSlug =
      err400
        { errBody =
            "slug must contain only ASCII letters, numbers, and hyphens, "
              <> "include at least one letter or number, and be 96 characters or fewer"
        }

validateOptionalCmsSlugFilter :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCmsSlugFilter Nothing = Right Nothing
validateOptionalCmsSlugFilter (Just rawSlug)
  | T.null (T.strip rawSlug) =
      Left err400
        { errBody =
            "slug must be omitted or use only ASCII letters, numbers, and hyphens"
        }
  | otherwise = Just <$> validateRequiredCmsSlug rawSlug

validateOptionalCmsSlugPrefix :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCmsSlugPrefix Nothing = Right Nothing
validateOptionalCmsSlugPrefix (Just rawPrefix)
  | T.null (T.strip rawPrefix) =
      Left invalidCmsSlugPrefix
  | otherwise =
      case validateRequiredCmsSlug rawPrefix of
        Right prefix -> Right (Just prefix)
        Left _ -> Left invalidCmsSlugPrefix
  where
    invalidCmsSlugPrefix =
      err400
        { errBody =
            "slugPrefix must be omitted or use only ASCII letters, numbers, and hyphens"
        }

validateRequiredCmsField :: Text -> Text -> Either ServerError Text
validateRequiredCmsField fieldName rawValue =
  case normalizeOptionalCmsFilter (Just rawValue) of
    Just value -> Right value
    Nothing ->
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " requerido"))
        }

validateCmsContentStatus :: Maybe Text -> Either ServerError Text
validateCmsContentStatus Nothing = Right "draft"
validateCmsContentStatus (Just rawStatus) =
  case T.toCaseFold <$> normalizeOptionalInput (Just rawStatus) of
    Just "draft" -> Right "draft"
    Just "published" -> Right "published"
    Just "archived" -> Right "archived"
    _ -> Left err400 { errBody = "status must be one of: draft, published, archived" }

validateCourseNonNegativeField :: Text -> Int -> Either ServerError Int
validateCourseNonNegativeField fieldName value
  | value < 0 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be greater than or equal to 0"))
        }
  | otherwise = Right value

validateOptionalCourseNonNegativeField :: Text -> Maybe Int -> Either ServerError (Maybe Int)
validateOptionalCourseNonNegativeField fieldName =
  traverse (validateCourseNonNegativeField fieldName)

validateCoursePositiveField :: Text -> Int -> Either ServerError Int
validateCoursePositiveField fieldName value
  | value <= 0 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 (fieldName <> " must be greater than 0"))
        }
  | otherwise = Right value

validateOptionalCoursePositiveField :: Text -> Maybe Int -> Either ServerError (Maybe Int)
validateOptionalCoursePositiveField fieldName =
  traverse (validateCoursePositiveField fieldName)

validateOptionalCourseSessionDurationHours :: Maybe Int -> Either ServerError (Maybe Int)
validateOptionalCourseSessionDurationHours =
  traverse validateCourseSessionDurationHours

validateCourseSessionDurationHours :: Int -> Either ServerError Int
validateCourseSessionDurationHours value
  | value <= 0 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "sessionDurationHours must be greater than 0")
        }
  | value > 24 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "sessionDurationHours must be 24 or fewer")
        }
  | otherwise = Right value

validateCourseCurrency :: Text -> Either ServerError Text
validateCourseCurrency rawCurrency =
  validateCurrencyCode (normalizeOptionalInput (Just rawCurrency))

validateOptionalCourseSessionStartHour :: Maybe Int -> Either ServerError (Maybe Int)
validateOptionalCourseSessionStartHour =
  traverse validateCourseSessionStartHour

validateCourseSessionStartHour :: Int -> Either ServerError Int
validateCourseSessionStartHour value
  | value < 0 || value > 23 =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "sessionStartHour must be between 0 and 23")
        }
  | otherwise = Right value

validateCourseSessionScheduleWindow :: Maybe Int -> Maybe Int -> Either ServerError ()
validateCourseSessionScheduleWindow (Just startHour) (Just durationHours)
  | startHour + durationHours > 24 =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "sessionStartHour plus sessionDurationHours must not exceed 24")
        }
  | otherwise = Right ()
validateCourseSessionScheduleWindow _ _ = Right ()

validateCourseSessionInputs :: [CourseSessionIn] -> Either ServerError [CourseSessionIn]
validateCourseSessionInputs rawSessions = do
  sessionsClean <- traverse (uncurry validateCourseSessionInput) (zip [1 :: Int ..] rawSessions)
  validateDistinctResolvedCourseOrderValues
    "sessions"
    [ (idx, fromMaybe idx orderVal)
    | (idx, CourseSessionIn _ _ orderVal) <- zip [1 :: Int ..] sessionsClean
    ]
  pure sessionsClean

validateCourseSessionInput :: Int -> CourseSessionIn -> Either ServerError CourseSessionIn
validateCourseSessionInput idx (CourseSessionIn rawLabel dayVal orderVal) = do
  labelClean <-
    validateRequiredCourseTextField
      ("sessions[" <> T.pack (show idx) <> "].label")
      160
      rawLabel
  orderClean <-
    validateOptionalCoursePositiveField
      ("sessions[" <> T.pack (show idx) <> "].order")
      orderVal
  Right (CourseSessionIn labelClean dayVal orderClean)

validateCourseSyllabusInputs :: [CourseSyllabusIn] -> Either ServerError [CourseSyllabusIn]
validateCourseSyllabusInputs rawItems = do
  syllabusClean <- traverse (uncurry validateCourseSyllabusInput) (zip [1 :: Int ..] rawItems)
  validateDistinctResolvedCourseOrderValues
    "syllabus"
    [ (idx, fromMaybe idx orderVal)
    | (idx, CourseSyllabusIn _ _ orderVal) <- zip [1 :: Int ..] syllabusClean
    ]
  pure syllabusClean

validateCourseSyllabusInput :: Int -> CourseSyllabusIn -> Either ServerError CourseSyllabusIn
validateCourseSyllabusInput idx (CourseSyllabusIn rawTitle rawTopics orderVal) = do
  titleClean <-
    validateRequiredCourseTextField
      ("syllabus[" <> T.pack (show idx) <> "].title")
      160
      rawTitle
  orderClean <-
    validateOptionalCoursePositiveField
      ("syllabus[" <> T.pack (show idx) <> "].order")
      orderVal
  topicsClean <-
    fmap catMaybes $
      traverse
        (uncurry validateTopic)
        (zip [1 :: Int ..] rawTopics)
  if null topicsClean
    then
      Left err400
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              "syllabus[" <> T.pack (show idx) <> "].topics must include at least one non-blank topic"
        }
    else
      Right (CourseSyllabusIn titleClean topicsClean orderClean)
  where
    validateTopic topicIdx rawTopic =
      validateOptionalCourseTextField
        ( "syllabus["
            <> T.pack (show idx)
            <> "].topics["
            <> T.pack (show topicIdx)
            <> "]"
        )
        160
        (Just rawTopic)

validateDistinctResolvedCourseOrderValues :: Text -> [(Int, Int)] -> Either ServerError ()
validateDistinctResolvedCourseOrderValues fieldName =
  go Set.empty
  where
    go _ [] = Right ()
    go seen ((idx, resolvedOrder) : rest)
      | Set.member resolvedOrder seen =
          Left err400
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  fieldName
                    <> "["
                    <> T.pack (show idx)
                    <> "].order must be unique after applying defaults; duplicate order "
                    <> T.pack (show resolvedOrder)
            }
      | otherwise =
          go (Set.insert resolvedOrder seen) rest

parseBookingStatus :: Text -> Either Text BookingStatus
parseBookingStatus raw =
  maybe
    (Left ("Estado inválido. Usa uno de: " <> bookingStatusOptions))
    Right
    (Map.lookup (normalizeBookingStatusToken raw) bookingStatusAliases)
  where
    bookingStatusAliases =
      Map.fromList
        [ (normalizeBookingStatusToken (T.pack (show statusVal)), statusVal)
        | statusVal <- ([minBound .. maxBound] :: [BookingStatus])
        ]
    bookingStatusOptions =
      T.intercalate ", "
        [ T.pack (show statusVal)
        | statusVal <- ([minBound .. maxBound] :: [BookingStatus])
        ]

normalizeBookingStatusToken :: Text -> Text
normalizeBookingStatusToken =
  T.toLower . T.filter isAlphaNum . T.strip

parseCourseRegistrationStatus :: Text -> Either ServerError Text
parseCourseRegistrationStatus raw =
  maybe invalidStatus pure (normalizeCourseRegistrationStatus raw)
  where
    invalidStatus =
      Left err400
        { errBody =
            "Invalid course registration status. Allowed values: pending_payment, paid, cancelled"
        }

validateOptionalCourseRegistrationStatusFilter :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalCourseRegistrationStatusFilter Nothing = Right Nothing
validateOptionalCourseRegistrationStatusFilter (Just rawStatus) =
  let trimmed = T.strip rawStatus
  in if T.null trimmed
       then
         Left err400
           { errBody = "status must be omitted or one of: pending_payment, paid, cancelled"
           }
       else Just <$> parseCourseRegistrationStatus trimmed

validateCourseRegistrationListLimit :: Int -> Maybe Int -> Either ServerError Int
validateCourseRegistrationListLimit defaultLimit Nothing = Right defaultLimit
validateCourseRegistrationListLimit _ (Just rawLimit)
  | rawLimit < 1 || rawLimit > 500 =
      Left err400 { errBody = "limit must be between 1 and 500" }
  | otherwise =
      Right rawLimit

validateCourseRegistrationEmailEventListLimit :: Maybe Int -> Either ServerError Int
validateCourseRegistrationEmailEventListLimit =
  validateCourseRegistrationListLimit 100

validateMarketplaceOrderListLimit :: Maybe Int -> Either ServerError Int
validateMarketplaceOrderListLimit Nothing = Right 50
validateMarketplaceOrderListLimit (Just rawLimit)
  | rawLimit < 1 || rawLimit > 200 =
      Left err400 { errBody = "limit must be between 1 and 200" }
  | otherwise =
      Right rawLimit

validateMarketplaceOrderListOffset :: Maybe Int -> Either ServerError Int
validateMarketplaceOrderListOffset Nothing = Right 0
validateMarketplaceOrderListOffset (Just rawOffset)
  | rawOffset < 0 =
      Left err400 { errBody = "offset must be greater than or equal to 0" }
  | otherwise =
      Right rawOffset

validateOptionalMarketplaceOrderStatus :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalMarketplaceOrderStatus Nothing = Right Nothing
validateOptionalMarketplaceOrderStatus (Just rawStatus) =
  case normalizeOptionalInput (Just rawStatus) of
    Nothing -> Left err400 { errBody = marketplaceOptionalOrderStatusErrorBody }
    Just statusVal ->
      case normalizeMarketplaceOrderStatus statusVal of
        Just normalized -> Right (Just normalized)
        Nothing -> Left err400 { errBody = marketplaceOptionalOrderStatusErrorBody }

validateMarketplaceOrderUpdateStatus :: Maybe Text -> Either ServerError (Maybe Text)
validateMarketplaceOrderUpdateStatus Nothing = Right Nothing
validateMarketplaceOrderUpdateStatus (Just rawStatus) =
  case normalizeOptionalInput (Just rawStatus) of
    Nothing -> Left err400 { errBody = "status cannot be blank" }
    Just statusVal ->
      case normalizeMarketplaceOrderStatus statusVal of
        Just normalized -> Right (Just normalized)
        Nothing -> Left err400 { errBody = marketplaceOrderStatusErrorBody }

validateOptionalMarketplacePaymentProviderUpdate :: Maybe (Maybe Text) -> Either ServerError (Maybe (Maybe Text))
validateOptionalMarketplacePaymentProviderUpdate Nothing =
  Right Nothing
validateOptionalMarketplacePaymentProviderUpdate (Just Nothing) =
  Right (Just Nothing)
validateOptionalMarketplacePaymentProviderUpdate (Just (Just rawProvider))
  | T.null normalized =
      Left err400 { errBody = "paymentProvider cannot be blank; use null to clear it" }
  | T.length normalized > 64 =
      Left err400 { errBody = "paymentProvider must be 64 characters or fewer" }
  | not (T.all isPaymentProviderSlugChar normalized) =
      Left err400
        { errBody =
            "paymentProvider must contain only ASCII letters, digits, hyphen, or underscore"
        }
  | otherwise =
      Right (Just (Just normalized))
  where
    normalized = T.toLower (T.strip rawProvider)
    isPaymentProviderSlugChar ch =
      isAsciiLower ch || isDigit ch || ch == '-' || ch == '_'

marketplaceOrderStatusErrorBody :: BL.ByteString
marketplaceOrderStatusErrorBody =
  "status must be one of: pending, contact, paid, cancelled, failed, "
    <> "datafast_init, datafast_pending, datafast_failed, paypal_pending, "
    <> "paypal_failed"

marketplaceOptionalOrderStatusErrorBody :: BL.ByteString
marketplaceOptionalOrderStatusErrorBody =
  "status must be omitted or one of: pending, contact, paid, cancelled, failed, "
    <> "datafast_init, datafast_pending, datafast_failed, paypal_pending, "
    <> "paypal_failed"

parsePayPalCaptureOrderStatus :: Text -> Either ServerError Text
parsePayPalCaptureOrderStatus rawStatus =
  case normalizePayPalCaptureOrderStatus rawStatus of
    Just normalized -> Right normalized
    Nothing ->
      Left err502
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              "Unsupported PayPal capture status: " <> T.strip rawStatus
        }

validatePayPalCaptureOrderId :: Text -> Either ServerError Text
validatePayPalCaptureOrderId rawOrderId =
  case normalizeOptionalInput (Just rawOrderId) of
    Nothing ->
      Left err400 { errBody = "paypalOrderId is required" }
    Just orderId
      | T.length orderId > 128 ->
          Left err400 { errBody = "paypalOrderId must be 128 characters or fewer" }
      | T.all isPayPalOrderIdChar orderId ->
          Right orderId
      | otherwise ->
          Left err400
            { errBody =
                "paypalOrderId must contain only ASCII letters, digits, hyphen, or underscore"
            }

validatePayPalCreateOrderIdField :: Text -> Either ServerError Text
validatePayPalCreateOrderIdField rawOrderId =
  case normalizeOptionalInput (Just rawOrderId) of
    Just orderId | isValidPayPalOrderId orderId ->
      Right orderId
    _ ->
      Left err502 { errBody = "PayPal create response returned an invalid order id" }

isValidPayPalOrderId :: Text -> Bool
isValidPayPalOrderId orderId =
  not (T.null orderId)
    && T.length orderId <= 128
    && T.all isPayPalOrderIdChar orderId

isPayPalOrderIdChar :: Char -> Bool
isPayPalOrderIdChar c =
  isDigit c || isAsciiLower c || isAsciiUpper c || c == '-' || c == '_'

validatePayPalCaptureOrderReference :: Maybe Text -> Text -> Either ServerError Text
validatePayPalCaptureOrderReference mStoredOrderId paypalOrderId =
  case normalizeOptionalInput mStoredOrderId of
    Nothing ->
      Left err409 { errBody = "Order does not have a PayPal order to capture" }
    Just storedOrderId
      | storedOrderId == paypalOrderId ->
          Right paypalOrderId
      | otherwise ->
          Left err400 { errBody = "paypalOrderId does not match this order's PayPal order" }

normalizeMarketplaceOrderStatus :: Text -> Maybe Text
normalizeMarketplaceOrderStatus rawStatus =
  case normalizeMarketplaceOrderStatusToken rawStatus of
    "pending" -> Just "pending"
    "contact" -> Just "contact"
    "paid" -> Just "paid"
    "cancelled" -> Just "cancelled"
    "canceled" -> Just "cancelled"
    "failed" -> Just "failed"
    "datafastinit" -> Just "datafast_init"
    "datafastpending" -> Just "datafast_pending"
    "datafastfailed" -> Just "datafast_failed"
    "paypalpending" -> Just "paypal_pending"
    "paypalfailed" -> Just "paypal_failed"
    _ -> Nothing

normalizePayPalCaptureOrderStatus :: Text -> Maybe Text
normalizePayPalCaptureOrderStatus rawStatus =
  case normalizeMarketplaceOrderStatusToken rawStatus of
    "completed" -> Just "paid"
    "approved" -> Just "paypal_pending"
    "created" -> Just "paypal_pending"
    "payeractionrequired" -> Just "paypal_pending"
    "pending" -> Just "paypal_pending"
    "saved" -> Just "paypal_pending"
    "cancelled" -> Just "cancelled"
    "canceled" -> Just "cancelled"
    "voided" -> Just "cancelled"
    "failed" -> Just "paypal_failed"
    "declined" -> Just "paypal_failed"
    "denied" -> Just "paypal_failed"
    "expired" -> Just "paypal_failed"
    _ -> Nothing

normalizeMarketplaceOrderStatusToken :: Text -> Text
normalizeMarketplaceOrderStatusToken =
  T.filter isAlphaNum . T.toLower . T.strip

normalizeCourseRegistrationStatus :: Text -> Maybe Text
normalizeCourseRegistrationStatus raw =
  case normalizeBookingStatusToken raw of
    "pendingpayment" -> Just "pending_payment"
    "paid" -> Just "paid"
    "cancelled" -> Just "cancelled"
    "canceled" -> Just "cancelled"
    _ -> Nothing

validateServiceMarketplaceCatalog :: Maybe ServiceCatalog -> Either ServerError ServiceKind
validateServiceMarketplaceCatalog Nothing =
  Left err404 { errBody = "Service catalog not found" }
validateServiceMarketplaceCatalog (Just catalog)
  | not (serviceCatalogActive catalog) =
      Left err409 { errBody = "Service catalog is inactive" }
  | otherwise =
      Right (serviceCatalogKind catalog)

validateServiceAdCurrency :: Maybe Text -> Either ServerError Text
validateServiceAdCurrency = validateCurrencyCode

validateReceiptCurrency :: Maybe Text -> Either ServerError (Maybe Text)
validateReceiptCurrency Nothing = Right Nothing
validateReceiptCurrency (Just rawCurrency) =
  Just <$> validateCurrencyCode (Just rawCurrency)

validateCurrencyCode :: Maybe Text -> Either ServerError Text
validateCurrencyCode Nothing = Right "USD"
validateCurrencyCode (Just rawCurrency) =
  case normalizeOptionalInput (Just rawCurrency) of
    Nothing ->
      Left err400 { errBody = "currency must be a 3-letter ISO code" }
    Just currency ->
      let normalized = T.toUpper currency
      in if T.length normalized == 3 && T.all isAsciiUpper normalized
           then Right normalized
           else Left err400 { errBody = "currency must be a 3-letter ISO code" }

validateServiceAdSlotMinutes :: Maybe Int -> Either ServerError Int
validateServiceAdSlotMinutes Nothing = Right 60
validateServiceAdSlotMinutes (Just rawMinutes)
  | rawMinutes < 15 =
      Left err400 { errBody = "slotMinutes must be at least 15" }
  | otherwise = Right rawMinutes

requiresEngineer :: Maybe Text -> Bool
requiresEngineer Nothing = False
requiresEngineer (Just svc) =
  let lowered = T.toLower (T.strip svc)
  in any (`T.isInfixOf` lowered) ["graba", "mezcl", "master"]

validateEngineer :: Maybe Text -> Maybe Int64 -> Maybe Text -> Either Text ()
validateEngineer svc mEngineerId mEngineerName
  | Just engineerName <- mEngineerName
  , T.length (T.strip engineerName) > 160 =
      Left "engineerName debe tener 160 caracteres o menos"
  | Just engineerName <- mEngineerName
  , T.any isControl (T.strip engineerName) =
      Left "engineerName no debe contener caracteres de control"
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
      notifyResult <- liftIO $
        ((try $
          EmailSvc.sendEngineerBooking
            emailSvc
            name
            engineerEmail
            subjectSvc
            startTxt
            customer
            noteText) :: IO (Either SomeException ()))
      case notifyResult of
        Left err -> do
          let msg = "[Bookings] Failed to notify engineer " <> engineerEmail <> ": " <> T.pack (displayException err)
          liftIO $ do
            hPutStrLn stderr (T.unpack msg)
            LogBuf.addLog LogBuf.LogWarning msg
        Right () -> pure ()

resolveResourcesForBooking :: Maybe Text -> [Text] -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
resolveResourcesForBooking service requested start end =
  case normalizeRequestedResourceIds requested of
    Left err -> liftIO (throwIO err)
    Right [] -> defaultResourcesForService service start end
    Right explicit -> resolveRequestedResources explicit start end

normalizeRequestedResourceIds :: [Text] -> Either ServerError [Text]
normalizeRequestedResourceIds rawIds =
  let normalized = map T.strip rawIds
  in if any T.null normalized
       then Left err400 { errBody = "resourceIds must not contain blank entries" }
       else case duplicateRequestedResourceIds normalized of
         [] -> Right normalized
         duplicateIds ->
           Left err400
             { errBody =
                 BL.fromStrict . TE.encodeUtf8 $
                   "resourceIds must not contain duplicate entries: "
                     <> T.intercalate ", " duplicateIds
             }

duplicateRequestedResourceIds :: [Text] -> [Text]
duplicateRequestedResourceIds = go Set.empty Set.empty []
  where
    go _ _ duplicates [] = reverse duplicates
    go seen reported duplicates (rid:rest) =
      let normalized = T.toLower rid
      in if Set.member normalized seen
           then
             if Set.member normalized reported
               then go seen reported duplicates rest
               else go seen (Set.insert normalized reported) (rid : duplicates) rest
           else go (Set.insert normalized seen) reported duplicates rest

resolveRequestedResources :: [Text] -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
resolveRequestedResources requestedIds start end = do
  rooms <- selectList [ResourceResourceType ==. Room, ResourceActive ==. True] [Asc ResourceId]
  let indexById = Map.fromList $ map (\(Entity k room) -> (k, room)) rooms
      indexBySlug = Map.fromList $ map (\(Entity k room) -> (T.toLower (resourceSlug room), k)) rooms
      indexByName = Map.fromList $ map (\(Entity k room) -> (T.toLower (resourceName room), k)) rooms
      resolveOne rid =
        case fromPathPiece rid of
          Just key | Map.member key indexById -> Just key
          _ ->
            let normalized = T.toLower rid
            in Map.lookup normalized indexBySlug <|> Map.lookup normalized indexByName
      unresolved = [rid | rid <- requestedIds, isNothing (resolveOne rid)]
  case dedupeStable unresolved of
    [] -> do
      let resolved = dedupeStable (mapMaybe resolveOne requestedIds)
      availability <- forM resolved $ \key -> do
        available <- isResourceAvailableDB key start end
        pure (key, available)
      let unavailable = [key | (key, False) <- availability]
          unavailableIds =
            dedupeStable
              [ rid
              | rid <- requestedIds
              , Just key <- [resolveOne rid]
              , key `elem` unavailable
              ]
      case unavailableIds of
        [] -> pure resolved
        invalidIds ->
          liftIO . throwIO $
            err409
              { errBody =
                  BL.fromStrict . TE.encodeUtf8 $
                    "resourceIds contain unavailable rooms: "
                      <> T.intercalate ", " invalidIds
              }
    invalidIds ->
      liftIO . throwIO $
        err400
          { errBody =
              BL.fromStrict . TE.encodeUtf8 $
                "resourceIds contain unknown or unavailable rooms: "
                  <> T.intercalate ", " invalidIds
          }

dedupeStable :: Ord a => [a] -> [a]
dedupeStable = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

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
  result <- liftIO $ flip runSqlPool pool $ do
    resolved <- resolvePackagePurchaseRefs req
    case resolved of
      Left err -> pure (Left err)
      Right (buyerKey, productKey, productRecord) -> do
        let qty = packageProductUnitsQty productRecord
            priceC = packageProductPriceCents productRecord
        _ <- insert PackagePurchase
              { packagePurchaseBuyerId        = buyerKey
              , packagePurchaseProductId      = productKey
              , packagePurchasePurchasedAt    = now
              , packagePurchasePriceCents     = priceC
              , packagePurchaseExpiresAt      = Nothing
              , packagePurchaseRemainingUnits = qty
              , packagePurchaseStatus         = "Active"
              }
        pure (Right ())
  either throwError pure result
  pure NoContent

resolvePackagePurchaseRefs
  :: PackagePurchaseReq
  -> SqlPersistT IO (Either ServerError (Key Party, Key PackageProduct, PackageProduct))
resolvePackagePurchaseRefs PackagePurchaseReq{buyerId = rawBuyerId, productId = rawProductId} =
  case
    (,) <$> validatePositiveIdField "buyerId" rawBuyerId
        <*> validatePositiveIdField "productId" rawProductId of
    Left err -> pure (Left err)
    Right (buyerIdValid, productIdValid) -> do
      let buyerKey = toSqlKey buyerIdValid :: Key Party
          productKey = toSqlKey productIdValid :: Key PackageProduct
      mBuyer <- get buyerKey
      mProduct <- get productKey
      pure $
        case mBuyer of
          Nothing ->
            Left err422
              { errBody = "buyerId references an unknown party" }
          Just _ ->
            case mProduct of
              Nothing ->
                Left err422
                  { errBody = "productId references an unknown package product" }
              Just productRecord
                | not (packageProductActive productRecord) ->
                    Left err409 { errBody = "Package product is inactive" }
                | otherwise ->
                    Right (buyerKey, productKey, productRecord)

resolveInvoiceCustomerId :: Int64 -> SqlPersistT IO (Either ServerError (Key Party))
resolveInvoiceCustomerId rawCustomerId =
  case validatePositiveIdField "customerId" rawCustomerId of
    Left err -> pure (Left err)
    Right customerIdValid -> do
      let customerKey = toSqlKey customerIdValid :: Key Party
      mCustomer <- get customerKey
      pure $
        case mCustomer of
          Nothing ->
            Left err422
              { errBody = "customerId references an unknown party" }
          Just _ ->
            Right customerKey

-- Invoices
invoiceServer :: AuthedUser -> ServerT InvoiceAPI AppM
invoiceServer user =
       listInvoices user
  :<|> createInvoice user
  :<|> generateInvoiceForSession user
  :<|> getInvoicesBySession user
  :<|> getInvoiceById user

generateInvoiceForSession :: AuthedUser -> Text -> Value -> AppM Value
generateInvoiceForSession user sessionId payload = do
  requireModule user ModuleInvoicing
  sessionKey <- parseSessionKey sessionId
  req <- case fromJSON payload of
    Error err -> throwBadRequest ("Invalid generate-invoice payload: " <> T.pack err)
    Success decoded -> pure decoded
  Env pool _ <- ask
  sessionEnt <- liftIO $ flip runSqlPool pool $ getEntity sessionKey
  session <- maybe (throwError err404 { errBody = "Session not found" }) pure sessionEnt
  customerKey <- resolveSessionInvoiceCustomer session req
  now <- liftIO getCurrentTime
  dto <- createInvoice user (toCreateInvoiceReq customerKey req)
  let invoiceKey = toSqlKey (invId dto) :: Key Invoice
  liftIO $ flip runSqlPool pool $ do
    _ <- insertUnique ME.SessionInvoice
      { ME.sessionInvoiceSessionId = sessionKey
      , ME.sessionInvoiceInvoiceId = invoiceKey
      , ME.sessionInvoiceCreatedAt = now
      }
    pure ()
  sriValue <-
    if fromMaybe True (gsiIssueSri req)
      then do
        customer <- loadCustomerForSri customerKey
        sriResult <- liftIO (Sri.runSriInvoiceScript (toSriScriptRequest customer req))
        case sriResult of
          Left err ->
            pure (object ["ok" .= False, "error" .= err])
          Right result -> do
            when (sirStatus result == "issued") $
              liftIO $ flip runSqlPool pool $
                update invoiceKey
                  [ InvoiceStatus =. Sent
                  , InvoiceSriDocumentId =. sirAuthorizationNumber result
                  , InvoiceNumber =. sirInvoiceNumber result
                  ]
            pure (toJSON result)
      else pure Null
  refreshed <- loadInvoiceDTOOr404 invoiceKey
  pure $
    object
      [ "ok" .= True
      , "sessionId" .= sessionId
      , "invoice" .= refreshed
      , "sri" .= sriValue
      ]

getInvoicesBySession :: AuthedUser -> Text -> AppM Value
getInvoicesBySession user sessionId = do
  requireModule user ModuleInvoicing
  sessionKey <- parseSessionKey sessionId
  Env pool _ <- ask
  invoices <- liftIO $ flip runSqlPool pool $ do
    links <- selectList [ME.SessionInvoiceSessionId ==. sessionKey] [Desc ME.SessionInvoiceCreatedAt]
    let invoiceIds = map (ME.sessionInvoiceInvoiceId . entityVal) links
    loadInvoiceDTOs invoiceIds
  pure (toJSON invoices)

getInvoiceById :: AuthedUser -> Int64 -> AppM Value
getInvoiceById user invoiceId = do
  requireModule user ModuleInvoicing
  invoiceIdValid <- either throwError pure (validatePositiveIdField "invoiceId" invoiceId)
  dto <- loadInvoiceDTOOr404 (toSqlKey invoiceIdValid)
  pure (toJSON dto)

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
  when (null ciLineItems) $ throwBadRequest "Invoice requires at least one line item"
  currency <- either throwError pure (validateCurrencyCode ciCurrency)
  preparedLines <- case traverse prepareLine ciLineItems of
    Left msg   -> throwBadRequest msg
    Right vals -> pure vals
  Env pool _ <- ask
  customerKey <- do
    resolved <- liftIO $ flip runSqlPool pool $ resolveInvoiceCustomerId ciCustomerId
    either throwError pure resolved
  now <- liftIO getCurrentTime
  let day      = utctDay now
      notes    = normalizeOptionalText ciNotes
      number   = normalizeOptionalText ciNumber
      subtotal = sum (map plSubtotal preparedLines)
      taxTotal = sum (map plTax preparedLines)
      grand    = sum (map plTotal preparedLines)
      invoiceRecord = Invoice
        { invoiceCustomerId    = customerKey
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
  invoiceIdValid <- either throwError pure (validatePositiveIdField "invoiceId" crInvoiceId)
  currencyOverride <- either throwError pure (validateReceiptCurrency crCurrency)
  Env pool _ <- ask
  now <- liftIO getCurrentTime
  let iid = toSqlKey invoiceIdValid :: Key Invoice
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
                               (normalizeOptionalText crNotes) currencyOverride
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
  receiptIdValid <- either throwError pure (validatePositiveIdField "receiptId" ridParam)
  Env pool _ <- ask
  let rid = toSqlKey receiptIdValid :: Key Receipt
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
  when (taxBpsVal > 10000) $ Left "Line item tax basis points must be 10000 or less"
  when (isJust cilServiceOrderId && isJust cilPackagePurchaseId) $
    Left "Line item may reference either serviceOrderId or packagePurchaseId, not both"
  serviceOrderKey <- case cilServiceOrderId of
    Nothing -> Right Nothing
    Just rawRef
      | rawRef > 0 -> Right (Just (toSqlKey rawRef :: Key ServiceOrder))
      | otherwise -> Left "serviceOrderId must be a positive integer"
  packagePurchaseKey <- case cilPackagePurchaseId of
    Nothing -> Right Nothing
    Just rawRef
      | rawRef > 0 -> Right (Just (toSqlKey rawRef :: Key PackagePurchase))
      | otherwise -> Left "packagePurchaseId must be a positive integer"
  let subtotal = cilQuantity * cilUnitCents
      tax      = (subtotal * taxBpsVal) `div` 10000
      total    = subtotal + tax
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

parseSessionKey :: Text -> AppM (Key ME.Session)
parseSessionKey raw =
  maybe (throwBadRequest "Invalid session identifier") pure (fromPathPiece raw)

resolveSessionInvoiceCustomer :: Entity ME.Session -> GenerateSessionInvoiceReq -> AppM (Key Party)
resolveSessionInvoiceCustomer (Entity _ session) req =
  case gsiCustomerId req of
    Just cid -> pure (toSqlKey cid)
    Nothing ->
      case ME.sessionClientPartyRef session >>= readMaybe . T.unpack of
        Just cid -> pure (toSqlKey cid)
        Nothing ->
          throwBadRequest
            "Session clientPartyRef is not a numeric party id; provide customerId explicitly."

toCreateInvoiceReq :: Key Party -> GenerateSessionInvoiceReq -> CreateInvoiceReq
toCreateInvoiceReq customerKey req =
  CreateInvoiceReq
    { ciCustomerId = fromSqlKey customerKey
    , ciCurrency = gsiCurrency req
    , ciNumber = gsiNumber req
    , ciNotes = gsiNotes req
    , ciLineItems = map toLineItem (gsiLineItems req)
    , ciGenerateReceipt = gsiGenerateReceipt req
    }
  where
    toLineItem GenerateSessionInvoiceLineReq{..} =
      CreateInvoiceLineReq
        { cilDescription = gsilDescription
        , cilQuantity = gsilQuantity
        , cilUnitCents = gsilUnitCents
        , cilTaxBps = gsilTaxBps
        , cilServiceOrderId = gsilServiceOrderId
        , cilPackagePurchaseId = gsilPackagePurchaseId
        }

loadCustomerForSri :: Key Party -> AppM Sri.SriScriptCustomer
loadCustomerForSri partyKey = do
  Env pool _ <- ask
  mParty <- liftIO $ flip runSqlPool pool $ get partyKey
  case mParty of
    Nothing -> throwError err404 { errBody = "Customer not found" }
    Just party ->
      case normalizeOptionalText (partyTaxId party) of
        Nothing -> throwBadRequest "Customer taxId/RUC is required for SRI emission"
        Just taxIdVal ->
          pure Sri.SriScriptCustomer
            { Sri.ruc = taxIdVal
            , Sri.legalName =
                fromMaybe (M.partyDisplayName party) (normalizeOptionalText (partyLegalName party))
            , Sri.email = normalizeOptionalText (partyPrimaryEmail party)
            , Sri.phone = normalizeOptionalText (partyPrimaryPhone party)
            }

toSriScriptRequest :: Sri.SriScriptCustomer -> GenerateSessionInvoiceReq -> Sri.SriScriptRequest
toSriScriptRequest customer req =
  Sri.SriScriptRequest
    { Sri.customer = customer
    , Sri.lines = map toSriLine (gsiLineItems req)
    , Sri.establishment = "1"
    , Sri.emissionPoint = "100"
    , Sri.paymentMode = "cash"
    , Sri.signAndSend = fromMaybe True (gsiIssueSri req)
    , Sri.certificatePassword = normalizeOptionalText (gsiCertificatePassword req)
    }
  where
    toSriLine GenerateSessionInvoiceLineReq{..} =
      Sri.SriScriptLine
        { Sri.code = normalizeOptionalText gsilSriCode
        , Sri.auxiliaryCode = normalizeOptionalText gsilSriAuxiliaryCode
        , Sri.description = gsilDescription
        , Sri.quantity = gsilQuantity
        , Sri.unitCents = gsilUnitCents
        , Sri.taxBps = gsilTaxBps
        , Sri.sriAdditionalInfo = normalizeOptionalText gsilSriAdditionalInfo
        , Sri.sriIvaCode = normalizeOptionalText gsilSriIvaCode
        }

loadInvoiceDTOOr404 :: Key Invoice -> AppM InvoiceDTO
loadInvoiceDTOOr404 invoiceKey = do
  Env pool _ <- ask
  mDto <- liftIO $ flip runSqlPool pool (loadInvoiceDTO invoiceKey)
  maybe (throwError err404 { errBody = "Invoice not found" }) pure mDto

loadInvoiceDTO :: Key Invoice -> SqlPersistT IO (Maybe InvoiceDTO)
loadInvoiceDTO invoiceKey = do
  mInvoice <- getEntity invoiceKey
  case mInvoice of
    Nothing -> pure Nothing
    Just invEnt -> do
      invLines <- selectList [InvoiceLineInvoiceId ==. invoiceKey] [Asc InvoiceLineId]
      mReceipt <- selectFirst [ReceiptInvoiceId ==. invoiceKey] []
      pure (Just (invoiceToDTO invEnt invLines (entityKey <$> mReceipt)))

loadInvoiceDTOs :: [Key Invoice] -> SqlPersistT IO [InvoiceDTO]
loadInvoiceDTOs invoiceKeys = do
  let orderedKeys = nub invoiceKeys
  if null orderedKeys
    then pure []
    else do
      invoices <- selectList [InvoiceId <-. orderedKeys] []
      invLines <- selectList [InvoiceLineInvoiceId <-. orderedKeys] [Asc InvoiceLineId]
      receipts <- selectList [ReceiptInvoiceId <-. orderedKeys] []
      let invoiceMap = Map.fromList [(entityKey ent, ent) | ent <- invoices]
          lineMap = Map.fromListWith (++)
            [(invoiceLineInvoiceId (entityVal ent), [ent]) | ent <- invLines]
          receiptMap = Map.fromList
            [(receiptInvoiceId (entityVal ent), entityKey ent) | ent <- receipts]
      pure
        [ invoiceToDTO invEnt (Map.findWithDefault [] key lineMap) (Map.lookup key receiptMap)
        | key <- orderedKeys
        , Just invEnt <- [Map.lookup key invoiceMap]
        ]

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
  , sriDocumentId = invoiceSriDocumentId inv
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

requireStrictAdmin :: AuthedUser -> AppM ()
requireStrictAdmin user
  | hasStrictAdminAccess user = pure ()
  | otherwise = throwError err403
      { errBody = BL.fromStrict (TE.encodeUtf8 "Admin role required") }
-- User roles API
userRolesServer :: AuthedUser -> ServerT UserRolesAPI AppM
userRolesServer user =
       listUsers
  :<|> userRoutes
  where
    listUsers = do
      requireStrictAdmin user
      Env pool _ <- ask
      liftIO $ flip runSqlPool pool loadUserRoleSummaries

    userRoutes userId =
           getUserRolesH userId
      :<|> updateUserRolesH userId

    getUserRolesH userId = do
      requireStrictAdmin user
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
      requireStrictAdmin user
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

validateAdsInquiry :: AdsInquiry -> Either ServerError AdsInquiry
validateAdsInquiry AdsInquiry{..} = do
  emailClean <- validateCourseRegistrationEmail aiEmail
  phoneClean <- validateAdsInquiryPhone aiPhone
  validateAdsInquiryContactChannels emailClean phoneClean
  channelClean <- validateAdsInquiryChannel aiChannel
  messageClean <- validateAdsInquiryMessage aiMessage
  pure AdsInquiry
    { aiName = normalizeOptionalInput aiName
    , aiEmail = emailClean
    , aiPhone = phoneClean
    , aiCourse = normalizeOptionalInput aiCourse
    , aiMessage = messageClean
    , aiChannel = channelClean
    }

adsInquiryPublic :: AdsInquiry -> AppM AdsInquiryOut
adsInquiryPublic rawInquiry = do
  inquiry <- either throwError pure (validateAdsInquiry rawInquiry)
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
  channels <- liftIO $ sendAutoReplies (envPool env) partyId (envConfig env) inquiry courseLabel
  pure AdsInquiryOut
    { aioOk = True
    , aioInquiryId = entityKeyInt inquiryId
    , aioPartyId = entityKeyInt partyId
    , aioRepliedVia = channels
    }

validateAdsAssistRequest
  :: AdsAssistRequest
  -> Either ServerError (Text, Maybe ME.AdCreativeId, Maybe ME.CampaignId, Maybe Text)
validateAdsAssistRequest
  AdsAssistRequest{aarAdId, aarCampaignId, aarMessage, aarChannel, aarPartyId} = do
  validatePublicAdsAssistPartyId aarPartyId
  body <- validateMessage
  adKey <- fmap toSqlKey <$> validateOptionalPositiveIdField "adId" aarAdId
  campaignKey <- fmap toSqlKey <$> validateOptionalPositiveIdField "campaignId" aarCampaignId
  channel <- validateAdsAssistChannel aarChannel
  pure (body, adKey, campaignKey, channel)
  where
    validateMessage =
      let body = T.strip aarMessage
      in if T.null body
           then Left err400 { errBody = "Mensaje vacío" }
           else
             if T.length body > 2000
               then Left err400 { errBody = "Mensaje demasiado largo (max 2000 caracteres)" }
               else
                 if T.any isUnsafeAdsAssistControl body
                   then Left err400 { errBody = "Mensaje no debe contener caracteres de control" }
                   else Right body
    isUnsafeAdsAssistControl ch =
      isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

validatePublicAdsAssistPartyId :: Maybe Int64 -> Either ServerError ()
validatePublicAdsAssistPartyId Nothing = Right ()
validatePublicAdsAssistPartyId (Just _) =
  Left err400 { errBody = "partyId is not allowed on public ads assist requests" }

validateAdsAssistChannel :: Maybe Text -> Either ServerError (Maybe Text)
validateAdsAssistChannel Nothing = Right Nothing
validateAdsAssistChannel (Just rawChannel) =
  case T.toLower <$> normalizeOptionalInput (Just rawChannel) of
    Nothing -> Right Nothing
    Just "instagram" -> Right (Just "instagram")
    Just "facebook" -> Right (Just "facebook")
    Just "whatsapp" -> Right (Just "whatsapp")
    _ -> Left err400 { errBody = "channel inválido (instagram|facebook|whatsapp)" }

validateAdCreativeLandingUrl :: Maybe Text -> Either ServerError (Maybe Text)
validateAdCreativeLandingUrl = validateCoursePublicUrlField "landingUrl"

validateCampaignBudgetCents :: Maybe Int -> Either ServerError (Maybe Int)
validateCampaignBudgetCents Nothing = Right Nothing
validateCampaignBudgetCents (Just amount)
  | amount < 0 = Left err400 { errBody = "budgetCents must be non-negative" }
  | otherwise = Right (Just amount)

adsAssistPublic :: AdsAssistRequest -> AppM AdsAssistResponse
adsAssistPublic req = do
  (body, adKey, campaignKey, channel) <- either throwError pure (validateAdsAssistRequest req)
  env <- ask
  let hasScope = isJust adKey || isJust campaignKey
  candidateAds <- runDB $
    case campaignKey of
      Nothing -> pure (maybeToList adKey)
      Just ck -> do
        ads <- selectKeysList [ME.AdCreativeCampaignId ==. Just ck] []
        pure (maybe ads (:ads) adKey)
  examples <- runDB $ loadAdExamples hasScope candidateAds
  kb <- liftIO $ retrieveRagContext (envConfig env) (envPool env) body
  reply <- liftIO $ runRagChatWithStatus (envConfig env) kb examples body channel
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
  campaignId <- either throwError pure (validatePositiveIdField "campaignId" cid)
  let key = toSqlKey campaignId :: ME.CampaignId
  mRow <- runDB $ get key
  case mRow of
    Nothing -> throwError err404
    Just row -> pure (campaignToDTO (Entity key row))

adsUpsertCampaign :: AuthedUser -> CampaignUpsert -> AppM CampaignDTO
adsUpsertCampaign user CampaignUpsert{..} = do
  requireModule user ModuleAdmin
  budgetCentsVal <- either throwError pure (validateCampaignBudgetCents cuBudgetCents)
  campaignIdUpdate <- either throwError pure (validateOptionalPositiveIdField "campaignId" cuId)
  let nameClean = T.strip cuName
      statusVal =
        case cuStatus of
          Nothing -> "active"
          Just raw ->
            let trimmed = T.strip raw
            in if T.null trimmed then "active" else trimmed
  when (T.null nameClean) $ throwBadRequest "Nombre requerido"
  now <- liftIO getCurrentTime
  cid <- case campaignIdUpdate of
    Nothing -> runDB $ insert ME.Campaign
      { ME.campaignName = nameClean
      , ME.campaignObjective = T.strip <$> cuObjective
      , ME.campaignPlatform = T.strip <$> cuPlatform
      , ME.campaignStatus = statusVal
      , ME.campaignBudgetCents = budgetCentsVal
      , ME.campaignStartDate = cuStartDate
      , ME.campaignEndDate = cuEndDate
      , ME.campaignCreatedAt = now
      , ME.campaignUpdatedAt = now
      }
    Just campaignId -> do
      let key = toSqlKey campaignId :: ME.CampaignId
      mCampaign <- runDB $ get key
      when (isNothing mCampaign) $ throwError err404
      runDB $ update key
        [ ME.CampaignName =. nameClean
        , ME.CampaignObjective =. (T.strip <$> cuObjective)
        , ME.CampaignPlatform =. (T.strip <$> cuPlatform)
        , ME.CampaignStatus =. statusVal
        , ME.CampaignBudgetCents =. budgetCentsVal
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
  landingUrlVal <- either throwError pure (validateAdCreativeLandingUrl acuLandingUrl)
  adIdUpdate <- either throwError pure (validateOptionalPositiveIdField "adId" acuId)
  campaignIdRef <- either throwError pure (validateOptionalPositiveIdField "campaignId" acuCampaignId)
  let nameClean = T.strip acuName
      mCampaign = fmap toSqlKey campaignIdRef :: Maybe ME.CampaignId
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
  adId <- case adIdUpdate of
    Nothing -> runDB $ insert ME.AdCreative
      { ME.adCreativeCampaignId = mCampaign
      , ME.adCreativeExternalId = fmap T.strip acuExternalId
      , ME.adCreativeName = nameClean
      , ME.adCreativeChannel = T.strip <$> acuChannel
      , ME.adCreativeAudience = T.strip <$> acuAudience
      , ME.adCreativeLandingUrl = landingUrlVal
      , ME.adCreativeCta = T.strip <$> acuCta
      , ME.adCreativeStatus = statusVal
      , ME.adCreativeNotes = T.strip <$> acuNotes
      , ME.adCreativeCreatedAt = now
      , ME.adCreativeUpdatedAt = now
      }
    Just adIdRaw -> do
      let key = toSqlKey adIdRaw :: ME.AdCreativeId
      mAd <- runDB $ get key
      when (isNothing mAd) $ throwError err404
      runDB $ update key
        [ ME.AdCreativeCampaignId =. mCampaign
        , ME.AdCreativeExternalId =. (T.strip <$> acuExternalId)
        , ME.AdCreativeName =. nameClean
        , ME.AdCreativeChannel =. (T.strip <$> acuChannel)
        , ME.AdCreativeAudience =. (T.strip <$> acuAudience)
        , ME.AdCreativeLandingUrl =. landingUrlVal
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
  campaignId <- either throwError pure (validatePositiveIdField "campaignId" cid)
  let key = toSqlKey campaignId :: ME.CampaignId
  rows <- runDB $ selectList [ME.AdCreativeCampaignId ==. Just key] [Desc ME.AdCreativeUpdatedAt, LimitTo 200]
  pure (map adToDTO rows)

adsListExamples :: AuthedUser -> Int64 -> AppM [AdConversationExampleDTO]
adsListExamples user adId = do
  requireModule user ModuleAdmin
  adIdValid <- either throwError pure (validatePositiveIdField "adId" adId)
  let key = toSqlKey adIdValid :: ME.AdCreativeId
  rows <- runDB $ selectList [ME.AdConversationExampleAdId ==. key] [Desc ME.AdConversationExampleUpdatedAt, LimitTo 200]
  pure (map adExampleToDTO rows)

adsCreateExample :: AuthedUser -> Int64 -> AdConversationExampleCreate -> AppM AdConversationExampleDTO
adsCreateExample user adId AdConversationExampleCreate{..} = do
  requireModule user ModuleAdmin
  adIdValid <- either throwError pure (validatePositiveIdField "adId" adId)
  let userMsg = T.strip aecUserMessage
      assistantMsg = T.strip aecAssistantMessage
      key = toSqlKey adIdValid :: ME.AdCreativeId
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
  statusAllowsFallback && hasMarker && not hasNonFallbackMarker
  where
    msg = T.toLower (T.strip rawMessage)
    statusAllowsFallback = status == 0 || status == 400 || status == 403 || status == 404
    hasMarker = any (`T.isInfixOf` msg) markers
    hasNonFallbackMarker = any (`T.isInfixOf` msg) nonFallbackMarkers
    markers =
      [ "does not have access to model"
      , "doesn't have access to model"
      , "do not have access to model"
      , "don't have access to model"
      , "no access to model"
      , "does not exist or you do not have access"
      , "model not found"
      , "unknown model"
      , "invalid model id"
      , "invalid model name"
      , "not a valid model"
      , "model_not_found"
      ]
    nonFallbackMarkers =
      [ "rate limit"
      , "too many requests"
      , "quota"
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
tidalAgentServer user TidalAgentRequest{..} = do
  unless (hasAiToolingAccess user) $
    throwError err403 { errBody = "Missing required module access" }
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
  unless (hasAiToolingAccess user) $
    throwError err403 { errBody = "Missing required module access" }
  Env{..} <- ask
  let cfg = envConfig
  workflowId <- either throwError pure (resolveWorkflowId cksWorkflowId (chatKitWorkflowId cfg))
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

resolveWorkflowId :: Maybe Text -> Maybe Text -> Either ServerError Text
resolveWorkflowId primary fallback =
  case (primary >>= nonEmptyText, fallback >>= nonEmptyText) of
    (Just rawWorkflowId, _) ->
      toServerError err400 (normalizeChatKitWorkflowId "workflowId" rawWorkflowId)
    (Nothing, Just rawWorkflowId) ->
      toServerError err500 (normalizeChatKitWorkflowId "CHATKIT_WORKFLOW_ID" rawWorkflowId)
    (Nothing, Nothing) ->
      Left err400 { errBody = "workflowId requerido" }
  where
    toServerError status result =
      case result of
        Right workflowId -> Right workflowId
        Left msg ->
          Left status { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

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
  rawSecret <- o .: "client_secret"
  secret <-
    maybe (fail "client_secret is required") pure (nonEmptyText rawSecret)
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

sendAutoReplies :: ConnectionPool -> PartyId -> AppConfig -> AdsInquiry -> Maybe Text -> IO [Text]
sendAutoReplies pool partyId cfg AdsInquiry{..} mCourse = do
  wa <- loadWhatsAppEnv
  now <- getCurrentTime
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
    [ case aiPhone >>= normalizePhone of
        Just ph -> do
          res <- sendWhatsAppTextIO wa ph body
          _ <- runSqlPool
            (recordOutgoingWhatsAppMessage now OutgoingWhatsAppRecord
              { owrRecipientPhone = ph
              , owrRecipientPartyId = Just partyId
              , owrRecipientName = aiName
              , owrRecipientEmail = aiEmail
              , owrActorPartyId = Nothing
              , owrBody = body
              , owrSource = Just "ads_auto_reply"
              , owrReplyToMessageId = Nothing
              , owrReplyToExternalId = Nothing
              , owrResendOfMessageId = Nothing
              , owrMetadata = Nothing
              }
              res)
            pool
          pure (either (const Nothing) (const (Just "whatsapp")) res)
        _ -> pure Nothing
    , case aiEmail of
        Just e -> do
          let svc = EmailSvc.mkEmailService cfg
          emailResult <- (try $
            EmailSvc.sendTestEmail svc (fromMaybe "Amigo TDF" aiName) e "Gracias por tu interés en TDF"
              [ "Paquete 1:1 (16 horas): $480."
              , "Grupo pequeño: detalles y fechas en " <> courseLanding
              , "Agenda tu clase de prueba gratis aquí: " <> cta <> "/trials"
              ] (Just (cta <> "/trials"))) :: IO (Either SomeException ())
          case emailResult of
            Left err -> do
              let msg = "[AdsInquiry] Failed to send follow-up email to " <> e <> ": " <> T.pack (displayException err)
              hPutStrLn stderr (T.unpack msg)
              LogBuf.addLog LogBuf.LogWarning msg
              pure Nothing
            Right () -> pure (Just "email")
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
      slug <-
        maybe (throwError err400 { errBody = "slug requerido" })
          (either throwError pure . validateRequiredCmsSlug)
          mSlug
      locale <- either throwError pure (validateCmsLocaleFilter mLocale)
      mPublished <- runDB $ selectFirst
        [ CMS.CmsContentSlug ==. slug
        , CMS.CmsContentLocale ==. locale
        , CMS.CmsContentStatus ==. "published"
        ] [Desc CMS.CmsContentVersion]
      maybe (fallbackContent slug locale) (pure . toCmsDTO) mPublished

    cmsList mLocale mPrefix = do
      locale <- either throwError pure (validateCmsLocaleFilter mLocale)
      mSlugPrefix <- either throwError pure (validateOptionalCmsSlugPrefix mPrefix)
      entries <- runDB $
        selectList
          [ CMS.CmsContentLocale ==. locale
          , CMS.CmsContentStatus ==. "published"
          ]
          [ Desc CMS.CmsContentPublishedAt
          , Desc CMS.CmsContentVersion
          ]
      let filtered = case mSlugPrefix of
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
    else if seedDatabase envConfig
    then do
      -- Auto-publish demo inventory so the public marketplace is never empty.
      liftIO $ flip runSqlPool envPool $ do
        seedInventoryAssets
        seedMarketplaceListings
      seeded <- liftIO $ flip runSqlPool envPool loadListings
      dtos <- forM seeded (toMarketplaceDTO assetsBase)
      pure (catMaybes dtos)
    else
      pure []

getMarketplaceItem :: Text -> AppM MarketplaceItemDTO
getMarketplaceItem rawId = do
  listingKey <- parseListingId rawId
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
resolveMarketplacePhotoUrl assetsBase (Just raw0) =
  case ServerExtra.validateAssetPhotoUrl (Just raw0) of
    Left _ -> pure Nothing
    Right Nothing -> pure Nothing
    Right (Just photoUrl) -> pure (Just (normalizePhoto assetsBase photoUrl))

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
  when (mciuQuantity > maxMarketplaceCartItemQuantity) $
    throwBadRequest $
      "quantity must be "
        <> T.pack (show maxMarketplaceCartItemQuantity)
        <> " or fewer"
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
  buyerNameTxt <- either throwError pure (validateMarketplaceBuyerName mcrBuyerName)
  buyerEmailTxt <- either throwError pure (validateMarketplaceBuyerEmail mcrBuyerEmail)
  buyerPhoneTxt <- either throwError pure (validateMarketplaceBuyerPhone mcrBuyerPhone)
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  cartTotalsState <- liftIO $ flip runSqlPool envPool $ loadCartTotals cartKey
  (cartItems, totalCents, currency) <-
    either throwError pure (requireMarketplaceCartTotals cartTotalsState)
  mOrder <- liftIO $ flip runSqlPool envPool $ do
    let statusTxt = if totalCents > 0 then "pending" else "contact"
    orderId <- insert ME.MarketplaceOrder
      { ME.marketplaceOrderCartId        = Just cartKey
      , ME.marketplaceOrderBuyerName     = buyerNameTxt
      , ME.marketplaceOrderBuyerEmail    = buyerEmailTxt
      , ME.marketplaceOrderBuyerPhone    = buyerPhoneTxt
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
  nameTxt <- either throwError pure (validateMarketplaceBuyerName (mcrBuyerName payload))
  emailTxt <- either throwError pure (validateMarketplaceBuyerEmail (mcrBuyerEmail payload))
  phoneTxt <- either throwError pure (validateMarketplaceBuyerPhone (mcrBuyerPhone payload))
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  cartTotalsState <- liftIO $ flip runSqlPool envPool $ loadCartTotals cartKey
  (cartItems, totalCentsRaw, currency) <-
    either throwError pure (requireMarketplaceCartTotals cartTotalsState)
  totalCents <-
    either throwError pure (validateMarketplaceOnlinePaymentTotal totalCentsRaw)
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
  orderKey <- case mOrderId of
    Just oid -> parseOrderId oid
    Nothing  -> throwBadRequest "orderId requerido"
  Env{ envPool } <- ask
  mOrder <- liftIO $ flip runSqlPool envPool $ get orderKey
  order <- maybe (throwError err404) pure mOrder
  resourcePathTxt <-
    either throwError pure $
      validateDatafastOrderResourcePath
        (ME.marketplaceOrderDatafastCheckoutId order)
        mResourcePath
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
  liftIO $ flip runSqlPool envPool $ update orderKey updateFields
  mDto <- liftIO $ flip runSqlPool envPool $ loadOrderDTO orderKey
  maybe (throwError err500) pure mDto

createPaypalOrder :: Text -> MarketplaceCheckoutReq -> AppM PaypalCreateDTO
createPaypalOrder rawId MarketplaceCheckoutReq{..} = do
  nameTxt <- either throwError pure (validateMarketplaceBuyerName mcrBuyerName)
  emailTxt <- either throwError pure (validateMarketplaceBuyerEmail mcrBuyerEmail)
  phoneTxt <- either throwError pure (validateMarketplaceBuyerPhone mcrBuyerPhone)
  cartKey <- parseCartId rawId
  now <- liftIO getCurrentTime
  Env{ envPool } <- ask
  cartTotalsState <- liftIO $ flip runSqlPool envPool $ loadCartTotals cartKey
  (cartItems, totalCentsRaw, currency) <-
    either throwError pure (requireMarketplaceCartTotals cartTotalsState)
  totalCents <-
    either throwError pure (validateMarketplaceOnlinePaymentTotal totalCentsRaw)
  (cid, sec, baseUrl) <- loadPaypalEnv
  manager <- liftIO $ newManager tlsManagerSettings
  (ppOrderId, approvalUrl) <- createPaypalOrderRemote manager cid sec baseUrl totalCents currency nameTxt emailTxt
  orderId <- liftIO $ flip runSqlPool envPool $ do
    oid <- insert ME.MarketplaceOrder
      { ME.marketplaceOrderCartId        = Just cartKey
      , ME.marketplaceOrderBuyerName     = nameTxt
      , ME.marketplaceOrderBuyerEmail    = emailTxt
      , ME.marketplaceOrderBuyerPhone    = phoneTxt
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
  paypalOrderId <- either throwError pure (validatePayPalCaptureOrderId pcCapturePaypalId)
  Env{ envPool } <- ask
  mOrder <- liftIO $ flip runSqlPool envPool $ get orderKey
  case mOrder of
    Nothing -> throwError err404
    Just order -> do
      paypalOrderIdForCapture <- either throwError pure $
        validatePayPalCaptureOrderReference
          (ME.marketplaceOrderPaypalOrderId order)
          paypalOrderId
      (cid, sec, baseUrl) <- loadPaypalEnv
      manager <- liftIO $ newManager tlsManagerSettings
      PayPalCaptureOutcome statusTxt payerEmail <-
        capturePaypalOrderRemote manager cid sec baseUrl paypalOrderIdForCapture
      now <- liftIO getCurrentTime
      nextStatus <- either throwError pure (parsePayPalCaptureOrderStatus statusTxt)
      let paidAtVal =
            if nextStatus == "paid"
              then Just now
              else ME.marketplaceOrderPaidAt order
      liftIO $ flip runSqlPool envPool $ update orderKey
        [ ME.MarketplaceOrderStatus =. nextStatus
        , ME.MarketplaceOrderPaypalOrderId =. Just paypalOrderId
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
      checkoutId <- either throwError pure (validateDatafastCheckoutId (dfcId dfResp))
      let widgetUrl =
            baseUrlClean ++ "/v1/paymentWidgets.js?checkoutId=" ++ T.unpack checkoutId
      pure (checkoutId, widgetUrl)

datafastPaymentStatus :: DatafastEnv -> Text -> AppM DFPaymentStatus
datafastPaymentStatus dfEnv resourcePathTxt = do
  resourcePath <- either throwError pure (validateDatafastResourcePath (Just resourcePathTxt))
  manager <- liftIO $ newManager tlsManagerSettings
  let rp = T.unpack resourcePath
      baseUrlClean = normalizeBaseUrl (dfBaseUrl dfEnv)
      basePath = baseUrlClean ++ rp
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

validateDatafastResourcePath :: Maybe Text -> Either ServerError Text
validateDatafastResourcePath Nothing =
  Left err400 { errBody = "resourcePath requerido" }
validateDatafastResourcePath (Just rawResourcePath)
  | T.null resourcePath =
      Left err400 { errBody = "resourcePath requerido" }
  | T.length resourcePath > 512 =
      invalidDatafastResourcePath
  | T.isPrefixOf "http://" lowered
      || T.isPrefixOf "https://" lowered
      || T.isPrefixOf "//" resourcePath =
      invalidDatafastResourcePath
  | T.any (not . isDatafastResourcePathChar) resourcePath =
      invalidDatafastResourcePath
  | any invalidSegment segments =
      invalidDatafastResourcePath
  | Just checkoutId <- datafastCheckoutIdFromSegments segments
  , isValidDatafastCheckoutId checkoutId =
      Right resourcePath
  | otherwise =
      invalidDatafastResourcePath
  where
    resourcePath = T.strip rawResourcePath
    lowered = T.toLower resourcePath
    segments = T.splitOn "/" (T.drop 1 resourcePath)
    invalidSegment segment =
      T.null segment || segment == "." || segment == ".."

validateDatafastOrderResourcePath :: Maybe Text -> Maybe Text -> Either ServerError Text
validateDatafastOrderResourcePath mExpectedCheckoutId mRawResourcePath = do
  resourcePath <- validateDatafastResourcePath mRawResourcePath
  expectedCheckoutId <- case mExpectedCheckoutId of
    Just checkoutId -> validateStoredDatafastCheckoutId checkoutId
    Nothing ->
      Left err409
        { errBody =
            "Order does not have a Datafast checkout to confirm"
        }
  actualCheckoutId <-
    maybe invalidDatafastResourcePath Right (datafastCheckoutIdFromResourcePath resourcePath)
  if actualCheckoutId == expectedCheckoutId
    then Right resourcePath
    else
      Left err400
        { errBody =
            "resourcePath does not match this order's Datafast checkout"
        }

validateStoredDatafastCheckoutId :: Text -> Either ServerError Text
validateStoredDatafastCheckoutId rawCheckoutId
  | isValidDatafastCheckoutId checkoutId =
      Right checkoutId
  | otherwise =
      Left err500
        { errBody =
            "Stored Datafast checkout id is invalid"
        }
  where
    checkoutId = T.strip rawCheckoutId

datafastCheckoutIdFromResourcePath :: Text -> Maybe Text
datafastCheckoutIdFromResourcePath resourcePath =
  datafastCheckoutIdFromSegments (T.splitOn "/" (T.drop 1 resourcePath))

datafastCheckoutIdFromSegments :: [Text] -> Maybe Text
datafastCheckoutIdFromSegments ["v1", "checkouts", checkoutId, "payment"]
  | not (T.null checkoutId) = Just checkoutId
datafastCheckoutIdFromSegments _ =
  Nothing

validateDatafastCheckoutId :: Text -> Either ServerError Text
validateDatafastCheckoutId rawCheckoutId
  | isValidDatafastCheckoutId checkoutId =
      Right checkoutId
  | otherwise =
      invalidDatafastCheckoutId
  where
    checkoutId = T.strip rawCheckoutId

isValidDatafastCheckoutId :: Text -> Bool
isValidDatafastCheckoutId checkoutId =
  not (T.null checkoutId)
    && T.length checkoutId <= 256
    && checkoutId /= "."
    && checkoutId /= ".."
    && T.all isDatafastCheckoutIdChar checkoutId

isDatafastCheckoutIdChar :: Char -> Bool
isDatafastCheckoutIdChar c =
  isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_." :: String)

isDatafastResourcePathChar :: Char -> Bool
isDatafastResourcePathChar c =
  isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-_./" :: String)

invalidDatafastCheckoutId :: Either ServerError a
invalidDatafastCheckoutId =
  Left err502
    { errBody =
        "Datafast returned an invalid checkout id"
    }

invalidDatafastResourcePath :: Either ServerError a
invalidDatafastResourcePath =
  Left err400
    { errBody =
        "resourcePath must be a Datafast relative checkout payment path"
    }

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
  limitCount <- either throwError pure (validateMarketplaceOrderListLimit mLimit)
  offsetCount <- either throwError pure (validateMarketplaceOrderListOffset mOffset)
  normalizedStatus <- either throwError pure (validateOptionalMarketplaceOrderStatus mStatus)
  let filters = maybe [] (\st -> [ME.MarketplaceOrderStatus ==. st]) normalizedStatus
  Env{..} <- ask
  liftIO $ flip runSqlPool envPool $ do
    orders <- selectList filters [Desc ME.MarketplaceOrderCreatedAt, OffsetBy offsetCount, LimitTo limitCount]
    catMaybes <$> mapM (loadOrderDTO . entityKey) orders

updateMarketplaceOrder :: AuthedUser -> Text -> MarketplaceOrderUpdate -> AppM MarketplaceOrderDTO
updateMarketplaceOrder user rawId MarketplaceOrderUpdate{..} = do
  requireMarketplaceAccess user
  orderKey <- parseOrderId rawId
  nextStatus <- either throwError pure (validateMarketplaceOrderUpdateStatus mouStatus)
  nextProvider <- either throwError pure (validateOptionalMarketplacePaymentProviderUpdate mouPaymentProvider)
  now <- liftIO getCurrentTime
  paidAtInput <- either throwError pure (validateMarketplaceOrderPaidAtUpdate now mouPaidAt)
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ do
    mOrder <- get orderKey
    case mOrder of
      Nothing -> pure Nothing
      Just order -> do
        let paidAtBase   = case paidAtInput of
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
parseOrderId rawId = do
  _ <- either throwError pure (validateMarketplacePathId "order" rawId)
  case fromPathPiece (T.strip rawId) of
    Nothing -> throwBadRequest "Invalid order id"
    Just k -> pure k

parseCartId :: Text -> AppM (Key ME.MarketplaceCart)
parseCartId rawId = do
  _ <- either throwError pure (validateMarketplacePathId "cart" rawId)
  case fromPathPiece (T.strip rawId) of
    Nothing -> throwBadRequest "Invalid cart id"
    Just k -> pure k

parseListingId :: Text -> AppM (Key ME.MarketplaceListing)
parseListingId rawId = do
  _ <- either throwError pure (validateMarketplacePathId "listing" rawId)
  case fromPathPiece (T.strip rawId) of
    Nothing -> throwBadRequest "Invalid listing id"
    Just k -> pure k

validateMarketplacePathId :: Text -> Text -> Either ServerError Int64
validateMarketplacePathId label rawId =
  let normalized = T.strip rawId
      invalid =
        Left err400
          { errBody =
              BL.fromStrict (TE.encodeUtf8 ("Invalid " <> label <> " id"))
          }
  in
    if T.null normalized || not (T.all isDigit normalized)
      then invalid
      else
        case readMaybe (T.unpack normalized) of
          Just pathId | pathId > 0 -> Right pathId
          _ -> invalid

requireMarketplaceAccess :: AuthedUser -> AppM ()
requireMarketplaceAccess user =
  unless (hasOperationsAccess user) $
    throwError err403

data MarketplaceCartTotalsState a
  = MarketplaceCartMissing
  | MarketplaceCartEmpty
  | MarketplaceCartInvalidQuantity Int
  | MarketplaceCartMixedCurrencies [Text]
  | MarketplaceCartTotalsReady a
  deriving (Eq, Show)

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
  -> SqlPersistT IO (MarketplaceCartTotalsState ([(Entity ME.MarketplaceCartItem, Entity ME.MarketplaceListing, Entity ME.Asset, Int)], Int, Text))
loadCartTotals cartId = do
  mCart <- get cartId
  case mCart of
    Nothing -> pure MarketplaceCartMissing
    Just _ -> do
      items <- loadCartLines cartId
      if null items
        then pure MarketplaceCartEmpty
        else do
          let quantities = [ qty | (_, _, _, qty) <- items ]
          case find isInvalidMarketplaceCartLineQuantity quantities of
            Just invalidQuantity ->
              pure (MarketplaceCartInvalidQuantity invalidQuantity)
            Nothing -> do
              let totalCents =
                    sum [ qty * ME.marketplaceListingPriceUsdCents (entityVal listing)
                        | (_, listing, _, qty) <- items
                        ]
                  currencies =
                    nub [ ME.marketplaceListingCurrency (entityVal listing)
                        | (_, listing, _, _) <- items
                        ]
              case currencies of
                [currency] -> pure (MarketplaceCartTotalsReady (items, totalCents, currency))
                mixed -> pure (MarketplaceCartMixedCurrencies mixed)

requireMarketplaceCartTotals :: MarketplaceCartTotalsState a -> Either ServerError a
requireMarketplaceCartTotals MarketplaceCartMissing =
  Left err404
requireMarketplaceCartTotals MarketplaceCartEmpty =
  Left err400 { errBody = "El carrito esta vacio." }
requireMarketplaceCartTotals (MarketplaceCartInvalidQuantity rawQuantity) =
  Left (marketplaceCartInvalidQuantityError rawQuantity)
requireMarketplaceCartTotals (MarketplaceCartMixedCurrencies currencies) =
  Left err400
    { errBody =
        BL.fromStrict . TE.encodeUtf8 $
          "El carrito no puede mezclar monedas: " <> T.intercalate ", " currencies
    }
requireMarketplaceCartTotals (MarketplaceCartTotalsReady totals) =
  Right totals

validateMarketplaceCartLineQuantity :: Int -> Either ServerError Int
validateMarketplaceCartLineQuantity rawQuantity
  | isInvalidMarketplaceCartLineQuantity rawQuantity =
      Left (marketplaceCartInvalidQuantityError rawQuantity)
  | otherwise =
      Right rawQuantity

isInvalidMarketplaceCartLineQuantity :: Int -> Bool
isInvalidMarketplaceCartLineQuantity rawQuantity =
  rawQuantity < 1 || rawQuantity > maxMarketplaceCartItemQuantity

marketplaceCartInvalidQuantityError :: Int -> ServerError
marketplaceCartInvalidQuantityError _ =
  err409
    { errBody =
        BL.fromStrict . TE.encodeUtf8 $
          "El carrito contiene una cantidad invalida; actualiza las cantidades entre 1 y "
            <> T.pack (show maxMarketplaceCartItemQuantity)
            <> "."
    }

validateMarketplaceOnlinePaymentTotal :: Int -> Either ServerError Int
validateMarketplaceOnlinePaymentTotal totalCents
  | totalCents > 0 = Right totalCents
  | otherwise =
      Left err400 { errBody = "El carrito debe tener un total mayor a 0 para pagar en linea." }

validateMarketplaceOrderPaidAtUpdate
  :: UTCTime
  -> Maybe (Maybe UTCTime)
  -> Either ServerError (Maybe (Maybe UTCTime))
validateMarketplaceOrderPaidAtUpdate _ Nothing = Right Nothing
validateMarketplaceOrderPaidAtUpdate _ (Just Nothing) = Right (Just Nothing)
validateMarketplaceOrderPaidAtUpdate now (Just (Just paidAt))
  | paidAt <= now = Right (Just (Just paidAt))
  | otherwise = Left err400 { errBody = "paidAt must not be in the future" }

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
  baseUrl <- either throwError pure (validateDatafastBaseUrl mBase)
  entityId <- either throwError pure (validateDatafastEntityId mEntity)
  bearer <- either throwError pure (validateDatafastCredential "DATAFAST_BEARER_TOKEN" mBearer)
  let testModeVal = mTest >>= (\v -> let t = T.strip (T.pack v) in if T.null t then Nothing else Just t)
      optPair k mv = (\v -> (k, TE.encodeUtf8 v)) <$> mv
      extras =
        catMaybes
          [ optPair "customParameters[SHOPPER_MID]" (T.strip . T.pack <$> mMid)
          , optPair "customParameters[SHOPPER_TID]" (T.strip . T.pack <$> mTid)
          , optPair "customParameters[SHOPPER_PSERV]" (T.strip . T.pack <$> mPserv)
          , optPair "risk.parameters[USER_DATA2]" (T.strip . T.pack <$> mUserData2)
          , optPair "customParameters[SHOPPER_VERSIONDF]" (Just (maybe "2" (T.pack) mVersionDf))
          ]
  pure DatafastEnv
    { dfEntityId = entityId
    , dfBearerToken = bearer
    , dfBaseUrl = baseUrl
    , dfTestMode = testModeVal
    , dfExtraParams = extras
    }

validateDatafastCredential :: Text -> Maybe String -> Either ServerError Text
validateDatafastCredential = validateRequiredGatewayCredential

validateDatafastEntityId :: Maybe String -> Either ServerError Text
validateDatafastEntityId mRawEntityId = do
  entityId <- validateDatafastCredential "DATAFAST_ENTITY_ID" mRawEntityId
  if T.all isDatafastGatewayIdChar entityId
    then Right entityId
    else
      Left err500
        { errBody =
            "DATAFAST_ENTITY_ID must contain only ASCII letters, digits, hyphen, underscore, or dot"
        }
  where
    isDatafastGatewayIdChar c =
      isDigit c || isAsciiLower c || isAsciiUpper c || c == '-' || c == '_' || c == '.'

validateDatafastBaseUrl :: Maybe String -> Either ServerError String
validateDatafastBaseUrl mRawBase
  | T.null cleanBase =
      invalidDatafastBaseUrl
  | not ("https://" `T.isPrefixOf` T.toLower cleanBase) =
      invalidDatafastBaseUrl
  | not (TrialsServer.isValidHttpUrl cleanBase) =
      invalidDatafastBaseUrl
  | not (isHttpsOrigin cleanBase) =
      invalidDatafastBaseUrl
  | not (isApprovedDatafastOrigin cleanBase) =
      invalidDatafastBaseUrl
  | otherwise =
      Right (T.unpack cleanBase)
  where
    rawBase = maybe "https://test.oppwa.com" T.pack mRawBase
    cleanBase = T.dropWhileEnd (== '/') (T.strip rawBase)

    isHttpsOrigin rawUrl =
      T.all (\c -> c /= '/' && c /= '?' && c /= '#') (T.drop 8 rawUrl)

    isApprovedDatafastOrigin rawUrl =
      case datafastOriginParts rawUrl of
        Just (host, portSuffix) ->
          (host == "oppwa.com" || ".oppwa.com" `T.isSuffixOf` host)
            && (T.null portSuffix || portSuffix == ":443")
        Nothing ->
          False

    datafastOriginParts rawUrl =
      let authority = T.drop 8 rawUrl
          (host, portSuffix) = T.breakOn ":" authority
          normalizedHost = T.toLower host
      in if T.null normalizedHost
           then Nothing
           else Just (normalizedHost, portSuffix)

invalidDatafastBaseUrl :: Either ServerError a
invalidDatafastBaseUrl =
  Left err500
    { errBody =
        "DATAFAST_BASE_URL must be an absolute https origin on oppwa.com "
          <> "using the default HTTPS port and without path, query, or fragment"
    }

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
  { payPalAccessToken :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PayPalToken where
  parseJSON = withObject "PayPalToken" $ \o ->
    PayPalToken <$> o .:? "access_token"

data PayPalCaptureOutcome = PayPalCaptureOutcome
  { pcoStatus :: Text
  , pcoPayerEmail :: Maybe Text
  } deriving (Show, Generic)

loadPaypalEnv :: AppM (Text, Text, String)
loadPaypalEnv = do
  mCid <- liftIO $ lookupEnv "PAYPAL_CLIENT_ID"
  mSecret <- liftIO $ lookupEnv "PAYPAL_CLIENT_SECRET"
  mEnv <- liftIO $ lookupEnv "PAYPAL_ENV"
  baseUrl <- either throwError pure (resolvePaypalBaseUrl mEnv)
  cid <- either throwError pure (validatePayPalCredential "PAYPAL_CLIENT_ID" mCid)
  secret <- either throwError pure (validatePayPalCredential "PAYPAL_CLIENT_SECRET" mSecret)
  pure (cid, secret, baseUrl)

validatePayPalCredential :: Text -> Maybe String -> Either ServerError Text
validatePayPalCredential = validateRequiredGatewayCredential

validateRequiredGatewayCredential :: Text -> Maybe String -> Either ServerError Text
validateRequiredGatewayCredential envName mRawCredential =
  case normalizeOptionalInput (T.pack <$> mRawCredential) of
    Nothing ->
      Left err500
        { errBody =
            BL.fromStrict . TE.encodeUtf8 $
              envName <> " must be configured"
        }
    Just credential
      | T.any (\ch -> isControl ch || isSpace ch) credential ->
          Left err500
            { errBody =
                BL.fromStrict . TE.encodeUtf8 $
                  envName <> " must not contain control characters or whitespace"
            }
      | otherwise ->
          Right credential

resolvePaypalBaseUrl :: Maybe String -> Either ServerError String
resolvePaypalBaseUrl mEnv =
  case fmap (T.toLower . T.strip . T.pack) mEnv of
    Nothing -> Right sandboxBase
    Just "" -> Right sandboxBase
    Just envTxt
      | envTxt `elem` ["sandbox", "test"] -> Right sandboxBase
      | envTxt `elem` ["live", "prod", "production"] -> Right liveBase
      | otherwise ->
          Left err500
            { errBody =
                "PAYPAL_ENV must be one of: sandbox, test, live, prod, production"
            }
  where
    sandboxBase = "https://api-m.sandbox.paypal.com"
    liveBase = "https://api-m.paypal.com"

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
  either throwError pure $
    validatePayPalAccessTokenField (payPalAccessToken token)

validatePayPalAccessTokenField :: Maybe Text -> Either ServerError Text
validatePayPalAccessTokenField Nothing =
  Left err502 { errBody = "PayPal token response did not include an access token" }
validatePayPalAccessTokenField (Just rawToken) =
  case normalizeOptionalInput (Just rawToken) of
    Nothing ->
      Left err502 { errBody = "PayPal token response access token cannot be blank" }
    Just token
      | T.any (\ch -> isControl ch || isSpace ch) token ->
          Left err502
            { errBody =
                "PayPal token response access token must not contain control characters or whitespace"
            }
      | otherwise ->
          Right token

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
  approval <-
    either throwError pure $
      validatePayPalApprovalUrl
        (fmap pplHref . find (\lnk -> pplRel lnk == "approve") $ pcrLinks resObj)
  ppOrderId <-
    either throwError pure $
      validatePayPalCreateOrderIdField (pcrId resObj)
  pure (ppOrderId, Just approval)

validatePayPalApprovalUrl :: Maybe Text -> Either ServerError Text
validatePayPalApprovalUrl Nothing =
  Left err502 { errBody = "PayPal response did not include an approval URL" }
validatePayPalApprovalUrl (Just rawUrl)
  | T.null url =
      invalidPayPalApprovalUrl
  | not ("https://" `T.isPrefixOf` T.toLower url) =
      invalidPayPalApprovalUrl
  | not (TrialsServer.isValidHttpUrl url) =
      invalidPayPalApprovalUrl
  | not (isPayPalCheckoutApprovalUrl url) =
      invalidPayPalApprovalUrl
  | otherwise =
      Right url
  where
    url = T.strip rawUrl

    isPayPalCheckoutApprovalUrl rawApprovalUrl =
      case paypalApprovalUrlParts rawApprovalUrl of
        Just (host, portSuffix, pathAndQuery) ->
          host `elem` ["www.paypal.com", "www.sandbox.paypal.com"]
            && T.null portSuffix
            && "/checkoutnow?" `T.isPrefixOf` pathAndQuery
            && not ("#" `T.isInfixOf` pathAndQuery)
            && hasSingleValidTokenParam (T.drop 13 pathAndQuery)
        Nothing ->
          False

    paypalApprovalUrlParts rawApprovalUrl =
      let afterScheme = T.drop 8 rawApprovalUrl
          authority =
            T.takeWhile
              (\c -> c /= '/' && c /= '?' && c /= '#')
              afterScheme
          pathAndQuery = T.drop (T.length authority) afterScheme
          (host, portSuffix) = T.breakOn ":" authority
          normalizedHost = T.toLower host
      in if T.null normalizedHost
           then Nothing
           else Just (normalizedHost, portSuffix, pathAndQuery)

    hasSingleValidTokenParam query =
      case T.splitOn "&" query of
        [param] ->
          case tokenValue param of
            Just token -> isValidPayPalOrderId token
            Nothing -> False
        _ -> False

    tokenValue param =
      T.stripPrefix "token=" param

invalidPayPalApprovalUrl :: Either ServerError a
invalidPayPalApprovalUrl =
  Left err502 { errBody = "PayPal returned an invalid approval URL" }

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
  statusTxt <-
    either throwError pure $
      validatePayPalCaptureStatusField
        (join $ parseMaybe (withObject "PayPalCapture" (\o -> o .:? "status")) parsed)
  payerEmail <-
    case parseEither parsePayPalPayerEmail parsed of
      Left _ ->
        throwError err502 { errBody = "PayPal payer email must be a string when present" }
      Right rawPayerEmail ->
        either throwError pure (validatePayPalPayerEmailField rawPayerEmail)
  pure PayPalCaptureOutcome
    { pcoStatus = statusTxt
    , pcoPayerEmail = payerEmail
    }

validatePayPalCaptureStatusField :: Maybe Text -> Either ServerError Text
validatePayPalCaptureStatusField Nothing =
  Left err502 { errBody = "PayPal capture response did not include a status" }
validatePayPalCaptureStatusField (Just rawStatus)
  | T.null statusTxt =
      Left err502 { errBody = "PayPal capture response status cannot be blank" }
  | T.any (\ch -> isControl ch || isSpace ch) statusTxt =
      Left err502
        { errBody =
            "PayPal capture response status must not contain control characters or whitespace"
        }
  | otherwise =
      Right statusTxt
  where
    statusTxt = T.strip rawStatus

parsePayPalPayerEmail :: Value -> Parser (Maybe Text)
parsePayPalPayerEmail =
  withObject "PayPalCapture" $ \o -> do
    mPayer <- o .:? "payer"
    case (mPayer :: Maybe Value) of
      Nothing ->
        pure Nothing
      Just payer ->
        withObject "PayPalPayer" (\po -> po .:? "email_address") payer

validatePayPalPayerEmailField :: Maybe Text -> Either ServerError (Maybe Text)
validatePayPalPayerEmailField Nothing = Right Nothing
validatePayPalPayerEmailField (Just rawEmail) =
  case validateCourseRegistrationEmail (Just rawEmail) of
    Right (Just emailVal) ->
      Right (Just emailVal)
    Right Nothing ->
      Left err502 { errBody = "PayPal payer email cannot be blank" }
    Left _ ->
      Left err502 { errBody = "PayPal returned an invalid payer email" }

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
  ownerId <- either throwError pure (validateLabelTrackOwnerIdFilter mOwnerId)
  let owner = if isAdmin then fmap toSqlKey ownerId else Just (auPartyId user)
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

validateLabelTrackTitle :: Text -> Either ServerError Text
validateLabelTrackTitle rawTitle =
  case normalizeOptionalInput (Just rawTitle) of
    Just title -> Right title
    Nothing -> Left err400 { errBody = "Título requerido" }

validateOptionalLabelTrackStatus :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalLabelTrackStatus Nothing = Right Nothing
validateOptionalLabelTrackStatus (Just rawStatus) =
  case T.toLower <$> normalizeOptionalInput (Just rawStatus) of
    Just "open" -> Right (Just "open")
    Just "done" -> Right (Just "done")
    _ -> Left err400 { errBody = "status must be one of: open, done" }

validateLabelTrackOwnerIdFilter :: Maybe Int64 -> Either ServerError (Maybe Int64)
validateLabelTrackOwnerIdFilter = validateOptionalPositiveIdField "ownerId"

validateLabelTrackPathId :: Text -> Either ServerError (Key ME.LabelTrack)
validateLabelTrackPathId rawId =
  let normalized = T.strip rawId
      invalid = Left err400 { errBody = "Invalid track id" }
  in if T.null normalized
       then invalid
       else maybe invalid Right (fromPathPiece normalized)

parseLabelTrackId :: Text -> AppM (Key ME.LabelTrack)
parseLabelTrackId =
  either throwError pure . validateLabelTrackPathId

listLabelTracks :: AuthedUser -> Maybe Int64 -> AppM [LabelTrackDTO]
listLabelTracks user mOwnerId = do
  scope <- resolveTrackScope user mOwnerId
  rows <- runDB $ selectList (ownerFilter scope) [Desc ME.LabelTrackCreatedAt]
  nameMap <- loadTrackOwnerNames rows
  pure (map (toLabelTrackDTO nameMap) rows)

createLabelTrack :: AuthedUser -> LabelTrackCreate -> AppM LabelTrackDTO
createLabelTrack user LabelTrackCreate{..} = do
  scope <- resolveTrackScope user ltcOwnerId
  title <- either throwError pure (validateLabelTrackTitle ltcTitle)
  now <- liftIO getCurrentTime
  let record = ME.LabelTrack
        { ME.labelTrackTitle      = title
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
  key <- parseLabelTrackId rawId
  scope <- resolveTrackScope user Nothing
  titleUpdate <- traverse (either throwError pure . validateLabelTrackTitle) ltuTitle
  statusUpdate <- either throwError pure (validateOptionalLabelTrackStatus ltuStatus)
  now <- liftIO getCurrentTime
  mTrack <- runDB $ getEntity key
  case mTrack of
    Nothing -> throwError err404
    Just (Entity _ track) -> do
      ensureTrackAccess scope track
      let updates = catMaybes
            [ (ME.LabelTrackTitle =.) <$> titleUpdate
            , case ltuNote of
                Nothing -> Nothing
                Just n  -> Just (ME.LabelTrackNote =. if T.null (T.strip n) then Nothing else Just (T.strip n))
            , (ME.LabelTrackStatus =.) <$> statusUpdate
            , Just (ME.LabelTrackUpdatedAt =. now)
            ]
      unless (null updates) (runDB $ update key updates)
      updated <- runDB $ getJustEntity key
      nameMap <- loadTrackOwnerNames [updated]
      pure (toLabelTrackDTO nameMap updated)

deleteLabelTrack :: AuthedUser -> Text -> AppM NoContent
deleteLabelTrack user rawId = do
  key <- parseLabelTrackId rawId
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
      slugFilter <- either throwError pure (validateOptionalCmsSlugFilter mSlug)
      localeFilter <- either throwError pure (validateOptionalCmsLocaleFilter mLocale)
      let filters = catMaybes
            [ (CMS.CmsContentSlug ==.) <$> slugFilter
            , (CMS.CmsContentLocale ==.) <$> localeFilter
            ]
      rows <- runDB $ selectList filters [Desc CMS.CmsContentCreatedAt]
      pure (map toCmsDTO rows)

    cmsCreateH CmsContentIn{..} = do
      requireWebmaster
      now <- liftIO getCurrentTime
      statusVal <- either throwError pure (validateCmsContentStatus cciStatus)
      slug <- either throwError pure (validateRequiredCmsSlug cciSlug)
      locale <- either throwError pure (validateRequiredCmsLocale cciLocale)
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
  parseJSON = withObject "DriveApiResp" $ \o -> do
    darId <- (o .: "id") >>= parseDriveApiFileId
    darWebViewLink <- o .:? "webViewLink"
    darWebContentLink <- o .:? "webContentLink"
    darResourceKey <- o .:? "resourceKey"
    pure DriveApiResp{..}

parseDriveApiFileId :: Text -> Parser Text
parseDriveApiFileId rawFileId
  | T.null fileId =
      fail "Drive file id is required"
  | T.length fileId > 256 || not (T.all isDriveFolderIdChar fileId) =
      fail "Drive file id must be 1-256 ASCII letters, digits, '-' or '_'"
  | otherwise =
      pure fileId
  where
    fileId = T.strip rawFileId

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
      publicUrl =
        Just $
          resolveDrivePublicUrl
            (darId driveResp)
            (darWebContentLink driveResp)
            (darResourceKey driveResp)
            metaResourceKey
  pure DriveUploadDTO
    { duFileId = darId driveResp
    , duWebViewLink = darWebViewLink driveResp
    , duWebContentLink = darWebContentLink driveResp
    , duPublicUrl = publicUrl
    }

resolveDrivePublicUrl :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text
resolveDrivePublicUrl fileId mWebContentLink mUploadResourceKey mMetaResourceKey =
  appendDriveResourceKey resolvedResourceKey baseUrl
  where
    fallbackPublicUrl = "https://drive.google.com/uc?export=download&id=" <> encodeQueryValue fileId
    baseUrl = fromMaybe fallbackPublicUrl (sanitizeDriveWebContentLink mWebContentLink)
    resolvedResourceKey =
      sanitizeDriveResourceKey mUploadResourceKey
        <|> sanitizeDriveResourceKey mMetaResourceKey

sanitizeDriveResourceKey :: Maybe Text -> Maybe Text
sanitizeDriveResourceKey mResourceKey = do
  resourceKey <- cleanOptional mResourceKey
  if T.length resourceKey <= 256 && T.all isDriveFolderIdChar resourceKey
    then Just resourceKey
    else Nothing

sanitizeDriveWebContentLink :: Maybe Text -> Maybe Text
sanitizeDriveWebContentLink mWebContentLink = do
  url <- cleanOptional mWebContentLink
  if "https://" `T.isPrefixOf` T.toLower url
      && TrialsServer.isValidHttpUrl url
      && isGoogleDriveDownloadHost url
      && hasDriveContentLocator url
    then Just url
    else Nothing

hasDriveContentLocator :: Text -> Bool
hasDriveContentLocator rawUrl =
  hasNonBlankQueryParam "id" rawUrl || length pathSegments >= 2
  where
    pathSegments =
      filter (not . T.null) (T.splitOn "/" path)
    remainder = T.drop 8 rawUrl
    afterAuthority =
      T.dropWhile
        (\c -> c /= '/' && c /= '?' && c /= '#')
        remainder
    path = T.takeWhile (\c -> c /= '?' && c /= '#') afterAuthority

isGoogleDriveDownloadHost :: Text -> Bool
isGoogleDriveDownloadHost rawUrl =
  case httpsUrlAuthority rawUrl of
    Just (host, portSuffix) ->
      host `elem` ["drive.google.com", "drive.usercontent.google.com"]
        && (T.null portSuffix || portSuffix == ":443")
    Nothing ->
      False

httpsUrlAuthority :: Text -> Maybe (Text, Text)
httpsUrlAuthority rawUrl
  | "https://" `T.isPrefixOf` T.toLower rawUrl =
      let remainder = T.drop 8 rawUrl
          authority =
            T.takeWhile
              (\c -> c /= '/' && c /= '?' && c /= '#')
              remainder
          (host, portSuffix) = T.breakOn ":" authority
          normalizedHost = T.toLower host
      in if T.null normalizedHost then Nothing else Just (normalizedHost, portSuffix)
  | otherwise =
      Nothing

appendDriveResourceKey :: Maybe Text -> Text -> Text
appendDriveResourceKey mResourceKey url =
  case cleanOptional mResourceKey of
    Nothing -> url
    Just resourceKey
      | hasNonBlankQueryParam "resourcekey" url -> url
      | otherwise ->
          appendQueryParam "resourcekey" resourceKey (dropBlankQueryParam "resourcekey" url)

hasNonBlankQueryParam :: Text -> Text -> Bool
hasNonBlankQueryParam paramName url =
  any (isNonBlankQueryParam paramName) params
  where
    (withoutFragment, _) = T.breakOn "#" url
    queryWithMarker = snd (T.breakOn "?" withoutFragment)
    query = T.drop 1 queryWithMarker
    params = filter (not . T.null) (T.splitOn "&" query)

dropBlankQueryParam :: Text -> Text -> Text
dropBlankQueryParam paramName url =
  case T.breakOn "?" withoutFragment of
    (_, "") -> url
    (base, queryWithMarker) ->
      let params = filter (not . T.null) (T.splitOn "&" (T.drop 1 queryWithMarker))
          filtered = filter (not . isBlankNamedParam) params
          rebuilt =
            if null filtered
              then base
              else base <> "?" <> T.intercalate "&" filtered
      in rebuilt <> fragment
  where
    (withoutFragment, fragment) = T.breakOn "#" url
    isBlankNamedParam rawParam =
      isNamedQueryParam paramName rawParam && not (isNonBlankQueryParam paramName rawParam)

isNamedQueryParam :: Text -> Text -> Bool
isNamedQueryParam paramName rawParam =
  let rawName = fst (T.breakOn "=" rawParam)
  in T.toLower rawName == T.toLower paramName

isNonBlankQueryParam :: Text -> Text -> Bool
isNonBlankQueryParam paramName rawParam =
  let (_rawName, rawValueWithEquals) = T.breakOn "=" rawParam
      rawValue = T.drop 1 rawValueWithEquals
  in isNamedQueryParam paramName rawParam
      && not (T.null rawValueWithEquals)
      && not (T.null (T.strip rawValue))

appendQueryParam :: Text -> Text -> Text -> Text
appendQueryParam paramName paramValue url =
  withoutFragment <> separator <> paramName <> "=" <> encodeQueryValue paramValue <> fragment
  where
    (withoutFragment, fragment) = T.breakOn "#" url
    separator
      | "?" `T.isInfixOf` withoutFragment =
          if T.isSuffixOf "?" withoutFragment || T.isSuffixOf "&" withoutFragment
            then ""
            else "&"
      | otherwise = "?"

encodeQueryValue :: Text -> Text
encodeQueryValue =
  TE.decodeUtf8 . urlEncode True . TE.encodeUtf8
