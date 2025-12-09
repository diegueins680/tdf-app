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
import           Control.Exception (SomeException, try)
import           Control.Concurrent (forkIO)
import           Control.Monad (forM, forM_, void, when, unless, (>=>), join)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.Int (Int64)
import           Data.List (find, foldl', nub, isPrefixOf)
import           Data.Foldable (for_)
import           Data.Char (isDigit, isSpace, isAlphaNum, toLower)
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Aeson (Value(..), object, (.=), eitherDecode, FromJSON(..), encode, fromJSON, Result(..))
import           Data.Aeson.Types (parseMaybe, withObject, (.:), (.:?), (.!=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Time (Day, UTCTime (..), fromGregorian, getCurrentTime, toGregorian, utctDay, addUTCTime, secondsToDiffTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time.Format.ISO8601 (iso8601ParseM)
import           GHC.Generics (Generic)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
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
import           TDF.API.Types (RolePayload(..), UserRoleSummaryDTO(..), UserRoleUpdatePayload(..), AccountStatusDTO(..), MarketplaceItemDTO(..), MarketplaceCartDTO(..), MarketplaceCartItemUpdate(..), MarketplaceCartItemDTO(..), MarketplaceOrderDTO(..), MarketplaceOrderItemDTO(..), MarketplaceOrderUpdate(..), MarketplaceCheckoutReq(..), DatafastCheckoutDTO(..), PaypalCreateDTO(..), PaypalCaptureReq(..), LabelTrackDTO(..), LabelTrackCreate(..), LabelTrackUpdate(..), DriveUploadDTO(..))
import qualified TDF.API      as Api
import           TDF.API.Marketplace (MarketplaceAPI, MarketplaceAdminAPI)
import           TDF.API.Label (LabelAPI)
import           TDF.API.Drive (DriveAPI, DriveUploadForm(..))
import           TDF.Contracts.API (ContractsAPI)
import           TDF.Config (AppConfig(..))
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
import           TDF.ServerExtra (bandsServer, instagramServer, inventoryServer, loadBandForParty, paymentsServer, pipelinesServer, roomsServer, sessionsServer)
import qualified Data.Map.Strict            as Map
import           TDF.ServerFuture (futureServer)
import           TDF.ServerRadio (radioServer)
import           TDF.ServerLiveSessions (liveSessionsServer)
import           TDF.ServerFeedback (feedbackServer)
import qualified TDF.Contracts.Server as Contracts
import           TDF.Trials.API (TrialsAPI)
import           TDF.Trials.Server (trialsServer)
import qualified TDF.Meta as Meta
import           TDF.Version      (getVersionInfo)
import qualified TDF.Handlers.InputList as InputList
import qualified TDF.Email as Email
import qualified TDF.Email.Service as EmailSvc
import qualified TDF.Services as Services
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
                                    , WhatsAppWebhookAPI
                                    , CourseMetadata(..)
                                    , CourseSession(..)
                                    , SyllabusItem(..)
                                    , CourseRegistrationRequest(..)
                                    , CourseRegistrationResponse(..)
                                    , UTMTags(..)
                                    )
import qualified TDF.Routes.Courses as Courses
import           TDF.WhatsApp.Types ( WAMetaWebhook(..)
                                    , WAEntry(..)
                                    , WAChange(..)
                                    , WAMessage(..)
                                    , WAValue(..)
                                    )
import qualified TDF.WhatsApp.Types as WA
import           TDF.WhatsApp.Client (sendText)
import           Network.HTTP.Client (Manager, RequestBody(..), Response, newManager, httpLbs, parseRequest, Request(..), responseBody, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.URI (urlEncode, renderQuery, renderSimpleQuery)
import           Network.HTTP.Types.Status (statusCode)
import           System.Environment (lookupEnv)
import qualified TDF.Trials.Models as Trials
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
      apiSrv   = hoistServerWithContext apiProxy ctxProxy (nt env) server
  in serveWithContext combinedProxy ctx (trials :<|> apiSrv)

nt :: Env -> AppM a -> Handler a
nt env x = runReaderT x env

server :: ServerT API AppM
server =
       versionServer
  :<|> health
  :<|> login
  :<|> googleLogin
  :<|> signup
  :<|> changePassword
  :<|> authV1Server
  :<|> fanPublicServer
  :<|> coursesPublicServer
  :<|> whatsappWebhookServer
  :<|> metaServer
  :<|> academyServer
  :<|> seedTrigger
  :<|> inputListServer
  :<|> adsInquiryPublic
  :<|> cmsPublicServer
  :<|> marketplacePublicServer
  :<|> labelPublicServer
  :<|> contractsServer
  :<|> socialEventsServer
  :<|> radioPresencePublicServer
  :<|> listEngineersPublic
  :<|> bookingPublicServer
  :<|> protectedServer

authV1Server :: ServerT Api.AuthV1API AppM
authV1Server = signup :<|> passwordReset :<|> passwordResetConfirm :<|> changePassword

versionServer :: ServerT Api.VersionAPI AppM
versionServer = liftIO getVersionInfo

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
      let incomingMessages = extractTextMessages payload
      for_ incomingMessages $ \(fromNumber, bodyTxt) -> do
        let lowerBody = T.toLower (T.strip bodyTxt)
        when ("inscribirme" `T.isInfixOf` lowerBody) $ do
          case normalizePhone fromNumber of
            Nothing -> pure ()
            Just phone -> do
              _ <- createOrUpdateRegistration productionCourseSlug CourseRegistrationRequest
                { fullName = Nothing
                , email = Nothing
                , phoneE164 = Just phone
                , source = "whatsapp"
                , howHeard = Just "whatsapp"
                , utm = Nothing
                }
              sendWhatsappReply cfg phone
      pure NoContent
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

driveServer :: AuthedUser -> ServerT DriveAPI AppM
driveServer _ mAccessToken DriveUploadForm{..} = do
  mEnvToken <- liftIO $ lookupEnv "DRIVE_ACCESS_TOKEN"
  let tokenTxt = fmap T.strip mAccessToken <|> duAccessToken <|> fmap (T.strip . T.pack) mEnvToken
  accessToken <- maybe (throwError err400 { errBody = "X-Goog-Access-Token requerido" }) pure tokenTxt
  mFolderEnv <- liftIO $ lookupEnv "DRIVE_UPLOAD_FOLDER_ID"
  let folder = duFolderId <|> fmap (T.strip . T.pack) mFolderEnv
      fallbackName = let raw = T.strip (fdFileName duFile) in if T.null raw then Nothing else Just raw
      nameOverride = duName <|> fallbackName
  manager <- liftIO $ newManager tlsManagerSettings
  dto <- liftIO $ uploadToDrive manager accessToken duFile nameOverride folder
  pure dto

countriesServer :: AppM [CountryDTO]
countriesServer = do
  countries <- runDB $ selectList [] [Asc CountryName]
  pure (map toCountryDTO countries)

protectedServer :: AuthedUser -> ServerT ProtectedAPI AppM
protectedServer user =
       partyServer user
  :<|> bookingServer user
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
  :<|> socialServer user
  :<|> adsAdminServer user
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
        _ -> throwError err500 { errBody = "Faltan variables GOOGLE_CLIENT_ID / GOOGLE_CLIENT_SECRET / GOOGLE_REDIRECT_URI" }

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

productionCourseSlug :: Text
productionCourseSlug = "produccion-musical-dic-2025"

productionCoursePrice :: Double
productionCoursePrice = 150

productionCourseCapacity :: Int
productionCourseCapacity = 10

data WhatsAppEnv = WhatsAppEnv
  { waToken        :: Maybe Text
  , waPhoneId      :: Maybe Text
  , waVerifyToken  :: Maybe Text
  , waContactNumber :: Maybe Text
  }

loadWhatsAppEnv :: IO WhatsAppEnv
loadWhatsAppEnv = do
  token   <- firstNonEmptyText ["WHATSAPP_TOKEN", "WA_TOKEN"]
  phoneId <- firstNonEmptyText ["WHATSAPP_PHONE_NUMBER_ID", "WA_PHONE_ID"]
  verify  <- firstNonEmptyText ["WHATSAPP_VERIFY_TOKEN", "WA_VERIFY_TOKEN"]
  contact <- firstNonEmptyText ["COURSE_WHATSAPP_NUMBER", "WHATSAPP_CONTACT_NUMBER", "WA_CONTACT_NUMBER"]
  pure WhatsAppEnv
    { waToken = token
    , waPhoneId = phoneId
    , waVerifyToken = verify
    , waContactNumber = contact
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
  cmsMeta <- loadCourseMetadataFromCMS normalized
  baseMeta <- case cmsMeta <|> courseMetadataFor envConfig (waContactNumber waEnv) normalized of
    Nothing -> throwNotFound "Curso no encontrado"
    Just m  -> pure m
  -- Count all registrations (including pendientes) to show remaining seats.
  countRegs <- runDB $
    count
      [ ME.CourseRegistrationCourseSlug ==. normalizeSlug (Courses.slug baseMeta)
      , ME.CourseRegistrationStatus !=. "cancelled"
      ]
  let remainingSeats = max 0 (Courses.capacity baseMeta - fromIntegral countRegs)
  pure baseMeta { Courses.remaining = remainingSeats }

loadCourseMetadataFromCMS :: Text -> AppM (Maybe CourseMetadata)
loadCourseMetadataFromCMS slugVal = do
  Env{..} <- ask
  let cmsSlug = "course:" <> slugVal
  mContent <- runDB $ selectFirst
    [ CMS.CmsContentSlug ==. cmsSlug
    , CMS.CmsContentStatus ==. "published"
    ]
    [ Desc CMS.CmsContentPublishedAt
    , Desc CMS.CmsContentVersion
    ]
  case mContent of
    Nothing -> pure Nothing
    Just (Entity _ c) -> do
      let mVal = CMS.unAesonValue <$> CMS.cmsContentPayload c
      case mVal of
        Nothing -> pure Nothing
        Just val -> case fromJSON val of
          Success meta -> pure (Just meta)
          Error err -> do
            liftIO $ hPutStrLn stderr ("[CourseMetadata] Failed to decode CMS payload for " <> T.unpack cmsSlug <> ": " <> err)
            pure Nothing

courseMetadataFor :: AppConfig -> Maybe Text -> Text -> Maybe CourseMetadata
courseMetadataFor cfg mWaContact slugVal
  | slugVal /= productionCourseSlug = Nothing
  | otherwise =
      let landingUrlVal = buildLandingUrl cfg
          whatsappUrl   = buildWhatsappCta mWaContact landingUrlVal
      in Just CourseMetadata
        { slug = productionCourseSlug
        , title = "Curso de Producción Musical"
        , subtitle = "Presencial · 4 sábados · 16 horas"
        , format = "Presencial"
        , duration = "4 sábados · 16 horas"
        , price = productionCoursePrice
        , currency = "USD"
        , capacity = productionCourseCapacity
        , remaining = productionCourseCapacity
        , sessionStartHour = 15  -- 10:00 Quito (UTC-5)
        , sessionDurationHours = 4
        , locationLabel = "TDF Records – Quito"
        , locationMapUrl = "https://maps.app.goo.gl/6pVYZ2CsbvQfGhAz6"
        , daws = ["Logic", "Luna"]
        , includes =
            [ "Acceso a grabaciones"
            , "Certificado de participación"
            , "Mentorías"
            , "Grupo de WhatsApp"
            , "Acceso a la plataforma de TDF Records"
            ]
        , sessions =
            [ CourseSession "Sábado 1 · Introducción" (fromGregorian 2025 12 13)
            , CourseSession "Sábado 2 · Grabación" (fromGregorian 2025 12 20)
            , CourseSession "Sábado 3 · Mezcla" (fromGregorian 2025 12 27)
            , CourseSession "Sábado 4 · Masterización" (fromGregorian 2026 1 3)
            ]
        , syllabus =
            [ SyllabusItem "Primer sábado – Introducción a la producción musical"
                [ "Conceptos básicos"
                , "Herramientas esenciales y software"
                ]
            , SyllabusItem "Segundo sábado – Grabación y captura de audio"
                [ "Técnicas de grabación"
                , "Configuración de micrófonos y espacios"
                ]
            , SyllabusItem "Tercer sábado – Mezcla y edición"
                [ "Ecualización, compresión y efectos"
                , "Técnicas de balance y panoramización"
                ]
            , SyllabusItem "Cuarto sábado – Masterización y publicación"
                [ "Técnicas de masterización"
                , "Preparación para distribución digital"
                ]
            ]
        , whatsappCtaUrl = whatsappUrl
        , landingUrl = landingUrlVal
        }

buildLandingUrl :: AppConfig -> Text
buildLandingUrl cfg =
  let rawBase = fromMaybe "https://tdf-app.pages.dev" (appBaseUrl cfg)
      base = T.dropWhileEnd (== '/') rawBase
  in base <> "/curso/" <> productionCourseSlug

buildWhatsappCta :: Maybe Text -> Text -> Text
buildWhatsappCta mNumber landingUrl =
  let msg = "INSCRIBIRME Curso Produccion Musical " <> landingUrl
      encoded = TE.decodeUtf8 (urlEncode True (TE.encodeUtf8 msg))
      cleanNumber txt = T.filter isDigit txt
  in case mNumber >>= nonEmpty of
       Just num -> "https://wa.me/" <> cleanNumber num <> "?text=" <> encoded
       Nothing  -> "https://wa.me/?text=" <> encoded
  where
    nonEmpty txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

createOrUpdateRegistration :: Text -> CourseRegistrationRequest -> AppM CourseRegistrationResponse
createOrUpdateRegistration rawSlug CourseRegistrationRequest{..} = do
  meta <- loadCourseMetadata rawSlug
  now <- liftIO getCurrentTime
  let slugVal = normalizeSlug (Courses.slug meta)
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
      sendConfirmation meta nameClean normalizedEmail mNewUser
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
      sendConfirmation meta nameClean normalizedEmail mNewUser
      pure CourseRegistrationResponse { id = fromSqlKey regId, status = pendingStatus }
  where
    sendConfirmation meta nameClean mEmail mNewUser =
      case mEmail of
        Nothing -> pure ()
        Just emailAddr -> do
          Env{..} <- ask
          let emailSvc = EmailSvc.mkEmailService envConfig
              displayName = fromMaybe "" nameClean
              CourseMetadata{ title = courseTitle
                            , sessions = metaSessions
                            , landingUrl = landing
                            } = meta
              datesSummary =
                let fmt d = T.pack (formatTime defaultTimeLocale "%d %b %Y" d)
                in T.intercalate ", " (map (fmt . Courses.date) metaSessions)
          -- Check if email is configured before attempting to send
          case EmailSvc.esConfig emailSvc of
            Nothing -> liftIO $ do
              let msg = "[CourseRegistration] WARNING: SMTP not configured. Email confirmation not sent to " <> emailAddr
              hPutStrLn stderr (T.unpack msg)
              LogBuf.addLog LogBuf.LogWarning msg
            Just _ -> liftIO $ do
              -- Send registration confirmation
              result <- try $ EmailSvc.sendCourseRegistration emailSvc displayName emailAddr courseTitle landing datesSummary
              case result of
                Left err -> do
                  let msg = "[CourseRegistration] Failed to send confirmation email to " <> emailAddr <> ": " <> T.pack (show (err :: SomeException))
                  hPutStrLn stderr (T.unpack msg)
                  LogBuf.addLog LogBuf.LogError msg
                Right () -> do
                  let msg = "[CourseRegistration] Successfully sent confirmation email to " <> emailAddr
                  LogBuf.addLog LogBuf.LogInfo msg
              -- Send welcome email if we just created credentials
              for_ mNewUser $ \(username, tempPassword) -> do
                welcomeResult <- try $ EmailSvc.sendWelcome emailSvc displayName emailAddr username tempPassword
                case welcomeResult of
                  Left err -> do
                    let msg = "[CourseRegistration] Failed to send welcome email to " <> emailAddr <> ": " <> T.pack (show (err :: SomeException))
                    hPutStrLn stderr (T.unpack msg)
                    LogBuf.addLog LogBuf.LogError msg
                  Right () -> do
                    let msg = "[CourseRegistration] Sent welcome credentials to " <> emailAddr
                    LogBuf.addLog LogBuf.LogInfo msg

-- | Returns messages that include text bodies, paired with the sender phone.
extractTextMessages :: WAMetaWebhook -> [(Text, Text)]
extractTextMessages WAMetaWebhook{entry} =
  [ (WA.from msg, body)
  | WAEntry{changes} <- entry
  , WAChange{value=WAValue{messages=Just msgs}} <- changes
  , msg@WAMessage{waType, text=Just txtBody} <- msgs
  , waType == "text"
  , let body = WA.body txtBody
  ]

sendWhatsappReply :: WhatsAppEnv -> Text -> AppM ()
sendWhatsappReply WhatsAppEnv{waToken = Just tok, waPhoneId = Just pid} phone = do
  meta <- loadCourseMetadata productionCourseSlug
  manager <- liftIO $ newManager tlsManagerSettings
  let msg = "¡Gracias por tu interés en el Curso de Producción Musical! Aquí tienes el link de inscripción: "
            <> landingUrl meta <> ". Cupos limitados (" <> T.pack (show productionCourseCapacity) <> ")."
  _ <- liftIO $ sendText manager tok pid phone msg
  pure ()
sendWhatsappReply _ _ = pure ()

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
  } = do
  let emailClean    = T.strip rawEmail
      passwordClean = T.strip rawPassword
      firstClean    = T.strip rawFirst
      lastClean     = T.strip rawLast
      phoneClean    = fmap T.strip rawPhone
      claimArtistIdClean = rawClaimArtistId >>= (\val -> if val > 0 then Just val else Nothing)
      displayNameText =
        case filter (not . T.null) [firstClean, lastClean] of
          [] -> emailClean
          xs -> T.unwords xs
  when (T.null emailClean) $ throwBadRequest "Email is required"
  when (T.null passwordClean) $ throwBadRequest "Password is required"
  when (T.length passwordClean < 8) $ throwBadRequest "Password must be at least 8 characters"
  when (T.null firstClean && T.null lastClean) $ throwBadRequest "First or last name is required"
  now <- liftIO getCurrentTime
  Env pool cfg <- ask
  let services = Services.buildServices cfg
      emailSvc = Services.emailService services
      allowedRoles = maybe [] (filter (`elem` signupAllowedRoles)) requestedRoles
      sanitizedRoles = nub (Customer : Fan : allowedRoles)
      sanitizedFanArtists = maybe [] (filter (> 0)) requestedFanArtistIds
  result <- liftIO $ flip runSqlPool pool $
    runSignupDb emailClean passwordClean displayNameText phoneClean sanitizedRoles sanitizedFanArtists claimArtistIdClean now
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
      -> UTCTime
      -> SqlPersistT IO (Either SignupDbError LoginResponse)
    runSignupDb emailVal passwordVal displayNameText phoneVal rolesVal fanArtistIdsVal mClaimArtistId nowVal = do
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
  let services = Services.buildServices cfg
      emailSvc = Services.emailService services
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
      in T.stripPrefix prefix lbl >>= nonEmptyText

    nonEmptyText :: Text -> Maybe Text
    nonEmptyText txt
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
  unless (Artist `elem` auRoles || Admin `elem` auRoles) $
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
    partyById pid = getParty user pid :<|> updateParty user pid :<|> addRole user pid

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

-- Bookings
bookingPublicServer :: ServerT Api.BookingPublicAPI AppM
bookingPublicServer = createPublicBooking

bookingServer :: AuthedUser -> ServerT BookingAPI AppM
bookingServer user =
       listBookings user
  :<|> createBooking user
  :<|> updateBooking user

listBookings :: AuthedUser -> AppM [BookingDTO]
listBookings user = do
  requireModule user ModuleScheduling
  Env pool cfg <- ask
  liftIO $ do
    dbBookings <- flip runSqlPool pool $ do
      bookings <- selectList [] [Desc BookingId]
      buildBookingDTOs bookings
    let courseSessions = courseCalendarBookings cfg
    pure (dbBookings ++ courseSessions)

courseCalendarBookings :: AppConfig -> [BookingDTO]
courseCalendarBookings cfg =
  case courseMetadataFor cfg Nothing productionCourseSlug of
    Nothing   -> []
    Just meta ->
      zipWith (mkBooking meta) ([1..] :: [Int]) (Courses.sessions meta)
  where
    mkBooking CourseMetadata{..} idx CourseSession{..} =
      let startUtc = UTCTime date (secondsToDiffTime (fromIntegral sessionStartHour * 60 * 60)) -- hour in UTC, calendar applies TZ
          endUtc   = addUTCTime (fromIntegral sessionDurationHours * 60 * 60) startUtc
      in BookingDTO
           { bookingId          = negate (fromIntegral (1000 + idx))
           , title              = "Curso: " <> label
           , startsAt           = startUtc
           , endsAt             = endUtc
           , status             = "Confirmed"
           , notes              = Just ("Curso " <> slug <> " · USD " <> T.pack (show (round price :: Int)))
           , partyId            = Nothing
           , engineerPartyId    = Nothing
           , engineerName       = Nothing
           , serviceType        = Just "Curso Producción Musical"
           , serviceOrderId     = Nothing
           , serviceOrderTitle  = Nothing
           , customerName       = Nothing
           , partyDisplayName   = Nothing
            , resources          = []
            , courseSlug         = Just slug
            , coursePrice        = Just price
            , courseCapacity     = Just capacity
            , courseRemaining    = Just remaining
            , courseLocation     = Just locationLabel
           }

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
    resolveResourcesForBooking serviceTypeClean [] pbStartsAt endsAt
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
            missingEngineer = requiresEngineer (bookingServiceType updated)
              && isNothing (bookingEngineerPartyId updated)
              && isNothing (bookingEngineerName updated)
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
resolveRequestedResources ids = fmap catMaybes $ mapM lookupResource ids
  where
    lookupResource :: Text -> SqlPersistT IO (Maybe (Key Resource))
    lookupResource rid =
      case fromPathPiece rid of
        Nothing  -> pure Nothing
        Just key -> do
          mRes <- get key
          case mRes of
            Just res | resourceResourceType res == Room && resourceActive res -> pure (Just key)
            _        -> pure Nothing

defaultResourcesForService :: Maybe Text -> UTCTime -> UTCTime -> SqlPersistT IO [Key Resource]
defaultResourcesForService Nothing _ _ = pure []
defaultResourcesForService (Just service) start end = do
  let normalized = T.toLower (T.strip service)
  rooms <- selectList [ResourceResourceType ==. Room, ResourceActive ==. True] [Asc ResourceId]
  let findByName name = find (\(Entity _ room) -> T.toLower (resourceName room) == T.toLower name) rooms
      boothPredicate (Entity _ room) = "booth" `T.isInfixOf` T.toLower (resourceName room)
  case normalized of
    "band recording" ->
      pure $ map entityKey $ catMaybes (map findByName ["Live Room", "Control Room"])
    "vocal recording" ->
      let vocal = findByName "Vocal Booth" <|> find (\(Entity _ room) -> let lower = T.toLower (resourceName room) in "vocal" `T.isInfixOf` lower || "booth" `T.isInfixOf` lower) rooms
          control = findByName "Control Room"
      in pure $ map entityKey $ catMaybes [vocal, control]
    "band rehearsal" ->
      pure $ maybe [] (pure . entityKey) (findByName "Live Room")
    "dj booth rental" -> do
      let candidateNames = ["Booth 1","Booth 2","Booth A","Booth B","DJ Booth 1","DJ Booth 2"]
          nameMatches = mapMaybe findByName candidateNames
          boothMatches = filter boothPredicate rooms
          candidates = dedupeEntities (nameMatches ++ boothMatches)
      pickBooth candidates
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

adsInquiryPublic :: ServerT AdsPublicAPI AppM
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

adsAdminServer :: AuthedUser -> ServerT AdsAdminAPI AppM
adsAdminServer user = do
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
  let ctaBase = fromMaybe "https://tdf-app.pages.dev" (appBaseUrl cfg)
      cta = ctaBase <> "/trials"
      courseLabel = fromMaybe "las clases 1:1" mCourse
      body = T.intercalate "\n"
        [ "Hola " <> fromMaybe "" aiName <> " 🙌"
        , "Gracias por tu interés en " <> courseLabel <> "."
        , "Paquete 1:1 (16 horas): $480."
        , "Grupo pequeño: más info y fechas en https://tdf-app.pages.dev/curso/produccion-musical-dic-2025"
        , "¿Te agendo una clase de prueba gratis para ver horarios y profesor ideal?"
        , "Confírmame tu disponibilidad y ciudad. Agendamos aquí: " <> cta
        ]
  channels <- fmap catMaybes . sequence $
    [ case (waToken wa, waPhoneId wa, aiPhone >>= normalizePhone) of
        (Just tok, Just pid, Just ph) ->
          do res <- sendText mgr tok pid ph body
             pure (either (const Nothing) (const (Just "whatsapp")) res)
        _ -> pure Nothing
    , case aiEmail of
        Just e -> do
          let svc = EmailSvc.mkEmailService cfg
          EmailSvc.sendTestEmail svc (fromMaybe "Amigo TDF" aiName) e "Gracias por tu interés en TDF"
            [ "Paquete 1:1 (16 horas): $480."
            , "Grupo pequeño: detalles y fechas en https://tdf-app.pages.dev/curso/produccion-musical-dic-2025"
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

labelPublicServer :: ServerT LabelAPI AppM
labelPublicServer = listLabelTracks :<|> createLabelTrack :<|> updateLabelTrack :<|> deleteLabelTrack

contractsServer :: ServerT ContractsAPI AppM
contractsServer = hoistServer contractsProxy lift Contracts.server
  where
    contractsProxy = Proxy :: Proxy ContractsAPI

listMarketplace :: AppM [MarketplaceItemDTO]
listMarketplace = do
  Env{..} <- ask
  let loadListings = do
        listings <- selectList [ME.MarketplaceListingActive ==. True] [Asc ME.MarketplaceListingTitle]
        forM listings $ \(Entity lid listing) -> do
          mAsset <- get (ME.marketplaceListingAssetId listing)
          pure (lid, listing, mAsset)
  rows <- liftIO $ flip runSqlPool envPool loadListings
  if not (null rows)
    then pure (mapMaybe toMarketplaceDTO rows)
    else do
      -- Auto-publish demo inventory so the public marketplace is never empty.
      liftIO $ flip runSqlPool envPool $ do
        seedInventoryAssets
        seedMarketplaceListings
      seeded <- liftIO $ flip runSqlPool envPool loadListings
      pure (mapMaybe toMarketplaceDTO seeded)

getMarketplaceItem :: Text -> AppM MarketplaceItemDTO
getMarketplaceItem rawId = do
  listingKey <- case fromPathPiece rawId of
    Nothing -> throwBadRequest "Invalid listing id"
    Just k  -> pure k
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ do
    mListing <- get listingKey
    case mListing of
      Nothing -> pure Nothing
      Just listing -> do
        mAsset <- get (ME.marketplaceListingAssetId listing)
        pure (toMarketplaceDTO (listingKey, listing, mAsset))
  maybe (throwError err404) pure mDto

toMarketplaceDTO
  :: (Key ME.MarketplaceListing, ME.MarketplaceListing, Maybe ME.Asset)
  -> Maybe MarketplaceItemDTO
toMarketplaceDTO (_, _, Nothing) = Nothing
toMarketplaceDTO (lid, listing, Just asset) =
  Just MarketplaceItemDTO
    { miListingId      = toPathPiece lid
    , miAssetId        = toPathPiece (ME.marketplaceListingAssetId listing)
    , miTitle          = ME.marketplaceListingTitle listing
    , miPurpose        = ME.marketplaceListingPurpose listing
    , miCategory       = ME.assetCategory asset
    , miBrand          = ME.assetBrand asset
    , miModel          = ME.assetModel asset
    , miPhotoUrl       = ME.assetPhotoUrl asset
    , miStatus         = Just (assetStatusLabel (ME.assetStatus asset))
    , miCondition      = Just (assetConditionLabel (ME.assetCondition asset))
    , miPriceUsdCents  = ME.marketplaceListingPriceUsdCents listing
    , miPriceDisplay   = formatUsd (ME.marketplaceListingPriceUsdCents listing) (ME.marketplaceListingCurrency listing)
    , miMarkupPct      = ME.marketplaceListingMarkupPct listing
    , miCurrency       = ME.marketplaceListingCurrency listing
    }

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

listLabelTracks :: AppM [LabelTrackDTO]
listLabelTracks = do
  Env{..} <- ask
  rows <- liftIO $ flip runSqlPool envPool $ selectList [] [Desc ME.LabelTrackCreatedAt]
  pure (map toLabelTrackDTO rows)

createLabelTrack :: LabelTrackCreate -> AppM LabelTrackDTO
createLabelTrack LabelTrackCreate{..} = do
  when (T.null (T.strip ltcTitle)) $ throwBadRequest "Título requerido"
  now <- liftIO getCurrentTime
  Env{..} <- ask
  let record = ME.LabelTrack
        { ME.labelTrackTitle     = T.strip ltcTitle
        , ME.labelTrackNote      = T.strip <$> ltcNote
        , ME.labelTrackStatus    = "open"
        , ME.labelTrackCreatedAt = now
        , ME.labelTrackUpdatedAt = now
        }
  dto <- liftIO $ flip runSqlPool envPool $ do
    key <- insert record
    pure (toLabelTrackDTO (Entity key record))
  pure dto

updateLabelTrack :: Text -> LabelTrackUpdate -> AppM LabelTrackDTO
updateLabelTrack rawId LabelTrackUpdate{..} = do
  key <- case (fromPathPiece rawId :: Maybe (Key ME.LabelTrack)) of
    Nothing -> throwBadRequest "Invalid track id"
    Just k  -> pure k
  now <- liftIO getCurrentTime
  Env{..} <- ask
  mDto <- liftIO $ flip runSqlPool envPool $ do
    mTrack <- get key
    case mTrack of
      Nothing -> pure Nothing
      Just _ -> do
        let updates = catMaybes
              [ (ME.LabelTrackTitle =.) . T.strip <$> ltuTitle
              , case ltuNote of
                  Nothing -> Nothing
                  Just n  -> Just (ME.LabelTrackNote =. if T.null (T.strip n) then Nothing else Just (T.strip n))
              , (ME.LabelTrackStatus =.) <$> ltuStatus
              , Just (ME.LabelTrackUpdatedAt =. now)
              ]
        unless (null updates) (update key updates)
        track' <- getJustEntity key
        pure (Just (toLabelTrackDTO track'))
  maybe (throwError err404) pure mDto

deleteLabelTrack :: Text -> AppM NoContent
deleteLabelTrack rawId = do
  key <- case (fromPathPiece rawId :: Maybe (Key ME.LabelTrack)) of
    Nothing -> throwBadRequest "Invalid track id"
    Just k  -> pure k
  Env{..} <- ask
  deleted <- liftIO $ flip runSqlPool envPool $ do
    mTrack <- get key
    case mTrack of
      Nothing -> pure False
      Just _  -> delete key >> pure True
  unless deleted (throwError err404)
  pure NoContent

toLabelTrackDTO :: Entity ME.LabelTrack -> LabelTrackDTO
toLabelTrackDTO (Entity key t) =
  LabelTrackDTO
    { ltId        = toPathPiece key
    , ltTitle     = ME.labelTrackTitle t
    , ltNote      = ME.labelTrackNote t
    , ltStatus    = ME.labelTrackStatus t
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
  } deriving (Show, Generic)

instance FromJSON DriveApiResp where
  parseJSON = withObject "DriveApiResp" $ \o ->
    DriveApiResp <$> o .: "id"
                 <*> o .:? "webViewLink"
                 <*> o .:? "webContentLink"

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

  req0 <- parseRequest "https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart"
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
  when (statusCode (responseStatus resp) >= 400) $
    fail ("Drive upload failed with status " <> show (statusCode (responseStatus resp)))
  driveResp <- case eitherDecode (responseBody resp) of
    Left err -> fail ("No pudimos interpretar la respuesta de Drive: " <> err)
    Right ok -> pure (ok :: DriveApiResp)

  -- Best-effort: make the file public.
  let permBody = encode (object ["role" .= ("reader" :: Text), "type" .= ("anyone" :: Text)])
  permReq0 <- parseRequest $ "https://www.googleapis.com/drive/v3/files/" <> T.unpack (darId driveResp) <> "/permissions"
  let permReq = permReq0
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", bearer)
            , ("Content-Type", "application/json")
            ]
        , requestBody = RequestBodyLBS permBody
        }
  _ <- (try (httpLbs permReq manager) :: IO (Either SomeException (Response BL.ByteString)))

  let publicUrl = darWebViewLink driveResp <|> darWebContentLink driveResp
  pure DriveUploadDTO
    { duFileId = darId driveResp
    , duWebViewLink = darWebViewLink driveResp
    , duWebContentLink = darWebContentLink driveResp
    , duPublicUrl = publicUrl
    }
