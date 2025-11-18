{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TDF.Server where

import           Control.Applicative ((<|>))
import           Control.Monad (forM, forM_, void, when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.Int (Int64)
import           Data.List (find, foldl', nub)
import           Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Aeson (Value, object, (.=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import           Data.Time (Day, UTCTime, fromGregorian, getCurrentTime, toGregorian, utctDay)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import           Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import           Network.Wai (Request)
import           Servant
import           Servant.Server.Experimental.Auth (AuthHandler)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import           Web.PathPieces (fromPathPiece, toPathPiece)
import           Data.Proxy (Proxy (..))

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Postgresql ()

import           TDF.API
import           TDF.API.Types (RolePayload(..), UserRoleSummaryDTO(..), UserRoleUpdatePayload(..), AccountStatusDTO(..))
import qualified TDF.API      as Api
import           TDF.Config (AppConfig(..))
import           TDF.DB
import           TDF.Models
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME
import           TDF.DTO
import           TDF.Auth (AuthedUser(..), ModuleAccess(..), authContext, hasModuleAccess, moduleName, loadAuthedUser)
import           TDF.Seed       (seedAll)
import           TDF.ServerAdmin (adminServer)
import           TDF.ServerExtra (bandsServer, inventoryServer, loadBandForParty, pipelinesServer, roomsServer, sessionsServer)
import           TDF.ServerFuture (futureServer)
import           TDF.ServerLiveSessions (liveSessionsServer)
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

type AppM = ReaderT Env Handler

type CombinedAPI = TrialsAPI :<|> API

mkApp :: Env -> Application
mkApp env =
  let apiProxy = Proxy :: Proxy API
      combinedProxy = Proxy :: Proxy CombinedAPI
      ctxProxy = Proxy :: Proxy '[AuthHandler Request AuthedUser]
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
  :<|> signup
  :<|> changePassword
  :<|> authV1Server
  :<|> fanPublicServer
  :<|> metaServer
  :<|> seedTrigger
  :<|> inputListServer
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
  let encode = BL.fromStrict . TE.encodeUtf8
      missingHeader = throwError err401 { errBody = encode "Missing X-Seed-Token header" }
      disabled = throwError err403 { errBody = encode "Seeding endpoint disabled" }
      invalid = throwError err403 { errBody = encode "Invalid seed token" }
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

fanSecureServer :: AuthedUser -> ServerT FanSecureAPI AppM
fanSecureServer user =
       (fanGetProfile user :<|> fanUpdateProfile user)
  :<|> (fanListFollows user :<|> fanFollowArtist user :<|> fanUnfollowArtist user)
  :<|> (artistGetOwnProfile user :<|> artistUpdateOwnProfile user)

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
  :<|> futureServer

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
  deriving (Eq, Show)

signup :: SignupRequest -> AppM LoginResponse
signup SignupRequest
  { firstName = rawFirst
  , lastName = rawLast
  , email = rawEmail
  , phone = rawPhone
  , password = rawPassword
  } = do
  let emailClean    = T.strip rawEmail
      passwordClean = T.strip rawPassword
      firstClean    = T.strip rawFirst
      lastClean     = T.strip rawLast
      phoneClean    = fmap T.strip rawPhone
      displayName =
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
  result <- liftIO $ flip runSqlPool pool $
    runSignupDb emailClean passwordClean displayName phoneClean now
  case result of
    Left SignupEmailExists ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Account already exists for this email") }
    Left SignupProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Right resp -> do
      liftIO $ EmailSvc.sendWelcome emailSvc displayName emailClean emailClean passwordClean
      pure resp
  where
    runSignupDb
      :: Text
      -> Text
      -> Text
      -> Maybe Text
      -> UTCTime
      -> SqlPersistT IO (Either SignupDbError LoginResponse)
    runSignupDb emailVal passwordVal displayName phoneVal nowVal = do
      existing <- getBy (UniqueCredentialUsername emailVal)
      case existing of
        Just _  -> pure (Left SignupEmailExists)
        Nothing -> do
          let partyRecord = Party
                { partyLegalName        = Nothing
                , partyDisplayName      = displayName
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
          _ <- upsert (PartyRole pid Customer True) [PartyRoleActive =. True]
          _ <- upsert (PartyRole pid Fan True) [PartyRoleActive =. True]
          hashed <- liftIO (hashPasswordText passwordVal)
          _ <- insert UserCredential
            { userCredentialPartyId      = pid
            , userCredentialUsername     = emailVal
            , userCredentialPasswordHash = hashed
            , userCredentialActive       = True
            }
          insert_ FanProfile
            { fanProfileFanPartyId     = pid
            , fanProfileDisplayName    = Just displayName
            , fanProfileAvatarUrl      = Nothing
            , fanProfileFavoriteGenres = Nothing
            , fanProfileBio            = Nothing
            , fanProfileCity           = Nothing
            , fanProfileCreatedAt      = nowVal
            , fanProfileUpdatedAt      = Nothing
            }
          token <- createSessionToken pid emailVal
          mUser <- loadAuthedUser token
          case mUser of
            Nothing   -> pure (Left SignupProfileError)
            Just user -> pure (Right (toLoginResponse token user))

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

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing   -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

metaServer :: ServerT Meta.MetaAPI AppM
metaServer = hoistServer metaProxy lift Meta.metaServer
  where
    metaProxy = Proxy :: Proxy Meta.MetaAPI

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
bookingServer :: AuthedUser -> ServerT BookingAPI AppM
bookingServer user =
       listBookings user
  :<|> createBooking user
  :<|> updateBooking user

listBookings :: AuthedUser -> AppM [BookingDTO]
listBookings user = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    bookings <- selectList [] [Desc BookingId]
    buildBookingDTOs bookings

createBooking :: AuthedUser -> CreateBookingReq -> AppM BookingDTO
createBooking user req = do
  requireModule user ModuleScheduling
  Env pool _ <- ask
  now <- liftIO getCurrentTime

  let status'          = parseStatusWithDefault Confirmed (cbStatus req)
      serviceTypeClean = normalizeOptionalInput (cbServiceType req)
      partyKey         = fmap (toSqlKey . fromIntegral) (cbPartyId req)
      requestedRooms   = fromMaybe [] (cbResourceIds req)

  resourceKeys <- liftIO $ flip runSqlPool pool $
    resolveResourcesForBooking serviceTypeClean requestedRooms (cbStartsAt req) (cbEndsAt req)

  let bookingRecord = Booking
        { bookingTitle          = cbTitle req
        , bookingServiceOrderId = Nothing
        , bookingPartyId        = partyKey
        , bookingServiceType    = serviceTypeClean
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
          , serviceType = bookingServiceType bookingRecord
          , serviceOrderId = Nothing
          , serviceOrderTitle = Nothing
          , customerName = Nothing
          , partyDisplayName = Nothing
          , resources   = []
          }
    pure (headDef fallback dtos)
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
      Nothing -> pure Nothing
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
              }
        replace bookingId updated
        dtos <- buildBookingDTOs [Entity bookingId updated]
        pure (listToMaybe dtos)
  maybe (throwError err404) pure result


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
        , serviceType = bookingServiceType b
        , serviceOrderId    = fmap fromSqlKey (bookingServiceOrderId b)
        , serviceOrderTitle = serviceOrderTitleText <|> fallbackOrderTitle
        , customerName      = customerNameText <|> fallbackCustomer
        , partyDisplayName  = partyDisplay
        , resources   = resources
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
    pickBooth (Entity key room : rest) = do
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
            lines <- selectList [ReceiptLineReceiptId ==. entityKey receiptEnt] [Asc ReceiptLineId]
            pure (Right (receiptToDTO receiptEnt lines))
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
        lines <- selectList [ReceiptLineReceiptId ==. rid] [Asc ReceiptLineId]
        pure (Just (receiptToDTO rec lines))
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
invoiceToDTO (Entity iid inv) lines mReceiptKey = InvoiceDTO
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
  , lineItems  = map invoiceLineToDTO lines
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
receiptToDTO (Entity rid rec) lines = ReceiptDTO
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
  , lineItems     = map receiptLineToDTO lines
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
