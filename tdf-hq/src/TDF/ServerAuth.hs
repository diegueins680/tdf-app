{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ServerAuth
  ( sessionServer
  , login
  , googleLogin
  , signup
  , changePassword
  , passwordReset
  , passwordResetConfirm
  , authV1Server
  , resolvePasswordResetDelivery
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, withObject, (.:), (.:?))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (Day, UTCTime, getCurrentTime)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity (..), SelectOpt (Asc), get, getBy, getEntity, insert, insert_, insertBy, insertUnique, selectFirst, selectList, update, upsert, (=.), (==.))
import Database.Persist.Sql (fromSqlKey, rawSql, runSqlPool, toSqlKey, SqlPersistT)
import Database.Persist.Types (PersistValue (PersistBool, PersistText))
import Network.HTTP.Client (Manager, Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Servant
import System.IO (hPutStrLn, stderr)

import qualified TDF.API as Api
import TDF.Auth (
    AuthedUser (..),
    clearSessionCookieHeader,
    loadAuthedUser,
    lookupUsernameFromToken,
    moduleName,
    resolveUsernameFromLabel,
    sessionCookieHeader,
  )
import TDF.Config (AppConfig (..))
import TDF.DB (Env (..))
import TDF.DTO
import qualified TDF.Email.Service as EmailSvc
import qualified TDF.LogBuffer as LogBuf
import TDF.Models
import qualified TDF.Models as M
import qualified TDF.ModelsExtra as ME

type AppM = ReaderT Env Handler

data GoogleIdTokenInfo = GoogleIdTokenInfo
  { gitAud :: Text
  , gitEmail :: Text
  , gitEmailVerified :: Bool
  , gitName :: Maybe Text
  , gitPicture :: Maybe Text
  , gitSub :: Text
  , gitIss :: Maybe Text
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
          Just (Bool b) -> b
          Just (String t) ->
            let lowered = T.toLower (T.strip t)
            in lowered == "true" || lowered == "1"
          _ -> False

data GoogleProfile = GoogleProfile
  { gpEmail :: Text
  , gpName :: Maybe Text
  , gpPicture :: Maybe Text
  } deriving (Show)

data SignupDbError
  = SignupEmailExists
  | SignupProfileError
  | SignupArtistUnavailable
  deriving (Eq, Show)

data PasswordChangeError
  = PasswordInvalid
  | PasswordAccountDisabled
  | PasswordProfileError
  deriving (Eq, Show)

data PasswordResetError
  = PasswordResetInvalidToken
  | PasswordResetAccountDisabled
  | PasswordResetProfileError
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

sessionServer :: AuthedUser -> ServerT Api.SessionAPI AppM
sessionServer user =
       currentSession user
  :<|> logoutSession

authV1Server :: ServerT Api.AuthV1API AppM
authV1Server = signup :<|> passwordReset :<|> passwordResetConfirm :<|> changePassword

currentSession :: AuthedUser -> AppM SessionResponse
currentSession AuthedUser{..} = do
  Env pool _ <- ask
  liftIO $ flip runSqlPool pool $ do
    mParty <- get auPartyId
    mCredential <- selectFirst [UserCredentialPartyId ==. auPartyId, UserCredentialActive ==. True] [Asc UserCredentialId]
    let fallbackUsername = "party-" <> T.pack (show (fromSqlKey auPartyId))
        usernameText = maybe fallbackUsername (userCredentialUsername . entityVal) mCredential
        displayNameText = fromMaybe usernameText (cleanOptional (M.partyDisplayName <$> mParty))
    pure SessionResponse
      { sessionUsername = usernameText
      , sessionDisplayName = displayNameText
      , sessionPartyId = fromSqlKey auPartyId
      , sessionRoles = auRoles
      , sessionModules = map moduleName (Set.toList auModules)
      }

logoutSession :: AppM (Api.SessionCookieHeaders NoContent)
logoutSession = do
  cfg <- asks envConfig
  pure (addHeader (clearSessionCookieHeader cfg) NoContent)

withSessionCookie :: LoginResponse -> AppM (Api.SessionCookieHeaders LoginResponse)
withSessionCookie response@LoginResponse{token = sessionToken} = do
  cfg <- asks envConfig
  pure (addHeader (sessionCookieHeader cfg sessionToken) response)

login :: LoginRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
login LoginRequest{..} = do
  Env pool _ <- ask
  result <- liftIO $ flip runSqlPool pool (runLogin username password)
  case result of
    Left msg -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
    Right res -> withSessionCookie res

googleLogin :: GoogleLoginRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
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
        Right resp -> withSessionCookie resp

signup :: SignupRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
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
  let emailClean = T.strip rawEmail
      passwordClean = T.strip rawPassword
      firstClean = T.strip rawFirst
      lastClean = T.strip rawLast
      phoneClean = fmap T.strip rawPhone
      claimArtistIdClean = rawClaimArtistId >>= (\val -> if val > 0 then Just val else Nothing)
      internshipSkillsClean = cleanOptional rawInternshipSkills
      internshipAreasClean = cleanOptional rawInternshipAreas
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
    runSignupDb
      emailClean
      passwordClean
      displayNameText
      phoneClean
      sanitizedRoles
      sanitizedFanArtists
      claimArtistIdClean
      rawInternshipStartAt
      rawInternshipEndAt
      rawInternshipRequiredHours
      internshipSkillsClean
      internshipAreasClean
      now
  case result of
    Left SignupEmailExists ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Account already exists for this email") }
    Left SignupArtistUnavailable ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Artist profile is not available to claim") }
    Left SignupProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Right resp -> do
      welcomeResult <-
        liftIO $
          ((try $
            EmailSvc.sendWelcome emailSvc displayNameText emailClean emailClean passwordClean) :: IO (Either SomeException ()))
      case welcomeResult of
        Left err -> do
          let msg = "[Signup] Account created but welcome email failed for " <> emailClean <> ": " <> T.pack (displayException err)
          liftIO $ do
            hPutStrLn stderr (T.unpack msg)
            LogBuf.addLog LogBuf.LogWarning msg
        Right () -> pure ()
      withSessionCookie resp

changePassword :: Maybe Text -> ChangePasswordRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
changePassword mAuthHeader ChangePasswordRequest{..} = do
  let currentPasswordClean = T.strip currentPassword
      newPasswordClean = T.strip newPassword
      maybeUsernameClean = T.strip <$> username
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
    Right resp -> withSessionCookie resp
  where
    resolveUsername pool mUsername header =
      case mUsername of
        Just uname | not (T.null uname) -> pure uname
        _ -> do
          tokenValue <- case header >>= parseBearer . T.strip of
            Nothing -> throwBadRequest "Username is required"
            Just tok -> pure tok
          mResolved <- liftIO $ flip runSqlPool pool (lookupUsernameFromToken tokenValue)
          case fmap T.strip mResolved of
            Nothing ->
              throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 "Invalid or inactive session token") }
            Just uname' -> pure uname'

    parseBearer headerText =
      case T.words headerText of
        [scheme, value]
          | T.toLower scheme == "bearer" -> Just value
        [value] -> Just value
        _ -> Nothing

passwordReset :: PasswordResetRequest -> AppM NoContent
passwordReset PasswordResetRequest{..} = do
  let emailClean = T.strip email
  when (T.null emailClean) $ throwBadRequest "Email is required"
  Env pool cfg <- ask
  let emailSvc = EmailSvc.mkEmailService cfg
  mPayload <- liftIO $ flip runSqlPool pool (runPasswordReset emailClean)
  for_ mPayload $ \(resetToken, displayName, recipientEmail) -> do
    resetResult <-
      liftIO $
        ((try $
          EmailSvc.sendPasswordReset emailSvc displayName recipientEmail resetToken) :: IO (Either SomeException ()))
    case resetResult of
      Left err -> do
        let msg = "[PasswordReset] Failed to email reset link to " <> recipientEmail <> ": " <> T.pack (displayException err)
        liftIO $ do
          hPutStrLn stderr (T.unpack msg)
          LogBuf.addLog LogBuf.LogWarning msg
      Right () -> pure ()
  pure NoContent
  where
    runPasswordReset :: Text -> SqlPersistT IO (Maybe (Text, Text, Text))
    runPasswordReset emailVal = do
      mDelivery <- resolvePasswordResetDelivery emailVal
      case mDelivery of
        Nothing -> pure Nothing
        Just (Entity _ cred, recipientEmail, displayName) -> do
          deactivatePasswordResetTokens (userCredentialPartyId cred)
          resetToken <- createPasswordResetToken (userCredentialPartyId cred) recipientEmail
          pure (Just (resetToken, displayName, recipientEmail))

passwordResetConfirm :: PasswordResetConfirmRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
passwordResetConfirm PasswordResetConfirmRequest{..} = do
  let tokenClean = T.strip token
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
    Right resp -> withSessionCookie resp

resolvePasswordResetDelivery :: Text -> SqlPersistT IO (Maybe (Entity UserCredential, Text, Text))
resolvePasswordResetDelivery rawEmail = do
  let emailQuery = T.strip rawEmail
      query =
        "SELECT ?? FROM user_credential \
        \ JOIN party ON user_credential.party_id = party.id \
        \ WHERE user_credential.active = ? \
        \   AND lower(trim(party.primary_email)) = lower(?) \
        \ ORDER BY user_credential.id ASC \
        \ LIMIT 1"
  if T.null emailQuery
    then pure Nothing
    else do
      creds <- rawSql query [PersistBool True, PersistText emailQuery]
      case listToMaybe creds of
        Nothing -> pure Nothing
        Just cred@(Entity _ credential) -> do
          mParty <- get (userCredentialPartyId credential)
          let mRecipientEmail = mParty >>= cleanOptional . M.partyPrimaryEmail
              displayName = maybe emailQuery M.partyDisplayName mParty
          pure ((\recipientEmail -> (cred, recipientEmail, displayName)) <$> mRecipientEmail)

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
              normalizedName = cleanOptional (gitName info)
              profile = GoogleProfile
                { gpEmail = normalizedEmail
                , gpName = normalizedName <|> Just normalizedEmail
                , gpPicture = gitPicture info
                }
           in Right profile

issuerAllowed :: Maybe Text -> Bool
issuerAllowed Nothing = False
issuerAllowed (Just issRaw) =
  let issuer = T.toLower (T.strip issRaw)
  in issuer == "accounts.google.com" || issuer == "https://accounts.google.com"

completeGoogleLogin :: GoogleProfile -> SqlPersistT IO (Either Text LoginResponse)
completeGoogleLogin GoogleProfile{..} = do
  mExisting <- lookupByEmail gpEmail
  case mExisting of
    Just (Entity _ cred)
      | not (userCredentialActive cred) ->
          pure (Left "Cuenta deshabilitada. Contacta a soporte.")
      | otherwise -> do
          sessionToken <- createTokenWithLabel (userCredentialPartyId cred) (Just ("google-login:" <> gpEmail))
          mUser <- loadAuthedUser sessionToken
          case mUser of
            Nothing -> pure (Left "No pudimos cargar tu perfil.")
            Just user -> pure (Right (toLoginResponse sessionToken user))
    Nothing -> do
      now <- liftIO getCurrentTime
      let displayName = fromMaybe gpEmail (cleanOptional gpName)
          partyRecord = Party
            { partyLegalName = Nothing
            , partyDisplayName = displayName
            , partyIsOrg = False
            , partyTaxId = Nothing
            , partyPrimaryEmail = Just gpEmail
            , partyPrimaryPhone = Nothing
            , partyWhatsapp = Nothing
            , partyInstagram = Nothing
            , partyEmergencyContact = Nothing
            , partyNotes = Nothing
            , partyCreatedAt = now
            }
      pid <- insert partyRecord
      applyRoles pid [Customer, Fan]
      ensureFanProfileIfMissing pid displayName now
      tempPassword <- liftIO generateTemporaryPassword
      hashed <- liftIO (hashPasswordText tempPassword)
      _ <- insert UserCredential
        { userCredentialPartyId = pid
        , userCredentialUsername = gpEmail
        , userCredentialPasswordHash = hashed
        , userCredentialActive = True
        }
      sessionToken <- createTokenWithLabel pid (Just ("google-login:" <> gpEmail))
      mUser <- loadAuthedUser sessionToken
      case mUser of
        Nothing -> pure (Left "No pudimos cargar tu perfil.")
        Just user -> pure (Right (toLoginResponse sessionToken user))

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
              sessionToken <- createSessionToken (userCredentialPartyId cred) (userCredentialUsername cred)
              mUser <- loadAuthedUser sessionToken
              case mUser of
                Nothing -> pure (Left "Failed to load user profile")
                Just user -> pure (Right (toLoginResponse sessionToken user))
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
        Nothing -> lookupByEmail trimmed

lookupByEmail :: Text -> SqlPersistT IO (Maybe (Entity UserCredential))
lookupByEmail emailAddress = do
  let query =
        "SELECT ?? FROM user_credential \
        \ JOIN party ON user_credential.party_id = party.id \
        \ WHERE lower(party.primary_email) = lower(?) \
        \ LIMIT 1"
  creds <- rawSql query [PersistText emailAddress]
  pure (listToMaybe creds)

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
    Just _ -> pure (Left SignupEmailExists)
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
          forM_ fanArtistIdsVal $ \artistId -> do
            let artistKey = toSqlKey (fromIntegral artistId) :: Key Party
            when (artistKey /= pid) $
              void $ insertBy (FanFollow pid artistKey nowVal)
          hashed <- liftIO (hashPasswordText passwordVal)
          _ <- insert UserCredential
            { userCredentialPartyId = pid
            , userCredentialUsername = emailVal
            , userCredentialPasswordHash = hashed
            , userCredentialActive = True
            }
          ensureFanProfileIfMissing pid partyLabel nowVal
          sessionToken <- createSessionToken pid emailVal
          mUser <- loadAuthedUser sessionToken
          case mUser of
            Nothing -> pure (Left SignupProfileError)
            Just user -> pure (Right (toLoginResponse sessionToken user))
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
            { ME.internProfilePartyId = pid
            , ME.internProfileStartAt = startAt
            , ME.internProfileEndAt = endAt
            , ME.internProfileRequiredHours = requiredHours
            , ME.internProfileSkills = skills
            , ME.internProfileAreas = areas
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
        { partyLegalName = Nothing
        , partyDisplayName = displayNameText
        , partyIsOrg = False
        , partyTaxId = Nothing
        , partyPrimaryEmail = Just emailVal
        , partyPrimaryPhone = phoneVal
        , partyWhatsapp = Nothing
        , partyInstagram = Nothing
        , partyEmergencyContact = Nothing
        , partyNotes = Nothing
        , partyCreatedAt = nowVal
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
        Just _ -> pure (Left SignupArtistUnavailable)
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
          sessionToken <- createSessionToken (userCredentialPartyId cred) uname
          mUser <- loadAuthedUser sessionToken
          case mUser of
            Nothing -> pure (Left PasswordProfileError)
            Just user -> pure (Right (toLoginResponse sessionToken user))

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
                guardResetTokenUsername labelText
          case mResetUsername of
            Nothing -> pure (Left PasswordResetInvalidToken)
            Just resetUsername -> do
              mCred <- getBy (UniqueCredentialUsername resetUsername)
              case mCred of
                Nothing -> pure (Left PasswordResetInvalidToken)
                Just (Entity credId cred)
                  | userCredentialPartyId cred /= apiTokenPartyId apiToken ->
                      pure (Left PasswordResetInvalidToken)
                  | not (userCredentialActive cred) ->
                      pure (Left PasswordResetAccountDisabled)
                  | otherwise -> do
                      hashed <- liftIO (hashPasswordText passwordVal)
                      update credId [UserCredentialPasswordHash =. hashed]
                      update tokenId [ApiTokenActive =. False]
                      deactivatePasswordTokens (userCredentialPartyId cred)
                      deactivatePasswordResetTokens (userCredentialPartyId cred)
                      sessionToken <- createSessionToken (userCredentialPartyId cred) (userCredentialUsername cred)
                      mUser <- loadAuthedUser sessionToken
                      case mUser of
                        Nothing -> pure (Left PasswordResetProfileError)
                        Just user -> pure (Right (toLoginResponse sessionToken user))
  where
    isResetToken Nothing = False
    isResetToken (Just lbl) = "password-reset:" `T.isPrefixOf` lbl

    guardResetTokenUsername lbl =
      if "password-reset:" `T.isPrefixOf` T.strip lbl
        then resolveUsernameFromLabel lbl
        else Nothing

hashPasswordText :: Text -> IO Text
hashPasswordText pwd = do
  let raw = TE.encodeUtf8 pwd
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy raw
  case mHash of
    Nothing -> fail "Failed to hash password"
    Just hash -> pure (TE.decodeUtf8 hash)

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
  tokenValue <- liftIO (toText <$> nextRandom)
  inserted <- insertUnique (ApiToken tokenValue pid label True)
  case inserted of
    Nothing -> createTokenWithLabel pid label
    Just _ -> pure tokenValue

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

toLoginResponse :: Text -> AuthedUser -> LoginResponse
toLoginResponse sessionToken AuthedUser{..} = LoginResponse
  { token = sessionToken
  , partyId = fromSqlKey auPartyId
  , roles = auRoles
  , modules = map moduleName (Set.toList auModules)
  }

ensureFanProfileIfMissing :: PartyId -> Text -> UTCTime -> SqlPersistT IO ()
ensureFanProfileIfMissing pid label nowVal = do
  mProfile <- getBy (UniqueFanProfile pid)
  case mProfile of
    Just _ -> pure ()
    Nothing -> insert_ FanProfile
      { fanProfileFanPartyId = pid
      , fanProfileDisplayName = Just label
      , fanProfileAvatarUrl = Nothing
      , fanProfileFavoriteGenres = Nothing
      , fanProfileBio = Nothing
      , fanProfileCity = Nothing
      , fanProfileCreatedAt = nowVal
      , fanProfileUpdatedAt = Nothing
      }

cleanOptional :: Maybe Text -> Maybe Text
cleanOptional Nothing = Nothing
cleanOptional (Just raw) =
  let trimmed = T.strip raw
  in if T.null trimmed then Nothing else Just trimmed

applyRoles :: PartyId -> [RoleEnum] -> SqlPersistT IO ()
applyRoles partyKey rolesList = do
  existing <- selectList [PartyRolePartyId ==. partyKey] []
  let desired = Set.fromList rolesList
  forM_ (Set.toList desired) $ \role ->
    void $ upsert (PartyRole partyKey role True) [PartyRoleActive =. True]
  forM_ existing $ \(Entity roleId record) ->
    when (partyRoleActive record && Set.notMember (partyRoleRole record) desired) $
      update roleId [PartyRoleActive =. False]

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
