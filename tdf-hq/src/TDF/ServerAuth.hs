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
  , GoogleIdTokenInfo(..)
  , GoogleProfile(..)
  , PasswordResetError
  , findReusableActiveToken
  , normalizeAuthEmailAddress
  , parsePasswordChangeAuthToken
  , resolvePasswordResetDelivery
  , runPasswordResetConfirm
  , selectUniqueGoogleLoginCredential
  , selectUniqueLoginEmailCredential
  , selectUniquePasswordResetCredential
  , signupEmailExists
  , validateLoginRequest
  , validatePasswordChangeUsernameInput
  , validateGoogleIdTokenInput
  , validateGoogleIdTokenInfo
  , validateAuthPassword
  , validateCurrentPasswordInput
  , validatePasswordResetToken
  , validateSignupDisplayName
  , validateSignupGoogleIdToken
  , validateGoogleAccountCreationTerms
  , validateSignupTermsAcceptance
  , validateSignupArtistClaimEmail
  , validateOptionalSignupClaimArtistId
  , validateOptionalSignupPhone
  , validateSignupFanArtistIds
  , validateSignupFanArtistTargets
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, displayException, try)
import Control.Exception.Safe (catch, throwM)
import Control.Monad (forM_, join, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char
  ( GeneralCategory (Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isAscii
  , isAsciiLower
  , isControl
  , isDigit
  , isSpace
  )
import Data.Foldable (for_)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID, fromText, toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (Entity (..), SelectOpt (Asc), get, getBy, getEntity, insert, insert_, insertBy, insertUnique, selectFirst, selectList, update, upsert, (=.), (==.), (<-.))
import Database.PostgreSQL.Simple (SqlError (..))
import Database.Persist.Sql (fromSqlKey, rawSql, runSqlPool, toSqlKey, transactionSave, transactionUndo, SqlPersistT)
import Database.Persist.Types (PersistValue (PersistBool, PersistText))
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Servant
import System.IO (hPutStrLn, stderr)

import qualified TDF.API as Api
import TDF.Auth (
    AuthedUser (..),
    clearSessionCookieHeader,
    extractTokenFromHeaders,
    loadAuthedUser,
    lookupUsernameFromToken,
    moduleName,
    parseBearerAuthorizationHeader,
    resolveUsernameFromLabel,
    sessionCookieHeader,
  )
import TDF.Config (AppConfig (..))
import qualified TDF.Catalog.Models as Catalog
import TDF.Catalog.Security (applySecurityRoleAssignmentPolicy)
import TDF.DB (ConnectionPool, Env (..), sharedTlsManager)
import TDF.DTO
import TDF.Internationalization
  ( normalizeCurrencyCode
  , normalizeTimeZone
  )
import qualified TDF.Email.Service as EmailSvc
import qualified TDF.LogBuffer as LogBuf
import TDF.Models
import qualified TDF.Models as M
import TDF.UserActivity (recordUserActivity)

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
        val <- obj .: "email_verified"
        case val of
          Bool b -> pure b
          String t ->
            case T.toLower (T.strip t) of
              "true" -> pure True
              "1" -> pure True
              "false" -> pure False
              "0" -> pure False
              _ -> fail "email_verified must be a boolean or one of true, false, 1, 0"
          _ -> fail "email_verified must be a boolean or one of true, false, 1, 0"

data GoogleProfile = GoogleProfile
  { gpEmail :: Text
  , gpName :: Maybe Text
  , gpPicture :: Maybe Text
  } deriving (Show)

data SignupDbError
  = SignupEmailExists
  | SignupProfileError
  | SignupArtistUnavailable
  | SignupSecurityPolicyError Text
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

validateOptionalSignupClaimArtistId :: Maybe Int64 -> Either ServerError (Maybe Int64)
validateOptionalSignupClaimArtistId Nothing = Right Nothing
validateOptionalSignupClaimArtistId (Just rawArtistId)
  | rawArtistId > 0 = Right (Just rawArtistId)
  | otherwise =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "claimArtistId must be a positive integer")
        }

validateSignupArtistClaimEmail :: Text -> Maybe Text -> Either Text ()
validateSignupArtistClaimEmail signupEmail storedArtistEmail =
  case normalizeAuthEmailAddress signupEmail of
    Nothing ->
      Left "Signup email must be a valid email address"
    Just signupEmailVal ->
      case cleanOptional storedArtistEmail of
        Nothing ->
          Right ()
        Just rawStoredEmail ->
          case normalizeAuthEmailAddress rawStoredEmail of
            Just storedEmailVal | storedEmailVal == signupEmailVal -> Right ()
            _ -> Left "Artist profile email does not match signup email"

validateSignupFanArtistIds :: Maybe [Int64] -> Either ServerError [Int64]
validateSignupFanArtistIds Nothing = Right []
validateSignupFanArtistIds (Just rawArtistIds)
  | length rawArtistIds > maxSignupFanArtistIds =
      Left err400
        { errBody =
            BL.fromStrict
              ( TE.encodeUtf8
                  ( "fanArtistIds must include "
                      <> T.pack (show maxSignupFanArtistIds)
                      <> " artists or fewer"
                  )
              )
        }
  | otherwise = do
      artistIds <- traverse validateArtistId rawArtistIds
      if length (nub artistIds) == length artistIds
        then Right artistIds
        else
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "fanArtistIds must not contain duplicate artist ids")
            }
  where
    validateArtistId artistId
      | artistId > 0 = Right artistId
      | otherwise =
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "fanArtistIds must contain only positive integers")
            }

maxSignupFanArtistIds :: Int
maxSignupFanArtistIds = 50

validateSignupFanArtistTargets :: [Int64] -> SqlPersistT IO (Either ServerError [Int64])
validateSignupFanArtistTargets artistIds =
  if null artistIds
    then pure (Right [])
    else do
      let artistKeys = map (toSqlKey . fromIntegral) artistIds :: [Key Party]
      profiles <- selectList [ArtistProfileArtistPartyId <-. artistKeys] []
      let knownArtistIds =
            Set.fromList
              (map (fromSqlKey . artistProfileArtistPartyId . entityVal) profiles)
          missingArtistIds =
            filter (`Set.notMember` knownArtistIds) artistIds
      if null missingArtistIds
        then pure (Right artistIds)
        else do
          let missingList =
                T.intercalate ", " (map (T.pack . show) missingArtistIds)
              msg =
                "fanArtistIds reference unavailable artist profiles: "
                  <> missingList
          pure
            ( Left
                err422
                  { errBody = BL.fromStrict (TE.encodeUtf8 msg)
                  }
            )

validateAuthPassword :: Text -> Text -> Either ServerError Text
validateAuthPassword fieldLabel rawPassword
  | T.null passwordClean =
      Left (passwordError (fieldLabel <> " is required"))
  | T.length passwordClean < 8 =
      Left (passwordError (fieldLabel <> " must be at least 8 characters"))
  | BS8.length (TE.encodeUtf8 passwordClean) > maxBcryptPasswordBytes =
      Left (passwordError (fieldLabel <> " must be 72 bytes or fewer"))
  | T.any isControl passwordClean =
      Left (passwordError (fieldLabel <> " must not contain control characters"))
  | T.any isHiddenPasswordFormattingChar passwordClean =
      Left (passwordError (fieldLabel <> " must not contain hidden formatting characters"))
  | otherwise =
      Right passwordClean
  where
    passwordClean = T.strip rawPassword
    passwordError msg = err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

maxBcryptPasswordBytes :: Int
maxBcryptPasswordBytes = 72

isHiddenPasswordFormattingChar :: Char -> Bool
isHiddenPasswordFormattingChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validateCurrentPasswordInput :: Text -> Either ServerError Text
validateCurrentPasswordInput rawPassword
  | T.null passwordClean =
      Left (passwordError "Current password is required")
  | BS8.length (TE.encodeUtf8 passwordClean) > maxBcryptPasswordBytes =
      Left (passwordError "Current password must be 72 bytes or fewer")
  | T.any isControl passwordClean =
      Left (passwordError "Current password must not contain control characters")
  | T.any isHiddenPasswordFormattingChar passwordClean =
      Left (passwordError "Current password must not contain hidden formatting characters")
  | otherwise =
      Right passwordClean
  where
    passwordClean = T.strip rawPassword
    passwordError msg = err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

validatePasswordChangeUsernameInput :: Text -> Either ServerError Text
validatePasswordChangeUsernameInput rawUsername
  | T.null usernameClean =
      Left err400 { errBody = "Username is required" }
  | T.length usernameClean > maxLoginIdentifierChars =
      Left err400 { errBody = "Username must be 254 characters or fewer" }
  | T.any invalidLoginIdentifierChar usernameClean =
      Left err400 { errBody = loginIdentifierError }
  | otherwise =
      Right usernameClean
  where
    usernameClean = T.strip rawUsername

validateSignupDisplayName :: Text -> Text -> Either ServerError Text
validateSignupDisplayName rawFirst rawLast
  | T.null firstClean && T.null lastClean =
      Left (signupNameError "First or last name is required")
  | T.any isUnsafeSignupNameChar firstClean =
      Left
        ( signupNameError
            "firstName must not contain control or hidden formatting characters"
        )
  | T.any isUnsafeSignupNameChar lastClean =
      Left
        ( signupNameError
            "lastName must not contain control or hidden formatting characters"
        )
  | not (hasMeaningfulDisplayNameChar displayNameText) =
      Left (signupNameError "displayName must include at least one letter or digit")
  | T.length firstClean > maxSignupNamePartChars =
      Left (signupNameError "firstName must be 80 characters or fewer")
  | T.length lastClean > maxSignupNamePartChars =
      Left (signupNameError "lastName must be 80 characters or fewer")
  | T.length displayNameText > maxSignupDisplayNameChars =
      Left (signupNameError "displayName must be 160 characters or fewer")
  | otherwise =
      Right displayNameText
  where
    firstClean = T.strip rawFirst
    lastClean = T.strip rawLast
    displayNameText = T.unwords (filter (not . T.null) [firstClean, lastClean])
    signupNameError msg = err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }

maxSignupNamePartChars :: Int
maxSignupNamePartChars = 80

maxSignupDisplayNameChars :: Int
maxSignupDisplayNameChars = 160

isUnsafeSignupNameChar :: Char -> Bool
isUnsafeSignupNameChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

hasMeaningfulDisplayNameChar :: Text -> Bool
hasMeaningfulDisplayNameChar =
  T.any isAlphaNum

validateOptionalSignupPhone :: Maybe Text -> Either ServerError (Maybe Text)
validateOptionalSignupPhone Nothing = Right Nothing
validateOptionalSignupPhone (Just rawPhone) =
  case cleanOptional (Just rawPhone) of
    Nothing -> Right Nothing
    Just _ ->
      case normalizeAuthPhoneNumber rawPhone of
        Just phoneVal -> Right (Just phoneVal)
        Nothing ->
          Left err400
            { errBody =
                BL.fromStrict
                  (TE.encodeUtf8 "phone must be a valid phone number")
            }

validateSignupGoogleIdToken :: Maybe Text -> Either ServerError ()
validateSignupGoogleIdToken Nothing = Right ()
validateSignupGoogleIdToken (Just rawToken)
  | T.null (T.strip rawToken) = Right ()
  | otherwise =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "googleIdToken is not supported on password signup; use Google login")
        }

validateSignupTermsAcceptance :: Maybe Bool -> Maybe Text -> Either ServerError (Maybe Text)
validateSignupTermsAcceptance Nothing Nothing = Right Nothing
validateSignupTermsAcceptance (Just True) (Just rawVersion)
  | let version = T.strip rawVersion
  , not (T.null version)
  , T.length version <= 100 = Right (Just version)
validateSignupTermsAcceptance _ _ =
  Left err400
    { errBody = "Terms acceptance and a valid termsVersion must be provided together"
    }

validateGoogleAccountCreationTerms :: Maybe Text -> Either Text ()
validateGoogleAccountCreationTerms (Just _) = Right ()
validateGoogleAccountCreationTerms Nothing =
  Left "Accept the terms and privacy policy through the signup flow before creating a Google account"

normalizeAuthPhoneNumber :: Text -> Maybe Text
normalizeAuthPhoneNumber raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isAsciiPhoneDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isAsciiPhoneDigit trimmed
      allowedPhoneChar ch =
        isAsciiPhoneDigit ch || ch == ' ' || ch `elem` ("+-()." :: String)
      hasUnsafeChars = T.any isUnsafeAuthPhoneChar trimmed
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
      hasInternationalPrefix =
        T.isPrefixOf "+" trimmed
          && maybe False (/= '0') (T.find isAsciiPhoneDigit trimmed)
   in
    if T.null onlyDigits
         || digitCount < 8
         || digitCount > 15
         || hasUnsafeChars
         || hasInvalidChars
         || not plusIsValid
         || not hasInternationalPrefix
      then Nothing
      else Just ("+" <> onlyDigits)

isAsciiPhoneDigit :: Char -> Bool
isAsciiPhoneDigit ch =
  ch >= '0' && ch <= '9'

isUnsafeAuthPhoneChar :: Char -> Bool
isUnsafeAuthPhoneChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

sessionServer :: ServerT Api.SessionAPI AppM
sessionServer =
       currentSessionMaybe
  :<|> logoutSession
  :<|> currentLocalePreferences
  :<|> updateLocalePreferences
  :<|> recordCurrencyConversion

authV1Server :: ServerT Api.AuthV1API AppM
authV1Server = signup :<|> passwordReset :<|> passwordResetConfirm :<|> changePassword

buildSessionResponse :: AppConfig -> Maybe Text -> AuthedUser -> SqlPersistT IO SessionResponse
buildSessionResponse cfg mResolvedUsername AuthedUser{..} = do
  mParty <- get auPartyId
  preferences <- loadLocalePreferences cfg auPartyId
  let fallbackUsername = "party-" <> T.pack (show (fromSqlKey auPartyId))
      usernameText = fromMaybe fallbackUsername (cleanOptional mResolvedUsername)
      displayNameText = fromMaybe usernameText (cleanOptional (M.partyDisplayName <$> mParty))
  pure SessionResponse
    { sessionUsername = usernameText
    , sessionDisplayName = displayNameText
    , sessionPartyId = fromSqlKey auPartyId
    , sessionRoles = auRoles
    , sessionModules = map moduleName (Set.toList auModules)
    , sessionFeatureFlags = ["EVENT_DISCOVERY_ENABLED" | eventDiscoveryEnabled cfg]
    , sessionPreferences = preferences
    }

currentSessionMaybe :: Maybe Text -> Maybe Text -> AppM (Maybe SessionResponse)
currentSessionMaybe mAuthorizationHeader mCookieHeader = do
  Env pool cfg <- ask
  case extractTokenFromHeaders cfg mAuthorizationHeader mCookieHeader of
    Left _ -> pure Nothing
    Right token ->
      liftIO $
        flip runSqlPool pool $ do
          mUser <- loadAuthedUser token
          case mUser of
            Nothing -> pure Nothing
            Just user -> do
              mUsername <- lookupUsernameFromToken token
              Just <$> buildSessionResponse cfg mUsername user

currentLocalePreferences :: Maybe Text -> Maybe Text -> AppM LocalePreferencesDTO
currentLocalePreferences mAuthorizationHeader mCookieHeader = do
  Env pool cfg <- ask
  user <- requireSessionUser cfg pool mAuthorizationHeader mCookieHeader
  liftIO $ flip runSqlPool pool $ loadLocalePreferences cfg (auPartyId user)

updateLocalePreferences
  :: Maybe Text
  -> Maybe Text
  -> LocalePreferencesUpdate
  -> AppM LocalePreferencesDTO
updateLocalePreferences mAuthorizationHeader mCookieHeader LocalePreferencesUpdate{..} = do
  Env pool cfg <- ask
  user <- requireSessionUser cfg pool mAuthorizationHeader mCookieHeader
  timezoneValue <- either throwError pure (validateConfiguredTimezone lpuTimezone)
  regionalValidation <- liftIO $ flip runSqlPool pool $ do
    localeResult <- validateActivePreferenceLocale lpuLocaleId
    currencyResult <- validateActivePreferenceCurrency lpuCurrencyId
    countryResult <- validateActivePreferenceCountry lpuCountryId
    pure $ (,,) <$> localeResult <*> currencyResult <*> countryResult
  ((localeIdValue, _), (currencyIdValue, _), countryValue) <- either throwError pure regionalValidation
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ do
    _ <- upsert
      UserLocalePreference
        { userLocalePreferenceUserId = auPartyId user
        , userLocalePreferenceLocale = Nothing
        , userLocalePreferenceCurrency = Nothing
        , userLocalePreferenceTimezone = timezoneValue
        , userLocalePreferenceCountryCode = Nothing
        , userLocalePreferenceLocaleId = Just localeIdValue
        , userLocalePreferenceCurrencyId = Just currencyIdValue
        , userLocalePreferenceCountryId = countryValue
        , userLocalePreferenceUpdatedAt = now
        }
      [ UserLocalePreferenceLocale =. Nothing
      , UserLocalePreferenceCurrency =. Nothing
      , UserLocalePreferenceTimezone =. timezoneValue
      , UserLocalePreferenceCountryCode =. Nothing
      , UserLocalePreferenceLocaleId =. Just localeIdValue
      , UserLocalePreferenceCurrencyId =. Just currencyIdValue
      , UserLocalePreferenceCountryId =. countryValue
      , UserLocalePreferenceUpdatedAt =. now
      ]
    loadLocalePreferences cfg (auPartyId user)

recordCurrencyConversion
  :: Maybe Text
  -> Maybe Text
  -> CurrencyConversionAuditCreate
  -> AppM NoContent
recordCurrencyConversion mAuthorizationHeader mCookieHeader CurrencyConversionAuditCreate{..} = do
  Env pool cfg <- ask
  user <- requireSessionUser cfg pool mAuthorizationHeader mCookieHeader
  sourceCurrency <- either throwError pure (validateConfiguredCurrency (supportedCurrencies cfg) ccaSourceCurrency)
  targetCurrency <- either throwError pure (validateConfiguredCurrency (supportedCurrencies cfg) ccaTargetCurrency)
  let sourceLabel = T.strip ccaRateSource
  when (isNaN ccaExchangeRate || isInfinite ccaExchangeRate || ccaExchangeRate <= 0 || ccaExchangeRate > 1000000000) $
    throwError err400 { errBody = "exchangeRate must be finite and positive" }
  unless (not (T.null sourceLabel) && T.length sourceLabel <= 80) $
    throwError err400 { errBody = "rateSource is required and must be 80 characters or fewer" }
  now <- liftIO getCurrentTime
  liftIO $ flip runSqlPool pool $ insert_ CurrencyConversionAudit
    { currencyConversionAuditUserId = Just (auPartyId user)
    , currencyConversionAuditSourceCurrency = sourceCurrency
    , currencyConversionAuditTargetCurrency = targetCurrency
    , currencyConversionAuditSourceMinorUnits = ccaSourceMinorUnits
    , currencyConversionAuditTargetMinorUnits = ccaTargetMinorUnits
    , currencyConversionAuditExchangeRate = ccaExchangeRate
    , currencyConversionAuditRateSource = sourceLabel
    , currencyConversionAuditRateObservedAt = now
    , currencyConversionAuditCreatedAt = now
    }
  pure NoContent

requireSessionUser
  :: AppConfig
  -> ConnectionPool
  -> Maybe Text
  -> Maybe Text
  -> AppM AuthedUser
requireSessionUser cfg pool mAuthorizationHeader mCookieHeader = do
  token <-
    either
      (\message -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 message) })
      pure
      (extractTokenFromHeaders cfg mAuthorizationHeader mCookieHeader)
  mUser <- liftIO $ flip runSqlPool pool (loadAuthedUser token)
  maybe (throwError err401 { errBody = "Invalid or inactive session token" }) pure mUser

loadLocalePreferences :: AppConfig -> PartyId -> SqlPersistT IO LocalePreferencesDTO
loadLocalePreferences cfg partyIdValue = do
  mStored <- getBy (UniqueUserLocalePreference partyIdValue)
  case mStored of
    Nothing -> localePreferencesFromConfig cfg
    Just (Entity _ stored) -> do
      (localeIdValue, localeCode) <- resolveStoredLocale stored
      (currencyIdValue, currencyCode) <- resolveStoredCurrency stored
      mCountry <- traverse (get . Catalog.CountryReferenceKey) (userLocalePreferenceCountryId stored)
      let canonicalCountryCode = Catalog.countryReferenceAlpha2 <$> join mCountry
      pure LocalePreferencesDTO
        { lpLocaleId = localeIdValue
        , lpLocale = localeCode
        , lpCurrencyId = currencyIdValue
        , lpCurrency = currencyCode
        , lpTimezone = userLocalePreferenceTimezone stored
        , lpCountryId = userLocalePreferenceCountryId stored
        , lpCountryCode = canonicalCountryCode <|> userLocalePreferenceCountryCode stored
        }
  where
    resolveStoredLocale stored =
      case userLocalePreferenceLocaleId stored of
        Nothing -> invalidStored "localeId"
        Just identifier -> do
          mItem <- get (Catalog.LocaleReferenceKey identifier)
          maybe (invalidStored "localeId") (\item -> pure (identifier, Catalog.localeReferenceCode item)) mItem
    resolveStoredCurrency stored =
      case userLocalePreferenceCurrencyId stored of
        Nothing -> invalidStored "currencyId"
        Just identifier -> do
          mItem <- get (Catalog.CurrencyReferenceKey identifier)
          maybe (invalidStored "currencyId") (\item -> pure (identifier, Catalog.currencyReferenceCode item)) mItem
    invalidStored fieldName =
      liftIO . ioError . userError $
        "Stored user locale preference is missing canonical " <> fieldName

localePreferencesFromConfig :: AppConfig -> SqlPersistT IO LocalePreferencesDTO
localePreferencesFromConfig cfg = do
  localeResult <- resolveConfiguredDefaultLocale (defaultLocale cfg)
  currencyResult <- resolveConfiguredDefaultCurrency (defaultCurrency cfg)
  (localeIdValue, localeCode) <- either (const (invalidDefault "locale")) pure localeResult
  (currencyIdValue, currencyCode) <- either (const (invalidDefault "currency")) pure currencyResult
  pure LocalePreferencesDTO
    { lpLocaleId = localeIdValue
    , lpLocale = localeCode
    , lpCurrencyId = currencyIdValue
    , lpCurrency = currencyCode
    , lpTimezone = defaultTimezone cfg
    , lpCountryId = Nothing
    , lpCountryCode = Nothing
    }
  where
    invalidDefault kind = liftIO . ioError . userError $
      "Configured default " <> kind <> " is not an active deployment-enabled persisted reference"

validateConfiguredCurrency :: [Text] -> Text -> Either ServerError Text
validateConfiguredCurrency supported raw =
  case normalizeCurrencyCode raw of
    Just currency | currency `elem` supported -> Right currency
    _ -> Left err400
      { errBody = BL.fromStrict $ TE.encodeUtf8 $
          "Unsupported currency. Supported currencies: " <> T.intercalate ", " supported
      }

validateActivePreferenceLocale
  :: UUID
  -> SqlPersistT IO (Either ServerError (UUID, Text))
validateActivePreferenceLocale localeId = do
  mLocale <- get (Catalog.LocaleReferenceKey localeId)
  mEnablement <- getBy (Catalog.UniqueDeploymentLocale "default" (Catalog.LocaleReferenceKey localeId))
  pure $ case (mLocale, mEnablement) of
    (Just locale, Just (Entity _ enablement))
      | Catalog.localeReferenceActive locale
      , isNothing (Catalog.localeReferenceDeprecatedAt locale)
      , Catalog.deploymentLocaleEnablementEnabled enablement ->
          Right (localeId, Catalog.localeReferenceCode locale)
    _ -> Left err400 { errBody = "localeId must reference an active deployment-enabled locale" }

validateActivePreferenceCurrency
  :: UUID
  -> SqlPersistT IO (Either ServerError (UUID, Text))
validateActivePreferenceCurrency currencyId = do
  mCurrency <- get (Catalog.CurrencyReferenceKey currencyId)
  mEnablement <- getBy (Catalog.UniqueDeploymentCurrency "default" (Catalog.CurrencyReferenceKey currencyId))
  pure $ case (mCurrency, mEnablement) of
    (Just currency, Just (Entity _ enablement))
      | Catalog.currencyReferenceActive currency
      , isNothing (Catalog.currencyReferenceDeprecatedAt currency)
      , Catalog.deploymentCurrencyEnablementEnabled enablement ->
          Right (currencyId, Catalog.currencyReferenceCode currency)
    _ -> Left err400 { errBody = "currencyId must reference an active deployment-enabled currency" }

resolveConfiguredDefaultLocale
  :: Text
  -> SqlPersistT IO (Either ServerError (UUID, Text))
resolveConfiguredDefaultLocale code = do
  mLocale <- getBy (Catalog.UniqueLocaleReferenceCode code)
  case mLocale of
    Nothing -> pure (Left err500)
    Just (Entity (Catalog.LocaleReferenceKey localeId) _) -> validateActivePreferenceLocale localeId

resolveConfiguredDefaultCurrency
  :: Text
  -> SqlPersistT IO (Either ServerError (UUID, Text))
resolveConfiguredDefaultCurrency code = do
  mCurrency <- getBy (Catalog.UniqueCurrencyReferenceCode code)
  case mCurrency of
    Nothing -> pure (Left err500)
    Just (Entity (Catalog.CurrencyReferenceKey currencyId) _) -> validateActivePreferenceCurrency currencyId

validateConfiguredTimezone :: Text -> Either ServerError Text
validateConfiguredTimezone raw =
  maybe
    (Left err400 { errBody = "timezone must be UTC or a valid IANA area/location name" })
    Right
    (normalizeTimeZone raw)

validateActivePreferenceCountry
  :: Maybe UUID
  -> SqlPersistT IO (Either ServerError (Maybe UUID))
validateActivePreferenceCountry Nothing = pure (Right Nothing)
validateActivePreferenceCountry requested@(Just countryId) = do
  mCountry <- get (Catalog.CountryReferenceKey countryId)
  pure $ case mCountry of
    Just country
      | Catalog.countryReferenceActive country
      , isNothing (Catalog.countryReferenceDeprecatedAt country) -> Right requested
    _ -> Left err400 { errBody = "countryId must reference an active country" }

logoutSession :: Maybe Text -> Maybe Text -> AppM (Api.SessionCookieHeaders NoContent)
logoutSession mAuthorizationHeader mCookieHeader = do
  Env pool cfg <- ask
  let presentedTokens =
        nub . mapMaybe (either (const Nothing) Just) $
          [ extractTokenFromHeaders cfg mAuthorizationHeader Nothing
          , extractTokenFromHeaders cfg Nothing mCookieHeader
          ]
  unless (null presentedTokens) . liftIO . flip runSqlPool pool $
    forM_ presentedTokens $ \tokenValue -> do
      mToken <- getBy (UniqueApiToken tokenValue)
      for_ mToken $ \(Entity tokenId storedToken) ->
        when (apiTokenActive storedToken) $
          update tokenId [ApiTokenActive =. False]
  pure (addHeader (clearSessionCookieHeader cfg) NoContent)

withSessionCookie :: LoginResponse -> AppM (Api.SessionCookieHeaders LoginResponse)
withSessionCookie response@LoginResponse{token = sessionToken} = do
  cfg <- asks envConfig
  pure (addHeader (sessionCookieHeader cfg sessionToken) response)

recordAuthActivity :: Text -> LoginResponse -> Maybe Text -> Maybe Bool -> AppM ()
recordAuthActivity actionName LoginResponse{partyId = responsePartyId} acceptedTermsVersion marketingConsent = do
  Env pool _ <- ask
  let actorId = toSqlKey responsePartyId :: PartyId
      entityId = T.pack (show responsePartyId)
  result <- liftIO $ try $
    flip runSqlPool pool $
      recordUserActivity
        (Just actorId)
        "auth"
        entityId
        actionName
        (Just (object
          [ "partyId" .= responsePartyId
          , "termsVersion" .= acceptedTermsVersion
          , "marketingOptIn" .= marketingConsent
          ]))
  case result of
    Left (err :: SomeException) -> do
      let msg =
            "[Auth][Activity] Failed to record "
              <> actionName
              <> " for partyId="
              <> entityId
              <> ": "
              <> T.pack (displayException err)
      liftIO $ LogBuf.addLog LogBuf.LogWarning msg
    Right () -> pure ()

login :: LoginRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
login rawRequest = do
  LoginRequest{..} <- either throwError pure (validateLoginRequest rawRequest)
  Env pool _ <- ask
  result <- liftIO $ flip runSqlPool pool (runLogin username password)
  case result of
    Left msg -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
    Right res -> do
      recordAuthActivity "login_password" res Nothing Nothing
      withSessionCookie res

googleLogin :: GoogleLoginRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
googleLogin GoogleLoginRequest{..} = do
  tokenClean <- either throwError pure (validateGoogleIdTokenInput idToken)
  acceptedTermsVersion <- either throwError pure (validateSignupTermsAcceptance termsAccepted termsVersion)
  Env pool cfg <- ask
  let mClientId = googleClientId cfg
  when (isNothing mClientId) $
    throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Google Sign-In is not configured") }
  manager <- pure sharedTlsManager
  verification <- liftIO $ verifyGoogleIdToken manager tokenClean mClientId
  case verification of
    Left msg -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
    Right profile -> do
      result <- liftIO $ flip runSqlPool pool (completeGoogleLogin acceptedTermsVersion profile)
      case result of
        Left err -> throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 err) }
        Right resp -> do
          recordAuthActivity "login_google" resp acceptedTermsVersion marketingOptIn
          withSessionCookie resp

signup :: SignupRequest -> AppM (Api.SessionCookieHeaders LoginResponse)
signup SignupRequest
  { firstName = rawFirst
  , lastName = rawLast
  , email = rawEmail
  , phone = rawPhone
  , password = rawPassword
  , googleIdToken = rawGoogleIdToken
  , marketingOptIn = requestedMarketingOptIn
  , termsAccepted = rawTermsAccepted
  , termsVersion = rawTermsVersion
  , fanArtistIds = requestedFanArtistIds
  , claimArtistId = rawClaimArtistId
  } = do
  let emailInput = T.strip rawEmail
  when (T.null emailInput) $ throwBadRequest "Email is required"
  emailClean <- maybe (throwBadRequest "Invalid email address") pure (normalizeAuthEmailAddress emailInput)
  either throwError pure (validateSignupGoogleIdToken rawGoogleIdToken)
  passwordClean <- either throwError pure (validateAuthPassword "Password" rawPassword)
  acceptedTermsVersion <- either throwError pure (validateSignupTermsAcceptance rawTermsAccepted rawTermsVersion)
  displayNameText <- either throwError pure (validateSignupDisplayName rawFirst rawLast)
  phoneClean <- either throwError pure (validateOptionalSignupPhone rawPhone)
  claimArtistIdClean <- either throwError pure (validateOptionalSignupClaimArtistId rawClaimArtistId)
  sanitizedFanArtists <- either throwError pure (validateSignupFanArtistIds requestedFanArtistIds)
  now <- liftIO getCurrentTime
  Env pool cfg <- ask
  validatedFanArtistIds <-
    liftIO (flip runSqlPool pool (validateSignupFanArtistTargets sanitizedFanArtists))
      >>= either throwError pure
  let emailSvc = EmailSvc.mkEmailService cfg
  result <- liftIO $ flip runSqlPool pool $
    runSignupDb
      emailClean
      passwordClean
      displayNameText
      phoneClean
      validatedFanArtistIds
      claimArtistIdClean
      now
  case result of
    Left SignupEmailExists ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Account already exists for this email") }
    Left SignupArtistUnavailable ->
      throwError err409 { errBody = BL.fromStrict (TE.encodeUtf8 "Artist profile is not available to claim") }
    Left SignupProfileError ->
      throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 "Failed to load user profile") }
    Left (SignupSecurityPolicyError policyError) ->
      throwError err503 { errBody = BL.fromStrict (TE.encodeUtf8 policyError) }
    Right resp -> do
      recordAuthActivity "signup" resp acceptedTermsVersion requestedMarketingOptIn
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
  currentPasswordClean <- either throwError pure (validateCurrentPasswordInput currentPassword)
  newPasswordClean <- either throwError pure (validateAuthPassword "New password" newPassword)
  Env pool _ <- ask
  usernameClean <- resolveUsername pool username mAuthHeader
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
        Just uname | not (T.null (T.strip uname)) ->
          either throwError pure (validatePasswordChangeUsernameInput uname)
        _ -> do
          tokenValue <- case traverse parsePasswordChangeAuthToken header of
            Left err -> throwError err
            Right Nothing -> throwBadRequest "Username is required"
            Right (Just tok) -> pure tok
          mResolved <- liftIO $ flip runSqlPool pool (lookupUsernameFromToken tokenValue)
          case fmap T.strip mResolved of
            Nothing ->
              throwError err401 { errBody = BL.fromStrict (TE.encodeUtf8 "Invalid or inactive session token") }
            Just uname' ->
              either throwError pure (validatePasswordChangeUsernameInput uname')

parsePasswordChangeAuthToken :: Text -> Either ServerError Text
parsePasswordChangeAuthToken rawHeader =
  case parseBearerAuthorizationHeader rawHeader of
    Right token ->
      validatePasswordChangeAuthToken token
    _ ->
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "Authorization header must be Bearer <token>")
        }

validatePasswordChangeAuthToken :: Text -> Either ServerError Text
validatePasswordChangeAuthToken token
  | T.null token =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "Authorization header must be Bearer <token>")
        }
  | T.length token > passwordChangeAuthTokenMaxLength =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Authorization token must be 512 characters or fewer")
        }
  | T.any invalidPasswordChangeAuthTokenChar token =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "Authorization header must be Bearer <token>")
        }
  | otherwise = Right token

passwordChangeAuthTokenMaxLength :: Int
passwordChangeAuthTokenMaxLength = 512

invalidPasswordChangeAuthTokenChar :: Char -> Bool
invalidPasswordChangeAuthTokenChar ch =
  isSpace ch
    || isControl ch
    || not (isAscii ch)
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]
    || ch `elem` ['"', ';', ',', '\\']

validateGoogleIdTokenInput :: Text -> Either ServerError Text
validateGoogleIdTokenInput rawToken
  | T.null token =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "Google idToken is required")
        }
  | T.length token > maxGoogleIdTokenChars =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Google idToken must be 4096 characters or fewer")
        }
  | T.any isSpace token =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "Google idToken must not contain whitespace")
        }
  | T.any isControl token =
      Left err400
        { errBody =
            BL.fromStrict (TE.encodeUtf8 "Google idToken must not contain control characters")
        }
  | T.any isHiddenPasswordFormattingChar token =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Google idToken must not contain hidden formatting characters")
        }
  | T.any (not . isAscii) token =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Google idToken must contain only ASCII characters")
        }
  | not (hasGoogleIdTokenShape token) =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Google idToken must be a JWT with three non-empty segments")
        }
  | not (hasGoogleIdTokenBase64UrlSegments token) =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Google idToken segments must contain only base64url characters")
        }
  | otherwise =
      Right token
  where
    token = stripOuterAsciiSpaces rawToken

maxGoogleIdTokenChars :: Int
maxGoogleIdTokenChars = 4096

hasGoogleIdTokenShape :: Text -> Bool
hasGoogleIdTokenShape token =
  case T.splitOn "." token of
    [headerPart, payloadPart, signaturePart] ->
      all (not . T.null) [headerPart, payloadPart, signaturePart]
    _ ->
      False

hasGoogleIdTokenBase64UrlSegments :: Text -> Bool
hasGoogleIdTokenBase64UrlSegments token =
  case T.splitOn "." token of
    [headerPart, payloadPart, signaturePart] ->
      all (T.all validGoogleIdTokenSegmentChar) [headerPart, payloadPart, signaturePart]
    _ ->
      False

validGoogleIdTokenSegmentChar :: Char -> Bool
validGoogleIdTokenSegmentChar ch =
  (ch >= 'A' && ch <= 'Z')
    || (ch >= 'a' && ch <= 'z')
    || isDigit ch
    || ch == '-'
    || ch == '_'

stripOuterAsciiSpaces :: Text -> Text
stripOuterAsciiSpaces =
  T.dropAround (== ' ')

validateLoginRequest :: LoginRequest -> Either ServerError LoginRequest
validateLoginRequest (LoginRequest rawUsername rawPassword)
  | T.null usernameClean =
      Left err400 { errBody = "Username is required" }
  | T.length usernameClean > maxLoginIdentifierChars =
      Left err400 { errBody = "Username must be 254 characters or fewer" }
  | T.any invalidLoginIdentifierChar usernameClean =
      Left err400
        { errBody = loginIdentifierError }
  | T.null passwordClean =
      Left err400 { errBody = "Password is required" }
  | BS8.length (TE.encodeUtf8 passwordClean) > maxBcryptPasswordBytes =
      Left err400 { errBody = "Password must be 72 bytes or fewer" }
  | T.any isControl passwordClean =
      Left err400 { errBody = "Password must not contain control characters" }
  | T.any isHiddenPasswordFormattingChar passwordClean =
      Left err400 { errBody = "Password must not contain hidden formatting characters" }
  | otherwise =
      Right (LoginRequest usernameClean passwordClean)
  where
    usernameClean = T.strip rawUsername
    passwordClean = T.strip rawPassword

maxLoginIdentifierChars :: Int
maxLoginIdentifierChars = 254

loginIdentifierError :: BL.ByteString
loginIdentifierError =
  "Username must not contain whitespace, control characters, or hidden formatting characters"

invalidLoginIdentifierChar :: Char -> Bool
invalidLoginIdentifierChar ch =
  isSpace ch
    || isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

validatePasswordResetToken :: Text -> Either ServerError Text
validatePasswordResetToken rawToken
  | T.null token =
      Left err400
        { errBody = BL.fromStrict (TE.encodeUtf8 "Token is required")
        }
  | T.length token > passwordChangeAuthTokenMaxLength =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Token must be 512 characters or fewer")
        }
  | T.any invalidPasswordChangeAuthTokenChar token =
      Left err400
        { errBody =
            BL.fromStrict
              (TE.encodeUtf8 "Token must not contain whitespace or control characters")
        }
  | otherwise =
      case fromText token of
        Nothing ->
          Left err400
            { errBody = BL.fromStrict (TE.encodeUtf8 "Token format is invalid")
            }
        Just parsed
          | isNilPasswordResetToken parsed ->
              Left err400
                { errBody = BL.fromStrict (TE.encodeUtf8 "Token format is invalid")
                }
          | otherwise -> Right (toText parsed)
  where
    token = T.strip rawToken

isNilPasswordResetToken :: UUID -> Bool
isNilPasswordResetToken parsedToken =
  toText parsedToken == "00000000-0000-0000-0000-000000000000"

passwordReset :: PasswordResetRequest -> AppM NoContent
passwordReset PasswordResetRequest{..} = do
  let emailInput = T.strip email
  when (T.null emailInput) $ throwBadRequest "Email is required"
  emailClean <- maybe (throwBadRequest "Invalid email address") pure (normalizeAuthEmailAddress emailInput)
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
  tokenClean <- either throwError pure (validatePasswordResetToken token)
  newPasswordClean <- either throwError pure (validateAuthPassword "New password" newPassword)
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
        \ LIMIT 2"
  if T.null emailQuery
    then pure Nothing
    else do
      creds <- rawSql query [PersistBool True, PersistText emailQuery]
      case selectUniquePasswordResetCredential creds of
        Nothing -> pure Nothing
        Just cred@(Entity _ credential) -> do
          mParty <- get (userCredentialPartyId credential)
          let mRecipientEmail = mParty >>= cleanOptional . M.partyPrimaryEmail
              mDisplayName = cleanOptional (M.partyDisplayName <$> mParty)
          pure
            ( (\recipientEmail ->
                  ( cred
                  , recipientEmail
                  , fromMaybe recipientEmail mDisplayName
                  )
              )
                <$> mRecipientEmail
            )

selectUniquePasswordResetCredential :: [Entity UserCredential] -> Maybe (Entity UserCredential)
selectUniquePasswordResetCredential [credential] = Just credential
selectUniquePasswordResetCredential _ = Nothing

normalizeAuthEmailAddress :: Text -> Maybe Text
normalizeAuthEmailAddress raw =
  let normalized = T.toLower (T.strip raw)
  in if isValidAuthEmailAddress normalized then Just normalized else Nothing

maxAuthEmailAddressChars :: Int
maxAuthEmailAddressChars = 254

maxAuthEmailLocalPartChars :: Int
maxAuthEmailLocalPartChars = 64

maxAuthEmailDomainLabelChars :: Int
maxAuthEmailDomainLabelChars = 63

isValidAuthEmailAddress :: Text -> Bool
isValidAuthEmailAddress candidate =
  T.length candidate <= maxAuthEmailAddressChars
    && hasValidShape
  where
    hasValidShape =
      case T.splitOn "@" candidate of
        [localPart, domain] ->
          isValidAuthEmailLocalPart localPart
            && not (T.null domain)
            && not (T.any isSpace candidate)
            && not (T.isPrefixOf "." domain)
            && not (T.isSuffixOf "." domain)
            && T.isInfixOf "." domain
            && hasValidAuthTopLevelLabel domain
            && all isValidAuthDomainLabel (T.splitOn "." domain)
        _ -> False

hasValidAuthTopLevelLabel :: Text -> Bool
hasValidAuthTopLevelLabel domain =
  case reverse (T.splitOn "." domain) of
    topLevelLabel : _ ->
      T.length topLevelLabel >= 2
        && T.any isAsciiLower topLevelLabel
    _ -> False

isValidAuthEmailLocalPart :: Text -> Bool
isValidAuthEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= maxAuthEmailLocalPartChars
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (".." `T.isInfixOf` localPart)
    && T.all isValidAuthEmailLocalChar localPart

isValidAuthEmailLocalChar :: Char -> Bool
isValidAuthEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidAuthDomainLabel :: Text -> Bool
isValidAuthDomainLabel label =
  not (T.null label)
    && T.length label <= maxAuthEmailDomainLabelChars
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidAuthDomainChar label

isValidAuthDomainChar :: Char -> Bool
isValidAuthDomainChar c = isAsciiLower c || isDigit c || c == '-'

signupEmailExists :: Text -> SqlPersistT IO Bool
signupEmailExists rawEmail = do
  let emailQuery = T.strip rawEmail
      query =
        "SELECT ?? FROM user_credential \
        \ LEFT JOIN party ON user_credential.party_id = party.id \
        \ WHERE lower(trim(user_credential.username)) = lower(?) \
        \    OR lower(trim(COALESCE(party.primary_email, ''))) = lower(?) \
        \ LIMIT 1"
  if T.null emailQuery
    then pure False
    else do
      creds <- rawSql query [PersistText emailQuery, PersistText emailQuery]
      pure (not (null (creds :: [Entity UserCredential])))

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
              Right info -> pure (validateGoogleIdTokenInfo mExpectedClientId info)

validateGoogleIdTokenInfo :: Maybe Text -> GoogleIdTokenInfo -> Either Text GoogleProfile
validateGoogleIdTokenInfo mExpectedClientId info
  | not (gitEmailVerified info) =
      Left "Tu correo de Google debe estar verificado."
  | Just expected <- mExpectedClientId
  , gitAud info /= expected =
      Left "El token de Google no coincide con el cliente configurado."
  | not (issuerAllowed (gitIss info)) =
      Left "El token de Google proviene de un emisor no permitido."
  | gitSub info /= subject
      || T.null subject
      || T.length subject > maxGoogleSubjectChars
      || T.any invalidGoogleSubjectChar subject =
      Left "El token de Google no contiene un identificador válido."
  | otherwise =
      case normalizeAuthEmailAddress (gitEmail info) of
        Nothing ->
          Left "El token de Google no contiene un correo válido."
        Just normalizedEmail ->
          let normalizedName = sanitizeGoogleProfileName (gitName info)
              profile = GoogleProfile
                { gpEmail = normalizedEmail
                , gpName = normalizedName <|> Just normalizedEmail
                , gpPicture = gitPicture info
                }
           in Right profile
  where
    subject = T.strip (gitSub info)

issuerAllowed :: Maybe Text -> Bool
issuerAllowed Nothing = False
issuerAllowed (Just issRaw) =
  let issuer = T.toLower (T.strip issRaw)
  in issuer == "accounts.google.com" || issuer == "https://accounts.google.com"

invalidGoogleSubjectChar :: Char -> Bool
invalidGoogleSubjectChar ch =
  not (isAscii ch)
    || isSpace ch
    || isControl ch
    || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

maxGoogleSubjectChars :: Int
maxGoogleSubjectChars = 255

sanitizeGoogleProfileName :: Maybe Text -> Maybe Text
sanitizeGoogleProfileName rawName = do
  name <- cleanOptional rawName
  if T.length name <= maxSignupDisplayNameChars
      && not (T.any isUnsafeSignupNameChar name)
      && hasMeaningfulDisplayNameChar name
    then Just name
    else Nothing

completeGoogleLogin :: Maybe Text -> GoogleProfile -> SqlPersistT IO (Either Text LoginResponse)
completeGoogleLogin acceptedTermsVersion GoogleProfile{..} = do
  existingResult <- lookupByEmail gpEmail
  case existingResult of
    Left err -> pure (Left err)
    Right mExisting ->
      case mExisting of
        Just (Entity _ cred)
          | not (userCredentialActive cred) ->
              pure (Left "Cuenta deshabilitada. Contacta a soporte.")
          | otherwise -> do
              sessionToken <-
                createReusableSessionToken
                  (userCredentialPartyId cred)
                  (Just ("google-login:" <> gpEmail))
              mUser <- loadAuthedUser sessionToken
              case mUser of
                Nothing -> pure (Left "No pudimos cargar tu perfil.")
                Just user -> pure (Right ((toLoginResponse sessionToken user) { accountCreated = Just False }))
        Nothing ->
          case validateGoogleAccountCreationTerms acceptedTermsVersion of
            Left consentError -> pure (Left consentError)
            Right () -> do
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
                    , partyStripeCustomerId = Nothing
                    , partyCountryCode = Nothing
                    , partyCountryId = Nothing
                    , partyCreatedAt = now
                    }
              pid <- insert partyRecord
              policyResult <- applySecurityRoleAssignmentPolicy
                "account.google.customer"
                pid
                True
                (Just pid)
                "google-auth"
                ("google-account:" <> T.pack (show (fromSqlKey pid)))
                now
              case policyResult of
                Left policyError -> do
                  transactionUndo
                  pure (Left policyError)
                Right _ -> do
                  ensureFanProfileIfMissing pid displayName now
                  tempPassword <- liftIO generateTemporaryPassword
                  hashed <- liftIO (hashPasswordText tempPassword)
                  _ <- insert UserCredential
                    { userCredentialPartyId = pid
                    , userCredentialUsername = gpEmail
                    , userCredentialPasswordHash = hashed
                    , userCredentialActive = True
                    }
                  sessionToken <-
                    createReusableSessionToken pid (Just ("google-login:" <> gpEmail))
                  mUser <- loadAuthedUser sessionToken
                  case mUser of
                    Nothing -> do
                      transactionUndo
                      pure (Left "No pudimos cargar tu perfil.")
                    Just user -> pure (Right ((toLoginResponse sessionToken user) { accountCreated = Just True }))

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
        Nothing -> lookupLoginEmailCredential trimmed

lookupLoginEmailCredential :: Text -> SqlPersistT IO (Maybe (Entity UserCredential))
lookupLoginEmailCredential emailAddress = do
  let query =
        "SELECT ?? FROM user_credential \
        \ JOIN party ON user_credential.party_id = party.id \
        \ WHERE lower(trim(COALESCE(party.primary_email, ''))) = lower(trim(?)) \
        \ ORDER BY user_credential.id ASC \
        \ LIMIT 2"
  creds <- rawSql query [PersistText emailAddress]
  pure (selectUniqueLoginEmailCredential creds)

selectUniqueLoginEmailCredential :: [Entity UserCredential] -> Maybe (Entity UserCredential)
selectUniqueLoginEmailCredential [credential] = Just credential
selectUniqueLoginEmailCredential _ = Nothing

lookupByEmail :: Text -> SqlPersistT IO (Either Text (Maybe (Entity UserCredential)))
lookupByEmail emailAddress = do
  let query =
        "SELECT ?? FROM user_credential \
        \ JOIN party ON user_credential.party_id = party.id \
        \ WHERE lower(trim(COALESCE(party.primary_email, ''))) = lower(trim(?)) \
        \ ORDER BY user_credential.id ASC \
        \ LIMIT 2"
  creds <- rawSql query [PersistText emailAddress]
  pure (selectUniqueGoogleLoginCredential creds)

selectUniqueGoogleLoginCredential
  :: [Entity UserCredential]
  -> Either Text (Maybe (Entity UserCredential))
selectUniqueGoogleLoginCredential [] = Right Nothing
selectUniqueGoogleLoginCredential [credential] = Right (Just credential)
selectUniqueGoogleLoginCredential _ =
  Left "Hay varias cuentas asociadas a este correo de Google. Contacta a soporte."

runSignupDb
  :: Text
  -> Text
  -> Text
  -> Maybe Text
  -> [Int64]
  -> Maybe Int64
  -> UTCTime
  -> SqlPersistT IO (Either SignupDbError LoginResponse)
runSignupDb emailVal passwordVal displayNameText phoneVal fanArtistIdsVal mClaimArtistId nowVal = do
  existing <- signupEmailExists emailVal
  if existing
    then pure (Left SignupEmailExists)
    else do
      partyResult <- resolveParty displayNameText mClaimArtistId emailVal phoneVal nowVal
      case partyResult of
        Left err -> pure (Left err)
        Right (pid, partyLabel) -> do
          customerPolicy <- applySecurityRoleAssignmentPolicy
            "account.signup.customer"
            pid
            False
            (Just pid)
            "signup-api"
            ("signup:" <> T.pack (show (fromSqlKey pid)))
            nowVal
          artistPolicy <- case (customerPolicy, mClaimArtistId) of
            (Right _, Just _) -> applySecurityRoleAssignmentPolicy
              "artist.verified-claim.artist"
              pid
              True
              (Just pid)
              "signup-api"
              ("artist-claim:" <> T.pack (show (fromSqlKey pid)))
              nowVal
            (Right _, Nothing) -> pure (Right Customer)
            (Left policyError, _) -> pure (Left policyError)
          case customerPolicy *> artistPolicy of
            Left policyError -> do
              transactionUndo
              pure (Left (SignupSecurityPolicyError policyError))
            Right _ -> do
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
                Nothing -> do
                  transactionUndo
                  pure (Left SignupProfileError)
                Just user -> pure (Right (toLoginResponse sessionToken user))

resolveParty
  :: Text
  -> Maybe Int64
  -> Text
  -> Maybe Text
  -> UTCTime
  -> SqlPersistT IO (Either SignupDbError (PartyId, Text))
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
        , partyStripeCustomerId = Nothing
        , partyCountryCode = Nothing
        , partyCountryId = Nothing
        , partyCreatedAt = nowVal
        }
  pid <- insert partyRecord
  pure (Right (pid, displayNameText))
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
              case validateSignupArtistClaimEmail emailVal (M.partyPrimaryEmail party) of
                Left _ -> pure (Left SignupArtistUnavailable)
                Right () -> do
                  let normalizedPhone = cleanOptional phoneVal
                      normalizedEmail = Just emailVal
                      emailMissing = isNothing (cleanOptional (M.partyPrimaryEmail party))
                      updates =
                        [PartyPrimaryEmail =. normalizedEmail | emailMissing]
                        ++ [ PartyPrimaryPhone =. normalizedPhone
                           | isNothing (M.partyPrimaryPhone party)
                           , isJust normalizedPhone
                           ]
                  unless (null updates) $
                    update artistKey updates
                  pure (Right (artistKey, M.partyDisplayName party))

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
          let mResetIdentifier = do
                labelText <- apiTokenLabel apiToken
                guardResetTokenIdentifier labelText
          case mResetIdentifier of
            Nothing -> pure (Left PasswordResetInvalidToken)
            Just resetIdentifier -> do
              mCred <- lookupCredential resetIdentifier
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

    guardResetTokenIdentifier lbl =
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
  createReusableSessionToken pid (Just ("password-login:" <> uname))

createPasswordResetToken :: PartyId -> Text -> SqlPersistT IO Text
createPasswordResetToken pid emailVal =
  createTokenWithLabel pid (Just ("password-reset:" <> emailVal))

createReusableSessionToken :: PartyId -> Maybe Text -> SqlPersistT IO Text
createReusableSessionToken pid label = do
  tokenValue <- liftIO (toText <$> nextRandom)
  transactionSave
  insertResult <-
    (Right <$> insertUnique (ApiToken tokenValue pid label True))
      `catch` \sqlErr ->
        if isReadOnlySqlError sqlErr
          then pure (Left sqlErr)
          else throwM sqlErr
  case insertResult of
    Right (Just _) -> pure tokenValue
    Right Nothing -> createReusableSessionToken pid label
    Left sqlErr -> do
      transactionUndo
      mExisting <- findReusableActiveToken pid label
      case mExisting of
        Just existing -> pure existing
        Nothing -> throwM sqlErr

createTokenWithLabel :: PartyId -> Maybe Text -> SqlPersistT IO Text
createTokenWithLabel pid label = do
  tokenValue <- liftIO (toText <$> nextRandom)
  inserted <- insertUnique (ApiToken tokenValue pid label True)
  case inserted of
    Nothing -> createTokenWithLabel pid label
    Just _ -> pure tokenValue

findReusableActiveToken :: PartyId -> Maybe Text -> SqlPersistT IO (Maybe Text)
findReusableActiveToken pid preferredLabel = do
  candidates <- case preferredLabel of
    Just lbl ->
      selectList
        [ ApiTokenPartyId ==. pid
        , ApiTokenActive ==. True
        , ApiTokenLabel ==. Just lbl
        ]
        [Asc ApiTokenId]
    Nothing ->
      selectList [ApiTokenPartyId ==. pid, ApiTokenActive ==. True] [Asc ApiTokenId]
  pure (apiTokenToken . entityVal <$> selectUniqueActiveToken candidates)

selectUniqueActiveToken :: [Entity ApiToken] -> Maybe (Entity ApiToken)
selectUniqueActiveToken [tokenEntity] = Just tokenEntity
selectUniqueActiveToken _ = Nothing

isReadOnlySqlError :: SqlError -> Bool
isReadOnlySqlError sqlErr = sqlState sqlErr == BS8.pack "25006"

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
  , accountCreated = Nothing
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

throwBadRequest :: Text -> AppM a
throwBadRequest msg = throwError err400 { errBody = BL.fromStrict (TE.encodeUtf8 msg) }
