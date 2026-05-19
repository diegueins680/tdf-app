{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.ServerInstagramOAuth
  ( instagramOAuthServer
  , FacebookAccessToken(..)
  , FacebookPage(..)
  , FacebookPageList(..)
  , resolveInstagramRedirectUri
  , sanitizeFacebookGraphErrorMessage
  , shouldFallbackToShortInstagramToken
  , selectPrimaryInstagramCandidate
  , validateInstagramRedirectUri
  , validateInstagramUsername
  ) where

import           Control.Exception          (SomeException, displayException, try)
import           Control.Monad              (forM, forM_, unless, when)
import           Control.Monad.Except       (MonadError, catchError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Aeson                 (FromJSON(..), eitherDecode, withObject, (.:), (.:?), (.!=))
import           Data.Aeson.Types           (Parser, parseEither)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isAlphaNum
  , isControl
  , isSpace
  )
import           Data.List                  (find, nub)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Text.Encoding.Error   (lenientDecode)
import           Data.Time                  (getCurrentTime)
import           Data.Time.Format.ISO8601   (iso8601ParseM)
import           Database.Persist           (Entity(..), SelectOpt(Desc), selectList, upsert, (=.), (==.))
import           Database.Persist.Sql       (runSqlPool)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, Request, Response, httpLbs, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Types.Status  (statusCode)
import           Network.HTTP.Types.URI     (renderSimpleQuery)
import           Servant
import           Text.Read                  (readMaybe)

import           TDF.API.InstagramOAuth
import           TDF.Auth                   (AuthedUser(..), hasSocialInboxAccess)
import           TDF.Config
  ( AppConfig(..)
  , normalizeConfiguredBaseUrl
  , resolveConfiguredAppBase
  )
import           TDF.DB                     (Env(..), sharedTlsManager)
import           TDF.Models                 (EntityField(SocialSyncAccountAccessToken, SocialSyncAccountHandle, SocialSyncAccountPartyId, SocialSyncAccountPlatform, SocialSyncAccountStatus, SocialSyncAccountTokenExpiresAt, SocialSyncAccountUpdatedAt), SocialSyncAccount(..))

data FacebookAccessToken = FacebookAccessToken
  { fatAccessToken :: Text
  , fatTokenType   :: Text
  , fatExpiresIn   :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON FacebookAccessToken where
  parseJSON = withObject "FacebookAccessToken" $ \o -> do
    accessToken <- normalizeFacebookAccessToken =<< o .: "access_token"
    tokenType <- normalizeFacebookTokenType =<< o .:? "token_type"
    expiresIn <- traverse validateFacebookExpiresIn =<< o .:? "expires_in"
    pure FacebookAccessToken
      { fatAccessToken = accessToken
      , fatTokenType = tokenType
      , fatExpiresIn = expiresIn
      }

normalizeFacebookAccessToken :: Text -> Parser Text
normalizeFacebookAccessToken =
  normalizeFacebookAccessTokenField "Facebook access_token"

normalizeFacebookAccessTokenField :: Text -> Text -> Parser Text
normalizeFacebookAccessTokenField fieldName rawToken =
  let tokenValue = T.strip rawToken
      fieldLabel = T.unpack fieldName
  in if T.null tokenValue
       then fail (fieldLabel <> " must not be blank")
       else if T.any isUnsafeFacebookAccessTokenChar tokenValue
         then fail $
           fieldLabel
             <> " must not contain whitespace or control characters "
             <> "or hidden formatting characters"
         else if T.length tokenValue > maxFacebookAccessTokenChars
           then fail (fieldLabel <> " must be 4096 characters or fewer")
         else pure tokenValue

maxFacebookAccessTokenChars :: Int
maxFacebookAccessTokenChars = 4096

isUnsafeFacebookAccessTokenChar :: Char -> Bool
isUnsafeFacebookAccessTokenChar ch =
  isSpace ch || isControl ch || generalCategory ch == Format

normalizeFacebookGraphId :: Text -> Text -> Parser Text
normalizeFacebookGraphId fieldName rawId =
  let graphId = T.strip rawId
      fieldLabel = T.unpack fieldName
  in if T.null graphId
       then fail (fieldLabel <> " must not be blank")
       else if T.any isUnsafeFacebookAccessTokenChar graphId
         then fail $
           fieldLabel
             <> " must not contain whitespace or control characters "
             <> "or hidden formatting characters"
       else if T.length graphId > maxFacebookGraphIdChars
         then fail (fieldLabel <> " must be 256 characters or fewer")
       else if not (T.all isFacebookGraphIdChar graphId)
         then fail $
           fieldLabel
             <> " must contain only ASCII letters, digits, '-' or '_'"
       else pure graphId

maxFacebookGraphIdChars :: Int
maxFacebookGraphIdChars = 256

isFacebookGraphIdChar :: Char -> Bool
isFacebookGraphIdChar ch =
  (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || (ch >= '0' && ch <= '9')
    || ch == '-'
    || ch == '_'

normalizeFacebookRequiredText :: Text -> Int -> Text -> Parser Text
normalizeFacebookRequiredText fieldName maxChars rawText =
  let textValue = T.strip rawText
      fieldLabel = T.unpack fieldName
  in if T.null textValue
       then fail (fieldLabel <> " must not be blank")
       else if T.any isUnsafeFacebookGraphTextChar textValue
         then fail $
           fieldLabel
             <> " must not contain control, separator, or hidden formatting characters"
       else if T.length textValue > maxChars
         then fail (fieldLabel <> " must be " <> show maxChars <> " characters or fewer")
       else pure textValue

isUnsafeFacebookGraphTextChar :: Char -> Bool
isUnsafeFacebookGraphTextChar ch =
  isControl ch || generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

normalizeFacebookTokenType :: Maybe Text -> Parser Text
normalizeFacebookTokenType Nothing = pure "bearer"
normalizeFacebookTokenType (Just rawTokenType) =
  let tokenType = T.toLower (T.strip rawTokenType)
  in if tokenType == "bearer"
       then pure tokenType
       else fail "Facebook token_type must be Bearer"

validateFacebookExpiresIn :: Int -> Parser Int
validateFacebookExpiresIn expiresIn
  | expiresIn > 0 = pure expiresIn
  | otherwise = fail "Facebook expires_in must be positive"

data FacebookUser = FacebookUser
  { fuId   :: Text
  , fuName :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON FacebookUser where
  parseJSON = withObject "FacebookUser" $ \o ->
    FacebookUser <$> (normalizeFacebookGraphId "Facebook user id" =<< o .: "id")
                 <*> o .:? "name"

data FacebookPage = FacebookPage
  { fpId          :: Text
  , fpName        :: Text
  , fpAccessToken :: Text
  } deriving (Show, Generic)

instance FromJSON FacebookPage where
  parseJSON = withObject "FacebookPage" $ \o ->
    FacebookPage
      <$> (normalizeFacebookGraphId "Facebook page id" =<< o .: "id")
      <*> (normalizeFacebookRequiredText "Facebook page name" 200 =<< o .: "name")
      <*> (normalizeFacebookAccessTokenField "Facebook page access_token" =<< o .: "access_token")

newtype FacebookPageList = FacebookPageList { fplData :: [FacebookPage] }
  deriving (Show)

instance FromJSON FacebookPageList where
  parseJSON = withObject "FacebookPageList" $ \o -> do
    pages <- o .: "data"
    let pageIds = map fpId pages
    when (length pageIds /= length (nub pageIds)) $
      fail "Facebook page list must not contain duplicate page ids"
    pure (FacebookPageList pages)

newtype InstagramBusinessAccount = InstagramBusinessAccount { ibaId :: Text }
  deriving (Show)

instance FromJSON InstagramBusinessAccount where
  parseJSON = withObject "InstagramBusinessAccount" $ \o ->
    InstagramBusinessAccount <$>
      (normalizeFacebookGraphId "Instagram business account id" =<< o .: "id")

data PageInstagramAccount = PageInstagramAccount
  { piaInstagramBusinessAccount :: Maybe InstagramBusinessAccount
  } deriving (Show, Generic)

instance FromJSON PageInstagramAccount where
  parseJSON = withObject "PageInstagramAccount" $ \o ->
    PageInstagramAccount <$> o .:? "instagram_business_account"

data InstagramUser = InstagramUser
  { iuId       :: Text
  , iuUsername :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON InstagramUser where
  parseJSON = withObject "InstagramUser" $ \o ->
    InstagramUser <$> (normalizeFacebookGraphId "Instagram user id" =<< o .: "id")
                  <*> ((o .:? "username") >>= traverse parseInstagramUsername)

parseInstagramUsername :: Text -> Parser Text
parseInstagramUsername rawUsername =
  either (fail . T.unpack) pure (validateInstagramUsername rawUsername)

validateInstagramUsername :: Text -> Either Text Text
validateInstagramUsername rawUsername
  | T.null username =
      Left "Instagram username must not be blank"
  | T.length username > maxInstagramUsernameChars =
      Left $
        "Instagram username must be "
          <> T.pack (show maxInstagramUsernameChars)
          <> " characters or fewer"
  | T.any isUnsafeInstagramUsernameChar username =
      Left $
        "Instagram username must not contain whitespace, control, "
          <> "or hidden formatting characters"
  | not (T.any isInstagramUsernameAtom username) =
      Left "Instagram username must contain at least one ASCII letter or digit"
  | not (T.all isInstagramUsernameChar username) =
      Left "Instagram username must contain only ASCII letters, digits, '.', or '_'"
  | otherwise =
      Right username
  where
    username = T.toLower (T.strip rawUsername)

maxInstagramUsernameChars :: Int
maxInstagramUsernameChars = 64

isUnsafeInstagramUsernameChar :: Char -> Bool
isUnsafeInstagramUsernameChar ch =
  isSpace ch || isControl ch || generalCategory ch == Format

isInstagramUsernameAtom :: Char -> Bool
isInstagramUsernameAtom ch =
  (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9')

isInstagramUsernameChar :: Char -> Bool
isInstagramUsernameChar ch =
  isInstagramUsernameAtom ch || ch == '.' || ch == '_'

data InstagramMedia = InstagramMedia
  { imId        :: Text
  , imCaption   :: Maybe Text
  , imMediaUrl  :: Maybe Text
  , imPermalink :: Maybe Text
  , imTimestamp :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON InstagramMedia where
  parseJSON = withObject "InstagramMedia" $ \o ->
    InstagramMedia <$> (normalizeFacebookGraphId "Instagram media id" =<< o .: "id")
                   <*> o .:? "caption"
                   <*> o .:? "media_url"
                   <*> o .:? "permalink"
                   <*> o .:? "timestamp"

newtype InstagramMediaList = InstagramMediaList { imlData :: [InstagramMedia] }

instance FromJSON InstagramMediaList where
  parseJSON = withObject "InstagramMediaList" $ \o ->
    InstagramMediaList <$> o .:? "data" .!= []

data PageContext = PageContext
  { pcPageId          :: Text
  , pcPageName        :: Text
  , pcAccessToken     :: Text
  , pcInstagramUserId :: Maybe Text
  , pcInstagramHandle :: Maybe Text
  }

instagramOAuthServer
  :: ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => AuthedUser
  -> ServerT InstagramOAuthAPI m
instagramOAuthServer user = exchangeHandler
  where
    exchangeHandler InstagramOAuthExchangeRequest{..} = do
      unless (hasSocialInboxAccess user) $
        throwError err403 { errBody = "Missing required module access" }
      Env{envConfig, envPool} <- asks id
      (appId, appSecret) <- loadFacebookCreds envConfig
      redirectUri <- either throwError pure (resolveInstagramRedirectUri envConfig ioeRedirectUri)
      manager <- pure sharedTlsManager

      shortToken <- requestFacebookToken manager envConfig appId appSecret redirectUri ioeCode
      longToken <- requestLongLivedToken manager envConfig appId appSecret (fatAccessToken shortToken)
      let token = fromMaybe shortToken longToken
          accessToken = fatAccessToken token
          expiresIn = fromMaybe 0 (fatExpiresIn token)

      fbUser <- requestFacebookUser manager envConfig accessToken
      pages <- requestFacebookPages manager envConfig accessToken
      existing <- liftIO $ flip runSqlPool envPool $ do
        selectList
          [ SocialSyncAccountPartyId ==. Just (auPartyId user)
          , SocialSyncAccountPlatform ==. "instagram"
          ]
          [Desc SocialSyncAccountUpdatedAt]
      let preferredIds = map (socialSyncAccountExternalUserId . entityVal) existing

      pageContexts <- forM pages $ \page -> do
        mIg <- requestInstagramAccount manager envConfig (fpAccessToken page) (fpId page)
        mHandle <- case mIg of
          Nothing -> pure Nothing
          Just ig -> do
            userInfo <- requestInstagramUser manager envConfig (fpAccessToken page) (ibaId ig)
            pure (iuUsername userInfo)
        let igId = ibaId <$> mIg
        pure PageContext
          { pcPageId = fpId page
          , pcPageName = fpName page
          , pcAccessToken = fpAccessToken page
          , pcInstagramUserId = igId
          , pcInstagramHandle = mHandle
          }

      primary <-
        either throwError pure (selectPrimaryInstagramPage preferredIds pageContexts)

      now <- liftIO getCurrentTime
      liftIO $ flip runSqlPool envPool $ do
        forM_ pageContexts $ \ctx ->
          forM_ (pcInstagramUserId ctx) $ \igUserId -> do
            let record = SocialSyncAccount
                  { socialSyncAccountPartyId = Just (auPartyId user)
                  , socialSyncAccountArtistProfileId = Nothing
                  , socialSyncAccountPlatform = "instagram"
                  , socialSyncAccountExternalUserId = igUserId
                  , socialSyncAccountHandle = pcInstagramHandle ctx
                  , socialSyncAccountAccessToken = Just (pcAccessToken ctx)
                  , socialSyncAccountTokenExpiresAt = Nothing
                  , socialSyncAccountStatus = "connected"
                  , socialSyncAccountLastSyncedAt = Nothing
                  , socialSyncAccountCreatedAt = now
                  , socialSyncAccountUpdatedAt = Just now
                  }
            _ <- upsert record
              [ SocialSyncAccountPartyId =. Just (auPartyId user)
              , SocialSyncAccountHandle =. pcInstagramHandle ctx
              , SocialSyncAccountAccessToken =. Just (pcAccessToken ctx)
              , SocialSyncAccountTokenExpiresAt =. Nothing
              , SocialSyncAccountStatus =. "connected"
              , SocialSyncAccountUpdatedAt =. Just now
              ]
            pure ()

      let pagesDto = map toPageDTO pageContexts
      (mPrimaryId, mPrimaryHandle, media) <-
        case primary of
          Nothing -> pure (Nothing, Nothing, [])
          Just ctx ->
            case pcInstagramUserId ctx of
              Nothing -> pure (Nothing, Nothing, [])
              Just igId -> do
                mediaList <- requestInstagramMedia manager envConfig (pcAccessToken ctx) igId
                let mediaDto = map toMediaDTO mediaList
                pure (Just igId, pcInstagramHandle ctx, mediaDto)

      pure InstagramOAuthExchangeResponse
        { ioeUserId = fuId fbUser
        , ioeUserName = fuName fbUser
        , ioeTokenType = fatTokenType token
        , ioeExpiresIn = expiresIn
        , ioePages = pagesDto
        , ioeInstagramUserId = mPrimaryId
        , ioeInstagramUsername = mPrimaryHandle
        , ioeMedia = media
        }

    toPageDTO PageContext{..} =
      InstagramOAuthPage
        { iopPageId = pcPageId
        , iopPageName = pcPageName
        , iopInstagramUserId = pcInstagramUserId
        , iopInstagramUsername = pcInstagramHandle
        }

    toMediaDTO InstagramMedia{..} =
      InstagramMediaDTO
        { imdId = imId
        , imdCaption = imCaption
        , imdMediaUrl = imMediaUrl
        , imdPermalink = imPermalink
        , imdTimestamp = imTimestamp >>= (iso8601ParseM . T.unpack)
        }

selectPrimaryInstagramPage :: [Text] -> [PageContext] -> Either ServerError (Maybe PageContext)
selectPrimaryInstagramPage preferredIds contexts =
  selectPrimaryInstagramCandidate
    preferredIds
    [ (igUserId, ctx) | ctx <- contexts, Just igUserId <- [pcInstagramUserId ctx] ]

selectPrimaryInstagramCandidate :: [Text] -> [(Text, a)] -> Either ServerError (Maybe a)
selectPrimaryInstagramCandidate preferredIds candidates = do
  preferredIdsClean <- validatePreferredInstagramCandidateIds preferredIds
  candidatesClean <- validateInstagramCandidateIds candidates
  let candidateIds = map fst candidatesClean
      hasDuplicateCandidateIds =
        length candidateIds /= length (nub candidateIds)
      go [] = Nothing
      go (pid:rest) =
        case find (\(candidateId, _) -> candidateId == pid) candidatesClean of
          Just (_, candidate) -> Just candidate
          Nothing -> go rest
  if hasDuplicateCandidateIds
    then
      Left err409
        { errBody =
            "Instagram OAuth candidate pages contain duplicate Instagram user ids; reconnect "
              <> "from an existing account or remove duplicate page links."
        }
    else
      case go preferredIdsClean of
        Just candidate -> Right (Just candidate)
        Nothing ->
          case candidatesClean of
            [] -> Right Nothing
            [(_, candidate)] -> Right (Just candidate)
            _ ->
              Left err409
                { errBody =
                    "Instagram OAuth primary page fallback is ambiguous; reconnect from an existing "
                      <> "account or keep only one Instagram page connected."
                }

validateInstagramCandidateIds :: [(Text, a)] -> Either ServerError [(Text, a)]
validateInstagramCandidateIds candidates =
  case traverse validateCandidate candidates of
    Left _ ->
      Left err409
        { errBody =
            "Instagram OAuth candidate page ids are malformed; reconnect "
              <> "from Facebook and try again."
        }
    Right cleanCandidates -> Right cleanCandidates
  where
    validateCandidate (rawId, candidate) =
      case parseEither (normalizeFacebookGraphId "Instagram candidate account id") rawId of
        Right cleanId
          | cleanId == rawId -> Right (cleanId, candidate)
        _ -> Left ("invalid Instagram candidate account id" :: String)

validatePreferredInstagramCandidateIds :: [Text] -> Either ServerError [Text]
validatePreferredInstagramCandidateIds preferredIds =
  case traverse validatePreferredId preferredIds of
    Left _ ->
      Left err409
        { errBody =
            "Instagram OAuth stored preferred page ids are malformed; reconnect "
              <> "from an existing account."
        }
    Right ids
      | length ids /= length (nub ids) ->
          Left err409
            { errBody =
                "Instagram OAuth stored preferred page ids contain duplicates; reconnect "
                  <> "from an existing account."
            }
      | otherwise -> Right ids
  where
    validatePreferredId rawId =
      case parseEither (normalizeFacebookGraphId "Instagram preferred account id") rawId of
        Right cleanId
          | cleanId == rawId -> Right cleanId
        _ -> Left ("invalid preferred Instagram account id" :: String)

loadFacebookCreds :: MonadError ServerError m => AppConfig -> m (Text, Text)
loadFacebookCreds cfg =
  case (facebookAppId cfg, facebookAppSecret cfg) of
    (Just appId, Just secret) -> pure (appId, secret)
    _ ->
      throwError err503
        { errBody = "Facebook app credentials not configured (FACEBOOK_APP_ID / FACEBOOK_APP_SECRET)." }

resolveInstagramRedirectUri :: AppConfig -> Maybe Text -> Either ServerError Text
resolveInstagramRedirectUri cfg mProvided = do
  configuredRedirectUri <-
    validateConfiguredInstagramRedirectUri
      (resolveConfiguredAppBase cfg <> "/oauth/instagram/callback")
  maybe
    (Right configuredRedirectUri)
    (validateProvidedRedirectUri configuredRedirectUri)
    mProvided
  where
    validateProvidedRedirectUri configuredRedirectUri rawRedirect = do
      redirectUri <- validateInstagramRedirectUri rawRedirect
      if redirectUri == configuredRedirectUri
        then Right redirectUri
        else
          Left err400
            { errBody = "redirectUri must match the configured Instagram OAuth callback URL" }

validateConfiguredInstagramRedirectUri :: Text -> Either ServerError Text
validateConfiguredInstagramRedirectUri rawRedirect =
  case validateInstagramRedirectUri rawRedirect of
    Right redirectUri -> Right redirectUri
    Left _ ->
      Left err503
        { errBody =
            "Configured Instagram OAuth callback URL must be an absolute https URL ending "
              <> "in /oauth/instagram/callback, or http://localhost for local development, "
              <> "without query or fragment"
        }

validateInstagramRedirectUri :: Text -> Either ServerError Text
validateInstagramRedirectUri rawRedirect =
  case normalizeConfiguredBaseUrl "redirectUri" (T.unpack rawRedirect) of
    Right (Just uri)
      | isSafeInstagramRedirectUri uri -> Right uri
      | otherwise -> invalidRedirect
    _ ->
      invalidRedirect
  where
    invalidRedirect =
      Left err400
        { errBody =
            "redirectUri must be an absolute https Instagram OAuth callback URL ending "
              <> "in /oauth/instagram/callback, or http://localhost for local development, "
              <> "without query or fragment"
        }

isSafeInstagramRedirectUri :: Text -> Bool
isSafeInstagramRedirectUri uri
  | not (instagramOAuthCallbackPath `T.isSuffixOf` uri) = False
  | "https://" `T.isPrefixOf` lowerUri =
      maybe False (not . isLocalInstagramRedirectHost) (instagramRedirectHost (T.drop 8 uri))
  | "http://" `T.isPrefixOf` lowerUri =
      maybe False isLocalInstagramRedirectHost (instagramRedirectHost (T.drop 7 uri))
  | otherwise = False
  where
    instagramOAuthCallbackPath = "/oauth/instagram/callback"
    lowerUri = T.toLower uri

instagramRedirectHost :: Text -> Maybe Text
instagramRedirectHost remainder =
  let authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') remainder
  in if T.null authority
       then Nothing
       else if "[" `T.isPrefixOf` authority
         then
           let (hostPart, rest) = T.breakOn "]" authority
           in if T.null rest
                then Nothing
                else Just (T.toLower (T.drop 1 hostPart))
         else
           let (host, _) = T.breakOn ":" authority
           in if T.null host then Nothing else Just (T.toLower host)

isLocalInstagramRedirectHost :: Text -> Bool
isLocalInstagramRedirectHost host =
  host == "localhost"
    || ".localhost" `T.isSuffixOf` host
    || host == "::1"
    || isLoopbackIpv4Host host

isLoopbackIpv4Host :: Text -> Bool
isLoopbackIpv4Host host =
  case traverse parseOctet (T.splitOn "." host) of
    Just [first, _, _, _] -> first == (127 :: Int)
    _ -> False
  where
    parseOctet segment = do
      value <- readMaybe (T.unpack segment)
      if value >= (0 :: Int) && value <= 255
        then Just value
        else Nothing

requestFacebookToken
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> Text
  -> Text
  -> Text
  -> m FacebookAccessToken
requestFacebookToken manager cfg appId secret redirectUri code =
  requestFacebookJson manager cfg "/oauth/access_token"
    [ ("client_id", appId)
    , ("redirect_uri", redirectUri)
    , ("client_secret", secret)
    , ("code", code)
    ]

requestLongLivedToken
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> Text
  -> Text
  -> m (Maybe FacebookAccessToken)
requestLongLivedToken manager cfg appId secret shortToken =
  (Just <$> requestFacebookJson manager cfg "/oauth/access_token"
      [ ("client_id", appId)
      , ("client_secret", secret)
      , ("grant_type", "fb_exchange_token")
      , ("fb_exchange_token", shortToken)
      ]) `catchError` \err ->
        if shouldFallbackToShortInstagramToken err
          then pure Nothing
          else throwError err

shouldFallbackToShortInstagramToken :: ServerError -> Bool
shouldFallbackToShortInstagramToken err =
  errHTTPCode err == 502
    && isRecoverableFacebookRequestFailure (errBody err)

isRecoverableFacebookRequestFailure :: ByteString -> Bool
isRecoverableFacebookRequestFailure body =
  case facebookRequestFailureStatus body of
    Just status -> status == 429 || status >= 500
    Nothing -> "Facebook request failed: " `BL.isPrefixOf` body

facebookRequestFailureStatus :: ByteString -> Maybe Int
facebookRequestFailureStatus body =
  case facebookRequestFailureParenthesizedStatus bodyText of
    Just status -> Just status
    Nothing -> facebookRequestFailureExceptionStatus bodyText
  where
    bodyText = TE.decodeUtf8With lenientDecode (BL.toStrict body)

facebookRequestFailureParenthesizedStatus :: Text -> Maybe Int
facebookRequestFailureParenthesizedStatus body = do
  rest <-
    T.stripPrefix
      "Facebook request failed ("
      body
  let (statusText, suffix) = T.breakOn ")" rest
  case T.stripPrefix "):" suffix of
    Nothing -> Nothing
    Just _ -> parseFacebookHttpStatus statusText

facebookRequestFailureExceptionStatus :: Text -> Maybe Int
facebookRequestFailureExceptionStatus body = do
  rest <- T.stripPrefix "Facebook request failed: " body
  let (_, statusMarker) = T.breakOn "statusCode" rest
  if T.null statusMarker
    then Nothing
    else do
      let afterMarker = T.drop (T.length ("statusCode" :: Text)) statusMarker
          afterSpaces = T.dropWhile (== ' ') afterMarker
      afterEquals <- T.stripPrefix "=" afterSpaces
      let statusText = T.takeWhile isAsciiDigit (T.dropWhile (== ' ') afterEquals)
      parseFacebookHttpStatus statusText

parseFacebookHttpStatus :: Text -> Maybe Int
parseFacebookHttpStatus statusText =
  case readMaybe (T.unpack statusText) of
    Just status | status >= 100 && status <= 599 -> Just status
    _ -> Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit ch = ch >= '0' && ch <= '9'

requestFacebookUser
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> m FacebookUser
requestFacebookUser manager cfg accessToken =
  requestFacebookJson manager cfg "/me"
    [ ("fields", "id,name")
    , ("access_token", accessToken)
    ]

requestFacebookPages
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> m [FacebookPage]
requestFacebookPages manager cfg accessToken = do
  resp <- requestFacebookJson manager cfg "/me/accounts"
    [ ("fields", "id,name,access_token")
    , ("access_token", accessToken)
    ]
  pure (fplData (resp :: FacebookPageList))

requestInstagramAccount
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> Text
  -> m (Maybe InstagramBusinessAccount)
requestInstagramAccount manager cfg pageToken pageId = do
  resp <- requestFacebookJson manager cfg ("/" <> pageId)
    [ ("fields", "instagram_business_account")
    , ("access_token", pageToken)
    ]
  pure (piaInstagramBusinessAccount (resp :: PageInstagramAccount))

requestInstagramUser
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> Text
  -> m InstagramUser
requestInstagramUser manager cfg pageToken igUserId =
  requestFacebookJson manager cfg ("/" <> igUserId)
    [ ("fields", "id,username")
    , ("access_token", pageToken)
    ]

requestInstagramMedia
  :: (MonadError ServerError m, MonadIO m)
  => Manager
  -> AppConfig
  -> Text
  -> Text
  -> m [InstagramMedia]
requestInstagramMedia manager cfg pageToken igUserId = do
  resp <- requestFacebookJson manager cfg ("/" <> igUserId <> "/media")
    [ ("fields", "id,caption,media_url,permalink,timestamp")
    , ("access_token", pageToken)
    , ("limit", "12")
    ]
  pure (imlData (resp :: InstagramMediaList))

requestFacebookJson
  :: (MonadError ServerError m, MonadIO m, FromJSON a)
  => Manager
  -> AppConfig
  -> Text
  -> [(Text, Text)]
  -> m a
requestFacebookJson manager cfg path params = do
  req <- buildRequest cfg path params
  respOrErr <- liftIO $ (try (httpLbs req manager) :: IO (Either SomeException (Response ByteString)))
  resp <- case respOrErr of
    Left err ->
      throwError err502
        { errBody =
            textBody $
              "Facebook request failed: "
                <> sanitizeFacebookGraphErrorMessage (T.pack (displayException err))
        }
    Right ok -> pure ok
  let status = statusCode (responseStatus resp)
  when (status >= 400) $ do
    let bodySnippet =
          TE.decodeUtf8With lenientDecode (BL.toStrict (responseBody resp))
    throwError err502
      { errBody =
          textBody $
            "Facebook request failed ("
              <> T.pack (show status)
              <> "): "
              <> sanitizeFacebookGraphErrorMessage bodySnippet
      }
  case eitherDecode (responseBody resp) of
    Left err ->
      throwError err502
        { errBody =
            textBody $
              "Facebook parse error: "
                <> sanitizeFacebookGraphErrorMessage (T.pack err)
        }
    Right val -> pure val

buildRequest
  :: (MonadError ServerError m, MonadIO m)
  => AppConfig
  -> Text
  -> [(Text, Text)]
  -> m Request
buildRequest cfg path params = do
  let base = T.dropWhileEnd (== '/') (facebookGraphBase cfg)
      query = renderSimpleQuery True (map toParam params)
      url = T.unpack (base <> path <> TE.decodeUtf8 query)
  reqE <- liftIO (try (parseRequest url) :: IO (Either SomeException Request))
  case reqE of
    Left err ->
      throwError err500
        { errBody =
            textBody $
              "Facebook request configuration invalid: "
                <> sanitizeFacebookGraphErrorMessage (T.pack (displayException err))
        }
    Right req -> pure req
  where
    toParam (k, v) = (TE.encodeUtf8 k, TE.encodeUtf8 v)

textBody :: Text -> BL.ByteString
textBody =
  BL.fromStrict . TE.encodeUtf8

maxFacebookGraphErrorChars :: Int
maxFacebookGraphErrorChars = 500

sanitizeFacebookGraphErrorMessage :: Text -> Text
sanitizeFacebookGraphErrorMessage raw =
  let compacted = T.unwords . T.words . redactFacebookGraphSecrets . T.map safeChar $ raw
  in if T.length compacted > maxFacebookGraphErrorChars
       then T.take maxFacebookGraphErrorChars compacted <> " [truncated]"
       else compacted
  where
    safeChar ch
      | isControl ch = ' '
      | generalCategory ch == Format = ' '
      | otherwise = ch

redactFacebookGraphSecrets :: Text -> Text
redactFacebookGraphSecrets = go Nothing
  where
    go _ textValue
      | T.null textValue = ""
    go previous textValue =
      case matchSensitiveField previous textValue of
        Just (prefix, rest) ->
          prefix <> "[redacted]" <> go Nothing rest
        Nothing ->
          let ch = T.head textValue
          in T.singleton ch <> go (Just ch) (T.tail textValue)

matchSensitiveField :: Maybe Char -> Text -> Maybe (Text, Text)
matchSensitiveField previous textValue
  | not (isSecretFieldBoundary previous) = Nothing
  | otherwise = firstMatch sensitiveFacebookGraphFields
  where
    lowered = T.toLower textValue

    firstMatch [] = Nothing
    firstMatch (fieldName:rest) =
      case parseSensitiveField fieldName lowered textValue of
        Just match -> Just match
        Nothing -> firstMatch rest

isSecretFieldBoundary :: Maybe Char -> Bool
isSecretFieldBoundary Nothing = True
isSecretFieldBoundary (Just ch) =
  not (isAlphaNum ch || ch == '_' || ch == '-')

sensitiveFacebookGraphFields :: [Text]
sensitiveFacebookGraphFields =
  [ "access_token"
  , "fb_exchange_token"
  , "client_secret"
  , "code"
  ]

parseSensitiveField :: Text -> Text -> Text -> Maybe (Text, Text)
parseSensitiveField fieldName lowered textValue
  | not (fieldName `T.isPrefixOf` lowered) = Nothing
  | otherwise = do
      let fieldLength = T.length fieldName
          fieldText = T.take fieldLength textValue
          afterField = T.drop fieldLength textValue
          (closingQuote, afterClosingQuote) = consumeOptionalQuote afterField
          (beforeSeparator, separatorCandidate) = T.span isSpace afterClosingQuote
      (separator, afterSeparator) <- T.uncons separatorCandidate
      if separator == '=' || (separator == ':' && fieldName /= "code")
        then
          let (afterSeparatorSpace, valueStart) = T.span isSpace afterSeparator
              (openingQuote, valueText, isValueEnd) = consumeValueOpeningQuote valueStart
              (_, rest) = T.break isValueEnd valueText
              prefix =
                fieldText
                  <> closingQuote
                  <> beforeSeparator
                  <> T.singleton separator
                  <> afterSeparatorSpace
                  <> openingQuote
          in Just (prefix, rest)
        else Nothing

consumeOptionalQuote :: Text -> (Text, Text)
consumeOptionalQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest)
    Just ('\'', rest) -> ("'", rest)
    _ -> ("", textValue)

consumeValueOpeningQuote :: Text -> (Text, Text, Char -> Bool)
consumeValueOpeningQuote textValue =
  case T.uncons textValue of
    Just ('"', rest) -> ("\"", rest, (== '"'))
    Just ('\'', rest) -> ("'", rest, (== '\''))
    _ -> ("", textValue, isUnquotedSecretValueEnd)

isUnquotedSecretValueEnd :: Char -> Bool
isUnquotedSecretValueEnd ch =
  isSpace ch || ch `elem` ("&,}]" :: String)
