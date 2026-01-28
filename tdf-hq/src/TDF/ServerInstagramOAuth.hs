{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.ServerInstagramOAuth
  ( instagramOAuthServer
  ) where

import           Control.Exception          (SomeException, displayException, try)
import           Control.Monad              (forM, forM_, when)
import           Control.Monad.Except       (MonadError, catchError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Data.Aeson                 (FromJSON(..), eitherDecode, withObject, (.:), (.:?), (.!=))
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time                  (getCurrentTime)
import           Data.Time.Format.ISO8601   (iso8601ParseM)
import           Database.Persist           (upsert, (=.))
import           Database.Persist.Sql       (runSqlPool)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, Request, Response, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status  (statusCode)
import           Network.HTTP.Types.URI     (renderSimpleQuery)
import           Servant

import           TDF.API.InstagramOAuth
import           TDF.Auth                   (AuthedUser(..))
import           TDF.Config                 (AppConfig(..), resolveConfiguredAppBase)
import           TDF.DB                     (Env(..))
import           TDF.Models                 (EntityField(SocialSyncAccountAccessToken, SocialSyncAccountHandle, SocialSyncAccountPartyId, SocialSyncAccountStatus, SocialSyncAccountUpdatedAt), SocialSyncAccount(..))

data FacebookAccessToken = FacebookAccessToken
  { fatAccessToken :: Text
  , fatTokenType   :: Text
  , fatExpiresIn   :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON FacebookAccessToken where
  parseJSON = withObject "FacebookAccessToken" $ \o ->
    FacebookAccessToken <$> o .: "access_token"
                       <*> o .:? "token_type" .!= "bearer"
                       <*> o .:? "expires_in"

data FacebookUser = FacebookUser
  { fuId   :: Text
  , fuName :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON FacebookUser where
  parseJSON = withObject "FacebookUser" $ \o ->
    FacebookUser <$> o .: "id"
                 <*> o .:? "name"

data FacebookPage = FacebookPage
  { fpId          :: Text
  , fpName        :: Text
  , fpAccessToken :: Text
  } deriving (Show, Generic)

instance FromJSON FacebookPage where
  parseJSON = withObject "FacebookPage" $ \o ->
    FacebookPage <$> o .: "id"
                 <*> o .: "name"
                 <*> o .: "access_token"

newtype FacebookPageList = FacebookPageList { fplData :: [FacebookPage] }

instance FromJSON FacebookPageList where
  parseJSON = withObject "FacebookPageList" $ \o ->
    FacebookPageList <$> o .:? "data" .!= []

newtype InstagramBusinessAccount = InstagramBusinessAccount { ibaId :: Text }
  deriving (Show)

instance FromJSON InstagramBusinessAccount where
  parseJSON = withObject "InstagramBusinessAccount" $ \o ->
    InstagramBusinessAccount <$> o .: "id"

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
    InstagramUser <$> o .: "id"
                  <*> o .:? "username"

data InstagramMedia = InstagramMedia
  { imId        :: Text
  , imCaption   :: Maybe Text
  , imMediaUrl  :: Maybe Text
  , imPermalink :: Maybe Text
  , imTimestamp :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON InstagramMedia where
  parseJSON = withObject "InstagramMedia" $ \o ->
    InstagramMedia <$> o .: "id"
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
      Env{envConfig, envPool} <- asks id
      (appId, appSecret) <- loadFacebookCreds envConfig
      let redirectUri = resolveInstagramRedirectUri envConfig ioeRedirectUri
      manager <- liftIO $ newManager tlsManagerSettings

      shortToken <- requestFacebookToken manager envConfig appId appSecret redirectUri ioeCode
      longToken <- requestLongLivedToken manager envConfig appId appSecret (fatAccessToken shortToken)
      let token = fromMaybe shortToken longToken
          accessToken = fatAccessToken token
          expiresIn = fromMaybe 3600 (fatExpiresIn token)

      fbUser <- requestFacebookUser manager envConfig accessToken
      pages <- requestFacebookPages manager envConfig accessToken
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
              , SocialSyncAccountStatus =. "connected"
              , SocialSyncAccountUpdatedAt =. Just now
              ]
            pure ()

      let pagesDto = map toPageDTO pageContexts
          primary = listToMaybe (filter hasInstagram pageContexts)
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

    hasInstagram PageContext{..} = pcInstagramUserId /= Nothing

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

loadFacebookCreds :: MonadError ServerError m => AppConfig -> m (Text, Text)
loadFacebookCreds cfg =
  case (facebookAppId cfg, facebookAppSecret cfg) of
    (Just appId, Just secret) -> pure (appId, secret)
    _ ->
      throwError err503
        { errBody = "Facebook app credentials not configured (FACEBOOK_APP_ID / FACEBOOK_APP_SECRET)." }

resolveInstagramRedirectUri :: AppConfig -> Maybe Text -> Text
resolveInstagramRedirectUri cfg mProvided =
  fromMaybe (resolveConfiguredAppBase cfg <> "/oauth/instagram/callback") (mProvided >>= nonEmptyText)
  where
    nonEmptyText txt =
      let trimmed = T.strip txt
      in if T.null trimmed then Nothing else Just trimmed

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
      ]) `catchError` \_ -> pure Nothing

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
    Left err -> throwError err500 { errBody = BL.fromStrict (TE.encodeUtf8 (T.pack (displayException err))) }
    Right req -> pure req
  where
    toParam (k, v) = (TE.encodeUtf8 k, TE.encodeUtf8 v)
