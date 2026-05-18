{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.InstagramOAuth where

import           Data.Aeson (FromJSON(..), Options(rejectUnknownFields), ToJSON(..),
                             Value(Null, Object),
                             defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON,
                             omitNothingFields)
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import           Data.Aeson.Types (Parser)
import           Data.Char (GeneralCategory(Format), generalCategory, isControl, isSpace, toLower)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []

data InstagramOAuthExchangeRequest = InstagramOAuthExchangeRequest
  { ioeCode        :: Text
  , ioeRedirectUri :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON InstagramOAuthExchangeRequest where
  parseJSON value@(Object o) = do
    case AKM.lookup (AKey.fromText "redirectUri") o of
      Just Null -> fail "redirectUri must be omitted instead of null"
      _         -> pure ()
    request <- genericParseJSON strictObjectOptions value
    code <- maybe (fail "code cannot be blank") pure (nonEmptyText (ioeCode request))
    redirectUri <- parseOptionalRedirectUri (ioeRedirectUri request)
    if T.length code > maxInstagramOAuthCodeChars
      then fail "code must be 4096 characters or fewer"
      else if T.any isUnsafeOAuthCodeChar code
        then fail "code must not contain whitespace, control, or hidden formatting characters"
        else pure request
          { ioeCode = code
          , ioeRedirectUri = redirectUri
          }
  parseJSON value = genericParseJSON strictObjectOptions value

parseOptionalRedirectUri :: Maybe Text -> Parser (Maybe Text)
parseOptionalRedirectUri Nothing = pure Nothing
parseOptionalRedirectUri (Just rawRedirectUri) =
  case nonEmptyText rawRedirectUri of
    Nothing -> fail "redirectUri must not be blank"
    Just redirectUri
      | T.any isControl redirectUri ->
          fail "redirectUri must not contain control characters"
      | T.any isUnsafeOAuthCodeChar redirectUri ->
          fail "redirectUri must not contain whitespace or hidden formatting characters"
      | T.length redirectUri > maxInstagramOAuthRedirectUriChars ->
          fail "redirectUri must be 2048 characters or fewer"
      | otherwise ->
          pure (Just redirectUri)

maxInstagramOAuthCodeChars :: Int
maxInstagramOAuthCodeChars = 4096

maxInstagramOAuthRedirectUriChars :: Int
maxInstagramOAuthRedirectUriChars = 2048

isUnsafeOAuthCodeChar :: Char -> Bool
isUnsafeOAuthCodeChar ch =
  isSpace ch || isControl ch || generalCategory ch == Format

data InstagramOAuthPage = InstagramOAuthPage
  { iopPageId              :: Text
  , iopPageName            :: Text
  , iopInstagramUserId     :: Maybe Text
  , iopInstagramUsername   :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON InstagramOAuthPage where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

data InstagramMediaDTO = InstagramMediaDTO
  { imdId        :: Text
  , imdCaption   :: Maybe Text
  , imdMediaUrl  :: Maybe Text
  , imdPermalink :: Maybe Text
  , imdTimestamp :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON InstagramMediaDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

data InstagramOAuthExchangeResponse = InstagramOAuthExchangeResponse
  { ioeUserId            :: Text
  , ioeUserName          :: Maybe Text
  , ioeTokenType         :: Text
  , ioeExpiresIn         :: Int
  , ioePages             :: [InstagramOAuthPage]
  , ioeInstagramUserId   :: Maybe Text
  , ioeInstagramUsername :: Maybe Text
  , ioeMedia             :: [InstagramMediaDTO]
  } deriving (Show, Generic)

instance ToJSON InstagramOAuthExchangeResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3, omitNothingFields = True }

type InstagramOAuthAPI =
  "instagram" :> "oauth" :>
    "exchange" :> ReqBody '[JSON] InstagramOAuthExchangeRequest :> Post '[JSON] InstagramOAuthExchangeResponse

strictObjectOptions :: Options
strictObjectOptions =
  defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

nonEmptyText :: Text -> Maybe Text
nonEmptyText rawText =
  let trimmed = T.strip rawText
  in if T.null trimmed then Nothing else Just trimmed
