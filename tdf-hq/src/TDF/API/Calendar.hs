{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Calendar where

import           Servant
import           Data.Aeson
  ( FromJSON (parseJSON)
  , Options
  , ToJSON
  , Value(..)
  , defaultOptions
  , genericParseJSON
  , rejectUnknownFields
  , withObject
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Aeson.Types (Parser)
import           Data.Char
  ( GeneralCategory(Format, LineSeparator, ParagraphSeparator)
  , generalCategory
  , isControl
  , isSpace
  )
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

requiredAuthorizationCode :: String -> Text -> Parser Text
requiredAuthorizationCode fieldName raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then fail (fieldName <> " must not be blank")
       else if T.any isControl trimmed
         then fail (fieldName <> " must not contain control characters")
       else if T.any isHiddenCalendarIdChar trimmed
         then fail (fieldName <> " must not contain hidden formatting characters")
       else if T.any isSpace trimmed
         then fail (fieldName <> " must not contain whitespace")
       else if T.length trimmed > maxCalendarAuthorizationCodeChars
         then fail (fieldName <> " must be 4096 characters or fewer")
       else pure trimmed

maxCalendarAuthorizationCodeChars :: Int
maxCalendarAuthorizationCodeChars = 4096

normalizeCalendarId :: Text -> Either Text Text
normalizeCalendarId raw =
  let trimmed = T.strip raw
  in if T.null trimmed
       then Left "calendarId must not be blank"
       else if T.any isControl trimmed
         then Left "calendarId must not contain control characters"
         else if T.any isHiddenCalendarIdChar trimmed
           then Left "calendarId must not contain hidden formatting characters"
         else if T.any isSpace trimmed
           then Left "calendarId must not contain whitespace"
          else if T.length trimmed > maxCalendarIdChars
            then Left "calendarId must be 1024 characters or fewer"
           else Right trimmed

maxCalendarIdChars :: Int
maxCalendarIdChars = 1024

isHiddenCalendarIdChar :: Char -> Bool
isHiddenCalendarIdChar ch =
  generalCategory ch `elem` [Format, LineSeparator, ParagraphSeparator]

requiredCalendarId :: Text -> Parser Text
requiredCalendarId raw =
  either (fail . T.unpack) pure (normalizeCalendarId raw)

optionalRedirectUri :: Maybe Text -> Parser (Maybe Text)
optionalRedirectUri Nothing = pure Nothing
optionalRedirectUri (Just raw) =
  let redirectUriVal = T.strip raw
  in if T.null redirectUriVal
       then fail "redirectUri must be omitted instead of blank"
       else if T.any isControl redirectUriVal
         then fail "redirectUri must not contain control characters"
       else if T.any isHiddenCalendarIdChar redirectUriVal
         then fail "redirectUri must not contain hidden formatting characters"
       else if T.any isSpace redirectUriVal
         then fail "redirectUri must not contain whitespace"
       else if T.length redirectUriVal > maxCalendarRedirectUriChars
         then fail "redirectUri must be 2048 characters or fewer"
       else pure (Just redirectUriVal)

maxCalendarRedirectUriChars :: Int
maxCalendarRedirectUriChars = 2048

rejectNullTokenRedirectUri :: Value -> Parser ()
rejectNullTokenRedirectUri =
  withObject "TokenExchangeIn" $ \o ->
    case KeyMap.lookup (Key.fromText "redirectUri") o of
      Just Null -> fail "redirectUri must be omitted instead of null"
      _ -> pure ()

data AuthUrlResponse = AuthUrlResponse
  { url :: Text
  } deriving (Show, Generic)
instance ToJSON AuthUrlResponse

data TokenExchangeIn = TokenExchangeIn
  { code        :: Text
  , redirectUri :: Maybe Text
  , calendarId  :: Text
  } deriving (Show, Generic)
instance FromJSON TokenExchangeIn where
  parseJSON value = do
    rejectNullTokenRedirectUri value
    TokenExchangeIn rawCode rawRedirectUri rawCalendarId <-
      genericParseJSON strictObjectOptions value
    TokenExchangeIn
      <$> requiredAuthorizationCode "code" rawCode
      <*> optionalRedirectUri rawRedirectUri
      <*> requiredCalendarId rawCalendarId

data CalendarConfigDTO = CalendarConfigDTO
  { configId   :: Int
  , calendarId :: Text
  , syncCursor :: Maybe Text
  , syncedAt   :: Maybe UTCTime
  } deriving (Show, Generic)
instance ToJSON CalendarConfigDTO

data SyncRequest = SyncRequest
  { calendarId :: Text
  , from       :: Maybe UTCTime
  , to         :: Maybe UTCTime
  } deriving (Show, Generic)
instance FromJSON SyncRequest where
  parseJSON value = do
    SyncRequest rawCalendarId rawFrom rawTo <-
      genericParseJSON strictObjectOptions value
    case (rawFrom, rawTo) of
      (Just fromTs, Just toTs) | toTs < fromTs ->
        fail "to must be on or after from"
      _ ->
        SyncRequest
          <$> requiredCalendarId rawCalendarId
          <*> pure rawFrom
          <*> pure rawTo

data SyncResult = SyncResult
  { created :: Int
  , updated :: Int
  , deleted :: Int
  , cursor  :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SyncResult

data CalendarEventDTO = CalendarEventDTO
  { eventId    :: Int
  , googleId   :: Text
  , calendarId :: Text
  , status     :: Text
  , summary    :: Maybe Text
  , description :: Maybe Text
  , location    :: Maybe Text
  , startAt    :: Maybe UTCTime
  , endAt      :: Maybe UTCTime
  , updatedAt  :: Maybe UTCTime
  , htmlLink   :: Maybe Text
  , rawPayload :: Maybe Value
  } deriving (Show, Generic)
instance ToJSON CalendarEventDTO

type CalendarAPI =
       "v1" :> "auth-url" :> Post '[JSON] AuthUrlResponse
  :<|> "v1" :> "tokens"   :> ReqBody '[JSON] TokenExchangeIn :> Post '[JSON] CalendarConfigDTO
  :<|> "v1" :> "config"   :> QueryParam "calendarId" Text :> Get '[JSON] (Maybe CalendarConfigDTO)
  :<|> "v1" :> "sync"     :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResult
  :<|> "v1" :> "events"   :> QueryParam "calendarId" Text :> QueryParam "from" UTCTime :> QueryParam "to" UTCTime :> QueryParam "status" Text :> Get '[JSON] [CalendarEventDTO]
