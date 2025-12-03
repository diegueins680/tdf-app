{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TDF.API.Calendar where

import           Servant
import           Data.Aeson (ToJSON, FromJSON, Value)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)

data AuthUrlResponse = AuthUrlResponse
  { url :: Text
  } deriving (Show, Generic)
instance ToJSON AuthUrlResponse

data TokenExchangeIn = TokenExchangeIn
  { code        :: Text
  , redirectUri :: Maybe Text
  , calendarId  :: Text
  } deriving (Show, Generic)
instance FromJSON TokenExchangeIn

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
instance FromJSON SyncRequest

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
  :<|> "v1" :> "sync"     :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResult
  :<|> "v1" :> "events"   :> QueryParam "calendarId" Text :> QueryParam "from" UTCTime :> QueryParam "to" UTCTime :> QueryParam "status" Text :> Get '[JSON] [CalendarEventDTO]
