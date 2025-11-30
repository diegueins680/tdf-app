{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.Routes.Courses
  ( CourseSession(..)
  , SyllabusItem(..)
  , CourseMetadata(..)
  , UTMTags(..)
  , CourseRegistrationRequest(..)
  , CourseRegistrationResponse(..)
  , CourseRegistrationStatusUpdate(..)
  , CoursesPublicAPI
  , CoursesAdminAPI
  , WhatsAppWebhookAPI
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (Day)
import           GHC.Generics (Generic)
import           Servant

import           TDF.WhatsApp.Types (WAMetaWebhook)
import qualified TDF.DTO

data CourseSession = CourseSession
  { label :: Text
  , date  :: Day
  } deriving (Show, Generic)

instance ToJSON CourseSession

data SyllabusItem = SyllabusItem
  { title  :: Text
  , topics :: [Text]
  } deriving (Show, Generic)

instance ToJSON SyllabusItem

data CourseMetadata = CourseMetadata
  { slug           :: Text
  , title          :: Text
  , subtitle       :: Text
  , format         :: Text
  , duration       :: Text
  , price          :: Double
  , currency       :: Text
  , capacity       :: Int
  , locationLabel  :: Text
  , locationMapUrl :: Text
  , daws           :: [Text]
  , includes       :: [Text]
  , sessions       :: [CourseSession]
  , syllabus       :: [SyllabusItem]
  , whatsappCtaUrl :: Text
  , landingUrl     :: Text
  } deriving (Show, Generic)

instance ToJSON CourseMetadata

data UTMTags = UTMTags
  { source   :: Maybe Text
  , medium   :: Maybe Text
  , campaign :: Maybe Text
  , content  :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON UTMTags
instance ToJSON UTMTags

data CourseRegistrationRequest = CourseRegistrationRequest
  { fullName  :: Maybe Text
  , email     :: Maybe Text
  , phoneE164 :: Maybe Text
  , source    :: Text
  , howHeard  :: Maybe Text
  , utm       :: Maybe UTMTags
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationRequest
instance ToJSON CourseRegistrationRequest

data CourseRegistrationResponse = CourseRegistrationResponse
  { id     :: Int64
  , status :: Text
  } deriving (Show, Generic)

instance ToJSON CourseRegistrationResponse

data CourseRegistrationStatusUpdate = CourseRegistrationStatusUpdate
  { status :: Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationStatusUpdate

type CoursesPublicAPI =
       "public" :> "courses" :> Capture "slug" Text :> Get '[JSON] CourseMetadata
  :<|> "public" :> "courses" :> Capture "slug" Text :> "registrations" :> ReqBody '[JSON] CourseRegistrationRequest :> PostCreated '[JSON] CourseRegistrationResponse

type CoursesAdminAPI =
       "courses" :> "registrations" :>
         QueryParam "slug" Text :>
         QueryParam "status" Text :>
         QueryParam "limit" Int :>
         Get '[JSON] [TDF.DTO.CourseRegistrationDTO]
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> Get '[JSON] TDF.DTO.CourseRegistrationDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "status" :> ReqBody '[JSON] CourseRegistrationStatusUpdate :> Patch '[JSON] CourseRegistrationResponse

type WhatsAppWebhookAPI =
       "webhooks" :> "whatsapp" :> QueryParam "hub.mode" Text :> QueryParam "hub.verify_token" Text :> QueryParam "hub.challenge" Text :> Get '[PlainText] Text
  :<|> "webhooks" :> "whatsapp" :> ReqBody '[JSON] WAMetaWebhook :> Post '[JSON] NoContent
