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
  , CourseRegistrationNotesUpdate(..)
  , CourseRegistrationReceiptCreate(..)
  , CourseRegistrationReceiptUpdate(..)
  , CourseRegistrationFollowUpCreate(..)
  , CourseRegistrationFollowUpUpdate(..)
  , CourseUpsert(..)
  , CourseSessionIn(..)
  , CourseSyllabusIn(..)
  , CoursesPublicAPI
  , CoursesAdminAPI
  , WhatsAppHooksAPI
  , WhatsAppWebhookAPI
  ) where

import           Data.Aeson (FromJSON(parseJSON), Options(..), ToJSON, defaultOptions, genericParseJSON)
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
instance FromJSON CourseSession

data SyllabusItem = SyllabusItem
  { title  :: Text
  , topics :: [Text]
  } deriving (Show, Generic)

instance ToJSON SyllabusItem
instance FromJSON SyllabusItem

data CourseMetadata = CourseMetadata
  { slug           :: Text
  , title          :: Text
  , subtitle       :: Text
  , format         :: Text
  , duration       :: Text
  , price          :: Double
  , currency       :: Text
  , capacity       :: Int
  , remaining      :: Int
  , sessionStartHour :: Int
  , sessionDurationHours :: Int
  , locationLabel  :: Text
  , locationMapUrl :: Text
  , daws           :: [Text]
  , includes       :: [Text]
  , instructorName :: Maybe Text
  , instructorBio  :: Maybe Text
  , instructorAvatarUrl :: Maybe Text
  , sessions       :: [CourseSession]
  , syllabus       :: [SyllabusItem]
  , whatsappCtaUrl :: Text
  , landingUrl     :: Text
  } deriving (Show, Generic)

instance ToJSON CourseMetadata
instance FromJSON CourseMetadata

data UTMTags = UTMTags
  { source   :: Maybe Text
  , medium   :: Maybe Text
  , campaign :: Maybe Text
  , content  :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON UTMTags where
  parseJSON = genericParseJSON strictObjectOptions
instance ToJSON UTMTags

data CourseRegistrationRequest = CourseRegistrationRequest
  { fullName  :: Maybe Text
  , email     :: Maybe Text
  , phoneE164 :: Maybe Text
  , source    :: Text
  , howHeard  :: Maybe Text
  , utm       :: Maybe UTMTags
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationRequest where
  parseJSON = genericParseJSON strictObjectOptions
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
instance ToJSON CourseRegistrationStatusUpdate

data CourseRegistrationNotesUpdate = CourseRegistrationNotesUpdate
  { notes :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationNotesUpdate
instance ToJSON CourseRegistrationNotesUpdate

data CourseRegistrationReceiptCreate = CourseRegistrationReceiptCreate
  { fileUrl  :: Text
  , fileName :: Maybe Text
  , mimeType :: Maybe Text
  , notes    :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationReceiptCreate
instance ToJSON CourseRegistrationReceiptCreate

data CourseRegistrationReceiptUpdate = CourseRegistrationReceiptUpdate
  { fileUrl  :: Maybe Text
  , fileName :: Maybe Text
  , mimeType :: Maybe Text
  , notes    :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationReceiptUpdate
instance ToJSON CourseRegistrationReceiptUpdate

data CourseRegistrationFollowUpCreate = CourseRegistrationFollowUpCreate
  { entryType      :: Maybe Text
  , subject        :: Maybe Text
  , notes          :: Text
  , attachmentUrl  :: Maybe Text
  , attachmentName :: Maybe Text
  , nextFollowUpAt :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationFollowUpCreate
instance ToJSON CourseRegistrationFollowUpCreate

data CourseRegistrationFollowUpUpdate = CourseRegistrationFollowUpUpdate
  { entryType      :: Maybe Text
  , subject        :: Maybe Text
  , notes          :: Maybe Text
  , attachmentUrl  :: Maybe Text
  , attachmentName :: Maybe Text
  , nextFollowUpAt :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON CourseRegistrationFollowUpUpdate
instance ToJSON CourseRegistrationFollowUpUpdate

data CourseSessionIn = CourseSessionIn
  { label :: Text
  , date  :: Day
  , order :: Maybe Int
  } deriving (Show, Generic)
instance FromJSON CourseSessionIn
instance ToJSON CourseSessionIn

data CourseSyllabusIn = CourseSyllabusIn
  { title  :: Text
  , topics :: [Text]
  , order  :: Maybe Int
  } deriving (Show, Generic)
instance FromJSON CourseSyllabusIn
instance ToJSON CourseSyllabusIn

data CourseUpsert = CourseUpsert
  { slug                 :: Text
  , title                :: Text
  , subtitle             :: Maybe Text
  , format               :: Maybe Text
  , duration             :: Maybe Text
  , priceCents           :: Int
  , currency             :: Text
  , capacity             :: Int
  , sessionStartHour     :: Maybe Int
  , sessionDurationHours :: Maybe Int
  , locationLabel        :: Maybe Text
  , locationMapUrl       :: Maybe Text
  , whatsappCtaUrl       :: Maybe Text
  , landingUrl           :: Maybe Text
  , daws                 :: [Text]
  , includes             :: [Text]
  , instructorName       :: Maybe Text
  , instructorBio        :: Maybe Text
  , instructorAvatarUrl  :: Maybe Text
  , sessions             :: [CourseSessionIn]
  , syllabus             :: [CourseSyllabusIn]
  } deriving (Show, Generic)
instance FromJSON CourseUpsert
instance ToJSON CourseUpsert

type CoursesPublicAPI =
       "public" :> "courses" :> Capture "slug" Text :> Get '[JSON] CourseMetadata
  :<|> "public" :> "courses" :> Capture "slug" Text :> "registrations" :> ReqBody '[JSON] CourseRegistrationRequest :> PostCreated '[JSON] CourseRegistrationResponse

type CoursesAdminAPI =
       "courses" :> ReqBody '[JSON] CourseUpsert :> Post '[JSON] CourseMetadata
  :<|> "courses" :> "cohorts" :> Get '[JSON] [TDF.DTO.CourseCohortOptionDTO]
  :<|> "courses" :> "registrations" :>
         QueryParam "slug" Text :>
         QueryParam "status" Text :>
         QueryParam "limit" Int :>
         Get '[JSON] [TDF.DTO.CourseRegistrationDTO]
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> Get '[JSON] TDF.DTO.CourseRegistrationDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "dossier" :> Get '[JSON] TDF.DTO.CourseRegistrationDossierDTO
  :<|> "courses" :> "registrations" :> Capture "registrationId" Int64 :> "emails" :> QueryParam "limit" Int :> Get '[JSON] [TDF.DTO.CourseEmailEventDTO]
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "status" :> ReqBody '[JSON] CourseRegistrationStatusUpdate :> Patch '[JSON] CourseRegistrationResponse
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "notes" :> ReqBody '[JSON] CourseRegistrationNotesUpdate :> Patch '[JSON] TDF.DTO.CourseRegistrationDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "receipts" :> ReqBody '[JSON] CourseRegistrationReceiptCreate :> PostCreated '[JSON] TDF.DTO.CourseRegistrationReceiptDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "receipts" :> Capture "receiptId" Int64 :> ReqBody '[JSON] CourseRegistrationReceiptUpdate :> Patch '[JSON] TDF.DTO.CourseRegistrationReceiptDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "receipts" :> Capture "receiptId" Int64 :> Delete '[JSON] NoContent
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "follow-ups" :> ReqBody '[JSON] CourseRegistrationFollowUpCreate :> PostCreated '[JSON] TDF.DTO.CourseRegistrationFollowUpDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "follow-ups" :> Capture "followUpId" Int64 :> ReqBody '[JSON] CourseRegistrationFollowUpUpdate :> Patch '[JSON] TDF.DTO.CourseRegistrationFollowUpDTO
  :<|> "courses" :> Capture "slug" Text :> "registrations" :> Capture "registrationId" Int64 :> "follow-ups" :> Capture "followUpId" Int64 :> Delete '[JSON] NoContent

type WhatsAppWebhookAPI =
       "webhooks" :> "whatsapp" :> QueryParam "hub.mode" Text :> QueryParam "hub.verify_token" Text :> QueryParam "hub.challenge" Text :> Get '[PlainText] Text
  :<|> "webhooks" :> "whatsapp" :> ReqBody '[JSON] WAMetaWebhook :> Post '[JSON] NoContent

type WhatsAppHooksAPI =
       "hooks" :> "whatsapp" :> QueryParam "hub.mode" Text :> QueryParam "hub.verify_token" Text :> QueryParam "hub.challenge" Text :> Get '[PlainText] Text
  :<|> "hooks" :> "whatsapp" :> ReqBody '[JSON] WAMetaWebhook :> Post '[JSON] NoContent

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }
