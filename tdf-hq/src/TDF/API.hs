
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API where

import           Control.Applicative ((<|>))
import           Servant
import           Database.Persist          (Entity)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Data.Char (toLower)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), defaultOptions, genericParseJSON, genericToJSON, fieldLabelModifier)
import           Data.Aeson.Types (camelTo2, withObject, (.:?))
import qualified Data.ByteString.Lazy as BL

import           TDF.API.Admin     (AdminAPI)
import           TDF.API.Future    (FutureAPI)
import           TDF.API.Bands     (BandsAPI)
import           TDF.API.Inventory (InventoryAPI)
import           TDF.API.Payments (PaymentsAPI)
import           TDF.API.Instagram (InstagramAPI, InstagramWebhookAPI)
import           TDF.API.Internships (InternshipsAPI)
import           TDF.API.Pipelines (PipelinesAPI)
import           TDF.API.Rooms     (RoomsAPI, RoomsPublicAPI)
import           TDF.API.Sessions  (SessionsAPI)
import           TDF.API.Drive     (DriveAPI)
import           TDF.API.Types     (LooseJSON, PartyRelatedDTO, RolePayload, UserRoleSummaryDTO, UserRoleUpdatePayload)
import           TDF.API.Radio     (RadioAPI)
import           TDF.Models        (RoleEnum)
import           TDF.DTO
import           TDF.Meta         (MetaAPI)
import           TDF.Version      (VersionInfo)
import qualified TDF.ModelsExtra  as ME
import           TDF.Routes.Academy (AcademyAPI)
import           TDF.Routes.Courses (CoursesPublicAPI, CoursesAdminAPI, WhatsAppHooksAPI, WhatsAppWebhookAPI)
import           TDF.API.LiveSessions (LiveSessionsAPI)
import           TDF.API.Feedback    (FeedbackAPI)
import           TDF.API.Calendar    (CalendarAPI)
import           TDF.API.Marketplace (MarketplaceAPI, MarketplaceAdminAPI)
import           TDF.API.Label (LabelAPI)
import           TDF.API.Services (ServiceCatalogAPI, ServiceCatalogPublicAPI)
import           TDF.API.SocialEventsAPI (SocialEventsAPI)
import           TDF.API.SocialSyncAPI (SocialSyncAPI)
import           TDF.Contracts.API (ContractsAPI)

type InventoryItem = ME.Asset
type InputListEntry = ME.InputRow

type VersionAPI = "version" :> Get '[JSON] VersionInfo
type AssetsAPI  = "inventory" :> Raw

type InputListPublicAPI =
       "inventory"
         :> QueryParam "field" Text
         :> QueryParam "sessionId" Text
         :> QueryParam "channel" Int
         :> Get '[JSON] [Entity InventoryItem]
  :<|> "sessions" :> QueryParam "index" Int :> QueryParam "sessionId" Text :> Get '[JSON] [Entity InputListEntry]
  :<|> "sessions" :> "pdf" :> QueryParam "index" Int :> QueryParam "sessionId" Text :> Get '[OctetStream] (Headers '[Header "Content-Disposition" Text] BL.ByteString)

type InputListSeedAPI =
       "inventory" :> "seed" :> SeedAPI
  :<|> "sessions" :> "seed" :> SeedAPI

type InputListAPI = InputListPublicAPI :<|> InputListSeedAPI

type AdsPublicAPI =
       "ads" :> "inquiry" :> ReqBody '[JSON] AdsInquiry :> Post '[JSON] AdsInquiryOut
  :<|> "ads" :> "assist" :> ReqBody '[JSON] AdsAssistRequest :> Post '[JSON] AdsAssistResponse

type AdsAdminAPI =
       "ads" :> "inquiries" :> Get '[JSON] [AdsInquiryDTO]
  :<|> "ads" :> "campaigns" :> Get '[JSON] [CampaignDTO]
  :<|> "ads" :> "campaigns" :> ReqBody '[JSON] CampaignUpsert :> Post '[JSON] CampaignDTO
  :<|> "ads" :> "campaigns" :> Capture "campaignId" Int64 :> Get '[JSON] CampaignDTO
  :<|> "ads" :> "ads" :> ReqBody '[JSON] AdCreativeUpsert :> Post '[JSON] AdCreativeDTO
  :<|> "ads" :> "campaigns" :> Capture "campaignId" Int64 :> "ads" :> Get '[JSON] [AdCreativeDTO]
  :<|> "ads" :> Capture "adId" Int64 :> "examples" :> Get '[JSON] [AdConversationExampleDTO]
  :<|> "ads" :> Capture "adId" Int64 :> "examples" :> ReqBody '[JSON] AdConversationExampleCreate :> Post '[JSON] AdConversationExampleDTO

type CmsPublicAPI =
       "cms" :> "content" :> QueryParam "slug" Text :> QueryParam "locale" Text :> Get '[JSON] CmsContentDTO
  :<|> "cms" :> "contents" :> QueryParam "locale" Text :> QueryParam "slugPrefix" Text :> Get '[JSON] [CmsContentDTO]

type CmsAdminAPI =
       "cms" :> "admin" :> "content" :>
         ( QueryParam "slug" Text :> QueryParam "locale" Text :> Get '[JSON] [CmsContentDTO]
      :<|> ReqBody '[JSON] CmsContentIn :> Post '[JSON] CmsContentDTO
      :<|> Capture "id" Int :> "publish" :> Post '[JSON] CmsContentDTO
      :<|> Capture "id" Int :> Delete '[JSON] NoContent
         )

type PartyAPI =
       Get '[JSON] [PartyDTO]
  :<|> ReqBody '[JSON] PartyCreate :> Post '[JSON] PartyDTO
      :<|> Capture "partyId" Int64 :> (
           Get '[JSON] PartyDTO
      :<|> ReqBody '[JSON] PartyUpdate :> Put '[JSON] PartyDTO
      :<|> "roles" :> ReqBody '[LooseJSON, PlainText, OctetStream] RolePayload :> Post '[JSON] NoContent
      :<|> "related" :> Get '[JSON] PartyRelatedDTO
      )

type SocialAPI =
       "followers" :> Get '[JSON] [PartyFollowDTO]
  :<|> "following" :> Get '[JSON] [PartyFollowDTO]
  :<|> "vcard-exchange" :> ReqBody '[JSON] VCardExchangeRequest :> Post '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Get '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Capture "partyId" Int64 :> Post '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Capture "partyId" Int64 :> Delete '[JSON] NoContent
  :<|> "suggestions" :> Get '[JSON] [SuggestedFriendDTO]

type ChatAPI =
       "chat" :> "threads" :> Get '[JSON] [ChatThreadDTO]
  :<|> "chat" :> "threads" :> "dm" :> Capture "otherPartyId" Int64 :> Post '[JSON] ChatThreadDTO
  :<|> "chat" :> "threads" :> Capture "threadId" Int64 :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "beforeId" Int64
         :> QueryParam "afterId" Int64
         :> Get '[JSON] [ChatMessageDTO]
  :<|> "chat" :> "threads" :> Capture "threadId" Int64 :> "messages"
         :> ReqBody '[JSON] ChatSendMessageRequest
         :> Post '[JSON] ChatMessageDTO

type ChatKitSessionAPI =
       "chatkit" :> "sessions" :> ReqBody '[JSON] ChatKitSessionRequest :> Post '[JSON] ChatKitSessionResponse

type WhatsAppMessagesAPI =
       "whatsapp" :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "direction" Text
         :> QueryParam "repliedOnly" Text
         :> Get '[JSON] Value

type BookingAPI =
       QueryParam "bookingId" Int64
         :> QueryParam "partyId" Int64
         :> QueryParam "engineerPartyId" Int64
         :> Get '[JSON] [BookingDTO]
  :<|> ReqBody '[JSON] CreateBookingReq :> Post '[JSON] BookingDTO
  :<|> Capture "bookingId" Int64 :> ReqBody '[JSON] UpdateBookingReq :> Put '[JSON] BookingDTO

type BookingPublicAPI =
       "bookings" :> "public" :> ReqBody '[JSON] PublicBookingReq :> Post '[JSON] BookingDTO

type PackageAPI =
       "products" :> Get '[JSON] [PackageProductDTO]
  :<|> "purchases" :> ReqBody '[JSON] PackagePurchaseReq :> Post '[JSON] NoContent

type InvoiceAPI =
       Get '[JSON] [InvoiceDTO]
  :<|> ReqBody '[JSON] CreateInvoiceReq :> Post '[JSON] InvoiceDTO
  :<|> Capture "sessionId" Text :> "generate" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "by-session" :> Capture "sessionId" Text :> Get '[JSON] Value
  :<|> Capture "invoiceId" Int64 :> Get '[JSON] Value

type ReceiptAPI =
      Get '[JSON] [ReceiptDTO]
  :<|> ReqBody '[JSON] CreateReceiptReq :> Post '[JSON] ReceiptDTO
  :<|> Capture "receiptId" Int64 :> Get '[JSON] ReceiptDTO

type HealthAPI = Get '[JSON] HealthStatus

type LoginAPI = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
type GoogleLoginAPI = ReqBody '[JSON] GoogleLoginRequest :> Post '[JSON] LoginResponse

type SignupAPI = ReqBody '[JSON] SignupRequest :> Post '[JSON] LoginResponse

type PasswordResetAPI = ReqBody '[JSON] PasswordResetRequest :> Post '[JSON] NoContent

type PasswordResetConfirmAPI = ReqBody '[JSON] PasswordResetConfirmRequest :> Post '[JSON] LoginResponse

type PasswordAPI = Header "Authorization" Text :> "change" :> ReqBody '[JSON] ChangePasswordRequest :> Post '[JSON] LoginResponse

type UserRolesAPI =
       "users" :> Get '[JSON] [UserRoleSummaryDTO]
  :<|> "users" :> Capture "userId" Int64 :> "roles" :>
        ( Get '[JSON] [RoleEnum]
     :<|> ReqBody '[JSON] UserRoleUpdatePayload :> Put '[JSON] NoContent
        )

type AuthV1API =
       "signup" :> SignupAPI
  :<|> "password-reset" :> PasswordResetAPI
  :<|> "password-reset" :> "confirm" :> PasswordResetConfirmAPI
  :<|> "password" :> PasswordAPI

type FanPublicAPI =
       "artists" :> Get '[JSON] [ArtistProfileDTO]
  :<|> "artists" :> Capture "artistId" Int64 :> Get '[JSON] ArtistProfileDTO
  :<|> "artists" :> Capture "artistId" Int64 :> "releases" :> Get '[JSON] [ArtistReleaseDTO]

type RadioPublicAPI =
       "radio" :> "presence" :> Capture "partyId" Int64 :> Get '[JSON] (Maybe RadioPresenceDTO)

type CountryAPI = "countries" :> Get '[JSON] [CountryDTO]

type FanSecureAPI =
       "me" :> "profile" :>
         ( Get '[JSON] FanProfileDTO
      :<|> ReqBody '[JSON] FanProfileUpdate :> Put '[JSON] FanProfileDTO
         )
  :<|> "me" :> "follows" :>
         ( Get '[JSON] [FanFollowDTO]
      :<|> Capture "artistId" Int64 :> Post '[JSON] FanFollowDTO
      :<|> Capture "artistId" Int64 :> Delete '[JSON] NoContent
         )
  :<|> "me" :> "artist-profile" :>
         ( Get '[JSON] ArtistProfileDTO
       :<|> ReqBody '[JSON] ArtistProfileUpsert :> Put '[JSON] ArtistProfileDTO
         )

type SeedAPI = Header "X-Seed-Token" Text :> Post '[JSON] NoContent

type ProtectedAPI =
       "parties"  :> PartyAPI
  :<|> "bookings" :> BookingAPI
  :<|> ServiceCatalogAPI
  :<|> "packages" :> PackageAPI
  :<|> "invoices" :> InvoiceAPI
  :<|> "receipts" :> ReceiptAPI
  :<|> "admin"    :> AdminAPI
  :<|> "api"      :> UserRolesAPI
  :<|> "fans"     :> FanSecureAPI
  :<|> InventoryAPI
  :<|> BandsAPI
  :<|> SessionsAPI
  :<|> PipelinesAPI
  :<|> RoomsAPI
  :<|> LiveSessionsAPI
  :<|> FeedbackAPI
  :<|> "marketplace" :> MarketplaceAdminAPI
  :<|> "payments" :> PaymentsAPI
  :<|> InstagramAPI
  :<|> WhatsAppMessagesAPI
  :<|> "social" :> SocialAPI
  :<|> ChatAPI
  :<|> ChatKitSessionAPI
  :<|> "social-sync" :> SocialSyncAPI
  :<|> "social-events" :> SocialEventsAPI
  :<|> InternshipsAPI
  :<|> AdsAdminAPI
  :<|> "admin" :> CoursesAdminAPI
  :<|> "label" :> LabelAPI
  :<|> "calendar" :> CalendarAPI
  :<|> CmsAdminAPI
  :<|> DriveAPI
  :<|> RadioAPI
  :<|> CountryAPI
  :<|> "stubs"    :> FutureAPI

type API =
       VersionAPI
  :<|> "health" :> HealthAPI
  :<|> "login"  :> LoginAPI
  :<|> "login"  :> "google" :> GoogleLoginAPI
  :<|> "signup" :> SignupAPI
  :<|> "password" :> PasswordAPI
  :<|> "v1" :> AuthV1API
  :<|> "fans" :> FanPublicAPI
  :<|> CoursesPublicAPI
  :<|> InstagramWebhookAPI
  :<|> WhatsAppHooksAPI
  :<|> WhatsAppWebhookAPI
  :<|> MetaAPI
  :<|> AcademyAPI
  :<|> "seed"   :> SeedAPI
  :<|> "input-list" :> InputListAPI
  :<|> AdsPublicAPI
  :<|> CmsPublicAPI
  :<|> "marketplace" :> MarketplaceAPI
  :<|> "contracts" :> ContractsAPI
  :<|> RadioPublicAPI
  :<|> RoomsPublicAPI
  :<|> ServiceCatalogPublicAPI
  :<|> "engineers" :> Get '[JSON] [PublicEngineerDTO]
  :<|> BookingPublicAPI
  :<|> AssetsAPI
  :<|> AuthProtect "bearer-token" :> ProtectedAPI

data HealthStatus = HealthStatus { status :: String, db :: String }

instance ToJSON HealthStatus where
  toJSON (HealthStatus s d) = object ["status" .= s, "db" .= d]

data CreateBookingReq = CreateBookingReq
  { cbTitle       :: Text
  , cbStartsAt    :: UTCTime
  , cbEndsAt      :: UTCTime
  , cbStatus      :: Text
  , cbNotes       :: Maybe Text
  , cbPartyId     :: Maybe Int64
  , cbEngineerPartyId :: Maybe Int64
  , cbEngineerName :: Maybe Text
  , cbServiceType :: Maybe Text
  , cbResourceIds :: Maybe [Text]
  } deriving (Show, Generic)
instance FromJSON CreateBookingReq

data UpdateBookingReq = UpdateBookingReq
  { ubTitle       :: Maybe Text
  , ubServiceType :: Maybe Text
  , ubStatus      :: Maybe Text
  , ubNotes       :: Maybe Text
  , ubStartsAt    :: Maybe UTCTime
  , ubEndsAt      :: Maybe UTCTime
  , ubEngineerPartyId :: Maybe Int64
  , ubEngineerName :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON UpdateBookingReq

data PublicBookingReq = PublicBookingReq
  { pbFullName         :: Text
  , pbEmail            :: Text
  , pbPhone            :: Maybe Text
  , pbServiceType      :: Text
  , pbStartsAt         :: UTCTime
  , pbDurationMinutes  :: Maybe Int
  , pbNotes            :: Maybe Text
  , pbEngineerPartyId  :: Maybe Int64
  , pbEngineerName     :: Maybe Text
  , pbResourceIds      :: Maybe [Text]
  } deriving (Show, Generic)
instance FromJSON PublicBookingReq

data PublicEngineerDTO = PublicEngineerDTO
  { peId   :: Int64
  , peName :: Text
  } deriving (Show, Generic)
instance ToJSON PublicEngineerDTO

data AdsInquiry = AdsInquiry
  { aiName    :: Maybe Text
  , aiEmail   :: Maybe Text
  , aiPhone   :: Maybe Text
  , aiCourse  :: Maybe Text
  , aiMessage :: Maybe Text
  , aiChannel :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AdsInquiry where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelDrop 2 }

data AdsInquiryDTO = AdsInquiryDTO
  { aidInquiryId :: Int
  , aidCreatedAt :: UTCTime
  , aidName      :: Maybe Text
  , aidEmail     :: Maybe Text
  , aidPhone     :: Maybe Text
  , aidCourse    :: Maybe Text
  , aidMessage   :: Maybe Text
  , aidChannel   :: Maybe Text
  , aidStatus    :: Text
  } deriving (Show, Generic)
instance ToJSON AdsInquiryDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data AdsInquiryOut = AdsInquiryOut
  { aioOk         :: Bool
  , aioInquiryId  :: Int
  , aioPartyId    :: Int
  , aioRepliedVia :: [Text]
  } deriving (Show, Generic)
instance ToJSON AdsInquiryOut where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data CmsContentIn = CmsContentIn
  { cciSlug    :: Text
  , cciLocale  :: Text
  , cciTitle   :: Maybe Text
  , cciStatus  :: Maybe Text
  , cciPayload :: Maybe Value
  } deriving (Show, Generic)
instance FromJSON CmsContentIn where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data CmsContentDTO = CmsContentDTO
  { ccdId        :: Int
  , ccdSlug      :: Text
  , ccdLocale    :: Text
  , ccdVersion   :: Int
  , ccdStatus    :: Text
  , ccdTitle     :: Maybe Text
  , ccdPayload   :: Maybe Value
  , ccdCreatedAt :: UTCTime
  , ccdPublishedAt :: Maybe UTCTime
  } deriving (Show, Generic)
instance ToJSON CmsContentDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data ChatKitSessionRequest = ChatKitSessionRequest
  { cksWorkflowId :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ChatKitSessionRequest where
  parseJSON = withObject "ChatKitSessionRequest" $ \o -> do
    workflowId <- o .:? "workflowId"
    workflow <- o .:? "workflow"
    nestedId <- case workflow of
      Just (Object w) -> w .:? "id"
      _ -> pure Nothing
    pure ChatKitSessionRequest
      { cksWorkflowId = workflowId <|> nestedId
      }

data ChatKitSessionResponse = ChatKitSessionResponse
  { ckrClientSecret :: Text
  , ckrExpiresAfter :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON ChatKitSessionResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . camelDrop 3 }

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []
