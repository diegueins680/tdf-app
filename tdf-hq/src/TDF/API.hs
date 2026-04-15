
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API where

import           Control.Applicative ((<|>))
import           Servant
import           Database.Persist          (Entity)
import           Data.Int (Int64)
import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Data.Char (toLower)
import           Data.Aeson (ToJSON(..), FromJSON(..), Object, Value(..), object, (.=), defaultOptions, genericParseJSON, genericToJSON, fieldLabelModifier, rejectUnknownFields)
import           Data.Aeson.Types (Parser, camelTo2, withObject, (.:?))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Lazy as BL

import           TDF.API.Admin     (AdminAPI)
import           TDF.API.Future    (FutureAPI)
import           TDF.API.Bands     (BandsAPI)
import           TDF.API.Facebook  (FacebookAPI, FacebookWebhookAPI)
import           TDF.API.Inventory (InventoryAPI)
import           TDF.API.Payments (PaymentsAPI)
import           TDF.API.Instagram (InstagramAPI, InstagramWebhookAPI)
import           TDF.API.InstagramOAuth (InstagramOAuthAPI)
import           TDF.API.Internships (InternshipsAPI)
import           TDF.API.Pipelines (PipelinesAPI)
import           TDF.API.Proposals (ProposalsAPI)
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
type AssetsServeAPI = "assets" :> "serve" :> Raw

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
  :<|> "profiles" :> QueryParams "partyId" Int64 :> Get '[JSON] [SocialPartyProfileDTO]
  :<|> "profiles" :> Capture "partyId" Int64 :> Get '[JSON] SocialPartyProfileDTO
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

type TidalAgentAPI =
       "tidal-agent" :> ReqBody '[JSON] TidalAgentRequest :> Post '[JSON] TidalAgentResponse

type MetaBackfillAPI =
       "meta" :> "backfill"
         :> ReqBody '[JSON] Value
         :> Post '[JSON] Value

type WhatsAppMessagesAPI =
       "whatsapp" :> "messages"
         :> QueryParam "limit" Int
         :> QueryParam "direction" Text
         :> QueryParam "repliedOnly" Text
         :> Get '[JSON] Value

data WhatsAppReplyReq = WhatsAppReplyReq
  { wrSenderId   :: Text
  , wrMessage    :: Text
  , wrExternalId :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON WhatsAppReplyReq
instance ToJSON WhatsAppReplyReq

type WhatsAppReplyAPI =
       "whatsapp" :> "reply" :> ReqBody '[JSON] WhatsAppReplyReq :> Post '[JSON] Value

type WhatsAppConsentRoutes =
       "consent"
         :> ReqBody '[JSON] WhatsAppConsentRequest
         :> Post '[JSON] WhatsAppConsentResponse
  :<|> "opt-out"
         :> ReqBody '[JSON] WhatsAppOptOutRequest
         :> Post '[JSON] WhatsAppConsentResponse
  :<|> "consent"
         :> QueryParam "phone" Text
         :> Get '[JSON] WhatsAppConsentStatus

type WhatsAppConsentAPI =
       "whatsapp" :> WhatsAppConsentRoutes

type WhatsAppConsentPublicAPI =
       "public" :> "whatsapp" :> WhatsAppConsentRoutes

data WhatsAppConsentRequest = WhatsAppConsentRequest
  { wcrPhone       :: Text
  , wcrName        :: Maybe Text
  , wcrConsent     :: Bool
  , wcrSource      :: Maybe Text
  , wcrSendMessage :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON WhatsAppConsentRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

data WhatsAppOptOutRequest = WhatsAppOptOutRequest
  { worPhone       :: Text
  , worReason      :: Maybe Text
  , worSendMessage :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON WhatsAppOptOutRequest where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

data WhatsAppConsentStatus = WhatsAppConsentStatus
  { wcsPhone       :: Text
  , wcsConsent     :: Bool
  , wcsConsentedAt :: Maybe UTCTime
  , wcsRevokedAt   :: Maybe UTCTime
  , wcsDisplayName :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON WhatsAppConsentStatus where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data WhatsAppConsentResponse = WhatsAppConsentResponse
  { wcrsStatus      :: WhatsAppConsentStatus
  , wcrsMessageSent :: Bool
  , wcrsMessage     :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON WhatsAppConsentResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 4 }

type BookingAPI =
       QueryParam "bookingId" Int64
         :> QueryParam "partyId" Int64
         :> QueryParam "engineerPartyId" Int64
         :> Get '[JSON] [BookingDTO]
  :<|> ReqBody '[JSON] CreateBookingReq :> Post '[JSON] BookingDTO
  :<|> Capture "bookingId" Int64 :> ReqBody '[JSON] UpdateBookingReq :> Put '[JSON] BookingDTO

type BookingPublicAPI =
       "bookings" :> "public" :> ReqBody '[JSON] PublicBookingReq :> Post '[JSON] BookingDTO

type ServiceMarketplaceAPI =
       "service-marketplace" :> "ads" :> Get '[JSON] [ServiceAdDTO]
  :<|> "service-marketplace" :> "ads" :> ReqBody '[JSON] ServiceAdCreateReq :> Post '[JSON] ServiceAdDTO
  :<|> "service-marketplace" :> "ads" :> Capture "adId" Int64 :> "slots" :> Get '[JSON] [ServiceAdSlotDTO]
  :<|> "service-marketplace" :> "ads" :> Capture "adId" Int64 :> "slots" :> ReqBody '[JSON] ServiceAdSlotCreateReq :> Post '[JSON] ServiceAdSlotDTO
  :<|> "service-marketplace" :> "bookings" :> ReqBody '[JSON] ServiceMarketplaceBookingReq :> Post '[JSON] ServiceMarketplaceBookingDTO
  :<|> "service-marketplace" :> "bookings" :> Capture "bookingId" Int64 :> "complete" :> Post '[JSON] ServiceMarketplaceBookingDTO
  :<|> "service-marketplace" :> "bookings" :> Capture "bookingId" Int64 :> "escrow" :> "release" :> Post '[JSON] ServiceMarketplaceBookingDTO

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
type McpAPI = ReqBody '[JSON] Value :> Post '[JSON] Value

type SessionCookieHeaders = Headers '[Header "Set-Cookie" Text]

type LoginAPI = ReqBody '[JSON] LoginRequest :> Post '[JSON] (SessionCookieHeaders LoginResponse)
type GoogleLoginAPI = ReqBody '[JSON] GoogleLoginRequest :> Post '[JSON] (SessionCookieHeaders LoginResponse)

type SignupAPI = ReqBody '[JSON] SignupRequest :> Post '[JSON] (SessionCookieHeaders LoginResponse)

type PasswordResetAPI = ReqBody '[JSON] PasswordResetRequest :> Post '[JSON] NoContent

type PasswordResetConfirmAPI = ReqBody '[JSON] PasswordResetConfirmRequest :> Post '[JSON] (SessionCookieHeaders LoginResponse)

type PasswordAPI = Header "Authorization" Text :> "change" :> ReqBody '[JSON] ChangePasswordRequest :> Post '[JSON] (SessionCookieHeaders LoginResponse)

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

type SessionAPI =
       Header "Authorization" Text :> Header "Cookie" Text :> "session" :> Get '[JSON] (Maybe SessionResponse)
  :<|> "session" :> "logout" :> Post '[JSON] (SessionCookieHeaders NoContent)

type ProtectedAPI =
       "parties"  :> PartyAPI
  :<|> "bookings" :> BookingAPI
  :<|> ServiceMarketplaceAPI
  :<|> ProposalsAPI
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
  :<|> FacebookAPI
  :<|> InstagramOAuthAPI
  :<|> WhatsAppMessagesAPI
  :<|> WhatsAppReplyAPI
  :<|> WhatsAppConsentAPI
  :<|> "social" :> SocialAPI
  :<|> ChatAPI
  :<|> ChatKitSessionAPI
  :<|> TidalAgentAPI
  :<|> "social-sync" :> SocialSyncAPI
  :<|> MetaBackfillAPI
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
  :<|> "mcp" :> McpAPI
  :<|> SessionAPI
  :<|> "login"  :> LoginAPI
  :<|> "login"  :> "google" :> GoogleLoginAPI
  :<|> "signup" :> SignupAPI
  :<|> "password" :> PasswordAPI
  :<|> "v1" :> AuthV1API
  :<|> "fans" :> FanPublicAPI
  :<|> CoursesPublicAPI
  :<|> InstagramWebhookAPI
  :<|> FacebookWebhookAPI
  :<|> WhatsAppHooksAPI
  :<|> WhatsAppWebhookAPI
  :<|> MetaAPI
  :<|> AcademyAPI
  :<|> "seed"   :> SeedAPI
  :<|> "input-list" :> InputListAPI
  :<|> AdsPublicAPI
  :<|> CmsPublicAPI
  :<|> WhatsAppConsentPublicAPI
  :<|> "marketplace" :> MarketplaceAPI
  :<|> "contracts" :> ContractsAPI
  :<|> RadioPublicAPI
  :<|> RoomsPublicAPI
  :<|> ServiceCatalogPublicAPI
  :<|> "engineers" :> Get '[JSON] [PublicEngineerDTO]
  :<|> BookingPublicAPI
  :<|> AssetsAPI
  :<|> AssetsServeAPI
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
instance FromJSON CreateBookingReq where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

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
instance FromJSON UpdateBookingReq where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

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
instance FromJSON PublicBookingReq where
  parseJSON = genericParseJSON defaultOptions { rejectUnknownFields = True }

data PublicEngineerDTO = PublicEngineerDTO
  { peId   :: Int64
  , peName :: Text
  } deriving (Show, Generic)
instance ToJSON PublicEngineerDTO


data ServiceAdDTO = ServiceAdDTO
  { sadId            :: Int64
  , sadProviderPartyId :: Int64
  , sadProviderName  :: Maybe Text
  , sadServiceCatalogId :: Maybe Int64
  , sadRoleTag       :: Text
  , sadHeadline      :: Text
  , sadDescription   :: Maybe Text
  , sadFeeCents      :: Int
  , sadCurrency      :: Text
  , sadSlotMinutes   :: Int
  , sadActive        :: Bool
  , sadCreatedAt     :: UTCTime
  } deriving (Show, Generic)
instance ToJSON ServiceAdDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data ServiceAdCreateReq = ServiceAdCreateReq
  { sacServiceCatalogId :: Maybe Int64
  , sacRoleTag          :: Text
  , sacHeadline         :: Text
  , sacDescription      :: Maybe Text
  , sacFeeCents         :: Int
  , sacCurrency         :: Maybe Text
  , sacSlotMinutes      :: Maybe Int
  } deriving (Show, Generic)
instance FromJSON ServiceAdCreateReq where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

data ServiceAdSlotDTO = ServiceAdSlotDTO
  { sasId       :: Int64
  , sasAdId     :: Int64
  , sasStartsAt :: UTCTime
  , sasEndsAt   :: UTCTime
  , sasStatus   :: Text
  } deriving (Show, Generic)
instance ToJSON ServiceAdSlotDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data ServiceAdSlotCreateReq = ServiceAdSlotCreateReq
  { sascStartsAt :: UTCTime
  , sascEndsAt   :: UTCTime
  } deriving (Show, Generic)
instance FromJSON ServiceAdSlotCreateReq where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 4
    , rejectUnknownFields = True
    }

data ServiceMarketplaceBookingReq = ServiceMarketplaceBookingReq
  { smbAdId         :: Int64
  , smbSlotId       :: Int64
  , smbTitle        :: Maybe Text
  , smbNotes        :: Maybe Text
  , smbPaymentMethod :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON ServiceMarketplaceBookingReq where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 3
    , rejectUnknownFields = True
    }

data ServiceMarketplaceBookingDTO = ServiceMarketplaceBookingDTO
  { smbBookingId         :: Int64
  , smbServiceOrderId    :: Int64
  , smbEscrowId          :: Int64
  , smbEscrowStatus      :: Text
  , smbEscrowAmountCents :: Int
  , smbEscrowCurrency    :: Text
  } deriving (Show, Generic)
instance ToJSON ServiceMarketplaceBookingDTO where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 3 }

data AdsInquiry = AdsInquiry
  { aiName    :: Maybe Text
  , aiEmail   :: Maybe Text
  , aiPhone   :: Maybe Text
  , aiCourse  :: Maybe Text
  , aiMessage :: Maybe Text
  , aiChannel :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AdsInquiry where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelDrop 2
    , rejectUnknownFields = True
    }

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
    rejectUnexpectedObjectFields "ChatKitSessionRequest" ["workflowId", "workflow"] o
    workflowId <- traverse (normalizeWorkflowId "workflowId") =<< o .:? "workflowId"
    workflow <- o .:? "workflow"
    nestedId <- case workflow of
      Just (Object w) -> do
        rejectUnexpectedObjectFields "ChatKitSessionRequest.workflow" ["id"] w
        mWorkflowId <- w .:? "id"
        case mWorkflowId of
          Nothing -> fail "workflow.id is required when workflow is provided"
          Just rawWorkflowId ->
            Just <$> normalizeWorkflowId "workflow.id" rawWorkflowId
      Just _ -> fail "workflow must be an object"
      _ -> pure Nothing
    case (workflowId, nestedId) of
      (Just topLevelWorkflowId, Just nestedWorkflowId)
        | topLevelWorkflowId /= nestedWorkflowId ->
            fail "workflowId and workflow.id must match when both are provided"
      _ -> pure ()
    pure ChatKitSessionRequest
      { cksWorkflowId = workflowId <|> nestedId
      }
    where
      normalizeWorkflowId fieldName rawWorkflowId =
        let trimmedWorkflowId = T.strip rawWorkflowId
        in if T.null trimmedWorkflowId
             then fail (T.unpack fieldName <> " cannot be blank")
             else pure trimmedWorkflowId

rejectUnexpectedObjectFields :: String -> [Text] -> Object -> Parser ()
rejectUnexpectedObjectFields objectName allowedFields rawObject =
  case unexpectedFields of
    [] -> pure ()
    _ ->
      fail
        ( objectName
            <> " contains unexpected field(s): "
            <> T.unpack (T.intercalate ", " unexpectedFields)
        )
  where
    allowedSet = sort allowedFields
    presentFields = sort (map AesonKey.toText (AesonKeyMap.keys rawObject))
    unexpectedFields = filter (`notElem` allowedSet) presentFields

data ChatKitSessionResponse = ChatKitSessionResponse
  { ckrClientSecret :: Text
  , ckrExpiresAfter :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON ChatKitSessionResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . camelDrop 3 }

data TidalAgentRequest = TidalAgentRequest
  { taPrompt :: Text
  , taModel  :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON TidalAgentRequest where
  parseJSON = withObject "TidalAgentRequest" $ \o -> do
    rejectUnexpectedObjectFields "TidalAgentRequest" ["prompt", "model"] o
    prompt <- normalizeRequiredTextField "prompt" =<< o .:? "prompt"
    model <- traverse (normalizeOptionalTextField "model") =<< o .:? "model"
    pure TidalAgentRequest
      { taPrompt = prompt
      , taModel = model
      }
    where
      normalizeRequiredTextField fieldName mRawValue =
        case mRawValue of
          Nothing -> fail (T.unpack fieldName <> " is required")
          Just rawValue ->
            let trimmedValue = T.strip rawValue
            in if T.null trimmedValue
                 then fail (T.unpack fieldName <> " cannot be blank")
                 else pure trimmedValue

      normalizeOptionalTextField fieldName rawValue =
        let trimmedValue = T.strip rawValue
        in if T.null trimmedValue
             then fail (T.unpack fieldName <> " cannot be blank")
             else pure trimmedValue

data TidalAgentResponse = TidalAgentResponse
  { taContent :: Text
  } deriving (Show, Generic)

instance ToJSON TidalAgentResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelDrop 2 }

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []
