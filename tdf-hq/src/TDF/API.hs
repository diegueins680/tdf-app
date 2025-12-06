
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TDF.API where

import           Servant
import           Database.Persist          (Entity)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Data.Char (toLower)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), defaultOptions, genericParseJSON, genericToJSON, fieldLabelModifier)
import qualified Data.ByteString.Lazy as BL

import           TDF.API.Admin     (AdminAPI)
import           TDF.API.Future    (FutureAPI)
import           TDF.API.Bands     (BandsAPI)
import           TDF.API.Inventory (InventoryAPI)
import           TDF.API.Payments (PaymentsAPI)
import           TDF.API.Instagram (InstagramAPI)
import           TDF.API.Pipelines (PipelinesAPI)
import           TDF.API.Rooms     (RoomsAPI)
import           TDF.API.Sessions  (SessionsAPI)
import           TDF.API.Drive     (DriveAPI)
import           TDF.API.Types     (LooseJSON, RolePayload, UserRoleSummaryDTO, UserRoleUpdatePayload)
import           TDF.API.Radio     (RadioAPI)
import           TDF.Models        (RoleEnum)
import           TDF.DTO
import           TDF.Meta         (MetaAPI)
import           TDF.Version      (VersionInfo)
import qualified TDF.ModelsExtra  as ME
import           TDF.Routes.Academy (AcademyAPI)
import           TDF.Routes.Courses (CoursesPublicAPI, WhatsAppWebhookAPI)
import           TDF.API.LiveSessions (LiveSessionsAPI)
import           TDF.API.Feedback    (FeedbackAPI)
import           TDF.API.Calendar    (CalendarAPI)
import           TDF.API.Marketplace (MarketplaceAPI, MarketplaceAdminAPI)
import           TDF.API.Label (LabelAPI)
import           TDF.API.SocialEventsAPI (SocialEventsAPI)

type InventoryItem = ME.Asset
type InputListEntry = ME.InputRow

type VersionAPI = "version" :> Get '[JSON] VersionInfo

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

type AdsAdminAPI =
       "ads" :> "inquiries" :> Get '[JSON] [AdsInquiryDTO]

type CmsPublicAPI =
       "cms" :> "content" :> QueryParam "slug" Text :> QueryParam "locale" Text :> Get '[JSON] CmsContentDTO
  :<|> "cms" :> "contents" :> QueryParam "locale" Text :> QueryParam "slugPrefix" Text :> Get '[JSON] [CmsContentDTO]

type CmsAdminAPI =
       "cms" :> "content" :>
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
      )

type SocialAPI =
       "followers" :> Get '[JSON] [PartyFollowDTO]
  :<|> "following" :> Get '[JSON] [PartyFollowDTO]
  :<|> "vcard-exchange" :> ReqBody '[JSON] VCardExchangeRequest :> Post '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Get '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Capture "partyId" Int64 :> Post '[JSON] [PartyFollowDTO]
  :<|> "friends" :> Capture "partyId" Int64 :> Delete '[JSON] NoContent

type BookingAPI =
       Get '[JSON] [BookingDTO]
  :<|> ReqBody '[JSON] CreateBookingReq :> Post '[JSON] BookingDTO
  :<|> Capture "bookingId" Int64 :> ReqBody '[JSON] UpdateBookingReq :> Put '[JSON] BookingDTO

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
  :<|> "instagram" :> InstagramAPI
  :<|> "social" :> SocialAPI
  :<|> AdsAdminAPI
  :<|> "calendar" :> CalendarAPI
  :<|> CmsAdminAPI
  :<|> DriveAPI
  :<|> RadioAPI
  :<|> "stubs"    :> FutureAPI

type API =
       VersionAPI
  :<|> "health" :> HealthAPI
  :<|> "login"  :> LoginAPI
  :<|> "signup" :> SignupAPI
  :<|> "password" :> PasswordAPI
  :<|> "v1" :> AuthV1API
  :<|> "fans" :> FanPublicAPI
  :<|> CoursesPublicAPI
  :<|> WhatsAppWebhookAPI
  :<|> MetaAPI
  :<|> AcademyAPI
  :<|> "seed"   :> SeedAPI
  :<|> "input-list" :> InputListAPI
  :<|> AdsPublicAPI
  :<|> CmsPublicAPI
  :<|> "marketplace" :> MarketplaceAPI
  :<|> "label" :> LabelAPI
  :<|> "social-events" :> SocialEventsAPI
  :<|> RadioPublicAPI
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
  } deriving (Show, Generic)
instance FromJSON UpdateBookingReq

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

camelDrop :: Int -> String -> String
camelDrop n xs = case drop n xs of
  (c:cs) -> toLower c : cs
  []     -> []
