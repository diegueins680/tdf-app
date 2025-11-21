
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
import           Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=))
import qualified Data.ByteString.Lazy as BL

import           TDF.API.Admin     (AdminAPI)
import           TDF.API.Future    (FutureAPI)
import           TDF.API.Bands     (BandsAPI)
import           TDF.API.Inventory (InventoryAPI)
import           TDF.API.Pipelines (PipelinesAPI)
import           TDF.API.Rooms     (RoomsAPI)
import           TDF.API.Sessions  (SessionsAPI)
import           TDF.API.Types     (LooseJSON, RolePayload, UserRoleSummaryDTO, UserRoleUpdatePayload)
import           TDF.Models        (RoleEnum)
import           TDF.DTO
import           TDF.Meta         (MetaAPI)
import           TDF.Version      (VersionInfo)
import qualified TDF.ModelsExtra  as ME
import           TDF.Routes.Academy (AcademyAPI)
import           Data.Int (Int64)
import           TDF.API.LiveSessions (LiveSessionsAPI)

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

type PartyAPI =
       Get '[JSON] [PartyDTO]
  :<|> ReqBody '[JSON] PartyCreate :> Post '[JSON] PartyDTO
      :<|> Capture "partyId" Int64 :> (
           Get '[JSON] PartyDTO
      :<|> ReqBody '[JSON] PartyUpdate :> Put '[JSON] PartyDTO
      :<|> "roles" :> ReqBody '[LooseJSON, PlainText, OctetStream] RolePayload :> Post '[JSON] NoContent
      )

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
  :<|> "stubs"    :> FutureAPI

type API =
       VersionAPI
  :<|> "health" :> HealthAPI
  :<|> "login"  :> LoginAPI
  :<|> "signup" :> SignupAPI
  :<|> "password" :> PasswordAPI
  :<|> "v1" :> AuthV1API
  :<|> "fans" :> FanPublicAPI
  :<|> MetaAPI
  :<|> AcademyAPI
  :<|> "seed"   :> SeedAPI
  :<|> "input-list" :> InputListAPI
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
