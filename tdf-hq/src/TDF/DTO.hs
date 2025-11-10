{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TDF.DTO where

import           GHC.Generics (Generic)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time (UTCTime, Day)

import           Database.Persist (Entity(..))
import           Database.Persist.Sql (fromSqlKey)

import           TDF.Models
import           TDF.API.Types (BandDTO)

-- Parties
data PartyDTO = PartyDTO
  { partyId          :: Int64
  , legalName        :: Maybe Text
  , displayName      :: Text
  , isOrg            :: Bool
  , taxId            :: Maybe Text
  , primaryEmail     :: Maybe Text
  , primaryPhone     :: Maybe Text
  , whatsapp         :: Maybe Text
  , instagram        :: Maybe Text
  , emergencyContact :: Maybe Text
  , notes            :: Maybe Text
  , band             :: Maybe BandDTO
  } deriving (Show, Generic)

instance ToJSON PartyDTO
instance FromJSON PartyDTO

data PartyCreate = PartyCreate
  { cLegalName        :: Maybe Text
  , cDisplayName      :: Text
  , cIsOrg            :: Bool
  , cTaxId            :: Maybe Text
  , cPrimaryEmail     :: Maybe Text
  , cPrimaryPhone     :: Maybe Text
  , cWhatsapp         :: Maybe Text
  , cInstagram        :: Maybe Text
  , cEmergencyContact :: Maybe Text
  , cNotes            :: Maybe Text
  , cRoles            :: Maybe [RoleEnum]
  } deriving (Show, Generic)
instance FromJSON PartyCreate

data PartyUpdate = PartyUpdate
  { uLegalName        :: Maybe Text
  , uDisplayName      :: Maybe Text
  , uIsOrg            :: Maybe Bool
  , uTaxId            :: Maybe Text
  , uPrimaryEmail     :: Maybe Text
  , uPrimaryPhone     :: Maybe Text
  , uWhatsapp         :: Maybe Text
  , uInstagram        :: Maybe Text
  , uEmergencyContact :: Maybe Text
  , uNotes            :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON PartyUpdate

toPartyDTO :: Entity Party -> PartyDTO
toPartyDTO = toPartyDTOWithBand Nothing

toPartyDTOWithBand :: Maybe BandDTO -> Entity Party -> PartyDTO
toPartyDTOWithBand mBand (Entity pid p) = PartyDTO
  { partyId          = fromSqlKey pid
  , legalName        = partyLegalName p
  , displayName      = partyDisplayName p
  , isOrg            = partyIsOrg p
  , taxId            = partyTaxId p
  , primaryEmail     = partyPrimaryEmail p
  , primaryPhone     = partyPrimaryPhone p
  , whatsapp         = partyWhatsapp p
  , instagram        = partyInstagram p
  , emergencyContact = partyEmergencyContact p
  , notes            = partyNotes p
  , band             = mBand
  }

-- Helper
tshow :: Show a => a -> Text
tshow = T.pack . show

-- Bookings
data BookingResourceDTO = BookingResourceDTO
  { brRoomId   :: Text
  , brRoomName :: Text
  , brRole     :: Text
  } deriving (Show, Generic)
instance ToJSON BookingResourceDTO

data BookingDTO = BookingDTO
  { bookingId   :: Int64
  , title       :: Text
  , startsAt    :: UTCTime
  , endsAt      :: UTCTime
  , status      :: Text
  , notes       :: Maybe Text
  , partyId     :: Maybe Int64
  , serviceType :: Maybe Text
  , resources   :: [BookingResourceDTO]
  } deriving (Show, Generic)
instance ToJSON BookingDTO

-- Packages
data PackageProductDTO = PackageProductDTO
  { ppId         :: Int64
  , ppName       :: Text
  , ppService    :: Text
  , ppUnitsKind  :: Text
  , ppUnitsQty   :: Int
  , ppPriceCents :: Int
  } deriving (Show, Generic)
instance ToJSON PackageProductDTO

data PackagePurchaseReq = PackagePurchaseReq
  { buyerId   :: Int64
  , productId :: Int64
  } deriving (Show, Generic)
instance FromJSON PackagePurchaseReq

-- Invoices
data InvoiceLineDTO = InvoiceLineDTO
  { lineId             :: Int64
  , description        :: Text
  , quantity           :: Int
  , unitCents          :: Int
  , taxBps             :: Int
  , totalCents         :: Int
  , serviceOrderId     :: Maybe Int64
  , packagePurchaseId  :: Maybe Int64
  } deriving (Show, Generic)
instance ToJSON InvoiceLineDTO

data InvoiceDTO = InvoiceDTO
  { invId        :: Int64
  , number       :: Maybe Text
  , statusI      :: Text
  , subtotalC    :: Int
  , taxC         :: Int
  , totalC       :: Int
  , currency     :: Text
  , customerId   :: Maybe Int64
  , notes        :: Maybe Text
  , receiptId    :: Maybe Int64
  , lineItems    :: [InvoiceLineDTO]
  } deriving (Show, Generic)
instance ToJSON InvoiceDTO

data CreateInvoiceLineReq = CreateInvoiceLineReq
  { cilDescription       :: Text
  , cilQuantity          :: Int
  , cilUnitCents         :: Int
  , cilTaxBps            :: Maybe Int
  , cilServiceOrderId    :: Maybe Int64
  , cilPackagePurchaseId :: Maybe Int64
  } deriving (Show, Generic)
instance FromJSON CreateInvoiceLineReq

data CreateInvoiceReq = CreateInvoiceReq
  { ciCustomerId      :: Int64
  , ciCurrency        :: Maybe Text
  , ciNumber          :: Maybe Text
  , ciNotes           :: Maybe Text
  , ciLineItems       :: [CreateInvoiceLineReq]
  , ciGenerateReceipt :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON CreateInvoiceReq

-- Receipts
data ReceiptLineDTO = ReceiptLineDTO
  { receiptLineId :: Int64
  , rlDescription :: Text
  , rlQuantity    :: Int
  , rlUnitCents   :: Int
  , rlTaxBps      :: Maybe Int
  , rlTotalCents  :: Int
  } deriving (Show, Generic)
instance ToJSON ReceiptLineDTO

data ReceiptDTO = ReceiptDTO
  { receiptId    :: Int64
  , receiptNumber :: Text
  , issuedAt     :: UTCTime
  , issueDate    :: Day
  , buyerName    :: Text
  , buyerEmail   :: Maybe Text
  , currency     :: Text
  , subtotalCents :: Int
  , taxCents     :: Int
  , totalCents   :: Int
  , notes        :: Maybe Text
  , invoiceId    :: Int64
  , lineItems    :: [ReceiptLineDTO]
  } deriving (Show, Generic)
instance ToJSON ReceiptDTO

data CreateReceiptReq = CreateReceiptReq
  { crInvoiceId  :: Int64
  , crBuyerName  :: Maybe Text
  , crBuyerEmail :: Maybe Text
  , crNotes      :: Maybe Text
  , crCurrency   :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON CreateReceiptReq

-- Auth
data LoginRequest = LoginRequest
  { username :: Text
  , password :: Text
  } deriving (Show, Generic)
instance FromJSON LoginRequest

data SignupRequest = SignupRequest
  { firstName       :: Text
  , lastName        :: Text
  , email           :: Text
  , phone           :: Maybe Text
  , password        :: Text
  , googleIdToken   :: Maybe Text
  , marketingOptIn  :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON SignupRequest

data ChangePasswordRequest = ChangePasswordRequest
  { username        :: Maybe Text
  , currentPassword :: Text
  , newPassword     :: Text
  } deriving (Show, Generic)
instance FromJSON ChangePasswordRequest

data PasswordResetRequest = PasswordResetRequest
  { email :: Text
  } deriving (Show, Generic)
instance FromJSON PasswordResetRequest

data PasswordResetConfirmRequest = PasswordResetConfirmRequest
  { token       :: Text
  , newPassword :: Text
  } deriving (Show, Generic)
instance FromJSON PasswordResetConfirmRequest

data LoginResponse = LoginResponse
  { token   :: Text
  , partyId :: Int64
  , roles   :: [RoleEnum]
  , modules :: [Text]
  } deriving (Show, Generic)
instance ToJSON LoginResponse
