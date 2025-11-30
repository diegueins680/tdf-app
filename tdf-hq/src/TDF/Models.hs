{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Models where

import           Data.Aeson (ToJSON(..), FromJSON(..), withText, Value(String))
import           GHC.Generics (Generic)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, Day)
import           Data.UUID (UUID)
import           Database.Persist.TH
import           TDF.UUIDInstances ()

-- Enums
data ServiceKind = Recording | Mixing | Mastering | Rehearsal | Classes | EventProduction
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "ServiceKind"

data PricingModel = Hourly | PerSong | Package | Quote | Retainer
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "PricingModel"

data RoleEnum
  = Admin
  | Manager
  | StudioManager
  | Engineer
  | Teacher
  | Reception
  | Accounting
  | LiveSessionsProducer
  | Artist
  | Artista
  | Webmaster
  | Promotor
  | Promoter
  | Producer
  | Songwriter
  | DJ
  | Publicist
  | TourManager
  | LabelRep
  | StageManager
  | RoadCrew
  | Photographer
  | AandR
  | Student
  | Vendor
  | ReadOnly
  | Customer
  | Fan
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "RoleEnum"

data ResourceType = Room | Person | Equipment
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "ResourceType"

data BookingStatus = Tentative | Confirmed | InProgress | Completed | Cancelled | NoShow
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "BookingStatus"

data AttendanceStatus = Enrolled | Attended | Absent | MakeUpNeeded | CompletedA | CancelledA
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "AttendanceStatus"

data UnitsKind = Hours | Lessons | Credits | Sessions
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "UnitsKind"

data RefundPolicy = CreditOnly | Cash | None
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "RefundPolicy"

data PaymentMethod = CashM | BankTransferM | CardPOSM | PayPalM | StripeM | WompiM | PayPhoneM | CryptoM | OtherM
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "PaymentMethod"

data InvoiceStatus = Draft | Sent | PartiallyPaid | Paid | CancelledI
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "InvoiceStatus"

data TicketStatus = OpenT | InProgressT | WaitingParts | DoneT | ClosedT
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "TicketStatus"

data StockTxnReason = Purchase | Consumption | Adjustment | Return | Transfer
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "StockTxnReason"

-- Provide JSON instances for enums (via Generic)
instance ToJSON ServiceKind
instance FromJSON ServiceKind
instance ToJSON PricingModel
instance FromJSON PricingModel
instance ToJSON RoleEnum where
  toJSON = String . roleToText

instance FromJSON RoleEnum where
  parseJSON = withText "RoleEnum" $ \txt ->
    maybe (fail $ "Invalid role: " <> T.unpack txt) pure (roleFromText txt)
instance ToJSON ResourceType
instance FromJSON ResourceType
instance ToJSON BookingStatus
instance FromJSON BookingStatus
instance ToJSON AttendanceStatus
instance FromJSON AttendanceStatus
instance ToJSON UnitsKind
instance FromJSON UnitsKind
instance ToJSON RefundPolicy
instance FromJSON RefundPolicy
instance ToJSON PaymentMethod
instance FromJSON PaymentMethod
instance ToJSON InvoiceStatus
instance FromJSON InvoiceStatus
instance ToJSON TicketStatus
instance FromJSON TicketStatus

roleToText :: RoleEnum -> Text
roleToText Admin         = "Admin"
roleToText Manager       = "Manager"
roleToText StudioManager = "Studio Manager"
roleToText Engineer      = "Engineer"
roleToText Teacher       = "Teacher"
roleToText Reception     = "Reception"
roleToText Accounting    = "Accounting"
roleToText LiveSessionsProducer = "Live Sessions Producer"
roleToText Artist        = "Artist"
roleToText Artista       = "Artista"
roleToText Webmaster     = "Webmaster"
roleToText Promotor      = "Promotor"
roleToText Promoter      = "Promoter"
roleToText Producer      = "Producer"
roleToText Songwriter    = "Songwriter"
roleToText DJ            = "DJ"
roleToText Publicist     = "Publicist"
roleToText TourManager   = "TourManager"
roleToText LabelRep      = "LabelRep"
roleToText StageManager  = "StageManager"
roleToText RoadCrew      = "RoadCrew"
roleToText Photographer  = "Photographer"
roleToText AandR         = "A&R"
roleToText Student       = "Student"
roleToText Vendor        = "Vendor"
roleToText ReadOnly      = "ReadOnly"
roleToText Customer      = "Customer"
roleToText Fan           = "Fan"

roleFromText :: Text -> Maybe RoleEnum
roleFromText raw = 
  let normalized = T.toLower (T.strip raw)
  in case normalized of
    "admin"        -> Just Admin
    "manager"      -> Just Manager
    "studio-manager" -> Just StudioManager
    "studiomanager" -> Just StudioManager
    "studio manager" -> Just StudioManager
    "engineer"     -> Just Engineer
    "teacher"      -> Just Teacher
    "reception"    -> Just Reception
    "accounting"   -> Just Accounting
    "live-sessions-producer" -> Just LiveSessionsProducer
    "livesessionsproducer" -> Just LiveSessionsProducer
    "live-session-producer" -> Just LiveSessionsProducer
    "live sessions producer" -> Just LiveSessionsProducer
    "live session producer" -> Just LiveSessionsProducer
    "artist"       -> Just Artist
    "artista"      -> Just Artista
    "webmaster"    -> Just Webmaster
    "promotor"     -> Just Promotor
    "promoter"     -> Just Promoter
    "producer"     -> Just Producer
    "songwriter"   -> Just Songwriter
    "dj"           -> Just DJ
    "publicist"    -> Just Publicist
    "tourmanager"  -> Just TourManager
    "labelrep"     -> Just LabelRep
    "stagemanager" -> Just StageManager
    "roadcrew"     -> Just RoadCrew
    "photographer" -> Just Photographer
    "a&r"          -> Just AandR
    "aandr"        -> Just AandR
    "ar"           -> Just AandR
    "student"      -> Just Student
    "vendor"       -> Just Vendor
    "readonly"     -> Just ReadOnly
    "customer"     -> Just Customer
    "fan"          -> Just Fan
    _              -> Nothing
instance ToJSON StockTxnReason
instance FromJSON StockTxnReason

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ApiToken
    token            Text
    partyId          PartyId
    label            Text Maybe
    active           Bool
    UniqueApiToken   token
    deriving Show Generic
UserCredential
    partyId          PartyId
    username         Text
    passwordHash     Text
    active           Bool
    UniqueCredentialUsername username
    deriving Show Generic
Party
    legalName        Text Maybe
    displayName      Text
    isOrg            Bool
    taxId            Text Maybe
    primaryEmail     Text Maybe
    primaryPhone     Text Maybe
    whatsapp         Text Maybe
    instagram        Text Maybe
    emergencyContact Text Maybe
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
PartyRole
    partyId          PartyId
    role             RoleEnum
    active           Bool
    UniquePartyRole  partyId role
    deriving Show Generic
ArtistProfile
    artistPartyId    PartyId
    slug             Text Maybe
    bio              Text Maybe
    city             Text Maybe
    heroImageUrl     Text Maybe
    spotifyArtistId  Text Maybe
    spotifyUrl       Text Maybe
    youtubeChannelId Text Maybe
    youtubeUrl       Text Maybe
    websiteUrl       Text Maybe
    featuredVideoUrl Text Maybe
    genres           Text Maybe
    highlights       Text Maybe
    createdAt        UTCTime
    updatedAt        UTCTime Maybe
    UniqueArtistProfile artistPartyId
    deriving Show Generic
ArtistRelease
    artistPartyId    PartyId
    title            Text
    releaseDate      Day Maybe
    description      Text Maybe
    coverImageUrl    Text Maybe
    spotifyUrl       Text Maybe
    youtubeUrl       Text Maybe
    createdAt        UTCTime
    deriving Show Generic
FanProfile
    fanPartyId       PartyId
    displayName      Text Maybe
    avatarUrl        Text Maybe
    favoriteGenres   Text Maybe
    bio              Text Maybe
    city             Text Maybe
    createdAt        UTCTime
    updatedAt        UTCTime Maybe
    UniqueFanProfile fanPartyId
    deriving Show Generic
FanFollow
    fanPartyId       PartyId
    artistPartyId    PartyId
    createdAt        UTCTime
    UniqueFanFollow  fanPartyId artistPartyId
    deriving Show Generic
PartyFollow
    followerPartyId  PartyId
    followingPartyId PartyId
    viaNfc           Bool
    createdAt        UTCTime
    UniquePartyFollow followerPartyId followingPartyId
    deriving Show Generic
ServiceCatalog
    name             Text
    kind             ServiceKind
    pricingModel     PricingModel
    defaultRateCents Int Maybe
    taxBps           Int Maybe
    active           Bool
    deriving Show Generic
ServiceOrder
    customerId       PartyId
    artistId         PartyId Maybe
    catalogId        ServiceCatalogId
    serviceKind      ServiceKind
    title            Text Maybe
    description      Text Maybe
    status           Text
    priceQuotedCents Int Maybe
    quoteSentAt      UTCTime Maybe
    scheduledStart   UTCTime Maybe
    scheduledEnd     UTCTime Maybe
    createdAt        UTCTime
    deriving Show Generic
ServiceStatusChange
    serviceOrderId   ServiceOrderId
    status           Text
    notes            Text Maybe
    changedBy        PartyId Maybe
    createdAt        UTCTime
    deriving Show Generic
Resource
    name             Text
    slug             Text
    resourceType     ResourceType
    capacity         Int Maybe
    active           Bool
    UniqueResourceSlug slug
    deriving Show Generic
Booking
    title            Text
    serviceOrderId   ServiceOrderId Maybe
    partyId          PartyId Maybe
    serviceType      Text Maybe
    startsAt         UTCTime
    endsAt           UTCTime
    status           BookingStatus
    createdBy        PartyId Maybe
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
BookingResource
    bookingId        BookingId
    resourceId       ResourceId
    role             Text
    UniqueBookingRes bookingId resourceId role
    deriving Show Generic
Attendance
    bookingId        BookingId
    partyId          PartyId
    status           AttendanceStatus
    notes            Text Maybe
    UniqueAttendance bookingId partyId
    deriving Show Generic
PackageProduct
    name             Text
    serviceKind      ServiceKind
    unitsKind        UnitsKind
    unitsQty         Int
    priceCents       Int
    expiresDays      Int Maybe
    transferable     Bool
    refundPolicy     RefundPolicy
    active           Bool
    deriving Show Generic
PackagePurchase
    buyerId          PartyId
    productId        PackageProductId
    purchasedAt      UTCTime
    priceCents       Int
    expiresAt        UTCTime Maybe
    remainingUnits   Int
    status           Text
    deriving Show Generic
PackageLedger
    purchaseId       PackagePurchaseId
    bookingId        BookingId Maybe
    deltaUnits       Int
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
Invoice
    customerId       PartyId
    issueDate        Day
    dueDate          Day
    number           Text Maybe
    status           InvoiceStatus
    currency         Text
    subtotalCents    Int
    taxCents         Int
    totalCents       Int
    sriDocumentId    Text Maybe
    notes            Text Maybe
    createdAt        UTCTime
    UniqueInvoiceNumber number !force
    deriving Show Generic
InvoiceLine
    invoiceId        InvoiceId
    serviceOrderId   ServiceOrderId Maybe
    packagePurchaseId PackagePurchaseId Maybe
    description      Text
    quantity         Int
    unitCents        Int
    taxBps           Int
    totalCents       Int
    deriving Show Generic
Receipt
    invoiceId        InvoiceId
    number           Text
    issueDate        Day
    issuedAt         UTCTime
    buyerPartyId     PartyId Maybe
    buyerName        Text
    buyerEmail       Text Maybe
    currency         Text
    subtotalCents    Int
    taxCents         Int
    totalCents       Int
    notes            Text Maybe
    createdAt        UTCTime
    UniqueReceiptNumber number !force
    deriving Show Generic
ReceiptLine
    receiptId        ReceiptId
    description      Text
    quantity         Int
    unitCents        Int
    taxBps           Int Maybe
    totalCents       Int
    deriving Show Generic
Payment
    invoiceId        InvoiceId
    method           PaymentMethod
    amountCents      Int
    receivedAt       UTCTime
    reference        Text Maybe
    createdBy        PartyId Maybe
    deriving Show Generic
PaymentSplit
    paymentId        PaymentId
    payerId          PartyId
    amountCents      Int
    deriving Show Generic
ExternalCalendarMapping
    resourceId       ResourceId
    googleCalendarId Text
    direction        Text
    UniqueCalMap     resourceId
    deriving Show Generic
AuditLog
    actorId          PartyId Maybe
    entity           Text
    entityId         Text
    action           Text
    diff             Text Maybe
    createdAt        UTCTime
    deriving Show Generic

AcademyUser
    Id UUID default=gen_random_uuid()
    email     Text
    role      Text
    platform  Text Maybe
    createdAt UTCTime default=now()
    UniqueAcademyUserEmail email
    deriving Show Generic

AcademyMicrocourse
    Id UUID default=gen_random_uuid()
    slug     Text
    title    Text
    summary  Text Maybe
    createdAt UTCTime default=now()
    UniqueAcademyMicrocourseSlug slug
    deriving Show Generic

AcademyLesson
    Id UUID default=gen_random_uuid()
    microcourseId AcademyMicrocourseId
    day      Int
    title    Text
    body     Text
    UniqueAcademyLesson microcourseId day
    deriving Show Generic

AcademyProgress
    userId   AcademyUserId
    lessonId AcademyLessonId
    completedAt UTCTime default=now()
    Primary userId lessonId
    deriving Show Generic

ReferralCode
    Id Text
    ownerUserId AcademyUserId Maybe
    createdAt   UTCTime default=now()
    deriving Show Generic

ReferralClaim
    Id UUID default=gen_random_uuid()
    codeId         ReferralCodeId
    claimantUserId AcademyUserId Maybe
    email          Text
    claimedAt      UTCTime default=now()
    UniqueReferralClaim codeId email
    deriving Show Generic

Cohort
    Id UUID default=gen_random_uuid()
    slug     Text
    title    Text
    startsAt UTCTime
    endsAt   UTCTime
    seatCap  Int
    UniqueCohortSlug slug
    deriving Show Generic

CohortEnrollment
    cohortId CohortId
    userId   AcademyUserId
    createdAt UTCTime default=now()
    Primary cohortId userId
    deriving Show Generic
|]
