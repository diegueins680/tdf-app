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
import           Data.Time (UTCTime, Day, TimeOfDay)
import           Data.Int (Int64)
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
  | Intern
  | Artist
  | Artista
  | Webmaster
  | Promotor
  | Promoter
  | Producer
  | Agency
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
  | Maintenance
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

data FanClubOfficerRole = President | VicePresident | Secretary | Treasurer | Coordinator
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "FanClubOfficerRole"

data ElectionStatus = Upcoming | CandidacyOpen | VotingOpen | Closed
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "ElectionStatus"

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
roleToText Intern        = "Intern"
roleToText Artist        = "Artist"
roleToText Artista       = "Artista"
roleToText Webmaster     = "Webmaster"
roleToText Promotor      = "Promotor"
roleToText Promoter      = "Promoter"
roleToText Producer      = "Producer"
roleToText Agency        = "Agency"
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
roleToText Maintenance   = "Maintenance"

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
    "intern"       -> Just Intern
    "pasante"      -> Just Intern
    "practicante"  -> Just Intern
    "artist"       -> Just Artist
    "artista"      -> Just Artista
    "webmaster"    -> Just Webmaster
    "promotor"     -> Just Promotor
    "promoter"     -> Just Promoter
    "producer"     -> Just Producer
    "agency"       -> Just Agency
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
    "maintenance"  -> Just Maintenance
    _              -> Nothing

-- Stable compile-time identifiers used only to bind exhaustive backend
-- enforcement to the persisted security registry. Labels, ordering and grants
-- remain database-authoritative.
roleRegistryCode :: RoleEnum -> Text
roleRegistryCode role = case role of
  Admin -> "admin"
  Manager -> "manager"
  StudioManager -> "studio-manager"
  Engineer -> "engineer"
  Teacher -> "teacher"
  Reception -> "reception"
  Accounting -> "accounting"
  LiveSessionsProducer -> "live-sessions-producer"
  Intern -> "intern"
  Artist -> "artist"
  Artista -> "artista"
  Webmaster -> "webmaster"
  Promotor -> "promotor"
  Promoter -> "promoter"
  Producer -> "producer"
  Agency -> "agency"
  Songwriter -> "songwriter"
  DJ -> "dj"
  Publicist -> "publicist"
  TourManager -> "tour-manager"
  LabelRep -> "label-rep"
  StageManager -> "stage-manager"
  RoadCrew -> "road-crew"
  Photographer -> "photographer"
  AandR -> "a-and-r"
  Student -> "student"
  Vendor -> "vendor"
  ReadOnly -> "read-only"
  Customer -> "customer"
  Fan -> "fan"
  Maintenance -> "maintenance"

roleFromRegistryCode :: Text -> Maybe RoleEnum
roleFromRegistryCode code =
  let normalized = T.toLower (T.strip code)
  in lookup normalized [(roleRegistryCode role, role) | role <- [minBound .. maxBound]]
instance ToJSON StockTxnReason
instance FromJSON StockTxnReason
instance ToJSON FanClubOfficerRole
instance FromJSON FanClubOfficerRole
instance ToJSON ElectionStatus
instance FromJSON ElectionStatus

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
    stripeCustomerId Text Maybe
    -- Legacy import/display value. Catalog cutover backfills this into the
    -- persisted country reference before the text column is retired.
    countryCode      Text Maybe
    countryId        UUID Maybe
    createdAt        UTCTime
    deriving Show Generic
UserLocalePreference sql=user_locale_preferences
    userId           PartyId
    -- Preserved migration evidence only. Runtime reads and writes localeId
    -- and currencyId; new writes clear these copied codes.
    locale           Text Maybe
    currency         Text Maybe
    timezone         Text
    countryCode      Text Maybe
    localeId         UUID Maybe
    currencyId       UUID Maybe
    countryId        UUID Maybe
    showEventRsvpsOnProfile Bool default=TRUE
    updatedAt        UTCTime
    UniqueUserLocalePreference userId
    deriving Show Generic
UserOnboardingProgress sql=user_onboarding_progress
    partyId               PartyId
    signupCompletedAt     UTCTime Maybe
    intent                Text Maybe
    completedAt           UTCTime Maybe
    firstValue            Text Maybe
    firstValueCompletedAt UTCTime Maybe
    updatedAt             UTCTime
    UniqueUserOnboardingProgress partyId
    deriving Show Generic
UserExperimentAssignment sql=user_experiment_assignment
    partyId          PartyId
    experimentId     Text
    experimentVersion Int
    variant          Text
    assignedAt       UTCTime
    eligibleUntil    UTCTime
    exposedAt        UTCTime Maybe
    UniqueUserExperimentAssignment partyId experimentId experimentVersion
    deriving Show Generic
CurrencyConversionAudit sql=currency_conversion_audit
    userId           PartyId Maybe
    sourceCurrency   Text
    targetCurrency   Text
    sourceMinorUnits Int64
    targetMinorUnits Int64
    exchangeRate     Double
    rateSource       Text
    rateObservedAt   UTCTime
    createdAt        UTCTime
    deriving Show Generic
ArtistProfile
    artistPartyId    PartyId
    slug             Text Maybe
    bio              Text Maybe
    city             Text Maybe
    -- Legacy import/display value; never accepted as a canonical catalog ID.
    countryCode      Text Maybe
    countryId        UUID Maybe
    heroImageUrl     Text Maybe
    spotifyArtistId  Text Maybe
    spotifyUrl       Text Maybe
    youtubeChannelId Text Maybe
    youtubeUrl       Text Maybe
    websiteUrl       Text Maybe
    featuredVideoUrl Text Maybe
    genres           Text Maybe
    highlights       Text Maybe
    stripeAccountId  Text Maybe
    createdAt        UTCTime
    updatedAt        UTCTime Maybe
    UniqueArtistProfile artistPartyId
    deriving Show Generic
ArtistProfileGenreMembership sql=artist_profile_genre_membership
    artistPartyId    PartyId
    genreId          UUID
    sortOrder        Int default=0
    createdAt        UTCTime default=CURRENT_TIMESTAMP
    Primary artistPartyId genreId
    deriving Show Generic
ArtistPromoSlot
    artistPartyId    PartyId
    day              Day
    startTime        TimeOfDay
    medium           Text
    program          Text
    interviewerHost  Text
    bandMembers      Text
    status           Text Maybe
    notes            Text Maybe
    createdAt        UTCTime
    updatedAt        UTCTime
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
ArtistProfileEnrichment
    artistPartyId    PartyId
    officialName     Text Maybe
    country          Text Maybe
    instagramUrl     Text Maybe
    socialLinks      Text Maybe
    discography      Text Maybe
    achievements     Text Maybe
    heroOriginalUrl  Text Maybe
    heroSquareUrl    Text Maybe
    heroLandscapeUrl Text Maybe
    heroResponsiveUrls Text Maybe
    heroFocalPoint   Text Maybe
    lastVerifiedAt   UTCTime Maybe
    confidence       Double Maybe
    reviewStatus     Text default='unverified'
    createdAt        UTCTime
    updatedAt        UTCTime
    UniqueArtistProfileEnrichment artistPartyId
    deriving Show Generic
ArtistInventoryReference
    idempotencyKey   Text
    sourceType       Text
    sourceRecordId   Text
    originalName     Text
    normalizedName   Text
    artistPartyId    PartyId Maybe
    socialArtistId   Int64 Maybe
    aliases          Text Maybe
    evidence         Text Maybe
    confidence       Double Maybe
    disposition      Text default='discovered'
    firstSeenAt      UTCTime
    lastSeenAt       UTCTime
    UniqueArtistInventoryReference idempotencyKey
    deriving Show Generic
ArtistResearchSource
    artistPartyId    PartyId Maybe
    inventoryReferenceId ArtistInventoryReferenceId Maybe
    sourceUrl        Text
    sourceType       Text
    retrievedAt      UTCTime
    supportedFields  Text
    attribution      Text Maybe
    contentHash      Text Maybe
    idempotencyKey   Text
    UniqueArtistResearchSource idempotencyKey
    deriving Show Generic
ArtistEnrichmentSuggestion
    artistPartyId    PartyId Maybe
    inventoryReferenceId ArtistInventoryReferenceId Maybe
    fieldName        Text
    currentValue     Text Maybe
    proposedValue    Text Maybe
    confidence       Double
    status           Text default='pending'
    autoPublish      Bool default=False
    evidence         Text
    idempotencyKey   Text
    createdAt        UTCTime
    updatedAt        UTCTime
    decidedAt        UTCTime Maybe
    decidedBy        PartyId Maybe
    decisionNote     Text Maybe
    UniqueArtistEnrichmentSuggestion idempotencyKey
    deriving Show Generic
ArtistFieldChange
    artistPartyId    PartyId
    suggestionId     ArtistEnrichmentSuggestionId Maybe
    fieldName        Text
    previousValue    Text Maybe
    newValue         Text Maybe
    evidence         Text
    confidence       Double
    actor             Text
    changedAt        UTCTime
    idempotencyKey   Text
    UniqueArtistFieldChange idempotencyKey
    deriving Show Generic
ArtistEnrichmentRun
    runKey           Text
    mode             Text
    scope            Text
    requestedArtistId PartyId Maybe
    status           Text
    phase            Text
    checkpoint       Text Maybe
    counters         Text Maybe
    errorSummary     Text Maybe
    startedAt        UTCTime
    heartbeatAt      UTCTime
    finishedAt       UTCTime Maybe
    UniqueArtistEnrichmentRun runKey
    deriving Show Generic
ArtistIdentityCandidate
    inventoryReferenceId ArtistInventoryReferenceId
    artistPartyId    PartyId Maybe
    provider         Text
    externalId       Text Maybe
    candidateUrl     Text Maybe
    evidence         Text
    confidence       Double
    status           Text default='pending'
    idempotencyKey   Text
    createdAt        UTCTime
    updatedAt        UTCTime
    decidedAt        UTCTime Maybe
    decidedBy        PartyId Maybe
    decisionNote     Text Maybe
    UniqueArtistIdentityCandidate idempotencyKey
    deriving Show Generic
ArtistMediaAsset
    artistPartyId    PartyId
    assetKind        Text
    sourceUrl        Text
    sourceAttribution Text
    retrievedAt      UTCTime
    sourceContentHash Text
    sourceWidth      Int
    sourceHeight     Int
    sourceMimeType   Text
    sourceByteSize   Int64
    contentHash      Text
    width            Int
    height           Int
    mimeType         Text
    byteSize         Int64
    rightsStatus     Text
    driveFileId      Text
    publicUrl        Text
    parentAssetId    ArtistMediaAssetId Maybe
    focalPoint       Text Maybe
    idempotencyKey   Text
    createdAt        UTCTime
    UniqueArtistMediaAsset idempotencyKey
    UniqueArtistMediaDriveFile driveFileId
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
FanProfileGenreMembership sql=fan_profile_genre_membership
    fanPartyId       PartyId
    genreId          UUID
    sortOrder        Int default=0
    createdAt        UTCTime default=CURRENT_TIMESTAMP
    Primary fanPartyId genreId
    deriving Show Generic
FanFollow
    fanPartyId       PartyId
    artistPartyId    PartyId
    createdAt        UTCTime
    UniqueFanFollow  fanPartyId artistPartyId
    deriving Show Generic
EngagementEvent
    actorPartyId     PartyId Maybe
    targetArtistId   PartyId Maybe
    entityType       Text
    entityId         Int Maybe
    eventType        Text
    metadata         Text Maybe
    createdAt        UTCTime
    deriving Show Generic
PartyFollow
    followerPartyId  PartyId
    followingPartyId PartyId
    viaNfc           Bool
    createdAt        UTCTime
    UniquePartyFollow followerPartyId followingPartyId
    deriving Show Generic
ChatThread
    dmPartyA         PartyId
    dmPartyB         PartyId
    createdAt        UTCTime
    updatedAt        UTCTime
    UniqueChatThread dmPartyA dmPartyB
    deriving Show Generic
ChatMessage
    threadId         ChatThreadId
    senderPartyId    PartyId
    body             Text
    createdAt        UTCTime
    deriving Show Generic
    -- Non-unique history index is governed by SQL; a Persistent unique here
    -- would incorrectly limit the conversation to one message.
