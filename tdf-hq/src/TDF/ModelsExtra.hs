{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module TDF.ModelsExtra where

import           Data.Aeson        (ToJSON(..), (.=), object)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Data.Time          (Day, UTCTime)
import           Data.UUID          (UUID)
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics       (Generic)
import           Web.PathPieces     (toPathPiece)

import           TDF.Models         (PartyId, ServiceKind)
import           TDF.UUIDInstances  ()

data AssetStatus = Active | Booked | OutForMaintenance | Retired
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "AssetStatus"

data AssetCondition = NewC | Good | Fair | Poor
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "AssetCondition"

data MaintenancePolicy = Quarterly | Annual | None
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "MaintenancePolicy"

data CheckoutTarget = TargetSession | TargetParty | TargetRoom
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "CheckoutTarget"

data StockUnit = Pcs | Set | Roll | Pack | Bottle | OtherUnit
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "StockUnit"

data StockMoveReason = Consume | Recount | Receive | Return | Damage | Loss | OtherMove
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "StockMoveReason"

data SessionStatus = InPrep | InSession | Break | Editing | Approved | Delivered | Closed
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "SessionStatus"

data DeliverableKind = Mix | Master | Stems | DDP | OtherDeliverable
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "DeliverableKind"

share [mkPersist sqlSettings, mkMigrate "migrateExtra"] [persistLowerCase|

Campaign
    name        Text
    objective   Text Maybe
    platform    Text Maybe
    status      Text default='active'
    budgetCents Int Maybe
    startDate   Day Maybe
    endDate     Day Maybe
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

AdCreative
    campaignId  CampaignId Maybe
    externalId  Text Maybe
    name        Text
    channel     Text Maybe
    audience    Text Maybe
    landingUrl  Text Maybe
    cta         Text Maybe
    status      Text default='active'
    notes       Text Maybe
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

AdConversationExample
    adId             AdCreativeId
    userMessage      Text
    assistantMessage Text
    tags             [Text] Maybe sqltype=text[]
    createdAt        UTCTime default=now()
    updatedAt        UTCTime default=now()
    deriving Show Generic

StudioBrainEntry
    title       Text
    body        Text
    category    Text Maybe
    tags        [Text] Maybe sqltype=text[]
    active      Bool default=True
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

WhatsAppMessage
    externalId       Text
    senderId         Text
    senderName       Text Maybe
    text             Text Maybe
    direction        Text
    adExternalId     Text Maybe
    adName           Text Maybe
    campaignExternalId Text Maybe
    campaignName     Text Maybe
    metadata         Text Maybe
    repliedAt        UTCTime Maybe
    replyText        Text Maybe
    replyError       Text Maybe
    createdAt        UTCTime
    UniqueWhatsAppMessage externalId
    deriving Show Generic

WhatsAppConsent
    phoneE164    Text
    displayName  Text Maybe
    consent      Bool default=False
    source       Text Maybe
    note         Text Maybe
    consentedAt  UTCTime Maybe
    revokedAt    UTCTime Maybe
    createdAt    UTCTime default=now()
    updatedAt    UTCTime default=now()
    UniqueWhatsAppConsent phoneE164
    deriving Show Generic

CourseRegistration
    courseSlug   Text
    fullName     Text Maybe
    email        Text Maybe
    phoneE164    Text Maybe
    source       Text
    status       Text
    howHeard     Text Maybe
    utmSource    Text Maybe
    utmMedium    Text Maybe
    utmCampaign  Text Maybe
    utmContent   Text Maybe
    createdAt    UTCTime default=now()
    updatedAt    UTCTime default=now()
    deriving Show Generic

DropdownOption
    Id         UUID default=gen_random_uuid()
    category   Text
    value      Text
    label      Text Maybe
    active     Bool default=True
    sortOrder  Int Maybe
    createdAt  UTCTime default=now()
    updatedAt  UTCTime default=now()
    UniqueDropdownOption category value
    deriving Show Generic

Room
    Id                UUID default=gen_random_uuid()
    name              Text
    isBookable        Bool default=True
    capacity          Int Maybe
    channelCount      Int Maybe
    defaultSampleRate Int Maybe
    patchbayNotes     Text Maybe
    UniqueRoomName name
    deriving Show Generic

RoomFeature
    Id        UUID default=gen_random_uuid()
    roomId    RoomId
    key       Text
    value     Text
    UniqueRoomFeature roomId key
    deriving Show Generic

Asset
    Id                   UUID default=gen_random_uuid()
    name                 Text
    category             Text
    brand                Text Maybe
    model                Text Maybe
    serialNumber         Text Maybe
    purchaseDate         Day  Maybe
    purchasePriceUsdCents Int Maybe
    condition            AssetCondition default='Good'
    status               AssetStatus    default='Active'
    locationId           RoomId Maybe
    owner                Text default='TDF'
    qrCode               Text Maybe
    photoUrl             Text Maybe
    notes                Text Maybe
    warrantyExpires      Day  Maybe
    maintenancePolicy    MaintenancePolicy default='None'
    nextMaintenanceDue   Day  Maybe
    UniqueAssetSerial serialNumber !force
    UniqueAssetQr     qrCode       !force
    deriving Show Generic

RoomDefaultGear
    Id        UUID default=gen_random_uuid()
    roomId    RoomId
    assetId   AssetId
    UniqueRoomDefaultGear roomId assetId
    deriving Show Generic

AssetKitMember
    Id         UUID default=gen_random_uuid()
    kitId      AssetId
    memberId   AssetId
    qty        Int default=1
    UniqueKitMember kitId memberId
    deriving Show Generic

PipelineCard
    Id          UUID default=gen_random_uuid()
    serviceKind ServiceKind
    title       Text
    artist      Text Maybe
    stage       Text
    sortOrder   Int default=0
    notes       Text Maybe
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

Proposal
    Id            UUID default=gen_random_uuid()
    title         Text
    serviceKind   ServiceKind Maybe
    clientPartyId PartyId Maybe
    contactName   Text Maybe
    contactEmail  Text Maybe
    contactPhone  Text Maybe
    pipelineCardId PipelineCardId Maybe
    status        Text default='draft'
    notes         Text Maybe
    createdAt     UTCTime default=now()
    updatedAt     UTCTime default=now()
    lastGeneratedAt UTCTime Maybe
    sentAt        UTCTime Maybe
    deriving Show Generic

ProposalVersion
    Id           UUID default=gen_random_uuid()
    proposalId   ProposalId
    version      Int
    latex        Text
    createdAt    UTCTime default=now()
    createdByRef Text Maybe
    notes        Text Maybe
    UniqueProposalVersion proposalId version
    deriving Show Generic

-- Party/Booking references use free-text refs to avoid coupling to existing tables
StockItem
    Id           UUID default=gen_random_uuid()
    name         Text
    sku          Text
    unit         StockUnit default='Pcs'
    binLocation  Text Maybe
    onHand       Int default=0
    reorderPoint Int Maybe
    vendorPartyRef Text Maybe
    notes        Text Maybe
    UniqueStockSku sku
    deriving Show Generic

Band
    Id           UUID default=gen_random_uuid()
    partyId      PartyId
    name         Text
    labelArtist  Bool default=False
    primaryGenre Text Maybe
    homeCity     Text Maybe
    photoUrl     Text Maybe
    contractFlags Text Maybe
    UniqueBandName name
    UniqueBandParty partyId
    deriving Show Generic

BandMember
    Id         UUID default=gen_random_uuid()
    bandId     BandId
    partyId    PartyId
    roleInBand Text Maybe
    UniqueBandMember bandId partyId
    deriving Show Generic

LiveSessionIntake
    Id           UUID default=gen_random_uuid()
    bandName     Text
    bandDescription Text Maybe
    primaryGenre Text Maybe
    inputList    Text Maybe
    contactEmail Text Maybe
    contactPhone Text Maybe
    sessionDate  Day Maybe
    availability Text Maybe
    acceptedTerms Bool default=False
    termsVersion Text Maybe
    riderPath    Text Maybe
    createdBy    PartyId Maybe
    createdAt    UTCTime default=now()
    deriving Show Generic

LiveSessionMusician
    Id          UUID default=gen_random_uuid()
    intakeId    LiveSessionIntakeId
    partyId     PartyId
    name        Text
    email       Text Maybe
    instrument  Text Maybe
    role        Text Maybe
    notes       Text Maybe
    isExisting  Bool default=False
    deriving Show Generic

LiveSessionSong
    Id          UUID default=gen_random_uuid()
    intakeId    LiveSessionIntakeId
    title       Text
    bpm         Int Maybe
    songKey     Text Maybe
    lyrics      Text Maybe
    sortOrder   Int default=0
    deriving Show Generic

Feedback
    Id           UUID default=gen_random_uuid()
    title        Text
    description  Text
    category     Text Maybe
    severity     Text Maybe
    contactEmail Text Maybe
    attachment   Text Maybe
    consent      Bool default=False
    createdBy    PartyId Maybe
    createdAt    UTCTime default=now()
    deriving Show Generic

Session
    Id               UUID default=gen_random_uuid()
    bookingRef       Text Maybe
    bandId           BandId   Maybe
    clientPartyRef   Text     Maybe
    service          Text
    startAt          UTCTime
    endAt            UTCTime
    engineerRef      Text
    assistantRef     Text     Maybe
    status           SessionStatus default='InPrep'
    sampleRate       Int Maybe
    bitDepth         Int Maybe
    daw              Text Maybe
    sessionFolderDriveId Text Maybe
    notes            Text Maybe
    deriving Show Generic

SessionRoom
    Id        UUID default=gen_random_uuid()
    sessionId SessionId
    roomId    RoomId
    UniqueSessionRoom sessionId roomId
    deriving Show Generic

SessionDeliverable
    Id          UUID default=gen_random_uuid()
    sessionId   SessionId
    kind        DeliverableKind
    name        Text
    driveFileId Text Maybe
    externalUrl Text Maybe
    deliveredAt UTCTime Maybe
    approvedAt  UTCTime Maybe
    notes       Text Maybe
    deriving Show Generic

InputListTemplate
    Id           UUID default=gen_random_uuid()
    name         Text
    genre        Text Maybe
    channelCount Int Maybe
    notes        Text Maybe
    deriving Show Generic

InputListTemplateRow
    Id                UUID default=gen_random_uuid()
    templateId        InputListTemplateId
    channelNumber     Int
    trackName         Text Maybe
    instrument        Text Maybe
    micId             AssetId Maybe
    standId           AssetId Maybe
    cableId           AssetId Maybe
    preampId          AssetId Maybe
    insertOutboardId  AssetId Maybe
    converterChannel  Text Maybe
    phantom           Bool Maybe
    polarity          Bool Maybe
    hpf               Bool Maybe
    pad               Bool Maybe
    notes             Text Maybe
    UniqueTemplateChannel templateId channelNumber
    deriving Show Generic

InputList
    Id         UUID default=gen_random_uuid()
    sessionId  SessionId
    createdAt  UTCTime default=now()
    deriving Show Generic

InputListVersion
    Id           UUID default=gen_random_uuid()
    inputListId  InputListId
    version      Int
    createdAt    UTCTime default=now()
    createdByRef Text Maybe
    notes        Text Maybe
    UniqueListVersion inputListId version
    deriving Show Generic

InputRow
    Id                UUID default=gen_random_uuid()
    versionId         InputListVersionId
    channelNumber     Int
    trackName         Text Maybe
    instrument        Text Maybe
    micId             AssetId Maybe
    standId           AssetId Maybe
    cableId           AssetId Maybe
    preampId          AssetId Maybe
    insertOutboardId  AssetId Maybe
    converterChannel  Text Maybe
    phantom           Bool Maybe
    polarity          Bool Maybe
    hpf               Bool Maybe
    pad               Bool Maybe
    notes             Text Maybe
    UniqueRowPerChannel versionId channelNumber
    deriving Show Generic

-- Party/Booking references use free-text refs to avoid coupling to existing tables
AssetCheckout
    Id               UUID default=gen_random_uuid()
    assetId          AssetId
    targetKind       CheckoutTarget
    targetSessionId  SessionId   Maybe
    targetPartyRef   Text        Maybe
    targetRoomId     RoomId      Maybe
    checkedOutByRef  Text
    checkedOutAt     UTCTime default=now()
    dueAt            UTCTime Maybe
    conditionOut     Text Maybe
    photoDriveFileId Text Maybe
    returnedAt       UTCTime Maybe
    conditionIn      Text Maybe
    notes            Text Maybe
    deriving Show Generic

AssetAudit
    Id        UUID default=gen_random_uuid()
    assetId   AssetId
    at        UTCTime default=now()
    event     Text
    detail    Text Maybe
    deriving Show Generic

MaintenanceTicket
    Id            UUID default=gen_random_uuid()
    assetId       AssetId
    status        Text
    openedAt      UTCTime default=now()
    closedAt      UTCTime Maybe
    vendorPartyRef Text Maybe
    summary       Text
    details       Text Maybe
    deriving Show Generic

MaintenanceAttachment
    Id          UUID default=gen_random_uuid()
    ticketId    MaintenanceTicketId
    driveFileId Text
    label       Text Maybe
    deriving Show Generic

StockMovement
    Id            UUID default=gen_random_uuid()
    stockItemId   StockItemId
    changeQty     Int
    reason        StockMoveReason default='OtherMove'
    at            UTCTime default=now()
    refCheckoutId AssetCheckoutId Maybe
    refSessionId  SessionId      Maybe
    notes         Text Maybe
    deriving Show Generic

MarketplaceListing
    Id             UUID default=gen_random_uuid()
    assetId        AssetId
    title          Text
    purpose        Text default='sale'
    priceUsdCents  Int
    markupPct      Int default=25
    currency       Text default='USD'
    active         Bool default=True
    createdAt      UTCTime default=now()
    updatedAt      UTCTime default=now()
    UniqueMarketplaceAsset assetId purpose
    deriving Show Generic

MarketplaceCart
    Id        UUID default=gen_random_uuid()
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

MarketplaceCartItem
    Id         UUID default=gen_random_uuid()
    cartId     MarketplaceCartId
    listingId  MarketplaceListingId
    quantity   Int default=1
    UniqueMarketplaceCartItem cartId listingId
    deriving Show Generic

MarketplaceOrder
    Id              UUID default=gen_random_uuid()
    cartId          MarketplaceCartId Maybe
    buyerName       Text
    buyerEmail      Text
    buyerPhone      Text Maybe
    totalUsdCents   Int
    currency        Text default='USD'
    status          Text default='pending'
    paymentProvider Text Maybe
    paypalOrderId   Text Maybe
    paypalPayerEmail Text Maybe
    datafastCheckoutId Text Maybe
    datafastResourcePath Text Maybe
    datafastPaymentId   Text Maybe
    datafastResultCode  Text Maybe
    datafastResultDescription Text Maybe
    datafastPaymentBrand Text Maybe
    datafastAuthCode     Text Maybe
    datafastAcquirerCode Text Maybe
    paidAt          UTCTime Maybe
    createdAt       UTCTime default=now()
    updatedAt       UTCTime default=now()
    deriving Show Generic

MarketplaceOrderItem
    Id                UUID default=gen_random_uuid()
    orderId           MarketplaceOrderId
    listingId         MarketplaceListingId
    quantity          Int
    unitPriceUsdCents Int
    subtotalUsdCents  Int
    deriving Show Generic

LabelTrack
    Id          UUID default=gen_random_uuid()
    title       Text
    note        Text Maybe
    status      Text default='open'
    ownerPartyId PartyId Maybe
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

InternProfile
    Id          UUID default=gen_random_uuid()
    partyId     PartyId
    startAt     Day Maybe
    endAt       Day Maybe
    requiredHours Int Maybe
    skills      Text Maybe
    areas       Text Maybe
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    UniqueInternProfile partyId
    deriving Show Generic

InternProject
    Id          UUID default=gen_random_uuid()
    title       Text
    description Text Maybe
    status      Text default='active'
    startAt     Day Maybe
    dueAt       Day Maybe
    createdBy   PartyId
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

InternTask
    Id          UUID default=gen_random_uuid()
    projectId   InternProjectId
    title       Text
    description Text Maybe
    status      Text default='todo'
    progress    Int default=0
    assignedTo  PartyId Maybe
    dueAt       Day Maybe
    createdBy   PartyId
    createdAt   UTCTime default=now()
    updatedAt   UTCTime default=now()
    deriving Show Generic

InternTodo
    Id           UUID default=gen_random_uuid()
    ownerPartyId PartyId
    text         Text
    done         Bool default=False
    createdAt    UTCTime default=now()
    updatedAt    UTCTime default=now()
    deriving Show Generic

InternTimeEntry
    Id         UUID default=gen_random_uuid()
    partyId    PartyId
    clockIn    UTCTime
    clockOut   UTCTime Maybe
    notes      Text Maybe
    createdAt  UTCTime default=now()
    updatedAt  UTCTime default=now()
    deriving Show Generic

InternPermissionRequest
    Id            UUID default=gen_random_uuid()
    partyId       PartyId
    category      Text
    reason        Text Maybe
    startAt       Day
    endAt         Day Maybe
    status        Text default='pending'
    reviewedBy    PartyId Maybe
    reviewedAt    UTCTime Maybe
    decisionNotes Text Maybe
    createdAt     UTCTime default=now()
    updatedAt     UTCTime default=now()
    deriving Show Generic

|]

instance ToJSON (Entity Asset) where
  toJSON (Entity key asset) = object
    [ "id"         .= toPathPiece key
    , "name"       .= assetName asset
    , "category"   .= assetCategory asset
    , "brand"      .= assetBrand asset
    , "model"      .= assetModel asset
    , "status"     .= T.pack (show (assetStatus asset))
    , "locationId" .= fmap toPathPiece (assetLocationId asset)
    ]

instance ToJSON (Entity InputRow) where
  toJSON (Entity key row) = object
    [ "id"             .= toPathPiece key
    , "channel"        .= inputRowChannelNumber row
    , "trackName"      .= inputRowTrackName row
    , "instrument"     .= inputRowInstrument row
    , "micId"          .= fmap toPathPiece (inputRowMicId row)
    , "standId"        .= fmap toPathPiece (inputRowStandId row)
    , "cableId"        .= fmap toPathPiece (inputRowCableId row)
    , "preampId"       .= fmap toPathPiece (inputRowPreampId row)
    , "insertOutboard" .= fmap toPathPiece (inputRowInsertOutboardId row)
    , "converter"      .= inputRowConverterChannel row
    , "phantom"        .= inputRowPhantom row
    , "polarity"       .= inputRowPolarity row
    , "hpf"            .= inputRowHpf row
    , "pad"            .= inputRowPad row
    , "notes"          .= inputRowNotes row
    ]
