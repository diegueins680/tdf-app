{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TDF.API.Types where

import           Control.Applicative ((<|>))
import           Data.Aeson   (FromJSON(..), ToJSON(..), Value(..), eitherDecode, object, withObject, (.:), (.:?), (.=))
import           Data.Int     (Int64)
import           Data.Text    (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import           Data.Time    (UTCTime, Day)
import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)
import           Network.HTTP.Media ((//))
import           Servant.API  (Accept(..), MimeUnrender(..), OctetStream, PlainText)

import           TDF.Models   (PricingModel, RoleEnum, ServiceKind)

data Page a = Page
  { items    :: [a]
  , page     :: Int
  , pageSize :: Int
  , total    :: Int
  } deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Page a)
instance (FromJSON a) => FromJSON (Page a)

data DropdownOptionDTO = DropdownOptionDTO
  { optionId  :: Text
  , category  :: Text
  , value     :: Text
  , label     :: Maybe Text
  , active    :: Bool
  , sortOrder :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON DropdownOptionDTO
instance FromJSON DropdownOptionDTO

data DropdownOptionCreate = DropdownOptionCreate
  { docValue     :: Text
  , docLabel     :: Maybe Text
  , docSortOrder :: Maybe Int
  , docActive    :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON DropdownOptionCreate
instance FromJSON DropdownOptionCreate

data DropdownOptionUpdate = DropdownOptionUpdate
  { douValue     :: Maybe Text
  , douLabel     :: Maybe (Maybe Text)
  , douSortOrder :: Maybe (Maybe Int)
  , douActive    :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON DropdownOptionUpdate
instance FromJSON DropdownOptionUpdate

data RoleDetailDTO = RoleDetailDTO
  { role    :: RoleEnum
  , label   :: Text
  , modules :: [Text]
  } deriving (Show, Generic)

instance ToJSON RoleDetailDTO
instance FromJSON RoleDetailDTO

data UserAccountDTO = UserAccountDTO
  { userId    :: Int64
  , partyId   :: Int64
  , partyName :: Text
  , username  :: Text
  , active    :: Bool
  , roles     :: [RoleEnum]
  , modules   :: [Text]
  } deriving (Show, Generic)

instance ToJSON UserAccountDTO
instance FromJSON UserAccountDTO

data UserAccountCreate = UserAccountCreate
  { uacPartyId  :: Int64
  , uacUsername :: Maybe Text
  , uacPassword :: Maybe Text
  , uacActive   :: Maybe Bool
  , uacRoles    :: Maybe [RoleEnum]
  } deriving (Show, Generic)

instance ToJSON UserAccountCreate
instance FromJSON UserAccountCreate

data UserAccountUpdate = UserAccountUpdate
  { uauUsername :: Maybe Text
  , uauPassword :: Maybe Text
  , uauActive   :: Maybe Bool
  , uauRoles    :: Maybe [RoleEnum]
  } deriving (Show, Generic)

instance ToJSON UserAccountUpdate
instance FromJSON UserAccountUpdate

data AccountStatusDTO = AccountStatusActive | AccountStatusInactive
  deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance ToJSON AccountStatusDTO
instance FromJSON AccountStatusDTO

data UserRoleSummaryDTO = UserRoleSummaryDTO
  { id        :: Int64
  , name      :: Text
  , email     :: Maybe Text
  , phone     :: Maybe Text
  , roles     :: [RoleEnum]
  , status    :: AccountStatusDTO
  , createdAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON UserRoleSummaryDTO
instance FromJSON UserRoleSummaryDTO

data UserRoleUpdatePayload = UserRoleUpdatePayload
  { roles :: [RoleEnum]
  } deriving (Show, Generic)

instance ToJSON UserRoleUpdatePayload
instance FromJSON UserRoleUpdatePayload

data ServiceCatalogDTO = ServiceCatalogDTO
  { scId            :: Int64
  , scName          :: Text
  , scKind          :: ServiceKind
  , scPricingModel  :: PricingModel
  , scRateCents     :: Maybe Int
  , scCurrency      :: Text
  , scBillingUnit   :: Maybe Text
  , scTaxBps        :: Maybe Int
  , scActive        :: Bool
  } deriving (Show, Generic)

instance ToJSON ServiceCatalogDTO
instance FromJSON ServiceCatalogDTO

data ServiceCatalogCreate = ServiceCatalogCreate
  { sccName         :: Text
  , sccKind         :: Maybe ServiceKind
  , sccPricingModel :: Maybe PricingModel
  , sccRateCents    :: Maybe Int
  , sccCurrency     :: Maybe Text
  , sccBillingUnit  :: Maybe Text
  , sccTaxBps       :: Maybe Int
  , sccActive       :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON ServiceCatalogCreate
instance FromJSON ServiceCatalogCreate

data ServiceCatalogUpdate = ServiceCatalogUpdate
  { scuName         :: Maybe Text
  , scuKind         :: Maybe ServiceKind
  , scuPricingModel :: Maybe PricingModel
  , scuRateCents    :: Maybe (Maybe Int)
  , scuCurrency     :: Maybe Text
  , scuBillingUnit  :: Maybe (Maybe Text)
  , scuTaxBps       :: Maybe (Maybe Int)
  , scuActive       :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON ServiceCatalogUpdate
instance FromJSON ServiceCatalogUpdate

data BandOptionsDTO = BandOptionsDTO
  { roles  :: [DropdownOptionDTO]
  , genres :: [DropdownOptionDTO]
  } deriving (Show, Generic)

instance ToJSON BandOptionsDTO
instance FromJSON BandOptionsDTO

data BandChoiceDTO = BandChoiceDTO
  { bandId :: Text
  , name   :: Text
  } deriving (Show, Generic)

instance ToJSON BandChoiceDTO
instance FromJSON BandChoiceDTO

data SessionOptionsDTO = SessionOptionsDTO
  { bands :: [BandChoiceDTO]
  } deriving (Show, Generic)

instance ToJSON SessionOptionsDTO
instance FromJSON SessionOptionsDTO

data AssetDTO = AssetDTO
  { assetId  :: Text
  , name     :: Text
  , category :: Text
  , status   :: Text
  , condition :: Maybe Text
  , brand    :: Maybe Text
  , model    :: Maybe Text
  , location :: Maybe Text
  , qrToken  :: Maybe Text
  , photoUrl :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AssetDTO
instance FromJSON AssetDTO

data MarketplaceItemDTO = MarketplaceItemDTO
  { miListingId      :: Text
  , miAssetId        :: Text
  , miTitle          :: Text
  , miPurpose        :: Text
  , miCategory       :: Text
  , miBrand          :: Maybe Text
  , miModel          :: Maybe Text
  , miPhotoUrl       :: Maybe Text
  , miStatus         :: Maybe Text
  , miCondition      :: Maybe Text
  , miPriceUsdCents  :: Int
  , miPriceDisplay   :: Text
  , miMarkupPct      :: Int
  , miCurrency       :: Text
  } deriving (Show, Generic)

instance ToJSON MarketplaceItemDTO
instance FromJSON MarketplaceItemDTO

data MarketplaceCartItemDTO = MarketplaceCartItemDTO
  { mciListingId         :: Text
  , mciTitle             :: Text
  , mciCategory          :: Text
  , mciBrand             :: Maybe Text
  , mciModel             :: Maybe Text
  , mciQuantity          :: Int
  , mciUnitPriceUsdCents :: Int
  , mciSubtotalCents     :: Int
  , mciUnitPriceDisplay  :: Text
  , mciSubtotalDisplay   :: Text
  } deriving (Show, Generic)

instance ToJSON MarketplaceCartItemDTO
instance FromJSON MarketplaceCartItemDTO

data MarketplaceCartDTO = MarketplaceCartDTO
  { mcCartId          :: Text
  , mcItems           :: [MarketplaceCartItemDTO]
  , mcCurrency        :: Text
  , mcSubtotalCents   :: Int
  , mcSubtotalDisplay :: Text
  } deriving (Show, Generic)

instance ToJSON MarketplaceCartDTO
instance FromJSON MarketplaceCartDTO

data MarketplaceCartItemUpdate = MarketplaceCartItemUpdate
  { mciuListingId :: Text
  , mciuQuantity  :: Int
  } deriving (Show, Generic)

instance FromJSON MarketplaceCartItemUpdate
instance ToJSON MarketplaceCartItemUpdate

data MarketplaceCheckoutReq = MarketplaceCheckoutReq
  { mcrBuyerName  :: Text
  , mcrBuyerEmail :: Text
  , mcrBuyerPhone :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON MarketplaceCheckoutReq
instance ToJSON MarketplaceCheckoutReq

data MarketplaceOrderItemDTO = MarketplaceOrderItemDTO
  { moiListingId         :: Text
  , moiTitle             :: Text
  , moiQuantity          :: Int
  , moiUnitPriceUsdCents :: Int
  , moiSubtotalCents     :: Int
  , moiUnitPriceDisplay  :: Text
  , moiSubtotalDisplay   :: Text
  } deriving (Show, Generic)

instance ToJSON MarketplaceOrderItemDTO
instance FromJSON MarketplaceOrderItemDTO

data MarketplaceOrderDTO = MarketplaceOrderDTO
  { moOrderId       :: Text
  , moCartId        :: Maybe Text
  , moCurrency      :: Text
  , moTotalUsdCents :: Int
  , moTotalDisplay  :: Text
  , moStatus        :: Text
  , moStatusHistory :: [(Text, UTCTime)]
  , moBuyerName     :: Text
  , moBuyerEmail    :: Text
  , moBuyerPhone    :: Maybe Text
  , moPaymentProvider :: Maybe Text
  , moPaypalOrderId :: Maybe Text
  , moPaypalPayerEmail :: Maybe Text
  , moPaidAt        :: Maybe UTCTime
  , moCreatedAt     :: UTCTime
  , moUpdatedAt     :: UTCTime
  , moItems         :: [MarketplaceOrderItemDTO]
  } deriving (Show, Generic)

instance ToJSON MarketplaceOrderDTO
instance FromJSON MarketplaceOrderDTO

data MarketplaceOrderUpdate = MarketplaceOrderUpdate
  { mouStatus          :: Maybe Text
  , mouPaymentProvider :: Maybe (Maybe Text)
  , mouPaidAt          :: Maybe (Maybe UTCTime)
  } deriving (Show, Generic)

instance ToJSON MarketplaceOrderUpdate
instance FromJSON MarketplaceOrderUpdate

data DatafastCheckoutDTO = DatafastCheckoutDTO
  { dcOrderId     :: Text
  , dcCheckoutId  :: Text
  , dcWidgetUrl   :: Text
  , dcAmount      :: Text
  , dcCurrency    :: Text
  } deriving (Show, Generic)

instance ToJSON DatafastCheckoutDTO
instance FromJSON DatafastCheckoutDTO

data PaypalCreateDTO = PaypalCreateDTO
  { pcOrderId       :: Text
  , pcPaypalOrderId :: Text
  , pcApprovalUrl   :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON PaypalCreateDTO
instance FromJSON PaypalCreateDTO

data PaypalCaptureReq = PaypalCaptureReq
  { pcCaptureOrderId   :: Text
  , pcCapturePaypalId  :: Text
  } deriving (Show, Generic)

instance ToJSON PaypalCaptureReq
instance FromJSON PaypalCaptureReq

data LabelTrackDTO = LabelTrackDTO
  { ltId        :: Text
  , ltTitle     :: Text
  , ltNote      :: Maybe Text
  , ltStatus    :: Text
  , ltOwnerId   :: Maybe Int64
  , ltOwnerName :: Maybe Text
  , ltCreatedAt :: UTCTime
  , ltUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON LabelTrackDTO
instance FromJSON LabelTrackDTO

data LabelTrackCreate = LabelTrackCreate
  { ltcTitle :: Text
  , ltcNote  :: Maybe Text
  , ltcOwnerId :: Maybe Int64
  } deriving (Show, Generic)

instance ToJSON LabelTrackCreate
instance FromJSON LabelTrackCreate

data LabelTrackUpdate = LabelTrackUpdate
  { ltuTitle  :: Maybe Text
  , ltuNote   :: Maybe Text
  , ltuStatus :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON LabelTrackUpdate
instance FromJSON LabelTrackUpdate

data AssetCreate = AssetCreate
  { cName     :: Text
  , cCategory :: Text
  , cPhotoUrl :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AssetCreate
instance FromJSON AssetCreate

data AssetUpdate = AssetUpdate
  { uName       :: Maybe Text
  , uCategory   :: Maybe Text
  , uStatus     :: Maybe Text
  , uLocationId :: Maybe Text
  , uNotes      :: Maybe Text
  , uPhotoUrl   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AssetUpdate
instance ToJSON AssetUpdate

data AssetCheckoutDTO = AssetCheckoutDTO
  { checkoutId     :: Text
  , assetId        :: Text
  , targetKind     :: Text
  , targetSessionId:: Maybe Text
  , targetPartyRef :: Maybe Text
  , targetRoomId   :: Maybe Text
  , checkedOutBy   :: Text
  , checkedOutAt   :: UTCTime
  , dueAt          :: Maybe UTCTime
  , conditionOut   :: Maybe Text
  , conditionIn    :: Maybe Text
  , returnedAt     :: Maybe UTCTime
  , notes          :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON AssetCheckoutDTO
instance FromJSON AssetCheckoutDTO

data DriveUploadDTO = DriveUploadDTO
  { duFileId         :: Text
  , duWebViewLink    :: Maybe Text
  , duWebContentLink :: Maybe Text
  , duPublicUrl      :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON DriveUploadDTO
instance FromJSON DriveUploadDTO

data AssetCheckoutRequest = AssetCheckoutRequest
  { coTargetKind    :: Maybe Text
  , coTargetSession :: Maybe Text
  , coTargetParty   :: Maybe Text
  , coTargetRoom    :: Maybe Text
  , coDueAt         :: Maybe UTCTime
  , coConditionOut  :: Maybe Text
  , coNotes         :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AssetCheckoutRequest
instance ToJSON AssetCheckoutRequest

data AssetCheckinRequest = AssetCheckinRequest
  { ciConditionIn :: Maybe Text
  , ciNotes       :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AssetCheckinRequest
instance ToJSON AssetCheckinRequest

data AssetQrDTO = AssetQrDTO
  { qrToken :: Text
  , qrUrl   :: Text
  } deriving (Show, Generic)
instance ToJSON AssetQrDTO
instance FromJSON AssetQrDTO

data RoomDTO = RoomDTO
  { roomId    :: Text
  , rName     :: Text
  , rBookable :: Bool
  } deriving (Show, Generic)

instance ToJSON RoomDTO
instance FromJSON RoomDTO

data RoomCreate = RoomCreate
  { rcName :: Text
  } deriving (Show, Generic)

instance ToJSON RoomCreate
instance FromJSON RoomCreate

data RoomUpdate = RoomUpdate
  { ruName       :: Maybe Text
  , ruIsBookable :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON RoomUpdate
instance FromJSON RoomUpdate

data PipelineCardDTO = PipelineCardDTO
  { pcId        :: Text
  , pcTitle     :: Text
  , pcArtist    :: Maybe Text
  , pcType      :: Text
  , pcStage     :: Text
  , pcSortOrder :: Int
  , pcNotes     :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON PipelineCardDTO where
  toJSON dto = object
    [ "id"        .= pcId dto
    , "title"     .= pcTitle dto
    , "artist"    .= pcArtist dto
    , "type"      .= pcType dto
    , "stage"     .= pcStage dto
    , "sortOrder" .= pcSortOrder dto
    , "notes"     .= pcNotes dto
    ]

instance FromJSON PipelineCardDTO where
  parseJSON = withObject "PipelineCardDTO" $ \o -> do
    sortOrder <- o .:? "sortOrder"
    PipelineCardDTO
      <$> o .:  "id"
      <*> o .:  "title"
      <*> o .:? "artist"
      <*> o .:  "type"
      <*> o .:  "stage"
      <*> pure (fromMaybe 0 sortOrder)
      <*> o .:? "notes"

data PipelineCardCreate = PipelineCardCreate
  { pccTitle     :: Text
  , pccArtist    :: Maybe Text
  , pccStage     :: Maybe Text
  , pccSortOrder :: Maybe Int
  , pccNotes     :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON PipelineCardCreate where
  parseJSON = withObject "PipelineCardCreate" $ \o ->
    PipelineCardCreate
      <$> o .:  "title"
      <*> o .:? "artist"
      <*> o .:? "stage"
      <*> o .:? "sortOrder"
      <*> o .:? "notes"

data PipelineCardUpdate = PipelineCardUpdate
  { pcuTitle     :: Maybe Text
  , pcuArtist    :: Maybe (Maybe Text)
  , pcuStage     :: Maybe Text
  , pcuSortOrder :: Maybe Int
  , pcuNotes     :: Maybe (Maybe Text)
  } deriving (Show, Generic)

instance FromJSON PipelineCardUpdate where
  parseJSON = withObject "PipelineCardUpdate" $ \o ->
    PipelineCardUpdate
      <$> o .:? "title"
      <*> o .:? "artist"
      <*> o .:? "stage"
      <*> o .:? "sortOrder"
      <*> o .:? "notes"

data SessionInputRow = SessionInputRow
  { channelNumber    :: Int
  , trackName        :: Maybe Text
  , instrument       :: Maybe Text
  , micId            :: Maybe Text
  , standId          :: Maybe Text
  , cableId          :: Maybe Text
  , preampId         :: Maybe Text
  , insertOutboardId :: Maybe Text
  , converterChannel :: Maybe Text
  , phantom          :: Maybe Bool
  , polarity         :: Maybe Bool
  , hpf              :: Maybe Bool
  , pad              :: Maybe Bool
  , notes            :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionInputRow
instance FromJSON SessionInputRow

data SessionDTO = SessionDTO
  { sessionId            :: Text
  , sStartAt             :: UTCTime
  , sEndAt               :: UTCTime
  , sStatus              :: Text
  , sBookingRef          :: Maybe Text
  , sBandId              :: Maybe Text
  , sClientPartyRef      :: Maybe Text
  , sService             :: Text
  , sEngineerRef         :: Text
  , sAssistantRef        :: Maybe Text
  , sRoomIds             :: [Text]
  , sSampleRate          :: Maybe Int
  , sBitDepth            :: Maybe Int
  , sDaw                 :: Maybe Text
  , sSessionFolderDriveId:: Maybe Text
  , sNotes               :: Maybe Text
  , sInputListRows       :: [SessionInputRow]
  } deriving (Show, Generic)

instance ToJSON SessionDTO
instance FromJSON SessionDTO

data SessionCreate = SessionCreate
  { scBookingRef          :: Maybe Text
  , scBandId              :: Maybe Text
  , scClientPartyRef      :: Maybe Text
  , scService             :: Text
  , scStartAt             :: UTCTime
  , scEndAt               :: UTCTime
  , scEngineerRef         :: Text
  , scAssistantRef        :: Maybe Text
  , scRoomIds             :: [Text]
  , scSampleRate          :: Maybe Int
  , scBitDepth            :: Maybe Int
  , scDaw                 :: Maybe Text
  , scSessionFolderDriveId:: Maybe Text
  , scNotes               :: Maybe Text
  , scInputListRows       :: Maybe [SessionInputRow]
  , scStatus              :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionCreate
instance FromJSON SessionCreate

data SessionUpdate = SessionUpdate
  { suBookingRef          :: Maybe (Maybe Text)
  , suBandId              :: Maybe (Maybe Text)
  , suClientPartyRef      :: Maybe (Maybe Text)
  , suService             :: Maybe Text
  , suStartAt             :: Maybe UTCTime
  , suEndAt               :: Maybe UTCTime
  , suEngineerRef         :: Maybe Text
  , suAssistantRef        :: Maybe (Maybe Text)
  , suRoomIds             :: Maybe [Text]
  , suSampleRate          :: Maybe (Maybe Int)
  , suBitDepth            :: Maybe (Maybe Int)
  , suDaw                 :: Maybe (Maybe Text)
  , suSessionFolderDriveId:: Maybe (Maybe Text)
  , suNotes               :: Maybe (Maybe Text)
  , suInputListRows       :: Maybe [SessionInputRow]
  , suStatus              :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON SessionUpdate
instance FromJSON SessionUpdate

data PartyRelatedBooking = PartyRelatedBooking
  { prbBookingId  :: Int64
  , prbRole       :: Text
  , prbTitle      :: Text
  , prbServiceType :: Maybe Text
  , prbStartsAt   :: UTCTime
  , prbEndsAt     :: UTCTime
  , prbStatus     :: Text
  } deriving (Show, Generic)

instance ToJSON PartyRelatedBooking
instance FromJSON PartyRelatedBooking

data PartyRelatedClassSession = PartyRelatedClassSession
  { prcClassSessionId :: Int64
  , prcRole           :: Text
  , prcSubjectId      :: Int64
  , prcSubjectName    :: Maybe Text
  , prcTeacherId      :: Int64
  , prcTeacherName    :: Maybe Text
  , prcStudentId      :: Int64
  , prcStudentName    :: Maybe Text
  , prcStartAt        :: UTCTime
  , prcEndAt          :: UTCTime
  , prcStatus         :: Text
  , prcBookingId      :: Maybe Int64
  } deriving (Show, Generic)

instance ToJSON PartyRelatedClassSession
instance FromJSON PartyRelatedClassSession

data PartyRelatedLabelTrack = PartyRelatedLabelTrack
  { prtId        :: Text
  , prtTitle     :: Text
  , prtStatus    :: Text
  , prtCreatedAt :: UTCTime
  , prtUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON PartyRelatedLabelTrack
instance FromJSON PartyRelatedLabelTrack

data PartyRelatedDTO = PartyRelatedDTO
  { prPartyId       :: Int64
  , prBookings      :: [PartyRelatedBooking]
  , prClassSessions :: [PartyRelatedClassSession]
  , prLabelTracks   :: [PartyRelatedLabelTrack]
  } deriving (Show, Generic)

instance ToJSON PartyRelatedDTO
instance FromJSON PartyRelatedDTO

newtype RolePayload = RolePayload { rolePayloadValue :: Text }
  deriving (Show, Eq, Generic)

instance FromJSON RolePayload where
  parseJSON v =
    case v of
      String t -> pure (RolePayload t)
      Object o -> RolePayload <$> (o .: "role" <|> o .: "value")
      _        -> fail "Expected role string or object with 'role'"

instance MimeUnrender PlainText RolePayload where
  mimeUnrender _ = Right . RolePayload . TE.decodeUtf8 . BL.toStrict

instance MimeUnrender OctetStream RolePayload where
  mimeUnrender _ = Right . RolePayload . TE.decodeUtf8 . BL.toStrict

data LooseJSON

instance Accept LooseJSON where
  contentType _ = "application" // "json"

instance MimeUnrender LooseJSON RolePayload where
  mimeUnrender _ bs =
    case eitherDecode bs of
      Right rp -> Right rp
      Left _   -> Right (RolePayload (TE.decodeUtf8 (BL.toStrict bs)))

data BandMemberDTO = BandMemberDTO
  { bmId         :: Text
  , bmPartyId    :: Int64
  , bmPartyName  :: Text
  , bmRole       :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON BandMemberDTO
instance FromJSON BandMemberDTO

data BandDTO = BandDTO
  { bandId        :: Text
  , partyId       :: Int64
  , bName         :: Text
  , bLabelArtist  :: Bool
  , bPrimaryGenre :: Maybe Text
  , bHomeCity     :: Maybe Text
  , bPhotoUrl     :: Maybe Text
  , bContractFlags:: Maybe Text
  , bMembers      :: [BandMemberDTO]
  } deriving (Show, Generic)

instance ToJSON BandDTO
instance FromJSON BandDTO

data BandMemberInput = BandMemberInput
  { bmiPartyId :: Int64
  , bmiRole    :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON BandMemberInput where
  toJSON BandMemberInput{bmiPartyId, bmiRole} = object
    [ "bmPartyId" .= bmiPartyId
    , "bmRole"     .= bmiRole
    ]

instance FromJSON BandMemberInput where
  parseJSON = withObject "BandMemberInput" $ \o ->
    BandMemberInput
      <$> o .:  "bmPartyId"
      <*> o .:? "bmRole"

-- Minimal Payment DTO for UI/backend bridging
data SimplePaymentDTO = SimplePaymentDTO
  { spId          :: Int64
  , spPartyId     :: Int64
  , spOrderId     :: Maybe Int64
  , spInvoiceId   :: Maybe Int64
  , spAmountCents :: Int
  , spCurrency    :: Text
  , spMethod      :: Text
  , spReference   :: Maybe Text
  , spPaidAt      :: Text
  , spConcept     :: Maybe Text
  , spPeriod      :: Maybe Text
  , spAttachment  :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON SimplePaymentDTO

data BandCreate = BandCreate
  { bcName          :: Text
  , bcLabelArtist   :: Maybe Bool
  , bcPrimaryGenre  :: Maybe Text
  , bcHomeCity      :: Maybe Text
  , bcPhotoUrl      :: Maybe Text
  , bcContractFlags :: Maybe Text
  , bcMembers       :: [BandMemberInput]
  } deriving (Show, Generic)

instance ToJSON BandCreate
instance FromJSON BandCreate

data RadioStreamDTO = RadioStreamDTO
  { rsId            :: Int64
  , rsName          :: Maybe Text
  , rsStreamUrl     :: Text
  , rsCountry       :: Maybe Text
  , rsGenre         :: Maybe Text
  , rsActive        :: Bool
  , rsLastCheckedAt :: Maybe UTCTime
  } deriving (Show, Generic)
instance ToJSON RadioStreamDTO
instance FromJSON RadioStreamDTO

data RadioStreamUpsert = RadioStreamUpsert
  { rsuStreamUrl :: Text
  , rsuName      :: Maybe Text
  , rsuCountry   :: Maybe Text
  , rsuGenre     :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON RadioStreamUpsert
instance FromJSON RadioStreamUpsert

data RadioImportRequest = RadioImportRequest
  { rirSources :: Maybe [Text]
  , rirLimit   :: Maybe Int
  } deriving (Show, Generic)
instance ToJSON RadioImportRequest
instance FromJSON RadioImportRequest

data RadioImportResult = RadioImportResult
  { rirProcessed :: Int
  , rirInserted  :: Int
  , rirUpdated   :: Int
  , rirSources   :: [Text]
  , rirFailed    :: Int
  , rirFailedSources :: [Text]
  } deriving (Show, Generic)
instance ToJSON RadioImportResult
instance FromJSON RadioImportResult

data RadioMetadataRefreshRequest = RadioMetadataRefreshRequest
  { rmrLimit       :: Maybe Int
  , rmrOnlyMissing :: Maybe Bool
  } deriving (Show, Generic)
instance ToJSON RadioMetadataRefreshRequest
instance FromJSON RadioMetadataRefreshRequest

data RadioMetadataRefreshResult = RadioMetadataRefreshResult
  { rmrProcessed :: Int
  , rmrUpdated   :: Int
  , rmrFailed    :: Int
  } deriving (Show, Generic)
instance ToJSON RadioMetadataRefreshResult
instance FromJSON RadioMetadataRefreshResult

data RadioTransmissionRequest = RadioTransmissionRequest
  { rtrName    :: Maybe Text
  , rtrGenre   :: Maybe Text
  , rtrCountry :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON RadioTransmissionRequest
instance FromJSON RadioTransmissionRequest

data RadioTransmissionInfo = RadioTransmissionInfo
  { rtiStreamId  :: Int64
  , rtiStreamUrl :: Text
  , rtiIngestUrl :: Text
  , rtiStreamKey :: Text
  , rtiWhipUrl   :: Text
  } deriving (Show, Generic)
instance ToJSON RadioTransmissionInfo
instance FromJSON RadioTransmissionInfo

data RadioPresenceDTO = RadioPresenceDTO
  { rpPartyId     :: Int64
  , rpStreamUrl   :: Text
  , rpStationName :: Maybe Text
  , rpStationId   :: Maybe Text
  , rpUpdatedAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON RadioPresenceDTO
instance FromJSON RadioPresenceDTO

data RadioPresenceUpsert = RadioPresenceUpsert
  { rpuStreamUrl   :: Text
  , rpuStationName :: Maybe Text
  , rpuStationId   :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON RadioPresenceUpsert
instance FromJSON RadioPresenceUpsert

data InternProfileDTO = InternProfileDTO
  { ipPartyId  :: Int64
  , ipStartAt  :: Maybe Day
  , ipEndAt    :: Maybe Day
  , ipRequiredHours :: Maybe Int
  , ipSkills   :: Maybe Text
  , ipAreas    :: Maybe Text
  , ipCreatedAt :: UTCTime
  , ipUpdatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternProfileDTO
instance FromJSON InternProfileDTO

data InternProfileUpdate = InternProfileUpdate
  { ipuStartAt :: Maybe (Maybe Day)
  , ipuEndAt   :: Maybe (Maybe Day)
  , ipuRequiredHours :: Maybe (Maybe Int)
  , ipuSkills  :: Maybe (Maybe Text)
  , ipuAreas   :: Maybe (Maybe Text)
  } deriving (Show, Generic)
instance ToJSON InternProfileUpdate
instance FromJSON InternProfileUpdate

data InternSummaryDTO = InternSummaryDTO
  { isPartyId :: Int64
  , isName    :: Text
  , isEmail   :: Maybe Text
  , isRoles   :: [RoleEnum]
  } deriving (Show, Generic)
instance ToJSON InternSummaryDTO
instance FromJSON InternSummaryDTO

data InternProjectDTO = InternProjectDTO
  { ipId        :: Text
  , ipTitle     :: Text
  , ipDescription :: Maybe Text
  , ipStatus    :: Text
  , ipStartAt   :: Maybe Day
  , ipDueAt     :: Maybe Day
  , ipCreatedAt :: UTCTime
  , ipUpdatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternProjectDTO
instance FromJSON InternProjectDTO

data InternProjectCreate = InternProjectCreate
  { ipcTitle       :: Text
  , ipcDescription :: Maybe Text
  , ipcStatus      :: Maybe Text
  , ipcStartAt     :: Maybe Day
  , ipcDueAt       :: Maybe Day
  } deriving (Show, Generic)
instance ToJSON InternProjectCreate
instance FromJSON InternProjectCreate

data InternProjectUpdate = InternProjectUpdate
  { ipuTitle       :: Maybe Text
  , ipuDescription :: Maybe (Maybe Text)
  , ipuStatus      :: Maybe Text
  , ipuStartAt     :: Maybe (Maybe Day)
  , ipuDueAt       :: Maybe (Maybe Day)
  } deriving (Show, Generic)
instance ToJSON InternProjectUpdate
instance FromJSON InternProjectUpdate

data InternTaskDTO = InternTaskDTO
  { itId          :: Text
  , itProjectId   :: Text
  , itProjectName :: Text
  , itTitle       :: Text
  , itDescription :: Maybe Text
  , itStatus      :: Text
  , itProgress    :: Int
  , itAssignedTo  :: Maybe Int64
  , itAssignedName :: Maybe Text
  , itDueAt       :: Maybe Day
  , itCreatedAt   :: UTCTime
  , itUpdatedAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternTaskDTO
instance FromJSON InternTaskDTO

data InternTaskCreate = InternTaskCreate
  { itcProjectId  :: Text
  , itcTitle      :: Text
  , itcDescription :: Maybe Text
  , itcAssignedTo :: Maybe Int64
  , itcDueAt      :: Maybe Day
  } deriving (Show, Generic)
instance ToJSON InternTaskCreate
instance FromJSON InternTaskCreate

data InternTaskUpdate = InternTaskUpdate
  { ituTitle       :: Maybe Text
  , ituDescription :: Maybe (Maybe Text)
  , ituStatus      :: Maybe Text
  , ituProgress    :: Maybe Int
  , ituAssignedTo  :: Maybe (Maybe Int64)
  , ituDueAt       :: Maybe (Maybe Day)
  } deriving (Show, Generic)
instance ToJSON InternTaskUpdate
instance FromJSON InternTaskUpdate

data InternTodoDTO = InternTodoDTO
  { itdId        :: Text
  , itdText      :: Text
  , itdDone      :: Bool
  , itdCreatedAt :: UTCTime
  , itdUpdatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternTodoDTO
instance FromJSON InternTodoDTO

data InternTodoCreate = InternTodoCreate
  { itdcText :: Text
  } deriving (Show, Generic)
instance ToJSON InternTodoCreate
instance FromJSON InternTodoCreate

data InternTodoUpdate = InternTodoUpdate
  { itduText :: Maybe Text
  , itduDone :: Maybe Bool
  } deriving (Show, Generic)
instance ToJSON InternTodoUpdate
instance FromJSON InternTodoUpdate

data ClockInRequest = ClockInRequest
  { cirNotes :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClockInRequest
instance FromJSON ClockInRequest

data ClockOutRequest = ClockOutRequest
  { corNotes :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClockOutRequest
instance FromJSON ClockOutRequest

data InternTimeEntryDTO = InternTimeEntryDTO
  { iteId       :: Text
  , itePartyId  :: Int64
  , itePartyName :: Text
  , iteClockIn  :: UTCTime
  , iteClockOut :: Maybe UTCTime
  , iteDurationMinutes :: Maybe Int
  , iteNotes    :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON InternTimeEntryDTO
instance FromJSON InternTimeEntryDTO

data InternPermissionDTO = InternPermissionDTO
  { iprId          :: Text
  , iprPartyId     :: Int64
  , iprPartyName   :: Text
  , iprCategory    :: Text
  , iprReason      :: Maybe Text
  , iprStartAt     :: Day
  , iprEndAt       :: Maybe Day
  , iprStatus      :: Text
  , iprReviewedBy  :: Maybe Int64
  , iprReviewedByName :: Maybe Text
  , iprReviewedAt  :: Maybe UTCTime
  , iprDecisionNotes :: Maybe Text
  , iprCreatedAt   :: UTCTime
  , iprUpdatedAt   :: UTCTime
  } deriving (Show, Generic)
instance ToJSON InternPermissionDTO
instance FromJSON InternPermissionDTO

data InternPermissionCreate = InternPermissionCreate
  { ipcCategory :: Text
  , ipcReason   :: Maybe Text
  , ipcStartAt  :: Day
  , ipcEndAt    :: Maybe Day
  } deriving (Show, Generic)
instance ToJSON InternPermissionCreate
instance FromJSON InternPermissionCreate

data InternPermissionUpdate = InternPermissionUpdate
  { ipuStatus        :: Maybe Text
  , ipuDecisionNotes :: Maybe (Maybe Text)
  } deriving (Show, Generic)
instance ToJSON InternPermissionUpdate
instance FromJSON InternPermissionUpdate
