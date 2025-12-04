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
import           Data.Time    (UTCTime)
import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)
import           Network.HTTP.Media ((//))
import           Servant.API  (Accept(..), MimeUnrender(..), OctetStream, PlainText)

import           TDF.Models   (RoleEnum)

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
  } deriving (Show, Generic)

instance ToJSON AssetDTO
instance FromJSON AssetDTO

data MarketplaceItemDTO = MarketplaceItemDTO
  { miListingId      :: Text
  , miAssetId        :: Text
  , miTitle          :: Text
  , miCategory       :: Text
  , miBrand          :: Maybe Text
  , miModel          :: Maybe Text
  , miPriceUsdCents  :: Int
  , miPriceDisplay   :: Text
  , miMarkupPct      :: Int
  , miCurrency       :: Text
  } deriving (Show, Generic)

instance ToJSON MarketplaceItemDTO
instance FromJSON MarketplaceItemDTO

data AssetCreate = AssetCreate
  { cName     :: Text
  , cCategory :: Text
  } deriving (Show, Generic)

instance ToJSON AssetCreate
instance FromJSON AssetCreate

data AssetUpdate = AssetUpdate
  { uName       :: Maybe Text
  , uCategory   :: Maybe Text
  , uStatus     :: Maybe Text
  , uLocationId :: Maybe Text
  , uNotes      :: Maybe Text
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
