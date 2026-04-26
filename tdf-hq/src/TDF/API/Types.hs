{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TDF.API.Types where

import           Data.Char    (isAsciiLower, isControl, isDigit, isSpace, toLower)
import           Data.Aeson   (FromJSON(..), Options, ToJSON(..), Value(..), defaultOptions, eitherDecode, fieldLabelModifier, genericParseJSON, object, rejectUnknownFields, withObject, (.:), (.:!), (.:?), (.=))
import           Data.Aeson.Types (Parser)
import           Data.Int     (Int64)
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import           Data.Time    (UTCTime, Day)
import           Data.Maybe   (fromMaybe)
import           GHC.Generics (Generic)
import           Network.HTTP.Media ((//))
import           Servant.API  (Accept(..), MimeUnrender(..), OctetStream, PlainText)

import           TDF.Models   (PricingModel, RoleEnum, ServiceKind)

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

prefixedStrictObjectOptions :: Int -> Options
prefixedStrictObjectOptions prefixLen =
  defaultOptions
    { fieldLabelModifier = camelDrop prefixLen
    , rejectUnknownFields = True
    }

camelDrop :: Int -> String -> String
camelDrop prefixLen fieldName =
  case drop prefixLen fieldName of
    (firstChar:rest) -> toLower firstChar : rest
    []               -> []

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
instance FromJSON DropdownOptionCreate where
  parseJSON = genericParseJSON strictObjectOptions

data DropdownOptionUpdate = DropdownOptionUpdate
  { douValue     :: Maybe Text
  , douLabel     :: Maybe (Maybe Text)
  , douSortOrder :: Maybe (Maybe Int)
  , douActive    :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON DropdownOptionUpdate
instance FromJSON DropdownOptionUpdate where
  parseJSON = withObject "DropdownOptionUpdate" $ \o -> do
    let allowedKeys =
          [ "douValue"
          , "douLabel"
          , "douSortOrder"
          , "douActive"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in DropdownOptionUpdate: " <> T.unpack key)
      [] ->
        DropdownOptionUpdate
          <$> o .:? "douValue"
          <*> o .:! "douLabel"
          <*> o .:! "douSortOrder"
          <*> o .:? "douActive"

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
  , primaryEmail :: Maybe Text
  , primaryPhone :: Maybe Text
  , whatsapp :: Maybe Text
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
instance FromJSON UserAccountCreate where
  parseJSON = genericParseJSON strictObjectOptions

data UserAccountUpdate = UserAccountUpdate
  { uauUsername :: Maybe Text
  , uauPassword :: Maybe Text
  , uauActive   :: Maybe Bool
  , uauRoles    :: Maybe [RoleEnum]
  } deriving (Show, Generic)

instance ToJSON UserAccountUpdate
instance FromJSON UserAccountUpdate where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON UserRoleUpdatePayload where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON ServiceCatalogCreate where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON ServiceCatalogUpdate where
  parseJSON = withObject "ServiceCatalogUpdate" $ \o -> do
    let allowedKeys =
          [ "scuName"
          , "scuKind"
          , "scuPricingModel"
          , "scuRateCents"
          , "scuCurrency"
          , "scuBillingUnit"
          , "scuTaxBps"
          , "scuActive"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in ServiceCatalogUpdate: " <> T.unpack key)
      [] ->
        ServiceCatalogUpdate
          <$> o .:? "scuName"
          <*> o .:? "scuKind"
          <*> o .:? "scuPricingModel"
          <*> o .:! "scuRateCents"
          <*> o .:? "scuCurrency"
          <*> o .:! "scuBillingUnit"
          <*> o .:! "scuTaxBps"
          <*> o .:? "scuActive"

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
  , currentCheckoutKind :: Maybe Text
  , currentCheckoutTarget :: Maybe Text
  , currentCheckoutDisposition :: Maybe Text
  , currentCheckoutHolderEmail :: Maybe Text
  , currentCheckoutHolderPhone :: Maybe Text
  , currentCheckoutAt :: Maybe UTCTime
  , currentCheckoutDueAt :: Maybe UTCTime
  , currentCheckoutPaymentType :: Maybe Text
  , currentCheckoutPaymentInstallments :: Maybe Int
  , currentCheckoutPaymentAmountCents :: Maybe Int
  , currentCheckoutPaymentCurrency :: Maybe Text
  , currentCheckoutPaymentOutstandingCents :: Maybe Int
  , currentCheckoutPhotoUrl :: Maybe Text
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

maxMarketplaceCartItemQuantity :: Int
maxMarketplaceCartItemQuantity = 99

instance FromJSON MarketplaceCartItemUpdate where
  parseJSON value = do
    payload <- genericParseJSON strictObjectOptions value
    listingId <-
      either fail pure $
        normalizeMarketplaceCartListingId (mciuListingId payload)
    let quantity = mciuQuantity payload
    if quantity < 0
      then fail "mciuQuantity must be non-negative"
      else
        if quantity > maxMarketplaceCartItemQuantity
          then
            fail $
              "mciuQuantity must be "
                <> show maxMarketplaceCartItemQuantity
                <> " or fewer"
          else pure payload { mciuListingId = listingId }
instance ToJSON MarketplaceCartItemUpdate

normalizeMarketplaceCartListingId :: Text -> Either String Text
normalizeMarketplaceCartListingId rawListingId =
  let listingId = T.strip rawListingId
  in if isPositiveDecimalId listingId
       then Right listingId
       else Left "mciuListingId must be a positive decimal id"
  where
    isPositiveDecimalId candidate =
      not (T.null candidate)
        && T.all isDigit candidate
        && case reads (T.unpack candidate) :: [(Integer, String)] of
             [(n, "")] -> n > 0 && n <= fromIntegral (maxBound :: Int64)
             _         -> False

data MarketplaceCheckoutReq = MarketplaceCheckoutReq
  { mcrBuyerName  :: Text
  , mcrBuyerEmail :: Text
  , mcrBuyerPhone :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON MarketplaceCheckoutReq where
  parseJSON value = do
    payload <- genericParseJSON strictObjectOptions value
    buyerName <- normalizeMarketplaceBuyerNameField (mcrBuyerName payload)
    buyerEmail <- normalizeMarketplaceBuyerEmailField (mcrBuyerEmail payload)
    buyerPhone <- normalizeMarketplaceOptionalPhoneField (mcrBuyerPhone payload)
    pure payload
      { mcrBuyerName = buyerName
      , mcrBuyerEmail = buyerEmail
      , mcrBuyerPhone = buyerPhone
      }
instance ToJSON MarketplaceCheckoutReq

normalizeMarketplaceBuyerNameField :: Text -> Parser Text
normalizeMarketplaceBuyerNameField rawName
  | T.null trimmed =
      fail "mcrBuyerName is required"
  | T.length trimmed > 160 =
      fail "mcrBuyerName must be 160 characters or fewer"
  | T.any isControl trimmed =
      fail "mcrBuyerName must not contain control characters"
  | otherwise =
      pure trimmed
  where
    trimmed = T.strip rawName

normalizeMarketplaceBuyerEmailField :: Text -> Parser Text
normalizeMarketplaceBuyerEmailField rawEmail
  | T.null normalized =
      fail "mcrBuyerEmail is required"
  | isValidMarketplaceBuyerEmail normalized =
      pure normalized
  | otherwise =
      fail "mcrBuyerEmail must be a valid email address"
  where
    normalized = T.toLower (T.strip rawEmail)

normalizeMarketplaceOptionalField :: Maybe Text -> Maybe Text
normalizeMarketplaceOptionalField Nothing = Nothing
normalizeMarketplaceOptionalField (Just rawValue) =
  let trimmed = T.strip rawValue
  in if T.null trimmed then Nothing else Just trimmed

normalizeMarketplaceOptionalPhoneField :: Maybe Text -> Parser (Maybe Text)
normalizeMarketplaceOptionalPhoneField rawPhone =
  case normalizeMarketplaceOptionalField rawPhone of
    Nothing -> pure Nothing
    Just phone ->
      case normalizeMarketplacePhone phone of
        Just phoneVal -> pure (Just phoneVal)
        Nothing -> fail "mcrBuyerPhone must be a valid phone number"

normalizeMarketplacePhone :: Text -> Maybe Text
normalizeMarketplacePhone raw =
  let trimmed = T.strip raw
      onlyDigits = T.filter isDigit trimmed
      digitCount = T.length onlyDigits
      plusCount = T.count "+" trimmed
      plusIndex = T.findIndex (== '+') trimmed
      firstDigitIndex = T.findIndex isDigit trimmed
      allowedPhoneChar ch =
        isDigit ch || isSpace ch || ch `elem` ("+-()." :: String)
      hasInvalidChars = T.any (not . allowedPhoneChar) trimmed
      plusIsValid =
        case plusIndex of
          Nothing -> True
          Just idx ->
            case firstDigitIndex of
              Nothing -> False
              Just digitIdx -> plusCount == 1 && idx < digitIdx
  in
    if T.null onlyDigits
        || digitCount < 8
        || digitCount > 15
        || hasInvalidChars
        || not plusIsValid
      then Nothing
      else Just ("+" <> onlyDigits)

isValidMarketplaceBuyerEmail :: Text -> Bool
isValidMarketplaceBuyerEmail candidate =
  case T.splitOn "@" candidate of
    [localPart, domain] ->
      T.length candidate <= 254
        && isValidMarketplaceEmailLocalPart localPart
        && not (T.null domain)
        && not (T.any isSpace candidate)
        && T.isInfixOf "." domain
        && all isValidMarketplaceEmailDomainLabel (T.splitOn "." domain)
    _ -> False

isValidMarketplaceEmailLocalPart :: Text -> Bool
isValidMarketplaceEmailLocalPart localPart =
  not (T.null localPart)
    && T.length localPart <= 64
    && not (T.isPrefixOf "." localPart)
    && not (T.isSuffixOf "." localPart)
    && not (T.isInfixOf ".." localPart)
    && T.all isValidMarketplaceEmailLocalChar localPart

isValidMarketplaceEmailLocalChar :: Char -> Bool
isValidMarketplaceEmailLocalChar c =
  isAsciiLower c || isDigit c || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

isValidMarketplaceEmailDomainLabel :: Text -> Bool
isValidMarketplaceEmailDomainLabel label =
  not (T.null label)
    && T.length label <= 63
    && not (T.isPrefixOf "-" label)
    && not (T.isSuffixOf "-" label)
    && T.all isValidMarketplaceEmailDomainChar label

isValidMarketplaceEmailDomainChar :: Char -> Bool
isValidMarketplaceEmailDomainChar c = isAsciiLower c || isDigit c || c == '-'

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
instance FromJSON MarketplaceOrderUpdate where
  parseJSON value@(Object o) = do
    MarketplaceOrderUpdateParsed
      { mouStatus = statusVal
      } <- genericParseJSON strictObjectOptions value
    paymentProviderVal <- o .:! "mouPaymentProvider"
    paidAtVal <- o .:! "mouPaidAt"
    pure MarketplaceOrderUpdate
      { mouStatus = statusVal
      , mouPaymentProvider = paymentProviderVal
      , mouPaidAt = paidAtVal
      }
  parseJSON _ = fail "MarketplaceOrderUpdate must be an object"

data MarketplaceOrderUpdateParsed = MarketplaceOrderUpdateParsed
  { mouStatus          :: Maybe Text
  , mouPaymentProvider :: Maybe Text
  , mouPaidAt          :: Maybe UTCTime
  } deriving (Show, Generic)

instance FromJSON MarketplaceOrderUpdateParsed where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON PaypalCaptureReq where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON LabelTrackCreate where
  parseJSON = genericParseJSON strictObjectOptions

data LabelTrackUpdate = LabelTrackUpdate
  { ltuTitle  :: Maybe Text
  , ltuNote   :: Maybe Text
  , ltuStatus :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON LabelTrackUpdate
instance FromJSON LabelTrackUpdate where
  parseJSON = genericParseJSON strictObjectOptions

data AssetCreate = AssetCreate
  { cName     :: Text
  , cCategory :: Text
  , cPhotoUrl :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON AssetCreate
instance FromJSON AssetCreate where
  parseJSON = genericParseJSON strictObjectOptions

data AssetUpdate = AssetUpdate
  { uName       :: Maybe Text
  , uCategory   :: Maybe Text
  , uStatus     :: Maybe Text
  , uLocationId :: Maybe Text
  , uNotes      :: Maybe Text
  , uPhotoUrl   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON AssetUpdate where
  parseJSON = genericParseJSON strictObjectOptions
instance ToJSON AssetUpdate

data AssetCheckoutDTO = AssetCheckoutDTO
  { checkoutId     :: Text
  , assetId        :: Text
  , targetKind     :: Text
  , targetSessionId:: Maybe Text
  , targetPartyRef :: Maybe Text
  , targetRoomId   :: Maybe Text
  , disposition    :: Text
  , termsAndConditions :: Maybe Text
  , holderEmail    :: Maybe Text
  , holderPhone    :: Maybe Text
  , paymentType    :: Maybe Text
  , paymentInstallments :: Maybe Int
  , paymentReference :: Maybe Text
  , paymentAmountCents :: Maybe Int
  , paymentCurrency :: Maybe Text
  , paymentOutstandingCents :: Maybe Int
  , checkedOutBy   :: Text
  , checkedOutAt   :: UTCTime
  , dueAt          :: Maybe UTCTime
  , conditionOut   :: Maybe Text
  , photoOutUrl    :: Maybe Text
  , conditionIn    :: Maybe Text
  , photoInUrl     :: Maybe Text
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

data AssetUploadDTO = AssetUploadDTO
  { auFileName  :: Text
  , auPath      :: Text
  , auPublicUrl :: Text
  } deriving (Show, Generic)
instance ToJSON AssetUploadDTO
instance FromJSON AssetUploadDTO

data DriveTokenExchangeRequest = DriveTokenExchangeRequest
  { code         :: Text
  , codeVerifier :: Text
  , redirectUri  :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON DriveTokenExchangeRequest
instance FromJSON DriveTokenExchangeRequest where
  parseJSON = genericParseJSON strictObjectOptions

data DriveTokenRefreshRequest = DriveTokenRefreshRequest
  { refreshToken :: Text
  } deriving (Show, Generic)
instance ToJSON DriveTokenRefreshRequest
instance FromJSON DriveTokenRefreshRequest where
  parseJSON = genericParseJSON strictObjectOptions

data DriveTokenResponse = DriveTokenResponse
  { accessToken  :: Text
  , refreshToken :: Maybe Text
  , expiresIn    :: Int
  , tokenType    :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON DriveTokenResponse
instance FromJSON DriveTokenResponse

data AssetCheckoutRequest = AssetCheckoutRequest
  { coTargetKind    :: Maybe Text
  , coTargetSession :: Maybe Text
  , coTargetParty   :: Maybe Text
  , coTargetRoom    :: Maybe Text
  , coDisposition   :: Maybe Text
  , coTermsAndConditions :: Maybe Text
  , coHolderEmail   :: Maybe Text
  , coHolderPhone   :: Maybe Text
  , coPaymentType   :: Maybe Text
  , coPaymentInstallments :: Maybe Int
  , coPaymentReference :: Maybe Text
  , coPaymentAmount :: Maybe Text
  , coPaymentCurrency :: Maybe Text
  , coPaymentOutstanding :: Maybe Text
  , coPhotoUrl      :: Maybe Text
  , coDueAt         :: Maybe UTCTime
  , coConditionOut  :: Maybe Text
  , coNotes         :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AssetCheckoutRequest where
  parseJSON = genericParseJSON strictObjectOptions
instance ToJSON AssetCheckoutRequest

data AssetCheckinRequest = AssetCheckinRequest
  { ciConditionIn :: Maybe Text
  , ciNotes       :: Maybe Text
  , ciPhotoUrl    :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON AssetCheckinRequest where
  parseJSON = genericParseJSON strictObjectOptions
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
instance FromJSON RoomCreate where
  parseJSON = genericParseJSON strictObjectOptions

data RoomUpdate = RoomUpdate
  { ruName       :: Maybe Text
  , ruIsBookable :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON RoomUpdate
instance FromJSON RoomUpdate where
  parseJSON = genericParseJSON strictObjectOptions

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
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 3)

data PipelineCardUpdate = PipelineCardUpdate
  { pcuTitle     :: Maybe Text
  , pcuArtist    :: Maybe (Maybe Text)
  , pcuStage     :: Maybe Text
  , pcuSortOrder :: Maybe Int
  , pcuNotes     :: Maybe (Maybe Text)
  } deriving (Show, Generic)

instance FromJSON PipelineCardUpdate where
  parseJSON value@(Object o) = do
    PipelineCardUpdateParsed
      { pcupTitle = titleValue
      , pcupStage = stageValue
      , pcupSortOrder = sortOrderValue
      } <- genericParseJSON (prefixedStrictObjectOptions 4) value
    artistValue <- o .:! "artist"
    notesValue <- o .:! "notes"
    pure PipelineCardUpdate
      { pcuTitle = titleValue
      , pcuArtist = artistValue
      , pcuStage = stageValue
      , pcuSortOrder = sortOrderValue
      , pcuNotes = notesValue
      }
  parseJSON _ = fail "PipelineCardUpdate must be an object"

data PipelineCardUpdateParsed = PipelineCardUpdateParsed
  { pcupTitle     :: Maybe Text
  , pcupArtist    :: Maybe Text
  , pcupStage     :: Maybe Text
  , pcupSortOrder :: Maybe Int
  , pcupNotes     :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON PipelineCardUpdateParsed where
  parseJSON = genericParseJSON (prefixedStrictObjectOptions 4)

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
instance FromJSON SessionInputRow where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON SessionCreate where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON SessionUpdate where
  parseJSON = withObject "SessionUpdate" $ \o -> do
    let allowedKeys =
          [ "suBookingRef"
          , "suBandId"
          , "suClientPartyRef"
          , "suService"
          , "suStartAt"
          , "suEndAt"
          , "suEngineerRef"
          , "suAssistantRef"
          , "suRoomIds"
          , "suSampleRate"
          , "suBitDepth"
          , "suDaw"
          , "suSessionFolderDriveId"
          , "suNotes"
          , "suInputListRows"
          , "suStatus"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in SessionUpdate: " <> T.unpack key)
      [] ->
        SessionUpdate
          <$> o .:! "suBookingRef"
          <*> o .:! "suBandId"
          <*> o .:! "suClientPartyRef"
          <*> o .:? "suService"
          <*> o .:? "suStartAt"
          <*> o .:? "suEndAt"
          <*> o .:? "suEngineerRef"
          <*> o .:! "suAssistantRef"
          <*> o .:? "suRoomIds"
          <*> o .:! "suSampleRate"
          <*> o .:! "suBitDepth"
          <*> o .:! "suDaw"
          <*> o .:! "suSessionFolderDriveId"
          <*> o .:! "suNotes"
          <*> o .:? "suInputListRows"
          <*> o .:? "suStatus"

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
      Object o -> do
        let allowedKeys = ["role", "value"]
            unknownKeys =
              filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
        case unknownKeys of
          key:_ -> fail ("Unknown field in RolePayload: " <> T.unpack key)
          [] -> pure ()
        mRole <- o .:? "role"
        mValue <- o .:? "value"
        case (mRole, mValue) of
          (Just role, Nothing) -> pure (RolePayload role)
          (Nothing, Just value) -> pure (RolePayload value)
          (Nothing, Nothing) -> fail "Expected role object with either 'role' or 'value'"
          (Just _, Just _) -> fail "Expected role object with exactly one of 'role' or 'value'"
      _        -> fail "Expected role string or object with exactly one of 'role' or 'value'"

instance MimeUnrender PlainText RolePayload where
  mimeUnrender _ = decodeUtf8RolePayload

instance MimeUnrender OctetStream RolePayload where
  mimeUnrender _ = decodeUtf8RolePayload

data LooseJSON

instance Accept LooseJSON where
  contentType _ = "application" // "json"

instance MimeUnrender LooseJSON RolePayload where
  mimeUnrender _ bs =
    case eitherDecode bs of
      Right rp -> Right rp
      Left decodeErr ->
        case decodeUtf8RolePayloadText bs of
          Left utf8Err -> Left utf8Err
          Right rawText ->
            let trimmed = T.strip rawText
            in if T.null trimmed
                 then Left "Expected non-empty role payload"
                 else if looksLikeStructuredJson trimmed
                   then Left decodeErr
                   else Right (RolePayload rawText)

decodeUtf8RolePayload :: BL.ByteString -> Either String RolePayload
decodeUtf8RolePayload = fmap RolePayload . decodeUtf8RolePayloadText

decodeUtf8RolePayloadText :: BL.ByteString -> Either String Text
decodeUtf8RolePayloadText raw =
  case TE.decodeUtf8' (BL.toStrict raw) of
    Left _ -> Left "Role payload must be valid UTF-8"
    Right txt -> Right txt

looksLikeStructuredJson :: Text -> Bool
looksLikeStructuredJson raw =
  case T.uncons raw of
    Nothing -> False
    Just (firstChar, _) ->
      firstChar `elem` ['{', '[', '"'] ||
      firstChar == '-' ||
      isDigit firstChar ||
      T.toLower raw `elem` ["true", "false", "null"]

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
  parseJSON = withObject "BandMemberInput" $ \o -> do
    let allowedKeys =
          [ "bmPartyId"
          , "bmRole"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in BandMemberInput: " <> T.unpack key)
      [] ->
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
instance FromJSON BandCreate where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON RadioStreamUpsert where
  parseJSON = genericParseJSON strictObjectOptions

data RadioImportRequest = RadioImportRequest
  { rirSources :: Maybe [Text]
  , rirLimit   :: Maybe Int
  } deriving (Show, Generic)
instance ToJSON RadioImportRequest
instance FromJSON RadioImportRequest where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON RadioMetadataRefreshRequest where
  parseJSON = genericParseJSON strictObjectOptions

data RadioMetadataRefreshResult = RadioMetadataRefreshResult
  { rmrProcessed :: Int
  , rmrUpdated   :: Int
  , rmrFailed    :: Int
  } deriving (Show, Generic)
instance ToJSON RadioMetadataRefreshResult
instance FromJSON RadioMetadataRefreshResult

data RadioNowPlayingRequest = RadioNowPlayingRequest
  { rnpStreamUrl :: Text
  } deriving (Show, Generic)
instance ToJSON RadioNowPlayingRequest
instance FromJSON RadioNowPlayingRequest where
  parseJSON = genericParseJSON strictObjectOptions

data RadioNowPlayingResult = RadioNowPlayingResult
  { rnpTitle  :: Maybe Text
  , rnpArtist :: Maybe Text
  , rnpTrack  :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON RadioNowPlayingResult
instance FromJSON RadioNowPlayingResult

data RadioTransmissionRequest = RadioTransmissionRequest
  { rtrName    :: Maybe Text
  , rtrGenre   :: Maybe Text
  , rtrCountry :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON RadioTransmissionRequest
instance FromJSON RadioTransmissionRequest where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON RadioPresenceUpsert where
  parseJSON = genericParseJSON strictObjectOptions

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
instance FromJSON InternProfileUpdate where
  parseJSON = withObject "InternProfileUpdate" $ \o -> do
    let allowedKeys =
          [ "ipuStartAt"
          , "ipuEndAt"
          , "ipuRequiredHours"
          , "ipuSkills"
          , "ipuAreas"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in InternProfileUpdate: " <> T.unpack key)
      [] ->
        InternProfileUpdate
          <$> o .:! "ipuStartAt"
          <*> o .:! "ipuEndAt"
          <*> o .:! "ipuRequiredHours"
          <*> o .:! "ipuSkills"
          <*> o .:! "ipuAreas"

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
instance FromJSON InternProjectCreate where
  parseJSON = genericParseJSON strictObjectOptions

data InternProjectUpdate = InternProjectUpdate
  { ipuTitle       :: Maybe Text
  , ipuDescription :: Maybe (Maybe Text)
  , ipuStatus      :: Maybe Text
  , ipuStartAt     :: Maybe (Maybe Day)
  , ipuDueAt       :: Maybe (Maybe Day)
  } deriving (Show, Generic)
instance ToJSON InternProjectUpdate
instance FromJSON InternProjectUpdate where
  parseJSON = withObject "InternProjectUpdate" $ \o -> do
    let allowedKeys =
          [ "ipuTitle"
          , "ipuDescription"
          , "ipuStatus"
          , "ipuStartAt"
          , "ipuDueAt"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in InternProjectUpdate: " <> T.unpack key)
      [] ->
        InternProjectUpdate
          <$> o .:? "ipuTitle"
          <*> o .:! "ipuDescription"
          <*> o .:? "ipuStatus"
          <*> o .:! "ipuStartAt"
          <*> o .:! "ipuDueAt"

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
instance FromJSON InternTaskCreate where
  parseJSON = genericParseJSON strictObjectOptions

data InternTaskUpdate = InternTaskUpdate
  { ituTitle       :: Maybe Text
  , ituDescription :: Maybe (Maybe Text)
  , ituStatus      :: Maybe Text
  , ituProgress    :: Maybe Int
  , ituAssignedTo  :: Maybe (Maybe Int64)
  , ituDueAt       :: Maybe (Maybe Day)
  } deriving (Show, Generic)
instance ToJSON InternTaskUpdate
instance FromJSON InternTaskUpdate where
  parseJSON = withObject "InternTaskUpdate" $ \o -> do
    let allowedKeys =
          [ "ituTitle"
          , "ituDescription"
          , "ituStatus"
          , "ituProgress"
          , "ituAssignedTo"
          , "ituDueAt"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in InternTaskUpdate: " <> T.unpack key)
      [] ->
        InternTaskUpdate
          <$> o .:? "ituTitle"
          <*> o .:! "ituDescription"
          <*> o .:? "ituStatus"
          <*> o .:? "ituProgress"
          <*> o .:! "ituAssignedTo"
          <*> o .:! "ituDueAt"

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
instance FromJSON InternTodoCreate where
  parseJSON = genericParseJSON strictObjectOptions

data InternTodoUpdate = InternTodoUpdate
  { itduText :: Maybe Text
  , itduDone :: Maybe Bool
  } deriving (Show, Generic)
instance ToJSON InternTodoUpdate
instance FromJSON InternTodoUpdate where
  parseJSON = genericParseJSON strictObjectOptions

data ClockInRequest = ClockInRequest
  { cirNotes :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClockInRequest
instance FromJSON ClockInRequest where
  parseJSON value = do
    request <- genericParseJSON strictObjectOptions value
    notes <- normalizeTimeEntryNotesField "cirNotes" (cirNotes request)
    pure request { cirNotes = notes }

data ClockOutRequest = ClockOutRequest
  { corNotes :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON ClockOutRequest
instance FromJSON ClockOutRequest where
  parseJSON value = do
    request <- genericParseJSON strictObjectOptions value
    notes <- normalizeTimeEntryNotesField "corNotes" (corNotes request)
    pure request { corNotes = notes }

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

timeEntryNotesMaxLength :: Int
timeEntryNotesMaxLength = 1000

normalizeTimeEntryNotesField :: String -> Maybe Text -> Parser (Maybe Text)
normalizeTimeEntryNotesField _ Nothing = pure Nothing
normalizeTimeEntryNotesField fieldName (Just rawValue)
  | T.null trimmed =
      pure Nothing
  | T.length trimmed > timeEntryNotesMaxLength =
      fail (fieldName <> " must be 1000 characters or fewer")
  | T.any isUnsafeTimeEntryNotesControl trimmed =
      fail (fieldName <> " must not contain control characters other than tabs or line breaks")
  | otherwise =
      pure (Just trimmed)
  where
    trimmed = T.strip rawValue

isUnsafeTimeEntryNotesControl :: Char -> Bool
isUnsafeTimeEntryNotesControl ch =
  isControl ch && ch /= '\n' && ch /= '\r' && ch /= '\t'

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
instance FromJSON InternPermissionCreate where
  parseJSON = genericParseJSON strictObjectOptions

data InternPermissionUpdate = InternPermissionUpdate
  { ipuStatus        :: Maybe Text
  , ipuDecisionNotes :: Maybe (Maybe Text)
  } deriving (Show, Generic)
instance ToJSON InternPermissionUpdate
instance FromJSON InternPermissionUpdate where
  parseJSON = withObject "InternPermissionUpdate" $ \o -> do
    let allowedKeys =
          [ "ipuStatus"
          , "ipuDecisionNotes"
          ]
        unknownKeys =
          filter (`notElem` allowedKeys) (map AKey.toText (AKM.keys o))
    case unknownKeys of
      key:_ -> fail ("Unknown field in InternPermissionUpdate: " <> T.unpack key)
      [] ->
        InternPermissionUpdate
          <$> o .:? "ipuStatus"
          <*> o .:! "ipuDecisionNotes"
