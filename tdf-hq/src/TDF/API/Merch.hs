{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Merch where

import           Data.Aeson (FromJSON(..), ToJSON(..), Options(..), Value, genericParseJSON, genericToJSON)
import           Data.Char (toLower)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (UUID)
import           GHC.Generics (Generic)
import           Servant
import           Servant.Multipart
  ( FileData, FromMultipart(..), Input(..), MultipartData(..), MultipartForm
  , Tmp, fdInputName
  )
import           Text.Read (readMaybe)

import           TDF.API.Types (strictObjectOptions)

merchOptions :: Int -> Options
merchOptions prefixLength = strictObjectOptions
  { fieldLabelModifier = lowerFirst . drop prefixLength
  }
  where
    lowerFirst [] = []
    lowerFirst (first:rest) = toLower first : rest

data MerchStoreApplicationRequest = MerchStoreApplicationRequest
  { msaProfileId       :: UUID
  , msaSlug            :: Text
  , msaDisplayName     :: Text
  , msaDescription     :: Maybe Text
  , msaApplicationNote :: Text
  } deriving (Show, Generic)
instance FromJSON MerchStoreApplicationRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchStoreApplicationRequest where toJSON = genericToJSON (merchOptions 3)

data MerchStoreUpdateRequest = MerchStoreUpdateRequest
  { msuDisplayName   :: Text
  , msuDescription   :: Maybe Text
  , msuCoverImageUrl :: Maybe Text
  , msuLogoImageUrl  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchStoreUpdateRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchStoreUpdateRequest where toJSON = genericToJSON (merchOptions 3)

data MerchStoreReviewRequest = MerchStoreReviewRequest
  { msrDecision        :: Text
  , msrReviewerNotes   :: Text
  , msrCommissionBps   :: Maybe Int
  , msrCommissionReason :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchStoreReviewRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchStoreReviewRequest where toJSON = genericToJSON (merchOptions 3)

data MerchMemberPermissions = MerchMemberPermissions
  { mmpCatalog     :: Bool
  , mmpStock       :: Bool
  , mmpOrders      :: Bool
  , mmpFulfillment :: Bool
  , mmpFinance     :: Bool
  , mmpSettings    :: Bool
  } deriving (Show, Generic)
instance FromJSON MerchMemberPermissions where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchMemberPermissions where toJSON = genericToJSON (merchOptions 3)

data MerchMemberInviteRequest = MerchMemberInviteRequest
  { mmiPartyId    :: Int64
  , mmiPermissions :: MerchMemberPermissions
  } deriving (Show, Generic)
instance FromJSON MerchMemberInviteRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchMemberInviteRequest where toJSON = genericToJSON (merchOptions 3)

data MerchMemberUpdateRequest = MerchMemberUpdateRequest
  { mmuStatus      :: Text
  , mmuPermissions :: Maybe MerchMemberPermissions
  , mmuReason      :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchMemberUpdateRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchMemberUpdateRequest where toJSON = genericToJSON (merchOptions 3)

data MerchPolicyRequest = MerchPolicyRequest
  { mprShippingPolicy :: Text
  , mprReturnPolicy   :: Text
  , mprPreorderPolicy :: Maybe Text
  , mprSupportEmail   :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchPolicyRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchPolicyRequest where toJSON = genericToJSON (merchOptions 3)

data MerchShippingZoneRequest = MerchShippingZoneRequest
  { mszName                 :: Text
  , mszCountryCode          :: Text
  , mszSubdivisionCodes     :: [Text]
  , mszDeliveryMethod       :: Text
  , mszRateMinor            :: Int64
  , mszFreeShippingMinMinor :: Maybe Int64
  , mszEstimatedMinDays     :: Maybe Int
  , mszEstimatedMaxDays     :: Maybe Int
  , mszActive               :: Bool
  } deriving (Show, Generic)
instance FromJSON MerchShippingZoneRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchShippingZoneRequest where toJSON = genericToJSON (merchOptions 3)

data MerchVariantRequest = MerchVariantRequest
  { mvrId             :: Maybe UUID
  , mvrSku            :: Text
  , mvrName           :: Text
  , mvrOptionValues   :: Value
  , mvrPriceMinor     :: Int64
  , mvrCompareAtPriceMinor :: Maybe Int64
  , mvrCurrency       :: Text
  , mvrWeightGrams    :: Int
  , mvrCustomsDescription :: Maybe Text
  , mvrStockMode      :: Text
  , mvrStockOnHand    :: Int
  , mvrReorderThreshold :: Int
  , mvrActive         :: Bool
  } deriving (Show, Generic)
instance FromJSON MerchVariantRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchVariantRequest where toJSON = genericToJSON (merchOptions 3)

data MerchProductRequest = MerchProductRequest
  { mpuSlug             :: Text
  , mpuName             :: Text
  , mpuDescription      :: Text
  , mpuCategory         :: Text
  , mpuVisibility       :: Text
  , mpuAvailabilityMode :: Text
  , mpuPreorderReleaseAt :: Maybe Text
  , mpuPublishAt        :: Maybe Text
  , mpuUnpublishAt      :: Maybe Text
  , mpuBuyerLimit       :: Maybe Int
  , mpuPolicyId         :: Maybe UUID
  , mpuVariants         :: [MerchVariantRequest]
  } deriving (Show, Generic)
instance FromJSON MerchProductRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchProductRequest where toJSON = genericToJSON (merchOptions 3)

data MerchStockRequest = MerchStockRequest
  { msqStockOnHand     :: Int
  , msqReorderThreshold :: Int
  , msqActive          :: Bool
  , msqVersion         :: Int64
  } deriving (Show, Generic)
instance FromJSON MerchStockRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchStockRequest where toJSON = genericToJSON (merchOptions 3)

data MerchStatusRequest = MerchStatusRequest
  { mstStatus :: Text
  , mstReason :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchStatusRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchStatusRequest where toJSON = genericToJSON (merchOptions 3)

data MerchCartCreateRequest = MerchCartCreateRequest
  { mccStoreSlug :: Text
  } deriving (Show, Generic)
instance FromJSON MerchCartCreateRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchCartCreateRequest where toJSON = genericToJSON (merchOptions 3)

data MerchCartItemRequest = MerchCartItemRequest
  { mciVariantId :: UUID
  , mciQuantity  :: Int
  } deriving (Show, Generic)
instance FromJSON MerchCartItemRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchCartItemRequest where toJSON = genericToJSON (merchOptions 3)

data MerchRecipientRequest = MerchRecipientRequest
  { mrrName         :: Text
  , mrrEmail        :: Text
  , mrrPhone        :: Maybe Text
  , mrrCountryCode  :: Text
  , mrrSubdivision  :: Maybe Text
  , mrrCity         :: Text
  , mrrAddressLine1 :: Text
  , mrrAddressLine2 :: Maybe Text
  , mrrPostalCode   :: Maybe Text
  , mrrDeliveryNote :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchRecipientRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchRecipientRequest where toJSON = genericToJSON (merchOptions 3)

data MerchCheckoutRequest = MerchCheckoutRequest
  { mcoRecipient      :: MerchRecipientRequest
  , mcoShippingZoneId :: UUID
  , mcoCreateAccount  :: Maybe Bool
  , mcoLocale         :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchCheckoutRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchCheckoutRequest where toJSON = genericToJSON (merchOptions 3)

data MerchIssueRequest = MerchIssueRequest
  { mirIssueType :: Text
  , mirMessage   :: Text
  } deriving (Show, Generic)
instance FromJSON MerchIssueRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchIssueRequest where toJSON = genericToJSON (merchOptions 3)

data MerchCancellationRequest = MerchCancellationRequest
  { mcrReason :: Text
  } deriving (Show, Generic)
instance FromJSON MerchCancellationRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchCancellationRequest where toJSON = genericToJSON (merchOptions 3)

data MerchIssueTriageRequest = MerchIssueTriageRequest
  { mitStatus         :: Text
  , mitPublicResponse :: Maybe Text
  , mitInternalNotes  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchIssueTriageRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchIssueTriageRequest where toJSON = genericToJSON (merchOptions 3)

data MerchFulfillmentRequest = MerchFulfillmentRequest
  { mfrStatus         :: Text
  , mfrPublicNote     :: Maybe Text
  , mfrPrivateNote    :: Maybe Text
  , mfrCarrier        :: Maybe Text
  , mfrTrackingNumber :: Maybe Text
  , mfrTrackingUrl    :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchFulfillmentRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchFulfillmentRequest where toJSON = genericToJSON (merchOptions 3)

data MerchSettlementRequest = MerchSettlementRequest
  { mseStoreId      :: UUID
  , msePeriodStart  :: Text
  , msePeriodEnd    :: Text
  , mseOrderIds     :: [UUID]
  , mseReviewNotes  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON MerchSettlementRequest where parseJSON = genericParseJSON (merchOptions 3)
instance ToJSON MerchSettlementRequest where toJSON = genericToJSON (merchOptions 3)

data MerchSettlementPaymentForm = MerchSettlementPaymentForm
  { mspfFile              :: FileData Tmp
  , mspfPaidAt            :: Text
  , mspfExternalReference :: Text
  , mspfNotes             :: Maybe Text
  }

instance FromMultipart Tmp MerchSettlementPaymentForm where
  fromMultipart multipart = do
    rejectUnexpected multipart
    file <- singleFile "file" multipart
    paidAt <- requiredInput "paidAt" multipart
    externalReference <- requiredInput "externalReference" multipart
    notes <- optionalInput "notes" multipart
    if T.null (T.strip paidAt) || T.length paidAt > 80
      then Left "paidAt must contain 1 to 80 characters"
      else if T.length (T.strip externalReference) < 3 || T.length (T.strip externalReference) > 160
        then Left "externalReference must contain 3 to 160 characters"
        else if maybe False (\value -> T.length value < 3 || T.length value > 2000) (T.strip <$> notes)
          then Left "notes must contain 3 to 2000 characters when provided"
          else Right MerchSettlementPaymentForm
            { mspfFile = file
            , mspfPaidAt = T.strip paidAt
            , mspfExternalReference = T.strip externalReference
            , mspfNotes = T.strip <$> notes
            }
    where
      requiredInput field mp =
        case [value | Input name value <- inputs mp, name == field] of
          [value] -> Right value
          [] -> Left ("Missing field: " <> T.unpack field)
          _ -> Left ("Duplicate field: " <> T.unpack field)
      optionalInput field mp =
        case [value | Input name value <- inputs mp, name == field] of
          [] -> Right Nothing
          [value] -> Right (Just value)
          _ -> Left ("Duplicate field: " <> T.unpack field)
      singleFile field mp =
        case [file | file <- files mp, fdInputName file == field] of
          [file] -> Right file
          [] -> Left ("Missing file field: " <> T.unpack field)
          _ -> Left ("Duplicate file field: " <> T.unpack field)
      rejectUnexpected mp =
        case [name | Input name _ <- inputs mp, name `notElem` ["paidAt","externalReference","notes"]] of
          name:_ -> Left ("Unexpected field: " <> T.unpack name)
          [] -> case [fdInputName file | file <- files mp, fdInputName file /= "file"] of
            name:_ -> Left ("Unexpected file field: " <> T.unpack name)
            [] -> Right ()

data MerchImageUploadForm = MerchImageUploadForm
  { miuFile      :: FileData Tmp
  , miuAltText   :: Text
  , miuSortOrder :: Int
  }

instance FromMultipart Tmp MerchImageUploadForm where
  fromMultipart multipart = do
    rejectUnexpected multipart
    file <- singleFile "file" multipart
    altText <- requiredInput "altText" multipart
    sortOrderText <- requiredInput "sortOrder" multipart
    sortOrder <- maybe (Left "sortOrder must be a non-negative integer") Right
      (readMaybe (T.unpack sortOrderText))
    if T.null (T.strip altText) || T.length altText > 500
      then Left "altText must contain 1 to 500 characters"
      else if sortOrder < 0
        then Left "sortOrder must be a non-negative integer"
        else Right MerchImageUploadForm
          { miuFile = file
          , miuAltText = T.strip altText
          , miuSortOrder = sortOrder
          }
    where
      requiredInput field mp =
        case [value | Input name value <- inputs mp, name == field] of
          [value] -> Right value
          [] -> Left ("Missing field: " <> T.unpack field)
          _ -> Left ("Duplicate field: " <> T.unpack field)
      singleFile field mp =
        case [file | file <- files mp, fdInputName file == field] of
          [file] -> Right file
          [] -> Left ("Missing file field: " <> T.unpack field)
          _ -> Left ("Duplicate file field: " <> T.unpack field)
      rejectUnexpected mp =
        case [name | Input name _ <- inputs mp, name `notElem` ["altText","sortOrder"]] of
          name:_ -> Left ("Unexpected field: " <> T.unpack name)
          [] -> case [fdInputName file | file <- files mp, fdInputName file /= "file"] of
            name:_ -> Left ("Unexpected file field: " <> T.unpack name)
            [] -> Right ()

type MerchPublicAPI =
       "merch" :> "capabilities" :> Get '[JSON] Value
  :<|> "merch" :> "storefronts"
         :> QueryParam "q" Text :> QueryParam "category" Text
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] [Value]
  :<|> "merch" :> "storefronts" :> Capture "storeSlug" Text :> Get '[JSON] Value
  :<|> "merch" :> "storefronts" :> Capture "storeSlug" Text
         :> "products" :> Capture "productSlug" Text :> Get '[JSON] Value
  :<|> "merch" :> "carts" :> ReqBody '[JSON] MerchCartCreateRequest :> PostCreated '[JSON] Value
  :<|> "merch" :> "carts" :> Capture "cartId" UUID :> Header "X-Cart-Lookup-Token" Text :> Get '[JSON] Value
  :<|> "merch" :> "carts" :> Capture "cartId" UUID :> "items"
         :> Header "X-Cart-Lookup-Token" Text :> ReqBody '[JSON] MerchCartItemRequest :> Put '[JSON] Value
  :<|> "merch" :> "carts" :> Capture "cartId" UUID :> "items" :> Capture "variantId" UUID
         :> Header "X-Cart-Lookup-Token" Text :> Delete '[JSON] Value
  :<|> "merch" :> "carts" :> Capture "cartId" UUID :> "checkout"
         :> Header "X-Cart-Lookup-Token" Text :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] MerchCheckoutRequest :> Post '[JSON] Value
  :<|> "merch" :> "orders" :> Capture "orderId" UUID
         :> Header "X-Order-Lookup-Token" Text :> Get '[JSON] Value
  :<|> "merch" :> "orders" :> Capture "orderId" UUID :> "issues"
         :> Header "X-Order-Lookup-Token" Text :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] MerchIssueRequest :> PostCreated '[JSON] Value
  :<|> "merch" :> "orders" :> Capture "orderId" UUID :> "cancel"
         :> Header "X-Order-Lookup-Token" Text :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] MerchCancellationRequest :> Post '[JSON] Value

type MerchProtectedAPI = "merch" :>
  (    "favorites" :> Capture "productId" UUID :> Put '[JSON] NoContent
  :<|> "favorites" :> Capture "productId" UUID :> Delete '[JSON] NoContent
  :<|> "seller" :> "stores" :> Get '[JSON] [Value]
  :<|> "seller" :> "applications" :> Header "Idempotency-Key" Text
         :> ReqBody '[JSON] MerchStoreApplicationRequest :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID
         :> ReqBody '[JSON] MerchStoreUpdateRequest :> Put '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "members" :> Get '[JSON] [Value]
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "members"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MerchMemberInviteRequest :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "members" :> Capture "memberId" UUID
         :> ReqBody '[JSON] MerchMemberUpdateRequest :> Patch '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "policies"
         :> ReqBody '[JSON] MerchPolicyRequest :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "shipping-zones"
         :> ReqBody '[JSON] MerchShippingZoneRequest :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "products" :> Get '[JSON] [Value]
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "products"
         :> Header "Idempotency-Key" Text :> ReqBody '[JSON] MerchProductRequest :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "products" :> Capture "productId" UUID
         :> ReqBody '[JSON] MerchProductRequest :> Put '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "products" :> Capture "productId" UUID
         :> "status" :> ReqBody '[JSON] MerchStatusRequest :> Patch '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "products" :> Capture "productId" UUID
         :> "images" :> MultipartForm Tmp MerchImageUploadForm :> PostCreated '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "variants" :> Capture "variantId" UUID
         :> "stock" :> ReqBody '[JSON] MerchStockRequest :> Patch '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "orders"
         :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "issues"
         :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "issues" :> Capture "issueId" UUID
         :> ReqBody '[JSON] MerchIssueTriageRequest :> Patch '[JSON] Value
  :<|> "seller" :> "stores" :> Capture "storeId" UUID :> "orders" :> Capture "orderId" UUID
         :> "fulfillment" :> ReqBody '[JSON] MerchFulfillmentRequest :> Patch '[JSON] Value
  :<|> "admin" :> "stores" :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "admin" :> "stores" :> Capture "storeId" UUID :> "review"
         :> ReqBody '[JSON] MerchStoreReviewRequest :> Post '[JSON] Value
  :<|> "admin" :> "products" :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "admin" :> "products" :> Capture "productId" UUID :> "review"
         :> ReqBody '[JSON] MerchStatusRequest :> Post '[JSON] Value
  :<|> "admin" :> "issues" :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "admin" :> "issues" :> Capture "issueId" UUID
         :> ReqBody '[JSON] MerchIssueTriageRequest :> Patch '[JSON] Value
  :<|> "admin" :> "settlements" :> ReqBody '[JSON] MerchSettlementRequest :> PostCreated '[JSON] Value
  :<|> "admin" :> "settlements" :> Capture "settlementId" UUID :> "status"
         :> ReqBody '[JSON] MerchStatusRequest :> Patch '[JSON] Value
  :<|> "admin" :> "settlements" :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "admin" :> "stores" :> Capture "storeId" UUID :> "settlement-orders" :> Get '[JSON] [Value]
  :<|> "admin" :> "settlements" :> Capture "settlementId" UUID :> "payment-evidence"
         :> Header "Idempotency-Key" Text :> MultipartForm Tmp MerchSettlementPaymentForm :> Post '[JSON] Value
  )
