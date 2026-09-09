{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.MerchReputation where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

data MerchReviewImageInput = MerchReviewImageInput
  { mediaAssetId :: UUID
  , altText :: Text
  } deriving (Show, Generic)

instance FromJSON MerchReviewImageInput
instance ToJSON MerchReviewImageInput

data MerchReviewSubmitRequest = MerchReviewSubmitRequest
  { overallRating :: Int
  , issueOccurred :: Bool
  , comment :: Maybe Text
  , dimensions :: Value
  , images :: Maybe [MerchReviewImageInput]
  , expectedRevision :: Int
  } deriving (Show, Generic)

instance FromJSON MerchReviewSubmitRequest
instance ToJSON MerchReviewSubmitRequest

data MerchSellerResponseRequest = MerchSellerResponseRequest
  { responseBody :: Text
  , responseExpectedRevision :: Int
  } deriving (Show, Generic)

instance FromJSON MerchSellerResponseRequest
instance ToJSON MerchSellerResponseRequest

data MerchContentReportRequest = MerchContentReportRequest
  { reportTargetType :: Text
  , reportTargetId :: UUID
  , reportReason :: Text
  , reportDetails :: Maybe Text
  , authorizedEvidence :: Maybe [Value]
  } deriving (Show, Generic)

instance FromJSON MerchContentReportRequest
instance ToJSON MerchContentReportRequest

data MerchModerationDecisionRequest = MerchModerationDecisionRequest
  { moderationDecision :: Text
  , moderationReasonCode :: Text
  , moderationRationale :: Text
  , moderationEvidence :: Value
  } deriving (Show, Generic)

instance FromJSON MerchModerationDecisionRequest
instance ToJSON MerchModerationDecisionRequest

data MerchModerationWorkflowRequest = MerchModerationWorkflowRequest
  { moderationAction :: Text
  , workflowRationale :: Text
  , workflowEvidence :: Value
  } deriving (Show, Generic)

instance FromJSON MerchModerationWorkflowRequest
instance ToJSON MerchModerationWorkflowRequest

data MerchAppealDecisionRequest = MerchAppealDecisionRequest
  { appealOutcome :: Text
  , appealRationale :: Text
  , appealEvidence :: Value
  } deriving (Show, Generic)

instance FromJSON MerchAppealDecisionRequest
instance ToJSON MerchAppealDecisionRequest

data MerchAppealRequest = MerchAppealRequest
  { appealGrounds :: Text
  } deriving (Show, Generic)

instance FromJSON MerchAppealRequest
instance ToJSON MerchAppealRequest

data MerchPriorityRequest = MerchPriorityRequest
  { orderedDimensionCodes :: [Text]
  , priorityExpectedRevision :: Int
  } deriving (Show, Generic)

instance FromJSON MerchPriorityRequest
instance ToJSON MerchPriorityRequest

data MerchCategorySuggestionRequest = MerchCategorySuggestionRequest
  { suggestionSubjectKind :: Text
  , suggestionLabel :: Text
  , suggestionDefinition :: Text
  } deriving (Show, Generic)

instance FromJSON MerchCategorySuggestionRequest
instance ToJSON MerchCategorySuggestionRequest

data MerchCategorySuggestionDecisionRequest = MerchCategorySuggestionDecisionRequest
  { suggestionStatus :: Text
  , suggestionMinimumSample :: Maybe Int
  , suggestionBiasTest :: Maybe Value
  , suggestionUtilityTest :: Maybe Value
  , suggestionDecisionReason :: Text
  } deriving (Show, Generic)

instance FromJSON MerchCategorySuggestionDecisionRequest
instance ToJSON MerchCategorySuggestionDecisionRequest

data MerchNotificationPreferenceRequest = MerchNotificationPreferenceRequest
  { reviewInvitation :: Bool
  , reviewReminder :: Bool
  , sellerResponseNotification :: Bool
  , moderationChange :: Bool
  , evidenceRequest :: Bool
  , appealResult :: Bool
  , badgeChange :: Bool
  } deriving (Show, Generic)

instance FromJSON MerchNotificationPreferenceRequest
instance ToJSON MerchNotificationPreferenceRequest

type RequiredMerchIdempotency =
  Header' '[Required, Strict] "Idempotency-Key" Text

type MerchReputationPublicAPI =
       "merch" :> "artists" :> Capture "artistPartyId" Int64 :> "stores"
         :> Get '[JSON] [Value]
  :<|> "merch" :> "stores" :> Capture "storeId" UUID :> "reputation"
         :> Get '[JSON] Value
  :<|> "merch" :> "stores" :> Capture "storeId" UUID :> "reviews"
         :> QueryParam "cursor" UUID :> QueryParam "limit" Int :> Get '[JSON] Value
  :<|> "merch" :> "products" :> Capture "productId" UUID :> "reputation"
         :> Get '[JSON] Value
  :<|> "merch" :> "products" :> Capture "productId" UUID :> "reviews"
         :> QueryParam "cursor" UUID :> QueryParam "limit" Int :> Get '[JSON] Value
  :<|> "merch" :> "reputation" :> "formula"
         :> QueryParam "locale" Text :> Get '[JSON] Value

type MerchReputationProtectedAPI =
       "merch" :> "orders" :> Capture "orderId" UUID :> "reviews" :> "eligibility"
         :> Get '[JSON] Value
  :<|> "merch" :> "orders" :> Capture "orderId" UUID :> "store-review"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchReviewSubmitRequest
         :> Put '[JSON] Value
  :<|> "merch" :> "orders" :> Capture "orderId" UUID :> "lines"
         :> Capture "lineId" UUID :> "product-review"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchReviewSubmitRequest
         :> Put '[JSON] Value
  :<|> "merch" :> "reviews" :> Capture "reviewId" UUID :> "response"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchSellerResponseRequest
         :> Put '[JSON] Value
  :<|> "merch" :> "reputation" :> "reports"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchContentReportRequest
         :> PostCreated '[JSON] Value
  :<|> "merch" :> "reputation" :> "decisions" :> Capture "decisionId" UUID :> "appeal"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchAppealRequest
         :> PostCreated '[JSON] Value
  :<|> "merch" :> "reputation" :> "preferences" :> Capture "subjectKind" Text
         :> Get '[JSON] Value
  :<|> "merch" :> "reputation" :> "preferences" :> Capture "subjectKind" Text
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchPriorityRequest
         :> Put '[JSON] Value
  :<|> "merch" :> "reputation" :> "category-suggestions"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchCategorySuggestionRequest
         :> Post '[JSON] Value
  :<|> "merch" :> "reputation" :> "notification-preferences"
         :> Get '[JSON] Value
  :<|> "merch" :> "reputation" :> "notification-preferences"
         :> ReqBody '[JSON] MerchNotificationPreferenceRequest :> Put '[JSON] Value
  :<|> "merch" :> "seller" :> "stores" :> Capture "storeId" UUID :> "reputation"
         :> Get '[JSON] Value
  :<|> "merch" :> "admin" :> "reputation" :> "cases"
         :> QueryParam "state" Text :> Get '[JSON] [Value]
  :<|> "merch" :> "admin" :> "reputation" :> "category-suggestions"
         :> QueryParam "status" Text :> Get '[JSON] [Value]
  :<|> "merch" :> "admin" :> "reputation" :> "category-suggestions"
         :> Capture "suggestionId" UUID :> "decision" :> RequiredMerchIdempotency
         :> ReqBody '[JSON] MerchCategorySuggestionDecisionRequest :> Post '[JSON] Value
  :<|> "merch" :> "admin" :> "reputation" :> "cases" :> Capture "caseId" UUID :> "workflow"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchModerationWorkflowRequest
         :> Post '[JSON] Value
  :<|> "merch" :> "admin" :> "reputation" :> "cases" :> Capture "caseId" UUID :> "decision"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchModerationDecisionRequest
         :> Post '[JSON] Value
  :<|> "merch" :> "admin" :> "reputation" :> "appeals" :> Capture "appealId" UUID :> "decision"
         :> RequiredMerchIdempotency :> ReqBody '[JSON] MerchAppealDecisionRequest
         :> Post '[JSON] Value
