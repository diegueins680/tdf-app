{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Reviews where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

data ExperienceReviewPage = ExperienceReviewPage
  { summary :: Value
  , items :: [Value]
  , nextCursor :: Maybe UUID
  } deriving (Show, Generic)

instance ToJSON ExperienceReviewPage

data ExperienceReviewCreateRequest = ExperienceReviewCreateRequest
  { targetKind :: Text
  , targetId :: Text
  , sourceKind :: Text
  , sourceId :: Text
  , rating :: Int
  , body :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ExperienceReviewCreateRequest
instance ToJSON ExperienceReviewCreateRequest

data ReputationPreferenceCategoryInput = ReputationPreferenceCategoryInput
  { categoryId :: UUID
  , position :: Int
  , weight :: Double
  , notApplicable :: Bool
  } deriving (Show, Generic)

instance FromJSON ReputationPreferenceCategoryInput
instance ToJSON ReputationPreferenceCategoryInput

data ReputationPreferenceSaveRequest = ReputationPreferenceSaveRequest
  { contextKind :: Text
  , expectedRevision :: Int
  , activate :: Bool
  , categories :: [ReputationPreferenceCategoryInput]
  } deriving (Show, Generic)

instance FromJSON ReputationPreferenceSaveRequest
instance ToJSON ReputationPreferenceSaveRequest

type RequiredReviewIdempotency =
  Header' '[Required, Strict] "Idempotency-Key" Text

type ReviewsPublicAPI =
       "reputation" :> "categories" :> QueryParam "locale" Text :> Get '[JSON] [Value]
  :<|> "reputation" :> "profiles" :> Capture "partyId" Int64 :> Get '[JSON] Value
  :<|> "reviews"
    :> Capture "targetKind" Text
    :> Capture "targetId" Text
    :> QueryParam "cursor" UUID
    :> QueryParam "limit" Int
    :> Get '[JSON] ExperienceReviewPage

type ReviewsProtectedAPI =
       "reviews" :>
         ( "eligibility"
             :> QueryParam "targetKind" Text
             :> QueryParam "targetId" Text
             :> Get '[JSON] [Value]
      :<|> RequiredReviewIdempotency
             :> ReqBody '[JSON] ExperienceReviewCreateRequest
             :> PostCreated '[JSON] Value
         )
  :<|> "reputation" :> "preferences"
         :> QueryParam "contextKind" Text
         :> Get '[JSON] Value
  :<|> "reputation" :> "preferences"
         :> RequiredReviewIdempotency
         :> ReqBody '[JSON] ReputationPreferenceSaveRequest
         :> Put '[JSON] Value
