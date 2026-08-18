{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.CommerceOperations
  ( CommerceOperationsAPI
  , CommerceProviderEventDTO(..)
  , CommerceProviderEventReplayCreate(..)
  ) where

import           Data.Aeson (FromJSON(..), ToJSON, genericParseJSON)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant

import           TDF.API.Types (strictObjectOptions)

-- | Sensitive fields are intentionally excluded from this operator DTO. In
-- particular, the encrypted provider payload and merchant account reference
-- never leave the backend.
data CommerceProviderEventDTO = CommerceProviderEventDTO
  { cpeId                 :: Text
  , cpeProvider           :: Text
  , cpeEnvironment        :: Text
  , cpeProviderEventId    :: Text
  , cpeEventType          :: Text
  , cpeProviderResourceId :: Maybe Text
  , cpeStatus             :: Text
  , cpeAttemptCount       :: Int
  , cpeCheckoutId         :: Maybe Text
  , cpePaymentAttemptId   :: Maybe Text
  , cpeRefundId           :: Maybe Text
  , cpeReceivedAt         :: UTCTime
  , cpeProviderCreatedAt  :: Maybe UTCTime
  , cpeProcessingStartedAt :: Maybe UTCTime
  , cpeLastAttemptAt      :: Maybe UTCTime
  , cpeNextAttemptAt      :: Maybe UTCTime
  , cpeProcessedAt        :: Maybe UTCTime
  , cpeErrorSummary       :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderEventDTO
instance FromJSON CommerceProviderEventDTO

data CommerceProviderEventReplayCreate = CommerceProviderEventReplayCreate
  { cperReason :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON CommerceProviderEventReplayCreate
instance FromJSON CommerceProviderEventReplayCreate where
  parseJSON = genericParseJSON strictObjectOptions

type CommerceOperationsAPI =
       "admin" :> "commerce" :> "provider-events"
         :> QueryParam "status" Text
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] [CommerceProviderEventDTO]
  :<|> "admin" :> "commerce" :> "provider-events"
         :> Capture "eventId" Text
         :> "replay"
         :> ReqBody '[JSON] CommerceProviderEventReplayCreate
         :> Post '[JSON] CommerceProviderEventDTO
