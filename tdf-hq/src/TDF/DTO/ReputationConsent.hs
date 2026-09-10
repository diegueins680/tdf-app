{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contract DTOs for the independent, self-service reputation consents.
-- Keep this small boundary separate from aggregate/review projections: it must
-- never grow evaluator, ranking, or interaction-identifying fields.
module TDF.DTO.ReputationConsent
  ( ReputationConsentUpdate(..)
  , ReputationConsentDTO(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data ReputationConsentUpdate = ReputationConsentUpdate
  { consentKind :: Text
  , granted :: Bool
  , consentCopyVersion :: Maybe Text
  , consentLocale :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON ReputationConsentUpdate
instance ToJSON ReputationConsentUpdate

data ReputationConsentDTO = ReputationConsentDTO
  { reputationConsentKind :: Text
  , reputationConsentGranted :: Bool
  , consentStateVersion :: Int
  , updatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ReputationConsentDTO where
  toJSON ReputationConsentDTO{reputationConsentKind, reputationConsentGranted, consentStateVersion, updatedAt} =
    object
      [ "consentKind" .= reputationConsentKind
      , "granted" .= reputationConsentGranted
      , "version" .= consentStateVersion
      , "updatedAt" .= updatedAt
      ]

instance FromJSON ReputationConsentDTO where
  parseJSON = withObject "ReputationConsentDTO" $ \objectValue ->
    ReputationConsentDTO
      <$> objectValue .: "consentKind"
      <*> objectValue .: "granted"
      <*> objectValue .: "version"
      <*> objectValue .:? "updatedAt"
