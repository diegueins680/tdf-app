{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TDF.DTO.EventResearchDTO
    ( EventResearchPilotDTO (..)
    , EventResearchPilotApprovalDTO (..)
    , EventResearchRunCreateDTO (..)
    , EventResearchRunUpdateDTO (..)
    , EventResearchRunDTO (..)
    , EventResearchEvidenceDTO (..)
    , EventResearchCandidateWriteDTO (..)
    , EventResearchCandidateDTO (..)
    , EventResearchChangeDTO (..)
    ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, Value, defaultOptions, genericParseJSON, rejectUnknownFields)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data EventResearchPilotDTO = EventResearchPilotDTO
    { erPilotApproved :: Bool
    , erPilotApprovedAt :: Maybe UTCTime
    , erPilotApprovedByPartyId :: Maybe Text
    , erPilotApprovalReference :: Maybe Text
    , erPilotMaxActiveCandidates :: Int
    , erPilotActiveCandidates :: Int
    , erPilotUpdatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchPilotDTO
instance FromJSON EventResearchPilotDTO

data EventResearchPilotApprovalDTO = EventResearchPilotApprovalDTO
    { erPilotApprovalReference :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchPilotApprovalDTO
instance FromJSON EventResearchPilotApprovalDTO where
    parseJSON = genericParseJSON defaultOptions{rejectUnknownFields = True}

data EventResearchRunCreateDTO = EventResearchRunCreateDTO
    { erRunKey :: Text
    , erRunReconciliation :: Bool
    , erRunCheckpoint :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchRunCreateDTO
instance FromJSON EventResearchRunCreateDTO where
    parseJSON = genericParseJSON defaultOptions{rejectUnknownFields = True}

data EventResearchRunUpdateDTO = EventResearchRunUpdateDTO
    { erRunStatus :: Text
    , erRunCheckpoint :: Maybe Text
    , erRunCounters :: Value
    , erRunErrorSummary :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchRunUpdateDTO
instance FromJSON EventResearchRunUpdateDTO where
    parseJSON = genericParseJSON defaultOptions{rejectUnknownFields = True}

data EventResearchRunDTO = EventResearchRunDTO
    { erRunId :: Text
    , erRunKey :: Text
    , erRunStatus :: Text
    , erRunReconciliation :: Bool
    , erRunCheckpoint :: Maybe Text
    , erRunCounters :: Value
    , erRunErrorSummary :: Maybe Text
    , erRunStartedAt :: UTCTime
    , erRunUpdatedAt :: UTCTime
    , erRunFinishedAt :: Maybe UTCTime
    , erRunCreatedByPartyId :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchRunDTO
instance FromJSON EventResearchRunDTO

data EventResearchEvidenceDTO = EventResearchEvidenceDTO
    { erEvidenceUrl :: Text
    , erEvidenceKind :: Text
    , erEvidenceConsultedAt :: UTCTime
    , erEvidenceNotes :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchEvidenceDTO
instance FromJSON EventResearchEvidenceDTO where
    parseJSON = genericParseJSON defaultOptions{rejectUnknownFields = True}

data EventResearchCandidateWriteDTO = EventResearchCandidateWriteDTO
    { erCandidateProvider :: Text
    , erCandidateExternalId :: Text
    , erCandidateRunId :: Text
    , erCandidateSourceId :: Maybe Text
    , erCandidateReviewState :: Text
    , erCandidateTitle :: Text
    , erCandidateStartTime :: Maybe UTCTime
    , erCandidateEndTime :: Maybe UTCTime
    , erCandidateTimezone :: Text
    , erCandidateVenueName :: Maybe Text
    , erCandidateCity :: Maybe Text
    , erCandidateProvince :: Maybe Text
    , erCandidateCountryCode :: Text
    , erCandidateSourceUrl :: Text
    , erCandidateInfoUrl :: Maybe Text
    , erCandidatePurchaseUrl :: Maybe Text
    , erCandidatePayload :: Value
    , erCandidateEvidence :: [EventResearchEvidenceDTO]
    , erCandidateConfidence :: Text
    , erCandidateManagedFields :: [Text]
    , erCandidateVerifiedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchCandidateWriteDTO
instance FromJSON EventResearchCandidateWriteDTO where
    parseJSON = genericParseJSON defaultOptions{rejectUnknownFields = True}

data EventResearchCandidateDTO = EventResearchCandidateDTO
    { erCandidateId :: Text
    , erCandidateProvider :: Text
    , erCandidateExternalId :: Text
    , erCandidateRunId :: Text
    , erCandidateSourceId :: Maybe Text
    , erCandidateEventId :: Maybe Text
    , erCandidateReviewState :: Text
    , erCandidateTitle :: Text
    , erCandidateStartTime :: Maybe UTCTime
    , erCandidateEndTime :: Maybe UTCTime
    , erCandidateTimezone :: Text
    , erCandidateVenueName :: Maybe Text
    , erCandidateCity :: Maybe Text
    , erCandidateProvince :: Maybe Text
    , erCandidateCountryCode :: Text
    , erCandidateSourceUrl :: Text
    , erCandidateInfoUrl :: Maybe Text
    , erCandidatePurchaseUrl :: Maybe Text
    , erCandidatePayload :: Value
    , erCandidateEvidence :: [EventResearchEvidenceDTO]
    , erCandidateConfidence :: Text
    , erCandidateManagedFields :: [Text]
    , erCandidateContentHash :: Text
    , erCandidateVerifiedAt :: UTCTime
    , erCandidateIsPilot :: Bool
    , erCandidateCreatedAt :: UTCTime
    , erCandidateUpdatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchCandidateDTO
instance FromJSON EventResearchCandidateDTO

data EventResearchChangeDTO = EventResearchChangeDTO
    { erChangeId :: Text
    , erChangeRunId :: Text
    , erChangeCandidateId :: Maybe Text
    , erChangeEventId :: Maybe Text
    , erChangeAction :: Text
    , erChangeBeforeValue :: Maybe Value
    , erChangeAfterValue :: Maybe Value
    , erChangeSourceUrl :: Text
    , erChangeConfidence :: Text
    , erChangeConsultedAt :: UTCTime
    , erChangeExternalId :: Text
    , erChangeResult :: Text
    , erChangeCreatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON EventResearchChangeDTO
instance FromJSON EventResearchChangeDTO
