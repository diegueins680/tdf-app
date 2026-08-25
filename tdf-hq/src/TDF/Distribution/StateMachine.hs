{-# LANGUAGE OverloadedStrings #-}

module TDF.Distribution.StateMachine
  ( DistributionState(..)
  , DistributionGates(..)
  , EvidenceEnvironment(..)
  , RecipientEvidence(..)
  , transitionDistribution
  , splitTotalValid
  , validateRecipientEvidence
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data DistributionState
  = DistributionDraft
  | DistributionValidationFailed
  | DistributionValidated
  | DistributionReadyForReview
  | DistributionRightsReview
  | DistributionPaymentDue
  | DistributionPaid
  | DistributionScheduled
  | DistributionPackageGenerated
  | DistributionDeliveryQueued
  | DistributionSent
  | DistributionAcknowledged
  | DistributionPartiallyRejected
  | DistributionAccepted
  | DistributionLive
  | DistributionReporting
  | DistributionTakedownRequested
  | DistributionTakedownCompleted
  | DistributionArchived
  deriving (Eq, Ord, Show, Enum, Bounded)

data DistributionGates = DistributionGates
  { metadataValid      :: Bool
  , identifiersValid   :: Bool
  , assetsValid        :: Bool
  , rightsComplete     :: Bool
  , splitsAccepted     :: Bool
  , termsAccepted      :: Bool
  , commerciallyCleared :: Bool
  } deriving (Eq, Show)

data EvidenceEnvironment = EvidenceSandbox | EvidenceStaging | EvidenceProduction
  deriving (Eq, Ord, Show)

data RecipientEvidence
  = MockEvidence
  | SandboxEvidence
  | ProviderSignedEvidence
  | OperatorVerifiedEvidence Bool
  deriving (Eq, Show)

transitionDistribution
  :: DistributionGates
  -> DistributionState
  -> DistributionState
  -> Either Text DistributionState
transitionDistribution gates current next
  | not ((current, next) `elem` allowedTransitions) =
      Left ("Invalid distribution transition from " <> showState current <> " to " <> showState next)
  | next `elem` gatedStates && not (intakeReady gates) =
      Left "Distribution metadata, identifiers, assets, rights, accepted splits, or terms are incomplete"
  | next == DistributionPaid && not (commerciallyCleared gates) =
      Left "Distribution payment is not verified or explicitly waived"
  | otherwise = Right next

splitTotalValid :: [Int] -> Bool
splitTotalValid shares = not (null shares) && all (> 0) shares && sum shares == 10000

validateRecipientEvidence :: EvidenceEnvironment -> RecipientEvidence -> Either Text ()
validateRecipientEvidence EvidenceProduction MockEvidence = Left "Mock evidence cannot transition production distribution"
validateRecipientEvidence EvidenceProduction SandboxEvidence = Left "Sandbox evidence cannot transition production distribution"
validateRecipientEvidence _ (OperatorVerifiedEvidence False) = Left "Manual evidence requires an accountable operator"
validateRecipientEvidence _ _ = Right ()

intakeReady :: DistributionGates -> Bool
intakeReady gates = and
  [ metadataValid gates
  , identifiersValid gates
  , assetsValid gates
  , rightsComplete gates
  , splitsAccepted gates
  , termsAccepted gates
  ]

gatedStates :: [DistributionState]
gatedStates =
  [ DistributionValidated
  , DistributionReadyForReview
  , DistributionRightsReview
  , DistributionPaymentDue
  , DistributionPaid
  , DistributionScheduled
  , DistributionPackageGenerated
  , DistributionDeliveryQueued
  , DistributionSent
  , DistributionAcknowledged
  , DistributionPartiallyRejected
  , DistributionAccepted
  , DistributionLive
  , DistributionReporting
  ]

allowedTransitions :: [(DistributionState, DistributionState)]
allowedTransitions =
  [ (DistributionDraft, DistributionValidationFailed)
  , (DistributionDraft, DistributionValidated)
  , (DistributionDraft, DistributionArchived)
  , (DistributionValidationFailed, DistributionDraft)
  , (DistributionValidationFailed, DistributionValidated)
  , (DistributionValidated, DistributionReadyForReview)
  , (DistributionValidated, DistributionDraft)
  , (DistributionReadyForReview, DistributionRightsReview)
  , (DistributionReadyForReview, DistributionDraft)
  , (DistributionRightsReview, DistributionPaymentDue)
  , (DistributionRightsReview, DistributionReadyForReview)
  , (DistributionPaymentDue, DistributionPaid)
  , (DistributionPaymentDue, DistributionArchived)
  , (DistributionPaid, DistributionScheduled)
  , (DistributionScheduled, DistributionPackageGenerated)
  , (DistributionPackageGenerated, DistributionDeliveryQueued)
  , (DistributionDeliveryQueued, DistributionSent)
  , (DistributionSent, DistributionAcknowledged)
  , (DistributionSent, DistributionPartiallyRejected)
  , (DistributionAcknowledged, DistributionAccepted)
  , (DistributionAcknowledged, DistributionPartiallyRejected)
  , (DistributionPartiallyRejected, DistributionDeliveryQueued)
  , (DistributionPartiallyRejected, DistributionAccepted)
  , (DistributionAccepted, DistributionLive)
  , (DistributionAccepted, DistributionTakedownRequested)
  , (DistributionLive, DistributionReporting)
  , (DistributionLive, DistributionTakedownRequested)
  , (DistributionReporting, DistributionLive)
  , (DistributionReporting, DistributionTakedownRequested)
  , (DistributionTakedownRequested, DistributionTakedownCompleted)
  , (DistributionTakedownCompleted, DistributionArchived)
  ]

showState :: DistributionState -> Text
showState = T.pack . show
