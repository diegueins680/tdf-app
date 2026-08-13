{-# LANGUAGE OverloadedStrings #-}

module TDF.Operations.Model
  ( TransitionContext(..)
  , TransitionError(..)
  , allowedTargets
  , validateTransition
  , canViewEntityType
  , requiresTwoPersonApproval
  , recommendedPriority
  , prioritySlaMinutes
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

import TDF.Models (RoleEnum(..))
import TDF.Operations.Types (WorkPriority(..), WorkStatus(..))

data TransitionContext = TransitionContext
  { currentStatus :: WorkStatus
  , targetStatus :: WorkStatus
  , actorRoles :: [RoleEnum]
  , hasAssignee :: Bool
  , reason :: Maybe Text
  , waitingExternalDependency :: Maybe Bool
  , resumeAtPresent :: Bool
  } deriving (Show, Eq)

data TransitionError
  = TransitionNotAllowed
  | TransitionRoleRequired
  | TransitionReasonRequired
  | TransitionAssigneeRequired
  | WaitingDependencyClassificationRequired
  deriving (Show, Eq)

allowedTargets :: WorkStatus -> [WorkStatus]
allowedTargets WorkNew = [WorkSeen, WorkAssigned, WorkInProgress, WorkResolved]
allowedTargets WorkSeen = [WorkAssigned, WorkInProgress, WorkWaiting, WorkResolved]
allowedTargets WorkAssigned = [WorkInProgress, WorkWaiting, WorkResolved]
allowedTargets WorkInProgress = [WorkAssigned, WorkWaiting, WorkResolved]
allowedTargets WorkWaiting = [WorkInProgress, WorkAssigned, WorkResolved]
allowedTargets WorkResolved = [WorkInProgress, WorkArchived]
allowedTargets WorkArchived = [WorkInProgress]

validateTransition :: TransitionContext -> Either TransitionError ()
validateTransition context
  | targetStatus context `notElem` allowedTargets (currentStatus context) =
      Left TransitionNotAllowed
  | not (canOperate (actorRoles context)) =
      Left TransitionRoleRequired
  | targetStatus context == WorkAssigned && not (hasAssignee context) =
      Left TransitionAssigneeRequired
  | targetStatus context `elem` [WorkWaiting, WorkResolved, WorkArchived]
      && missingReason (reason context) =
      Left TransitionReasonRequired
  | targetStatus context == WorkWaiting && waitingExternalDependency context == Nothing =
      Left WaitingDependencyClassificationRequired
  | otherwise = Right ()
  where
    missingReason = maybe True (T.null . T.strip)

canOperate :: [RoleEnum] -> Bool
canOperate roles = any (`elem` roles)
  [ Admin, Manager, StudioManager, Accounting, Reception, Teacher, Engineer
  , LiveSessionsProducer, Producer, AandR, Maintenance
  ]

canViewEntityType :: [RoleEnum] -> Bool -> Text -> Bool
canViewEntityType roles assignedToActor entityType
  | entityType == "security_incident" = Admin `elem` roles
  | Admin `elem` roles || Manager `elem` roles || StudioManager `elem` roles = True
  | Accounting `elem` roles = entityType `elem`
      ["invoice", "payment", "marketplace_order", "course_registration"]
  | Reception `elem` roles = entityType `elem`
      ["course_registration", "booking", "party", "invoice", "payment", "manual", "uncorrelated_inbound", "proposal", "social_event"]
  | Teacher `elem` roles || Engineer `elem` roles = assignedToActor && entityType `elem`
      ["course_registration", "booking", "maintenance_ticket", "manual", "social_event", "intern_project"]
  | Maintenance `elem` roles = entityType `elem` ["maintenance_ticket", "stock_item", "booking", "manual"]
  | otherwise = False

requiresTwoPersonApproval :: Text -> Maybe Int64 -> Int64 -> Bool
requiresTwoPersonApproval rawAction threshold amount =
  normalizedAction `elem`
    [ "refund", "payment_reversal", "payment_void", "chargeback_resolution"
    , "cancel_paid_reservation", "cancel_near_term_reservation"
    , "credit_note", "debit_note", "modify_issued_document"
    , "privacy_erasure", "permanent_deletion"
    ] || maybe False (amount >=) threshold
  where
    normalizedAction = T.toLower (T.strip rawAction)

recommendedPriority :: Text -> Bool -> Bool -> WorkPriority
recommendedPriority rawEvent nearTerm affectsMultiple
  | securityOrOutage || affectsMultiple = Urgent
  | nearTerm || highRisk = High
  | otherwise = Normal
  where
    eventType = T.toLower rawEvent
    securityOrOutage = any (`T.isInfixOf` eventType)
      ["security", "unauthorized", "service_outage", "payment_failure_multiple"]
    highRisk = any (`T.isInfixOf` eventType)
      ["payment.failed", "payment.dispute", "invoice.overdue", "registration", "lead", "conflict", "maintenance"]

-- (acknowledge, mitigate, resolve) in SLA minutes. Non-urgent values are
-- business minutes; urgent values are continuous elapsed minutes.
prioritySlaMinutes :: WorkPriority -> (Int, Int, Int)
prioritySlaMinutes Urgent = (15, 60, 240)
prioritySlaMinutes High = (60, 60, 480)
prioritySlaMinutes Normal = (240, 240, 1440)
prioritySlaMinutes Low = (480, 480, 2400)
