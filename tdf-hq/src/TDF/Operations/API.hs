{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Operations.API (OperationsAPI) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

import TDF.Operations.Types

type WorkItemFilters api =
       QueryParam "cursor" Text
  :> QueryParam "limit" Int
  :> QueryParam "q" Text
  :> QueryParam "seen" Bool
  :> QueryParam "entityType" Text
  :> QueryParam "status" Text
  :> QueryParam "priority" Text
  :> QueryParam "slaState" Text
  :> QueryParam "assigneePartyId" Int64
  :> QueryParam "customerPartyId" Int64
  :> QueryParam "service" Text
  :> QueryParam "from" Text
  :> QueryParam "to" Text
  :> QueryParam "minAmountMinor" Int64
  :> QueryParam "maxAmountMinor" Int64
  :> QueryParam "paymentState" Text
  :> QueryParam "organizationId" UUID
  :> QueryParam "branchId" UUID
  :> QueryParam "sourceChannel" Text
  :> api

type OperationsAPI = "operations" :>
  (    "metrics"
         :> QueryParam "organizationId" UUID
         :> QueryParam "branchId" UUID
         :> Get '[JSON] OperationsMetricsDTO
  :<|> "work-items" :> WorkItemFilters (Get '[JSON] WorkItemPageDTO)
  :<|> "work-items" :> ReqBody '[JSON] ManualWorkItemCreate :> PostCreated '[JSON] WorkItemDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> Get '[JSON] WorkItemDetailDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> "seen"
         :> ReqBody '[JSON] VersionedCommand :> Patch '[JSON] WorkItemDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> "transition"
         :> ReqBody '[JSON] TransitionCommand :> Patch '[JSON] WorkItemDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> "assignment"
         :> ReqBody '[JSON] AssignmentCommand :> Patch '[JSON] WorkItemDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> "priority"
         :> ReqBody '[JSON] PriorityCommand :> Patch '[JSON] WorkItemDTO
  :<|> "work-items" :> Capture "workItemId" UUID :> "notes"
         :> ReqBody '[JSON] NoteCreate :> PostCreated '[JSON] WorkItemNoteDTO
  :<|> "approvals" :> ReqBody '[JSON] ApprovalCreate :> PostCreated '[JSON] ApprovalDTO
  :<|> "approvals" :> Capture "approvalId" UUID :> "decision"
         :> ReqBody '[JSON] ApprovalDecision :> Patch '[JSON] ApprovalDTO
  :<|> "integration-failures"
         :> QueryParam "organizationId" UUID
         :> QueryParam "status" Text
         :> QueryParam "limit" Int
         :> Get '[JSON] [IntegrationFailureDTO]
  :<|> "integration-failures" :> Capture "failureId" UUID :> "replay"
         :> ReqBody '[JSON] ReplayCommand :> PostAccepted '[JSON] IntegrationFailureDTO
  :<|> "events"
         :> QueryParam "afterId" Int64
         :> QueryParam "limit" Int
         :> QueryParam "organizationId" UUID
         :> Get '[JSON] StreamBatchDTO
  :<|> "saved-views"
         :> QueryParam "organizationId" UUID
         :> Get '[JSON] [SavedViewDTO]
  :<|> "saved-views"
         :> ReqBody '[JSON] SavedViewCreate
         :> PostCreated '[JSON] SavedViewDTO
  :<|> "push-subscriptions"
         :> ReqBody '[JSON] PushSubscriptionCreate
         :> PostCreated '[JSON] PushSubscriptionDTO
  )
