{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.EventOperations.API (EventOperationsAPI) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

import TDF.EventOperations.Types

type EventOperationsAPI = "event-operations" :> "events" :> Capture "eventId" Int64 :>
  (    Get '[JSON] EventOperationSnapshotDTO
  :<|> "transitions"
         :> Header' '[Required, Strict] "Idempotency-Key" UUID
         :> ReqBody '[JSON] EventTransitionCommand
         :> Post '[JSON] EventTransitionOutcomeDTO
  :<|> "tasks" :> Capture "activityId" Int64
       :> Get '[JSON] (Headers '[Header "Cache-Control" Text] EventOperationTaskDTO)
  :<|> "tasks" :> Capture "activityId" Int64 :> "revisioned"
       :> Get '[JSON] (Headers '[Header "Cache-Control" Text] EventOperationTaskWithRevisionDTO)
  :<|> "tasks" :> Capture "activityId" Int64 :> "raci" :> "reassign"
       :> Header' '[Required, Strict] "Idempotency-Key" UUID
       :> ReqBody '[JSON] EventRaciReassignmentCommand
       :> Post '[JSON] (Headers '[Header "Cache-Control" Text] EventRaciReassignmentOutcomeDTO)
  :<|> "tasks" :> Capture "activityId" Int64 :> "raci" :> "context"
       :> QueryParam "afterPartyId" Int64
       :> Get '[JSON] (Headers '[Header "Cache-Control" Text] EventRaciEditorContextDTO)
  :<|> "tasks" :> Capture "activityId" Int64 :> "complete"
       :> Header' '[Required, Strict] "Idempotency-Key" UUID
       :> ReqBody '[JSON] EventTaskCompletionCommand
       :> Post '[JSON] (Headers '[Header "Cache-Control" Text] EventTaskCompletionOutcomeDTO)
  )
