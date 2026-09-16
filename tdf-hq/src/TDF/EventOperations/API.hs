{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.EventOperations.API (EventOperationsAPI) where

import Data.Int (Int64)
import Data.UUID (UUID)
import Servant

import TDF.EventOperations.Types

type EventOperationsAPI = "event-operations" :> "events" :> Capture "eventId" Int64 :>
  (    Get '[JSON] EventOperationSnapshotDTO
  :<|> "transitions"
         :> Header' '[Required, Strict] "Idempotency-Key" UUID
         :> ReqBody '[JSON] EventTransitionCommand
         :> Post '[JSON] EventTransitionOutcomeDTO
  )
