{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.EventResearchAPI (EventResearchRoutes) where

import Data.Text (Text)
import Servant
import TDF.DTO.EventResearchDTO

type EventResearchRoutes =
    "event-research" :> "pilot" :> Get '[JSON] EventResearchPilotDTO
        :<|> "event-research"
            :> "pilot"
            :> "approve"
            :> ReqBody '[JSON] EventResearchPilotApprovalDTO
            :> Post '[JSON] EventResearchPilotDTO
        :<|> "event-research"
            :> "runs"
            :> QueryParam "limit" Int
            :> Get '[JSON] [EventResearchRunDTO]
        :<|> "event-research"
            :> "runs"
            :> ReqBody '[JSON] EventResearchRunCreateDTO
            :> Post '[JSON] EventResearchRunDTO
        :<|> "event-research"
            :> "runs"
            :> Capture "runId" Text
            :> ReqBody '[JSON] EventResearchRunUpdateDTO
            :> Put '[JSON] EventResearchRunDTO
        :<|> "event-research"
            :> "candidates"
            :> QueryParam "provider" Text
            :> QueryParam "review_state" Text
            :> QueryParam "limit" Int
            :> Get '[JSON] [EventResearchCandidateDTO]
        :<|> "event-research"
            :> "candidates"
            :> ReqBody '[JSON] EventResearchCandidateWriteDTO
            :> Put '[JSON] EventResearchCandidateDTO
        :<|> "event-research"
            :> "changes"
            :> QueryParam "run_id" Text
            :> QueryParam "limit" Int
            :> Get '[JSON] [EventResearchChangeDTO]
