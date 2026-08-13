{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Pipelines where

import           Data.Text (Text)
import           Servant

import           TDF.API.Types

type PipelinesAPI =
  "pipelines" :>
    (    "snapshot" :> Get '[JSON] PipelineSnapshotDTO
     :<|> "definitions" :> Get '[JSON] [PipelineDefinitionDTO]
     :<|> Capture "workflowId" Text :>
           (    Get '[JSON] [PipelineCardDTO]
            :<|> "stages" :> Get '[JSON] [PipelineStageDTO]
            :<|> ReqBody '[JSON] PipelineCardCreate :> PostCreated '[JSON] PipelineCardDTO
            :<|> Capture "cardId" Text :>
                  (    Get '[JSON] PipelineCardDTO
                   :<|> ReqBody '[JSON] PipelineCardUpdate :> Patch '[JSON] PipelineCardDTO
                   :<|> DeleteNoContent
                  )
           )
    )
