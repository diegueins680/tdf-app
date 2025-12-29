{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Internships where

import           Data.Int (Int64)
import           Data.Text (Text)
import           Servant

import           TDF.API.Types

type InternshipsAPI =
  "internships" :>
    (    "interns" :> Get '[JSON] [InternSummaryDTO]
    :<|> "profile" :> Get '[JSON] InternProfileDTO
    :<|> "profile" :> ReqBody '[JSON] InternProfileUpdate :> Patch '[JSON] InternProfileDTO
    :<|> "projects" :>
            (    Get '[JSON] [InternProjectDTO]
            :<|> ReqBody '[JSON] InternProjectCreate :> PostCreated '[JSON] InternProjectDTO
            )
    :<|> "projects" :> Capture "projectId" Text
            :> ReqBody '[JSON] InternProjectUpdate :> Patch '[JSON] InternProjectDTO
    :<|> "tasks" :>
            (    Get '[JSON] [InternTaskDTO]
            :<|> ReqBody '[JSON] InternTaskCreate :> PostCreated '[JSON] InternTaskDTO
            )
    :<|> "tasks" :> Capture "taskId" Text
            :> ReqBody '[JSON] InternTaskUpdate :> Patch '[JSON] InternTaskDTO
    :<|> "todos" :>
            (    Get '[JSON] [InternTodoDTO]
            :<|> ReqBody '[JSON] InternTodoCreate :> PostCreated '[JSON] InternTodoDTO
            :<|> Capture "todoId" Text :> ReqBody '[JSON] InternTodoUpdate :> Patch '[JSON] InternTodoDTO
            :<|> Capture "todoId" Text :> DeleteNoContent
            )
    :<|> "time-entries" :> QueryParam "partyId" Int64 :> Get '[JSON] [InternTimeEntryDTO]
    :<|> "time-entries" :> "clock-in" :> ReqBody '[JSON] ClockInRequest :> Post '[JSON] InternTimeEntryDTO
    :<|> "time-entries" :> "clock-out" :> ReqBody '[JSON] ClockOutRequest :> Post '[JSON] InternTimeEntryDTO
    :<|> "permissions" :>
            (    Get '[JSON] [InternPermissionDTO]
            :<|> ReqBody '[JSON] InternPermissionCreate :> PostCreated '[JSON] InternPermissionDTO
            :<|> Capture "permissionId" Text :> ReqBody '[JSON] InternPermissionUpdate :> Patch '[JSON] InternPermissionDTO
            )
    )
