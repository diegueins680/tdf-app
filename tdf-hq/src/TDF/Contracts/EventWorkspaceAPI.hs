{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.Contracts.EventWorkspaceAPI (EventWorkspaceAPI, WorkspaceJSON, WorkspacePDF) where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.UUID (UUID)
import Servant

type WorkspaceJSON = Headers '[Header "Cache-Control" Text] Value
type WorkspacePDF = Headers '[Header "Cache-Control" Text, Header "Content-Disposition" Text] BL.ByteString
type EventWorkspaceAPI = "events" :> Capture "eventId" Integer :> "workspace" :>
  ( Get '[JSON] WorkspaceJSON
  :<|> ReqBody '[JSON] Value :> Post '[JSON] WorkspaceJSON
  :<|> "plan.pdf" :> Get '[OctetStream] WorkspacePDF
  :<|> "agreements" :> Capture "agreementId" UUID :> Capture "version" Int :> "document.pdf" :> Get '[OctetStream] WorkspacePDF
  :<|> "agreements" :> Capture "agreementId" UUID :> "acceptance.pdf" :> Get '[OctetStream] WorkspacePDF
  )
