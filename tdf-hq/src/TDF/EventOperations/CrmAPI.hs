{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module TDF.EventOperations.CrmAPI (EventCrmAPI, InviteToken(..), EmailProof(..)) where

import Data.Aeson (FromJSON(..), Value, withObject, (.:))
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant

data InviteToken = InviteToken { invitationToken :: Text, acceptInvitation :: Bool }
  deriving (Generic)
-- Deliberately no Show instance: secrets must not enter diagnostic logs.
instance FromJSON InviteToken where
  parseJSON = withObject "Invitation" $ \o -> do
    if KM.size o /= 2 then fail "invalid invitation request" else pure ()
    InviteToken <$> o .: "token" <*> o .: "accept"
newtype EmailProof = EmailProof { emailProofToken :: Text }
instance FromJSON EmailProof where
  parseJSON = withObject "EmailProof" $ \o -> do
    if KM.size o /= 1 then fail "invalid proof request" else pure ()
    EmailProof <$> o .: "token"

type PrivateResult = Headers '[Header "Cache-Control" Text] Value
type EventCrmAPI = "event-crm" :>
  ( "events" :> Capture "eventId" Int64 :>
      ( QueryParam "stage" Text :> QueryParam "responsibleId" Int64
          :> QueryParam "overdue" Bool :> Get '[JSON] PrivateResult
      :<|> "commands" :> Header' '[Required, Strict] "Idempotency-Key" UUID
          :> ReqBody '[JSON] Value :> Post '[JSON] PrivateResult
      :<|> "export" :> Get '[JSON] PrivateResult
      )
  :<|> "invitations" :> ReqBody '[JSON] InviteToken :> Post '[JSON] PrivateResult
  :<|> "email" :> "request" :> Post '[JSON] PrivateResult
  :<|> "email" :> "confirm" :> ReqBody '[JSON] EmailProof :> Post '[JSON] PrivateResult
  )
