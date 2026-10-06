{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.RecordsIngestion (RecordsIngestionAPI, SourceRequest(..), RunRequest(..), ControlRequest(..)) where
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

-- Requests cannot carry provider payloads, thumbnail URLs, access tokens or
-- arbitrary network destinations. Source verification always runs server-side.
data SourceRequest = SourceRequest
  { channelId :: Text, enabled :: Bool, approvalReference :: Maybe Text
  , collectionId :: Text, partyId :: Maybe Int64, artistProfileId :: Maybe Int64
  } deriving (Generic, Show)
instance FromJSON SourceRequest where parseJSON = genericParseJSON defaultOptions{rejectUnknownFields=True}
data RunRequest = RunRequest
  { sourceAccountId :: Int64, executionKey :: Text, reconciliation :: Bool, dryRun :: Bool
  } deriving (Generic, Show)
instance FromJSON RunRequest where parseJSON = genericParseJSON defaultOptions{rejectUnknownFields=True}
data ControlRequest = ControlRequest { running :: Bool, intervalSeconds :: Int }
  deriving (Generic, Show)
instance FromJSON ControlRequest where parseJSON = genericParseJSON defaultOptions{rejectUnknownFields=True}

type RecordsIngestionAPI =
       Get '[JSON] Value
  :<|> "sources" :> ReqBody '[JSON] SourceRequest :> Put '[JSON] Value
  :<|> "control" :> ReqBody '[JSON] ControlRequest :> Put '[JSON] Value
  :<|> "runs" :> ReqBody '[JSON] RunRequest :> Post '[JSON] Value

