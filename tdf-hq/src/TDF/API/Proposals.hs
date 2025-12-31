{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.Proposals
  ( ProposalsAPI
  , ProposalDTO(..)
  , ProposalCreate(..)
  , ProposalUpdate(..)
  , ProposalVersionSummaryDTO(..)
  , ProposalVersionDTO(..)
  , ProposalVersionCreate(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Servant
import qualified Data.ByteString.Lazy as BL

import           TDF.Models (ServiceKind)

type ProposalsAPI =
  "proposals" :>
    (    Get '[JSON] [ProposalDTO]
    :<|> ReqBody '[JSON] ProposalCreate :> PostCreated '[JSON] ProposalDTO
    :<|> Capture "proposalId" Text :>
          (    Get '[JSON] ProposalDTO
          :<|> ReqBody '[JSON] ProposalUpdate :> Patch '[JSON] ProposalDTO
          :<|> "versions" :> Get '[JSON] [ProposalVersionSummaryDTO]
          :<|> "versions" :> ReqBody '[JSON] ProposalVersionCreate :> PostCreated '[JSON] ProposalVersionDTO
          :<|> "versions" :> Capture "version" Int :> Get '[JSON] ProposalVersionDTO
          :<|> "pdf" :> QueryParam "version" Int
              :> Get '[OctetStream]
                   (Headers '[Header "Content-Disposition" Text] BL.ByteString)
          )
    )

data ProposalDTO = ProposalDTO
  { proposalId      :: Text
  , title           :: Text
  , status          :: Text
  , serviceKind     :: Maybe ServiceKind
  , clientPartyId   :: Maybe Int64
  , contactName     :: Maybe Text
  , contactEmail    :: Maybe Text
  , contactPhone    :: Maybe Text
  , pipelineCardId  :: Maybe Text
  , notes           :: Maybe Text
  , createdAt       :: UTCTime
  , updatedAt       :: UTCTime
  , lastGeneratedAt :: Maybe UTCTime
  , sentAt          :: Maybe UTCTime
  , latestVersion   :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON ProposalDTO

data ProposalCreate = ProposalCreate
  { pcTitle          :: Text
  , pcStatus         :: Maybe Text
  , pcServiceKind    :: Maybe ServiceKind
  , pcClientPartyId  :: Maybe Int64
  , pcContactName    :: Maybe Text
  , pcContactEmail   :: Maybe Text
  , pcContactPhone   :: Maybe Text
  , pcPipelineCardId :: Maybe Text
  , pcNotes          :: Maybe Text
  , pcLatex          :: Maybe Text
  , pcTemplateKey    :: Maybe Text
  , pcVersionNotes   :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ProposalCreate

data ProposalUpdate = ProposalUpdate
  { puTitle          :: Maybe Text
  , puStatus         :: Maybe Text
  , puServiceKind    :: Maybe (Maybe ServiceKind)
  , puClientPartyId  :: Maybe (Maybe Int64)
  , puContactName    :: Maybe (Maybe Text)
  , puContactEmail   :: Maybe (Maybe Text)
  , puContactPhone   :: Maybe (Maybe Text)
  , puPipelineCardId :: Maybe (Maybe Text)
  , puNotes          :: Maybe (Maybe Text)
  } deriving (Show, Generic)

instance FromJSON ProposalUpdate

data ProposalVersionSummaryDTO = ProposalVersionSummaryDTO
  { versionId :: Text
  , version   :: Int
  , createdAt :: UTCTime
  , createdBy :: Maybe Text
  , notes     :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ProposalVersionSummaryDTO

data ProposalVersionDTO = ProposalVersionDTO
  { versionId  :: Text
  , proposalId :: Text
  , version    :: Int
  , latex      :: Text
  , createdAt  :: UTCTime
  , createdBy  :: Maybe Text
  , notes      :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON ProposalVersionDTO

data ProposalVersionCreate = ProposalVersionCreate
  { pvcLatex       :: Maybe Text
  , pvcTemplateKey :: Maybe Text
  , pvcNotes       :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON ProposalVersionCreate
