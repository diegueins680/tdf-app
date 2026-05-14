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

import           Data.Aeson (FromJSON (parseJSON), Options (rejectUnknownFields), ToJSON,
                             Value (Object), (.:!), defaultOptions, genericParseJSON)
import           Data.Aeson.Types (Parser)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
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

instance FromJSON ProposalCreate where
  parseJSON = genericParseJSON strictObjectOptions

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

instance FromJSON ProposalUpdate where
  parseJSON value@(Object o) = do
    _ <- (genericParseJSON strictObjectOptions value :: Parser ProposalUpdate)
    rawTitle <- o .:! "puTitle"
    titleVal <- case rawTitle of
      Nothing -> pure Nothing
      Just Nothing -> fail "puTitle must be omitted instead of null"
      Just (Just value') -> pure (Just value')
    rawStatus <- o .:! "puStatus"
    statusVal <- case rawStatus of
      Nothing -> pure Nothing
      Just Nothing -> fail "puStatus must be omitted instead of null"
      Just (Just value') -> pure (Just value')
    payload <-
      ProposalUpdate
        <$> pure titleVal
        <*> pure statusVal
        <*> o .:! "puServiceKind"
        <*> o .:! "puClientPartyId"
        <*> o .:! "puContactName"
        <*> o .:! "puContactEmail"
        <*> o .:! "puContactPhone"
        <*> o .:! "puPipelineCardId"
        <*> o .:! "puNotes"
    validateProposalUpdateIntent payload
    pure payload
  parseJSON _ = fail "ProposalUpdate must be an object"

validateProposalUpdateIntent :: ProposalUpdate -> Parser ()
validateProposalUpdateIntent
  (ProposalUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) =
  fail "ProposalUpdate must include at least one field"
validateProposalUpdateIntent _ =
  pure ()

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

instance FromJSON ProposalVersionCreate where
  parseJSON value = do
    payload <- genericParseJSON strictObjectOptions value
    validateProposalVersionCreateContentSource payload
    pure payload

strictObjectOptions :: Options
strictObjectOptions = defaultOptions { rejectUnknownFields = True }

validateProposalVersionCreateContentSource :: ProposalVersionCreate -> Parser ()
validateProposalVersionCreateContentSource payload = do
  hasLatex <- validateSourceField "pvcLatex" (pvcLatex payload)
  hasTemplateKey <- validateSourceField "pvcTemplateKey" (pvcTemplateKey payload)
  case (hasLatex, hasTemplateKey) of
    (True, False) -> pure ()
    (False, True) -> pure ()
    (False, False) ->
      fail "ProposalVersionCreate requires pvcLatex or pvcTemplateKey"
    (True, True) ->
      fail "ProposalVersionCreate must provide either pvcLatex or pvcTemplateKey, not both"
  where
    validateSourceField _ Nothing = pure False
    validateSourceField fieldName (Just rawValue)
      | T.null (T.strip rawValue) = fail (fieldName <> " must not be blank")
      | otherwise = pure True
