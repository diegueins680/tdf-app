{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.Admin where

import           Data.Text     (Text)
import           Data.Time     (UTCTime)
import           Servant

import           TDF.API.Types ( DropdownOptionCreate
                                , DropdownOptionDTO
                                , DropdownOptionUpdate
                                , RoleDetailDTO
                                , UserAccountCreate
                                , UserAccountDTO
                                , UserAccountUpdate
                                )
import           Data.Aeson (FromJSON(..), ToJSON, Value(..), withObject, (.:?))
import           Data.Aeson.Types (Parser)
import           GHC.Generics (Generic)
import           Data.Int      (Int64)
import           TDF.DTO       (ArtistProfileDTO, ArtistProfileUpsert, ArtistReleaseDTO, ArtistReleaseUpsert, LogEntryDTO)

type DropdownCategoryAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [DropdownOptionDTO]
  :<|> ReqBody '[JSON] DropdownOptionCreate :> PostCreated '[JSON] DropdownOptionDTO
  :<|> Capture "optionId" Text :> ReqBody '[JSON] DropdownOptionUpdate :> Patch '[JSON] DropdownOptionDTO

type UsersAPI =
       QueryParam "includeInactive" Bool :> Get '[JSON] [UserAccountDTO]
  :<|> ReqBody '[JSON] UserAccountCreate :> PostCreated '[JSON] UserAccountDTO
  :<|> Capture "userId" Int64 :>
        ( Get '[JSON] UserAccountDTO
     :<|> ReqBody '[JSON] UserAccountUpdate :> Patch '[JSON] UserAccountDTO
        )

type RolesAPI = Get '[JSON] [RoleDetailDTO]

type ArtistAdminAPI =
       "profiles" :>
         ( Get '[JSON] [ArtistProfileDTO]
       :<|> ReqBody '[JSON] ArtistProfileUpsert :> Post '[JSON] ArtistProfileDTO
         )
  :<|> "releases" :>
         ( ReqBody '[JSON] ArtistReleaseUpsert :> Post '[JSON] ArtistReleaseDTO
       :<|> Capture "releaseId" Int64 :> ReqBody '[JSON] ArtistReleaseUpsert :> Put '[JSON] ArtistReleaseDTO
         )

type LogsAPI =
       QueryParam "limit" Int :> Get '[JSON] [LogEntryDTO]
  :<|> Delete '[JSON] NoContent

type BrainAdminAPI =
       "brain" :> "entries"
         :> QueryParam "includeInactive" Bool
         :> Get '[JSON] [BrainEntryDTO]
  :<|> "brain" :> "entries"
         :> ReqBody '[JSON] BrainEntryCreate
         :> PostCreated '[JSON] BrainEntryDTO
  :<|> "brain" :> "entries" :> Capture "entryId" Int64
         :> ReqBody '[JSON] BrainEntryUpdate
         :> Patch '[JSON] BrainEntryDTO

type RagAdminAPI =
       "rag" :> "status" :> Get '[JSON] RagIndexStatus
  :<|> "rag" :> "refresh" :> Post '[JSON] RagRefreshResponse

type SocialAdminAPI =
       "social" :> "unhold" :> ReqBody '[JSON] SocialUnholdRequest :> Post '[JSON] Value
  :<|> "social" :> "status" :> Get '[JSON] Value

type AdminAPI =
       "seed" :> Post '[JSON] NoContent
  :<|> "dropdowns" :> Capture "category" Text :> DropdownCategoryAPI
  :<|> "users" :> UsersAPI
  :<|> "roles" :> RolesAPI
  :<|> "artists" :> ArtistAdminAPI
  :<|> "logs" :> LogsAPI
  :<|> "email-test" :> ReqBody '[JSON] EmailTestRequest :> Post '[JSON] EmailTestResponse
  :<|> BrainAdminAPI
  :<|> RagAdminAPI
  :<|> SocialAdminAPI

-- | Unhold a social inbound message so the auto-reply worker can retry.
-- One of externalId/ids should be provided.
data SocialUnholdRequest = SocialUnholdRequest
  { surChannel    :: Text  -- instagram|facebook|whatsapp
  , surExternalId :: Maybe Text
  , surSenderId   :: Maybe Text
  , surNote       :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON SocialUnholdRequest


data EmailTestRequest = EmailTestRequest
  { etrEmail   :: Text
  , etrName    :: Maybe Text
  , etrSubject :: Maybe Text
  , etrBody    :: Maybe Text
  , etrCtaUrl  :: Maybe Text
  } deriving (Show, Generic)
instance FromJSON EmailTestRequest

data EmailTestResponse = EmailTestResponse
  { status  :: Text
  , message :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON EmailTestResponse

data BrainEntryDTO = BrainEntryDTO
  { bedId        :: Int64
  , bedTitle     :: Text
  , bedBody      :: Text
  , bedCategory  :: Maybe Text
  , bedTags      :: [Text]
  , bedActive    :: Bool
  , bedUpdatedAt :: UTCTime
  } deriving (Show, Generic)
instance ToJSON BrainEntryDTO

data BrainEntryCreate = BrainEntryCreate
  { becTitle    :: Text
  , becBody     :: Text
  , becCategory :: Maybe Text
  , becTags     :: Maybe [Text]
  , becActive   :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON BrainEntryCreate

data BrainEntryUpdate = BrainEntryUpdate
  { beuTitle    :: Maybe Text
  , beuBody     :: Maybe Text
  , beuCategory :: Maybe (Maybe Text)
  , beuTags     :: Maybe [Text]
  , beuActive   :: Maybe Bool
  } deriving (Show, Generic)
instance FromJSON BrainEntryUpdate where
  parseJSON = withObject "BrainEntryUpdate" $ \o -> do
    titleVal <- o .:? "beuTitle"
    bodyVal <- o .:? "beuBody"
    mCategory <- (o .:? "beuCategory" :: Parser (Maybe Value))
    categoryVal <- case mCategory of
      Nothing -> pure Nothing
      Just Null -> pure (Just Nothing)
      Just value -> Just <$> parseJSON value
    tagsVal <- o .:? "beuTags"
    activeVal <- o .:? "beuActive"
    pure BrainEntryUpdate
      { beuTitle = titleVal
      , beuBody = bodyVal
      , beuCategory = categoryVal
      , beuTags = tagsVal
      , beuActive = activeVal
      }

data RagIndexStatus = RagIndexStatus
  { risCount     :: Int
  , risUpdatedAt :: Maybe UTCTime
  , risStale     :: Bool
  } deriving (Show, Generic)
instance ToJSON RagIndexStatus

data RagRefreshResponse = RagRefreshResponse
  { rrrStatus :: Text
  , rrrChunks :: Int
  } deriving (Show, Generic)
instance ToJSON RagRefreshResponse
