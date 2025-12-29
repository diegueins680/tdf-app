{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

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
import           Data.Aeson (FromJSON, ToJSON)
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

type RagAdminAPI =
       "rag" :> "status" :> Get '[JSON] RagIndexStatus
  :<|> "rag" :> "refresh" :> Post '[JSON] RagRefreshResponse

type AdminAPI =
       "seed" :> Post '[JSON] NoContent
  :<|> "dropdowns" :> Capture "category" Text :> DropdownCategoryAPI
  :<|> "users" :> UsersAPI
  :<|> "roles" :> RolesAPI
  :<|> "artists" :> ArtistAdminAPI
  :<|> "logs" :> LogsAPI
  :<|> "email-test" :> ReqBody '[JSON] EmailTestRequest :> Post '[JSON] EmailTestResponse
  :<|> RagAdminAPI

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
