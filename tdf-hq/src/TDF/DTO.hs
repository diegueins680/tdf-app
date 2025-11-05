{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import TDF.Models (PartyRole, UserId, PartyId)
import Database.Persist (Entity(..), fromSqlKey, toSqlKey)

-- | User with Party information for API responses
data UserWithParty = UserWithParty
  { uwpUserId :: Int
  , uwpEmail :: Maybe Text
  , uwpName :: Text
  , uwpRoles :: [PartyRole]
  , uwpIsActive :: Bool
  , uwpLastLoginAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON UserWithParty
instance FromJSON UserWithParty

-- | Request to update a user's roles
data UpdateRolesRequest = UpdateRolesRequest
  { urrRoles :: [PartyRole]
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRolesRequest
instance FromJSON UpdateRolesRequest

-- | Response for role update
data UpdateRoleResponse = UpdateRoleResponse
  { urrSuccess :: Bool
  , urrMessage :: Text
  , urrUser :: Maybe UserWithParty
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRoleResponse
instance FromJSON UpdateRoleResponse
