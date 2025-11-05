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
  , uwpRole :: PartyRole
  , uwpIsActive :: Bool
  , uwpLastLoginAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON UserWithParty
instance FromJSON UserWithParty

-- | Request to update a user's role
data UpdateRoleRequest = UpdateRoleRequest
  { urrRole :: PartyRole
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRoleRequest
instance FromJSON UpdateRoleRequest

-- | Response for role update
data UpdateRoleResponse = UpdateRoleResponse
  { urrSuccess :: Bool
  , urrMessage :: Text
  , urrUser :: Maybe UserWithParty
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRoleResponse
instance FromJSON UpdateRoleResponse
