{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import TDF.Models (PartyRole, PartyStatus)

-- DTO for user information with roles
data UserDTO = UserDTO
    { userId :: Int
    , userName :: Text
    , userEmail :: Maybe Text
    , userPhone :: Maybe Text
    , userRoles :: [PartyRole]
    , userStatus :: PartyStatus
    , userCreatedAt :: UTCTime
    } deriving (Show, Eq, Generic)

instance ToJSON UserDTO where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = \s -> case s of
            "userId" -> "id"
            "userName" -> "name"
            "userEmail" -> "email"
            "userPhone" -> "phone"
            "userRoles" -> "roles"
            "userStatus" -> "status"
            "userCreatedAt" -> "createdAt"
            _ -> s
        }

instance FromJSON UserDTO where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case s of
            "userId" -> "id"
            "userName" -> "name"
            "userEmail" -> "email"
            "userPhone" -> "phone"
            "userRoles" -> "roles"
            "userStatus" -> "status"
            "userCreatedAt" -> "createdAt"
            _ -> s
        }

-- DTO for updating user roles
data UserRoleUpdateDTO = UserRoleUpdateDTO
    { roles :: [PartyRole]
    } deriving (Show, Eq, Generic)

instance ToJSON UserRoleUpdateDTO
instance FromJSON UserRoleUpdateDTO
