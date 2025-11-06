{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TDF.DTO where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import TDF.Models (PartyRole, PartyStatus)
import Data.Char (toLower)
import Data.List (stripPrefix)

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

-- Party DTOs
dropPrefixLower :: String -> String -> String
dropPrefixLower prefix str =
    case stripPrefix prefix str of
        Just (c:cs) -> toLower c : cs
        _ -> str

data PartyDTO = PartyDTO
    { partyId :: Int
    , partyLegalName :: Maybe Text
    , partyDisplayName :: Text
    , partyIsOrg :: Bool
    , partyTaxId :: Maybe Text
    , partyPrimaryEmail :: Maybe Text
    , partyPrimaryPhone :: Maybe Text
    , partyWhatsapp :: Maybe Text
    , partyInstagram :: Maybe Text
    , partyEmergencyContact :: Maybe Text
    , partyNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON PartyDTO where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = dropPrefixLower "party" }
instance FromJSON PartyDTO where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = dropPrefixLower "party" }

data PartyCreateDTO = PartyCreateDTO
    { cDisplayName :: Text
    , cIsOrg :: Bool
    , cLegalName :: Maybe Text
    , cPrimaryEmail :: Maybe Text
    , cPrimaryPhone :: Maybe Text
    , cWhatsapp :: Maybe Text
    , cInstagram :: Maybe Text
    , cTaxId :: Maybe Text
    , cEmergencyContact :: Maybe Text
    , cNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON PartyCreateDTO where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON PartyCreateDTO where
    toJSON = genericToJSON defaultOptions

data PartyUpdateDTO = PartyUpdateDTO
    { uDisplayName :: Maybe Text
    , uIsOrg :: Maybe Bool
    , uLegalName :: Maybe Text
    , uPrimaryEmail :: Maybe Text
    , uPrimaryPhone :: Maybe Text
    , uWhatsapp :: Maybe Text
    , uInstagram :: Maybe Text
    , uTaxId :: Maybe Text
    , uEmergencyContact :: Maybe Text
    , uNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON PartyUpdateDTO where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON PartyUpdateDTO where
    toJSON = genericToJSON defaultOptions

-- Booking DTOs
data BookingDTO = BookingDTO
    { bookingId :: Int
    , bookingTitle :: Text
    , bookingStartsAt :: Text
    , bookingEndsAt :: Text
    , bookingStatusText :: Text
    , bookingNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON BookingDTO where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = bookingFieldModifier }

instance FromJSON BookingDTO where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = bookingFieldModifier }

data BookingCreateDTO = BookingCreateDTO
    { cbTitle :: Text
    , cbStartsAt :: Text
    , cbEndsAt :: Text
    , cbStatus :: Text
    , cbNotes :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON BookingCreateDTO where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON BookingCreateDTO where
    toJSON = genericToJSON defaultOptions

bookingFieldModifier :: String -> String
bookingFieldModifier "bookingStatusText" = "status"
bookingFieldModifier field = dropPrefixLower "booking" field

-- Health check DTO
data HealthDTO = HealthDTO
    { status :: Text
    , version :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON HealthDTO
instance FromJSON HealthDTO

-- Version information DTO
data VersionDTO = VersionDTO
    { name :: Text
    , versionField :: Text
    , commit :: Maybe Text
    , buildTime :: Maybe Text
    } deriving (Show, Eq, Generic)

instance ToJSON VersionDTO where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = \s -> case s of
            "versionField" -> "version"
            _ -> s
        }

instance FromJSON VersionDTO where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case s of
            "versionField" -> "version"
            _ -> s
        }
