{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}

module TDF.Models where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- Define the PartyRole enum
data PartyRole = Admin
               | Manager
               | Engineer
               | Teacher
               | Reception
               | Accounting
               | Artist
               | Student
               | ReadOnly
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "PartyRole"

-- Define the PartyStatus enum
data PartyStatus = Active
                 | Inactive
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "PartyStatus"

-- Define BookingStatus enum
data BookingStatus = Confirmed
                   | Tentative
                   | Cancelled
                   | Completed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "BookingStatus"

enumToJSON :: Show a => a -> Value
enumToJSON = String . T.pack . show

parseEnumValue :: (MonadFail m, Read a) => String -> T.Text -> m a
parseEnumValue label t =
    maybe (fail $ "Invalid " <> label <> ": " <> T.unpack t) pure (readMaybe $ T.unpack t)

instance ToJSON PartyRole where
    toJSON = enumToJSON

instance FromJSON PartyRole where
    parseJSON = withText "PartyRole" (parseEnumValue "PartyRole")

instance ToJSON PartyStatus where
    toJSON = enumToJSON

instance FromJSON PartyStatus where
    parseJSON = withText "PartyStatus" (parseEnumValue "PartyStatus")

instance ToJSON BookingStatus where
    toJSON = enumToJSON

instance FromJSON BookingStatus where
    parseJSON = withText "BookingStatus" (parseEnumValue "BookingStatus")

-- Database schema definition
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Party
    displayName Text
    legalName Text Maybe
    isOrg Bool
    primaryEmail Text Maybe
    primaryPhone Text Maybe
    whatsapp Text Maybe
    instagram Text Maybe
    taxId Text Maybe
    emergencyContact Text Maybe
    notes Text Maybe
    status PartyStatus default='Active'
    createdAt UTCTime
    updatedAt UTCTime
    UniquePrimaryEmail primaryEmail !force
    deriving Show Eq

PartyRoleAssignment
    partyId PartyId
    role PartyRole
    assignedAt UTCTime
    assignedBy PartyId Maybe
    UniquePartyRole partyId role
    deriving Show Eq

Package
    name Text
    description Text Maybe
    serviceType Text
    priceUsd Int
    hoursQty Int
    expiresDays Int
    transferable Bool
    createdAt UTCTime
    deriving Show Eq

Booking
    title Text
    partyId PartyId Maybe
    resourceId ResourceId Maybe
    serviceType Text Maybe
    startTime UTCTime
    endTime UTCTime
    status BookingStatus
    notes Text Maybe
    createdAt UTCTime
    deriving Show Eq

Resource
    name Text
    resourceType Text
    capacity Int Maybe
    deriving Show Eq
|]
