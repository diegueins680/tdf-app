{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module TDF.Models where

import Database.Persist.TH
import Data.Text (Text)
import Data.Time (UTCTime)

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

-- Database schema definition
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Party
    name Text
    email Text Maybe
    phone Text Maybe
    instagram Text Maybe
    whatsapp Text Maybe
    taxId Text Maybe
    emergencyContact Text Maybe
    status PartyStatus default='Active'
    createdAt UTCTime
    updatedAt UTCTime
    UniqueEmail email !force
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
    partyId PartyId
    resourceId ResourceId
    startTime UTCTime
    endTime UTCTime
    status BookingStatus
    createdAt UTCTime
    deriving Show Eq

Resource
    name Text
    resourceType Text
    capacity Int Maybe
    deriving Show Eq
|]

-- Define BookingStatus enum
data BookingStatus = Confirmed
                   | Tentative
                   | Cancelled
                   | Completed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "BookingStatus"
