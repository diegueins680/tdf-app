{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Models where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH
import GHC.Generics (Generic)

-- | Party role enumeration
-- Represents the role of a party in the system
data PartyRole
  = AdminRole
  | ManagerRole
  | EngineerRole
  | TeacherRole
  | ReceptionRole
  | AccountingRole
  | ReadOnlyRole
  | CustomerRole
  | ArtistRole
  | StudentRole
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON PartyRole
instance FromJSON PartyRole

derivePersistField "PartyRole"

-- | Database schema definition
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Party json
  name Text
  email Text Maybe
  phone Text Maybe
  role PartyRole
  instagram Text Maybe
  whatsapp Text Maybe
  taxId Text Maybe
  emergencyContact Text Maybe
  createdAt UTCTime default=CURRENT_TIMESTAMP
  updatedAt UTCTime default=CURRENT_TIMESTAMP
  UniquePartyEmail email !force
  deriving Show Eq Generic

User json
  partyId PartyId
  passwordHash Text
  isActive Bool default=true
  lastLoginAt UTCTime Maybe
  createdAt UTCTime default=CURRENT_TIMESTAMP
  updatedAt UTCTime default=CURRENT_TIMESTAMP
  UniqueUserParty partyId
  deriving Show Eq Generic
|]
