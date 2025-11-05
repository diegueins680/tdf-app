{-# LANGUAGE OverloadedStrings #-}

module TDF.Seed where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Data.Text (Text)
import Data.Time (getCurrentTime)
import TDF.Models
import TDF.DB (DB)

-- | Seed sample data for development
seedData :: DB ()
seedData = do
  now <- liftIO getCurrentTime
  
  -- Create sample parties with different roles
  party1 <- insert $ Party
    { partyName = "John Admin"
    , partyEmail = Just "admin@tdfrecords.com"
    , partyPhone = Just "+593-99-123-4567"
    , partyRole = AdminRole
    , partyInstagram = Just "@johnadmin"
    , partyWhatsapp = Just "+593991234567"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  party2 <- insert $ Party
    { partyName = "Maria Manager"
    , partyEmail = Just "maria@tdfrecords.com"
    , partyPhone = Just "+593-99-234-5678"
    , partyRole = ManagerRole
    , partyInstagram = Just "@mariamanager"
    , partyWhatsapp = Just "+593992345678"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  party3 <- insert $ Party
    { partyName = "Carlos Engineer"
    , partyEmail = Just "carlos@tdfrecords.com"
    , partyPhone = Just "+593-99-345-6789"
    , partyRole = EngineerRole
    , partyInstagram = Just "@carlosengineer"
    , partyWhatsapp = Just "+593993456789"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  party4 <- insert $ Party
    { partyName = "Ana Teacher"
    , partyEmail = Just "ana@tdfrecords.com"
    , partyPhone = Just "+593-99-456-7890"
    , partyRole = TeacherRole
    , partyInstagram = Just "@anateacher"
    , partyWhatsapp = Just "+593994567890"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  party5 <- insert $ Party
    { partyName = "Luis Reception"
    , partyEmail = Just "luis@tdfrecords.com"
    , partyPhone = Just "+593-99-567-8901"
    , partyRole = ReceptionRole
    , partyInstagram = Nothing
    , partyWhatsapp = Just "+593995678901"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  -- Create corresponding users
  _ <- insert $ User
    { userPartyId = party1
    , userPasswordHash = "$2b$10$dummyhashforadmin"
    , userIsActive = True
    , userLastLoginAt = Just now
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  _ <- insert $ User
    { userPartyId = party2
    , userPasswordHash = "$2b$10$dummyhashformanager"
    , userIsActive = True
    , userLastLoginAt = Just now
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  _ <- insert $ User
    { userPartyId = party3
    , userPasswordHash = "$2b$10$dummyhashforengineer"
    , userIsActive = True
    , userLastLoginAt = Just now
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  _ <- insert $ User
    { userPartyId = party4
    , userPasswordHash = "$2b$10$dummyhashforteacher"
    , userIsActive = True
    , userLastLoginAt = Nothing
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  _ <- insert $ User
    { userPartyId = party5
    , userPasswordHash = "$2b$10$dummyhashforreception"
    , userIsActive = False
    , userLastLoginAt = Nothing
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  return ()
