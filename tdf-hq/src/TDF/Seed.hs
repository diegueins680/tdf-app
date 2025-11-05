{-# LANGUAGE OverloadedStrings #-}

module TDF.Seed where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Data.Text (Text)
import Data.Time (getCurrentTime)
import TDF.Models
import TDF.DB (DB)

-- | Seed sample data for development
-- WARNING: This uses dummy password hashes and is for DEVELOPMENT ONLY
seedData :: DB ()
seedData = do
  now <- liftIO getCurrentTime
  
  -- Create sample parties
  party1 <- insert $ Party
    { partyName = "John Admin"
    , partyEmail = Just "admin@tdfrecords.com"
    , partyPhone = Just "+593-99-123-4567"
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
    , partyInstagram = Just "@anateacher"
    , partyWhatsapp = Just "+593994567890"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  party5 <- insert $ Party
    { partyName = "Luis Multi-Role"
    , partyEmail = Just "luis@tdfrecords.com"
    , partyPhone = Just "+593-99-567-8901"
    , partyInstagram = Nothing
    , partyWhatsapp = Just "+593995678901"
    , partyTaxId = Nothing
    , partyEmergencyContact = Nothing
    , partyCreatedAt = now
    , partyUpdatedAt = now
    }
  
  -- Create role assignments for parties
  -- John has Admin role
  _ <- insert $ PartyRoleAssignment party1 AdminRole now
  
  -- Maria has Manager and Accounting roles
  _ <- insert $ PartyRoleAssignment party2 ManagerRole now
  _ <- insert $ PartyRoleAssignment party2 AccountingRole now
  
  -- Carlos has Engineer role
  _ <- insert $ PartyRoleAssignment party3 EngineerRole now
  
  -- Ana has Teacher and Artist roles
  _ <- insert $ PartyRoleAssignment party4 TeacherRole now
  _ <- insert $ PartyRoleAssignment party4 ArtistRole now
  
  -- Luis has Reception, Student, and Customer roles (multi-role example)
  _ <- insert $ PartyRoleAssignment party5 ReceptionRole now
  _ <- insert $ PartyRoleAssignment party5 StudentRole now
  _ <- insert $ PartyRoleAssignment party5 CustomerRole now
  
  -- Create corresponding users (using dummy hashes for DEVELOPMENT ONLY)
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
    , userPasswordHash = "$2b$10$dummyhashformultirole"
    , userIsActive = True
    , userLastLoginAt = Nothing
    , userCreatedAt = now
    , userUpdatedAt = now
    }
  
  return ()
