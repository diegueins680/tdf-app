{-# LANGUAGE OverloadedStrings #-}

module TDF.DB where

import Database.Persist
import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import TDF.Models

-- Get all users with their roles
getUsersWithRoles :: SqlPersistT IO [(Entity Party, [PartyRole])]
getUsersWithRoles = do
    parties <- selectList [] []
    mapM attachRoles parties
  where
    attachRoles party = do
        roleAssignments <- selectList [PartyRoleAssignmentPartyId ==. entityKey party] []
        let roles = map (partyRoleAssignmentRole . entityVal) roleAssignments
        return (party, roles)

-- Get roles for a specific user
getUserRoles :: Key Party -> SqlPersistT IO [PartyRole]
getUserRoles partyId = do
    roleAssignments <- selectList [PartyRoleAssignmentPartyId ==. partyId] []
    return $ map (partyRoleAssignmentRole . entityVal) roleAssignments

-- Update roles for a user (replaces all existing roles)
updateUserRoles :: Key Party -> [PartyRole] -> Maybe (Key Party) -> SqlPersistT IO ()
updateUserRoles partyId newRoles assignedBy = do
    -- Delete all existing role assignments for this user
    deleteWhere [PartyRoleAssignmentPartyId ==. partyId]
    
    -- Insert new role assignments
    now <- liftIO getCurrentTime
    mapM_ (insertRoleAssignment now) newRoles
  where
    insertRoleAssignment timestamp role = insert_ $ PartyRoleAssignment
        { partyRoleAssignmentPartyId = partyId
        , partyRoleAssignmentRole = role
        , partyRoleAssignmentAssignedAt = timestamp
        , partyRoleAssignmentAssignedBy = assignedBy
        }

-- Add a single role to a user
addUserRole :: Key Party -> PartyRole -> Maybe (Key Party) -> SqlPersistT IO ()
addUserRole partyId role assignedBy = do
    now <- liftIO getCurrentTime
    insert_ $ PartyRoleAssignment
        { partyRoleAssignmentPartyId = partyId
        , partyRoleAssignmentRole = role
        , partyRoleAssignmentAssignedAt = now
        , partyRoleAssignmentAssignedBy = assignedBy
        }

-- Remove a single role from a user
removeUserRole :: Key Party -> PartyRole -> SqlPersistT IO ()
removeUserRole partyId role = do
    deleteWhere 
        [ PartyRoleAssignmentPartyId ==. partyId
        , PartyRoleAssignmentRole ==. role
        ]

-- Check if a user has a specific role
userHasRole :: Key Party -> PartyRole -> SqlPersistT IO Bool
userHasRole partyId role = do
    mAssignment <- getBy $ UniquePartyRole partyId role
    return $ case mAssignment of
        Just _  -> True
        Nothing -> False

-- Party operations
listParties :: SqlPersistT IO [Entity Party]
listParties = selectList [] [Asc PartyDisplayName]

getParty :: Key Party -> SqlPersistT IO (Maybe (Entity Party))
getParty = getEntity

insertParty :: Party -> SqlPersistT IO (Entity Party)
insertParty party = do
    partyId <- insert party
    pure $ Entity partyId party

updateParty :: Key Party -> [Update Party] -> SqlPersistT IO (Maybe (Entity Party))
updateParty partyId updates = do
    update partyId updates
    getEntity partyId

-- Booking operations
listBookings :: SqlPersistT IO [Entity Booking]
listBookings = selectList [] [Asc BookingStartTime]

insertBooking :: Booking -> SqlPersistT IO (Entity Booking)
insertBooking booking = do
    bookingId <- insert booking
    pure $ Entity bookingId booking
