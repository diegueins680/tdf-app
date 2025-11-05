{-# LANGUAGE OverloadedStrings #-}

module TDF.DB where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Data.Text (Text)
import qualified Data.Text as T
import TDF.Models
import TDF.DTO
import Data.Time (getCurrentTime)

-- | Database configuration
type DB a = SqlPersistT (LoggingT IO) a

-- | Create connection pool
createPool :: Text -> IO ConnectionPool
createPool connStr = runStdoutLoggingT $ createPostgresqlPool connStr 10

-- | Run migrations
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runSqlPool (runMigration migrateAll) pool

-- | Get all users with their party information
getAllUsersWithParty :: DB [UserWithParty]
getAllUsersWithParty = do
  usersWithParties <- selectList [] []
  mapM toUserWithParty usersWithParties
  where
    toUserWithParty (Entity userId user) = do
      maybeParty <- get (userPartyId user)
      case maybeParty of
        Nothing -> error "User without party - database integrity issue"
        Just party -> return $ UserWithParty
          { uwpUserId = fromIntegral $ fromSqlKey userId
          , uwpEmail = partyEmail party
          , uwpName = partyName party
          , uwpRole = partyRole party
          , uwpIsActive = userIsActive user
          , uwpLastLoginAt = userLastLoginAt user
          }

-- | Update user's role (by updating the associated party)
updateUserRole :: Int -> PartyRole -> DB (Maybe UserWithParty)
updateUserRole userIdInt newRole = do
  let userId = toSqlKey (fromIntegral userIdInt)
  maybeUser <- get userId
  case maybeUser of
    Nothing -> return Nothing
    Just user -> do
      now <- liftIO getCurrentTime
      update (userPartyId user) [PartyRole =. newRole, PartyUpdatedAt =. now]
      maybeParty <- get (userPartyId user)
      case maybeParty of
        Nothing -> return Nothing
        Just party -> return $ Just $ UserWithParty
          { uwpUserId = userIdInt
          , uwpEmail = partyEmail party
          , uwpName = partyName party
          , uwpRole = partyRole party
          , uwpIsActive = userIsActive user
          , uwpLastLoginAt = userLastLoginAt user
          }
