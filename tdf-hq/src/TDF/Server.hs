{-# LANGUAGE OverloadedStrings #-}

module TDF.Server where

import Servant
import Database.Persist.Postgresql
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Int (Int64)
import TDF.Models
import TDF.DTO
import TDF.DB
import qualified TDF.API as API

-- Application monad that carries database connection pool
type AppM = ReaderT ConnectionPool Handler

-- Convert database entities to DTOs
entityToUserDTO :: Entity Party -> [PartyRole] -> UserDTO
entityToUserDTO (Entity partyId party) roles = UserDTO
    { userId = fromIntegral $ fromSqlKey partyId
    , userName = partyName party
    , userEmail = partyEmail party
    , userPhone = partyPhone party
    , userRoles = roles
    , userStatus = partyStatus party
    , userCreatedAt = partyCreatedAt party
    }

-- Server implementation
usersServer :: ServerT API.UsersAPI AppM
usersServer = getUsers
         :<|> getUserRoles'
         :<|> updateUserRoles'

-- Get all users with their roles
getUsers :: AppM [UserDTO]
getUsers = do
    pool <- ask
    users <- liftIO $ runSqlPool getUsersWithRoles pool
    return $ map (uncurry entityToUserDTO) users

-- Get roles for a specific user
getUserRoles' :: Int -> AppM [PartyRole]
getUserRoles' userId = do
    pool <- ask
    let partyId = toSqlKey $ fromIntegral userId
    liftIO $ runSqlPool (getUserRoles partyId) pool

-- Update roles for a user
updateUserRoles' :: Int -> UserRoleUpdateDTO -> AppM ()
updateUserRoles' userId roleUpdate = do
    pool <- ask
    let partyId = toSqlKey $ fromIntegral userId
    liftIO $ runSqlPool (updateUserRoles partyId (roles roleUpdate) Nothing) pool

-- Main server
server :: ConnectionPool -> Server API.API
server pool = hoistServer API.api (`runReaderT` pool) usersServer
