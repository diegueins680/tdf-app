{-# LANGUAGE OverloadedStrings #-}

module TDF.Server where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (runSqlPool)
import Servant
import TDF.API
import TDF.DB
import TDF.DTO
import Database.Persist.Postgresql (ConnectionPool)

-- | Server implementation
server :: ConnectionPool -> Server API
server pool = userHandlers
  where
    userHandlers = listUsers :<|> updateRole
    
    listUsers :: Handler [UserWithParty]
    listUsers = liftIO $ runStdoutLoggingT $ runSqlPool getAllUsersWithParty pool
    
    updateRole :: Int -> UpdateRoleRequest -> Handler UpdateRoleResponse
    updateRole userId req = do
      result <- liftIO $ runStdoutLoggingT $ runSqlPool (updateUserRole userId (urrRole req)) pool
      case result of
        Nothing -> return $ UpdateRoleResponse
          { urrSuccess = False
          , urrMessage = "User not found"
          , urrUser = Nothing
          }
        Just updatedUser -> return $ UpdateRoleResponse
          { urrSuccess = True
          , urrMessage = "Role updated successfully"
          , urrUser = Just updatedUser
          }

-- | Application
app :: ConnectionPool -> Application
app pool = serve (Proxy :: Proxy API) (server pool)
