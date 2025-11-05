{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API where

import Data.Text (Text)
import Servant.API
import TDF.DTO

-- | User management API
type UserAPI = 
  "api" :> "users" :> Get '[JSON] [UserWithParty]
  :<|> "api" :> "users" :> Capture "userId" Int :> "role" :> ReqBody '[JSON] UpdateRoleRequest :> Put '[JSON] UpdateRoleResponse

-- | Combined API
type API = UserAPI
