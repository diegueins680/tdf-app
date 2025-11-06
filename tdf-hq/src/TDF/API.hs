{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API where

import Servant
import Data.Text (Text)
import TDF.Models (PartyRole)
import TDF.DTO (UserDTO, UserRoleUpdateDTO)

-- Main API definition
type API = "api" :> 
    (    UsersAPI
    )

-- Users API with role management
type UsersAPI = "users" :>
    (    Get '[JSON] [UserDTO]
    :<|> Capture "userId" Int :> "roles" :> Get '[JSON] [PartyRole]
    :<|> Capture "userId" Int :> "roles" :> ReqBody '[JSON] UserRoleUpdateDTO :> Put '[JSON] ()
    )

api :: Proxy API
api = Proxy
