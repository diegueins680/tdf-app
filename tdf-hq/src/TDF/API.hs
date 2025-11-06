{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TDF.API where

import Servant
import TDF.Models (PartyRole)
import TDF.DTO

type VersionAPI = "version" :> Get '[JSON] VersionDTO
type HealthAPI  = "health"  :> Get '[JSON] HealthDTO

-- Main API definition
type API =
        VersionAPI
   :<|> HealthAPI
   :<|> PartiesAPI
   :<|> BookingsAPI
   :<|> ("api" :> UsersAPI)

-- Users API with role management
type UsersAPI = "users" :>
    (    Get '[JSON] [UserDTO]
    :<|> Capture "userId" Int :> "roles" :> Get '[JSON] [PartyRole]
    :<|> Capture "userId" Int :> "roles" :> ReqBody '[JSON] UserRoleUpdateDTO :> Put '[JSON] ()
    )

api :: Proxy API
api = Proxy

usersApi :: Proxy UsersAPI
usersApi = Proxy

versionApi :: Proxy VersionAPI
versionApi = Proxy

healthApi :: Proxy HealthAPI
healthApi = Proxy

type PartiesAPI =
       Get '[JSON] [PartyDTO]
  :<|> ReqBody '[JSON] PartyCreateDTO :> PostCreated '[JSON] PartyDTO
  :<|> Capture "partyId" Int :> Get '[JSON] PartyDTO
  :<|> Capture "partyId" Int :> ReqBody '[JSON] PartyUpdateDTO :> Put '[JSON] PartyDTO
  :<|> Capture "partyId" Int :> "roles" :> ReqBody '[JSON] PartyRole :> Post '[JSON] NoContent

partiesApi :: Proxy PartiesAPI
partiesApi = Proxy

type BookingsAPI =
       Get '[JSON] [BookingDTO]
  :<|> ReqBody '[JSON] BookingCreateDTO :> PostCreated '[JSON] BookingDTO

bookingsApi :: Proxy BookingsAPI
bookingsApi = Proxy
