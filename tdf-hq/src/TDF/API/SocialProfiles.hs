{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module TDF.API.SocialProfiles (ProfileListAPI, ProfileGetAPI, SocialProfilesAPI) where

import Data.Int (Int64)
import Servant
import TDF.DTO (SocialPartyProfileDTO)

-- Exact existing wire contracts; shared with the focused HTTP harness.
type ProfileListAPI = "profiles" :> QueryParams "partyId" Int64
  :> Get '[JSON] [SocialPartyProfileDTO]
type ProfileGetAPI = "profiles" :> Capture "partyId" Int64
  :> Get '[JSON] SocialPartyProfileDTO
type SocialProfilesAPI = ProfileListAPI :<|> ProfileGetAPI
