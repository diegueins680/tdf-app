{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.API.SocialEventsAPI
  ( SocialEventsAPI
  ) where

import Servant

-- | API type for Social Events feature. Keep signatures minimal and
-- easy to extend. These correspond to endpoints described in
-- tdf-hq/SOCIAL_EVENTS_IMPLEMENTATION.md.

type IdParam = Capture "id" String

type EventsRoutes =
       "events" :> QueryParam "city" String :> QueryParam "start_after" String :> Get '[JSON] Value
  :<|> "events" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "events" :> IdParam :> Get '[JSON] Value
  :<|> "events" :> IdParam :> ReqBody '[JSON] Value :> Put '[JSON] Value
  :<|> "events" :> IdParam :> DeleteNoContent '[JSON] NoContent

type VenuesRoutes =
       "venues" :> QueryParam "city" String :> QueryParam "near" String :> Get '[JSON] Value
  :<|> "venues" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "venues" :> IdParam :> Get '[JSON] Value
  :<|> "venues" :> IdParam :> ReqBody '[JSON] Value :> Put '[JSON] Value

type ArtistsRoutes =
       "artists" :> QueryParam "name" String :> QueryParam "genre" String :> Get '[JSON] Value
  :<|> "artists" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "artists" :> IdParam :> Get '[JSON] Value
  :<|> "artists" :> IdParam :> ReqBody '[JSON] Value :> Put '[JSON] Value

type RsvpRoutes =
       "events" :> Capture "eventId" String :> "rsvps" :> Get '[JSON] Value
  :<|> "events" :> Capture "eventId" String :> "rsvps" :> ReqBody '[JSON] Value :> Post '[JSON] Value

type InvitationsRoutes =
       "events" :> Capture "eventId" String :> "invitations" :> ReqBody '[JSON] Value :> Post '[JSON] Value

type SocialEventsAPI = EventsRoutes
               :<|> VenuesRoutes
               :<|> ArtistsRoutes
               :<|> RsvpRoutes
               :<|> InvitationsRoutes
