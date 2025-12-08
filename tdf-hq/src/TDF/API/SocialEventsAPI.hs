{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.API.SocialEventsAPI
  ( SocialEventsAPI
  , EventsRoutes
  , VenuesRoutes
  , ArtistsRoutes
  , RsvpRoutes
  , InvitationsRoutes
  , IdParam
  ) where

import Servant
import Data.Text (Text)

import TDF.DTO.SocialEventsDTO (EventDTO, VenueDTO, ArtistDTO, RsvpDTO, InvitationDTO)

type IdParam = Capture "id" Text

type EventsRoutes =
       "events" :> QueryParam "city" Text :> QueryParam "start_after" Text :> Get '[JSON] [EventDTO]
  :<|> "events" :> ReqBody '[JSON] EventDTO :> Post '[JSON] EventDTO
  :<|> "events" :> IdParam :> Get '[JSON] EventDTO
  :<|> "events" :> IdParam :> ReqBody '[JSON] EventDTO :> Put '[JSON] EventDTO
  :<|> "events" :> IdParam :> DeleteNoContent

type VenuesRoutes =
       "venues" :> QueryParam "city" Text :> QueryParam "near" Text :> Get '[JSON] [VenueDTO]
  :<|> "venues" :> ReqBody '[JSON] VenueDTO :> Post '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> Get '[JSON] VenueDTO
  :<|> "venues" :> IdParam :> ReqBody '[JSON] VenueDTO :> Put '[JSON] VenueDTO

type ArtistsRoutes =
       "artists" :> QueryParam "name" Text :> QueryParam "genre" Text :> Get '[JSON] [ArtistDTO]
  :<|> "artists" :> ReqBody '[JSON] ArtistDTO :> Post '[JSON] ArtistDTO
  :<|> "artists" :> IdParam :> Get '[JSON] ArtistDTO
  :<|> "artists" :> IdParam :> ReqBody '[JSON] ArtistDTO :> Put '[JSON] ArtistDTO

type RsvpRoutes =
       "events" :> Capture "eventId" Text :> "rsvps" :> Get '[JSON] [RsvpDTO]
  :<|> "events" :> Capture "eventId" Text :> "rsvps" :> ReqBody '[JSON] RsvpDTO :> Post '[JSON] RsvpDTO

type InvitationsRoutes =
       "events" :> Capture "eventId" Text :> "invitations" :>
         ( Get '[JSON] [InvitationDTO]
      :<|> ReqBody '[JSON] InvitationDTO :> Post '[JSON] InvitationDTO
      :<|> Capture "invitationId" Text :> ReqBody '[JSON] InvitationDTO :> Put '[JSON] InvitationDTO
         )

type SocialEventsAPI = EventsRoutes
               :<|> VenuesRoutes
               :<|> ArtistsRoutes
               :<|> RsvpRoutes
               :<|> InvitationsRoutes
