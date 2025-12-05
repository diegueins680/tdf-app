{-# LANGUAGE OverloadedStrings #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  ) where

import Servant
import Servant.Server
import RIO
import Data.Aeson (Value, object, (.=))
import qualified Network.HTTP.Types.Status as Status

import TDF.API.SocialEventsAPI

-- | Handler placeholders for Social Events API. Implementations should
-- call into DB layer (TDF.Models.SocialEventsModels) and convert to DTOs.

socialEventsServer :: Server SocialEventsAPI
socialEventsServer = eventsServer
                 :<|> venuesServer
                 :<|> artistsServer
                 :<|> rsvpsServer
                 :<|> invitationsServer

  where
    -- Events
    eventsServer :: Server EventsRoutes
    eventsServer = listEvents
              :<|> createEvent
              :<|> getEvent
              :<|> updateEvent
              :<|> deleteEvent

    listEvents :: Maybe String -> Maybe String -> Handler Value
    listEvents _ _ = throwError err501 { errBody = "Not implemented: listEvents" }

    createEvent :: Value -> Handler Value
    createEvent _ = throwError err501 { errBody = "Not implemented: createEvent" }

    getEvent :: String -> Handler Value
    getEvent _ = throwError err501 { errBody = "Not implemented: getEvent" }

    updateEvent :: String -> Value -> Handler Value
    updateEvent _ _ = throwError err501 { errBody = "Not implemented: updateEvent" }

    deleteEvent :: String -> Handler NoContent
    deleteEvent _ = throwError err501 { errBody = "Not implemented: deleteEvent" }

    -- Venues
    venuesServer :: Server VenuesRoutes
    venuesServer = listVenues
              :<|> createVenue
              :<|> getVenue
              :<|> updateVenue

    listVenues :: Maybe String -> Maybe String -> Handler Value
    listVenues _ _ = throwError err501 { errBody = "Not implemented: listVenues" }

    createVenue :: Value -> Handler Value
    createVenue _ = throwError err501 { errBody = "Not implemented: createVenue" }

    getVenue :: String -> Handler Value
    getVenue _ = throwError err501 { errBody = "Not implemented: getVenue" }

    updateVenue :: String -> Value -> Handler Value
    updateVenue _ _ = throwError err501 { errBody = "Not implemented: updateVenue" }

    -- Artists
    artistsServer :: Server ArtistsRoutes
    artistsServer = listArtists
               :<|> createArtist
               :<|> getArtist
               :<|> updateArtist

    listArtists :: Maybe String -> Maybe String -> Handler Value
    listArtists _ _ = throwError err501 { errBody = "Not implemented: listArtists" }

    createArtist :: Value -> Handler Value
    createArtist _ = throwError err501 { errBody = "Not implemented: createArtist" }

    getArtist :: String -> Handler Value
    getArtist _ = throwError err501 { errBody = "Not implemented: getArtist" }

    updateArtist :: String -> Value -> Handler Value
    updateArtist _ _ = throwError err501 { errBody = "Not implemented: updateArtist" }

    -- RSVPs
    rsvpsServer :: Server RsvpRoutes
    rsvpsServer = listRsvps
             :<|> createRsvp

    listRsvps :: String -> Handler Value
    listRsvps _ = throwError err501 { errBody = "Not implemented: listRsvps" }

    createRsvp :: String -> Value -> Handler Value
    createRsvp _ _ = throwError err501 { errBody = "Not implemented: createRsvp" }

    -- Invitations
    invitationsServer :: Server InvitationsRoutes
    invitationsServer = createInvitation

    createInvitation :: String -> Value -> Handler Value
    createInvitation _ _ = throwError err501 { errBody = "Not implemented: createInvitation" }
