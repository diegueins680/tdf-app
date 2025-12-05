{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module TDF.Server.SocialEventsHandlers
  ( socialEventsServer
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Except (throwError)
import           Data.Aeson (Value, object, (.=), toJSON)
import qualified Data.Text as T
import           Text.Read (readMaybe)

import           Servant
import           Servant.Server

import           Database.Persist (Entity(..), SelectOpt(..), selectList, selectFirst)
import           Database.Persist.Sql (runSqlPool, fromSqlKey)

import           TDF.API.SocialEventsAPI
import           TDF.DB (Env(..))
import           TDF.Models.SocialEventsModels

-- | Social Events server implemented with access to the application's Env.
-- Returns JSON 'Value' for flexibility; replace with typed DTOs later.
socialEventsServer
  :: forall m.
     ( MonadReader Env m
     , MonadIO m
     , MonadError ServerError m
     )
  => ServerT SocialEventsAPI m
socialEventsServer = eventsServer
                 :<|> venuesServer
                 :<|> artistsServer
                 :<|> rsvpsServer
                 :<|> invitationsServer

  where
    -- Helper to convert Venue entity to JSON-like Value
    venueToValue :: Entity Venue -> Value
    venueToValue (Entity vid v) = object
      [ "id" .= (fromSqlKey vid)
      , "name" .= venueName v
      , "address" .= venueAddress v
      , "city" .= venueCity v
      , "country" .= venueCountry v
      , "latitude" .= venueLatitude v
      , "longitude" .= venueLongitude v
      , "capacity" .= venueCapacity v
      , "contact" .= venueContact v
      ]

    eventToValue :: Entity SocialEvent -> Value
    eventToValue (Entity eid e) = object
      [ "id" .= (fromSqlKey eid)
      , "title" .= socialEventTitle e
      , "description" .= socialEventDescription e
      , "start_time" .= socialEventStartTime e
      , "end_time" .= socialEventEndTime e
      , "venue_id" .= socialEventVenueId e
      , "price_cents" .= socialEventPriceCents e
      , "capacity" .= socialEventCapacity e
      ]

    -- Events
    eventsServer :: ServerT EventsRoutes m
    eventsServer = listEvents
              :<|> createEvent
              :<|> getEvent
              :<|> updateEvent
              :<|> deleteEvent

    listEvents :: Maybe String -> Maybe String -> m Value
    listEvents _mCity _mStartAfter = do
      Env{..} <- ask
      rows <- liftIO $ runSqlPool (selectList [] [Desc SocialEventStartTime, LimitTo 200]) envPool
      pure $ toJSON (map eventToValue rows)

    createEvent :: EventDTO -> m EventDTO
    createEvent dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      let title = eventTitle dto
      -- Insert minimal event record; TODO: validate fields and link artists
      key <- liftIO $ runSqlPool (insert SocialEvent
        { socialEventOrganizerPartyId = Nothing
        , socialEventTitle = eventTitle dto
        , socialEventDescription = eventDescription dto
        , socialEventVenueId = Nothing
        , socialEventStartTime = eventStart dto
        , socialEventEndTime = eventEnd dto
        , socialEventPriceCents = eventPriceCents dto
        , socialEventCapacity = eventCapacity dto
        , socialEventMetadata = Nothing
        , socialEventCreatedAt = now
        , socialEventUpdatedAt = now
        }) envPool
      let createdDto = dto { eventId = Just (T.pack (show (fromSqlKey key))) }
      pure createdDto

    getEvent :: String -> m Value
    getEvent rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip (T.pack rawId))) of
        Nothing -> throwError err400 { errBody = "Invalid event id" }
        Just (_ :: Int) -> do
          mRow <- liftIO $ runSqlPool (selectFirst [] []) envPool
          case mRow of
            Nothing -> throwError err404 { errBody = "Event not found" }
            Just r  -> pure $ eventToValue r

    updateEvent :: String -> Value -> m Value
    updateEvent _ _ = throwError err501 { errBody = "Not implemented: updateEvent" }

    deleteEvent :: String -> m NoContent
    deleteEvent _ = throwError err501 { errBody = "Not implemented: deleteEvent" }

    -- Venues
    venuesServer :: ServerT VenuesRoutes m
    venuesServer = listVenues
              :<|> createVenue
              :<|> getVenue
              :<|> updateVenue

    listVenues :: Maybe String -> Maybe String -> m Value
    listVenues mCity _mNear = do
      Env{..} <- ask
      let filters = case mCity of
                      Just c | not (T.null (T.strip (T.pack c))) -> [VenueCity ==. Just (T.pack c)]
                      _ -> []
      rows <- liftIO $ runSqlPool (selectList filters [Asc VenueName, LimitTo 200]) envPool
      pure $ toJSON (map venueToValue rows)

    createVenue :: VenueDTO -> m VenueDTO
    createVenue dto = do
      Env{..} <- ask
      now <- liftIO getCurrentTime
      key <- liftIO $ runSqlPool (insert Venue
        { venueName = venueName dto
        , venueAddress = venueAddress dto
        , venueCity = venueCity dto
        , venueCountry = venueCountry dto
        , venueLatitude = venueLat dto
        , venueLongitude = venueLng dto
        , venueCapacity = venueCapacity dto
        , venueContact = venueContact dto
        , venueCreatedAt = now
        , venueUpdatedAt = now
        }) envPool
      let created = dto { venueId = Just (T.pack (show (fromSqlKey key))) }
      pure created

    getVenue :: String -> m Value
    getVenue rawId = do
      Env{..} <- ask
      case readMaybe (T.unpack (T.strip (T.pack rawId))) of
        Nothing -> throwError err400 { errBody = "Invalid venue id" }
        Just (_ :: Int) -> do
          mRow <- liftIO $ runSqlPool (selectFirst [] []) envPool
          case mRow of
            Nothing -> throwError err404 { errBody = "Venue not found" }
            Just r  -> pure $ venueToValue r

    updateVenue :: String -> Value -> m Value
    updateVenue _ _ = throwError err501 { errBody = "Not implemented: updateVenue" }

    -- Artists (left as not-implemented for now, reuse artist utilities later)
    artistsServer :: ServerT ArtistsRoutes m
    artistsServer = listArtists
               :<|> createArtist
               :<|> getArtist
               :<|> updateArtist

    listArtists :: Maybe String -> Maybe String -> m Value
    listArtists _ _ = throwError err501 { errBody = "Not implemented: listArtists" }

    createArtist :: Value -> m Value
    createArtist _ = throwError err501 { errBody = "Not implemented: createArtist" }

    getArtist :: String -> m Value
    getArtist _ = throwError err501 { errBody = "Not implemented: getArtist" }

    updateArtist :: String -> Value -> m Value
    updateArtist _ _ = throwError err501 { errBody = "Not implemented: updateArtist" }

    -- RSVPs
    rsvpsServer :: ServerT RsvpRoutes m
    rsvpsServer = listRsvps
             :<|> createRsvp

    listRsvps :: String -> m Value
    listRsvps _ = throwError err501 { errBody = "Not implemented: listRsvps" }

    createRsvp :: String -> Value -> m Value
    createRsvp _ _ = throwError err501 { errBody = "Not implemented: createRsvp" }

    -- Invitations
    invitationsServer :: ServerT InvitationsRoutes m
    invitationsServer = createInvitation

    createInvitation :: String -> Value -> m Value
    createInvitation _ _ = throwError err501 { errBody = "Not implemented: createInvitation" }
