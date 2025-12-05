{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialEventsDTO where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Placeholder DTOs for Social Events feature.
-- Replace or extend with actual DTOs used by Servant handlers and generated OpenAPI.

data ArtistDTO = ArtistDTO
  { artistId :: Text
  , artistName :: Text
  , artistGenres :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistDTO
instance FromJSON ArtistDTO

data VenueDTO = VenueDTO
  { venueId :: Text
  , venueName :: Text
  , venueLat :: Maybe Double
  , venueLng :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON VenueDTO
instance FromJSON VenueDTO

data EventDTO = EventDTO
  { eventId :: Text
  , eventTitle :: Text
  , eventStart :: Text
  , eventEnd :: Text
  , eventVenue :: Maybe VenueDTO
  , eventArtists :: [ArtistDTO]
  } deriving (Show, Eq, Generic)

instance ToJSON EventDTO
instance FromJSON EventDTO
