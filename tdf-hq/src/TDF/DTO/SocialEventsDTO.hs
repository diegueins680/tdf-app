{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialEventsDTO where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Text (Text)
import Data.Time (UTCTime)

-- Placeholder DTOs for Social Events feature.
-- Replace or extend with actual DTOs used by Servant handlers and generated OpenAPI.

data ArtistDTO = ArtistDTO
  { artistId :: Maybe Text
  , artistName :: Text
  , artistGenres :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistDTO
instance FromJSON ArtistDTO

data VenueDTO = VenueDTO
  { venueId :: Maybe Text
  , venueName :: Text
  , venueAddress :: Maybe Text
  , venueCity :: Maybe Text
  , venueCountry :: Maybe Text
  , venueLat :: Maybe Double
  , venueLng :: Maybe Double
  , venueCapacity :: Maybe Int
  , venueContact :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON VenueDTO
instance FromJSON VenueDTO

data EventDTO = EventDTO
  { eventId :: Maybe Text
  , eventTitle :: Text
  , eventDescription :: Maybe Text
  , eventStart :: UTCTime
  , eventEnd :: UTCTime
  , eventVenueId :: Maybe Text
  , eventPriceCents :: Maybe Int
  , eventCapacity :: Maybe Int
  , eventArtists :: [ArtistDTO]
  } deriving (Show, Eq, Generic)

instance ToJSON EventDTO
instance FromJSON EventDTO
