{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TDF.DTO.SocialEventsDTO where

import           Data.Aeson (FromJSON, ToJSON, Value)
import           Data.Text  (Text)
import           Data.Time  (UTCTime)
import           GHC.Generics (Generic)

data ArtistDTO = ArtistDTO
  { artistId       :: Maybe Text
  , artistName     :: Text
  , artistGenres   :: [Text]
  , artistBio      :: Maybe Text
  , artistAvatarUrl :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON ArtistDTO
instance FromJSON ArtistDTO

data VenueDTO = VenueDTO
  { venueId       :: Maybe Text
  , venueName     :: Text
  , venueAddress  :: Maybe Text
  , venueCity     :: Maybe Text
  , venueCountry  :: Maybe Text
  , venueLat      :: Maybe Double
  , venueLng      :: Maybe Double
  , venueCapacity :: Maybe Int
  , venueContact  :: Maybe Value
  } deriving (Show, Eq, Generic)
instance ToJSON VenueDTO
instance FromJSON VenueDTO

data EventDTO = EventDTO
  { eventId          :: Maybe Text
  , eventTitle       :: Text
  , eventDescription :: Maybe Text
  , eventStart       :: UTCTime
  , eventEnd         :: UTCTime
  , eventVenueId     :: Maybe Text
  , eventPriceCents  :: Maybe Int
  , eventCapacity    :: Maybe Int
  , eventArtists     :: [ArtistDTO]
  } deriving (Show, Eq, Generic)
instance ToJSON EventDTO
instance FromJSON EventDTO

