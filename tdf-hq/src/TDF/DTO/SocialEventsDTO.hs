{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TDF.DTO.SocialEventsDTO
  ( ArtistDTO(..)
  , ArtistSocialLinksDTO(..)
  , ArtistFollowerDTO(..)
  , ArtistFollowRequest(..)
  , VenueDTO(..)
  , EventDTO(..)
  , RsvpDTO(..)
  , InvitationDTO(..)
  ) where

import           Data.Aeson (FromJSON, ToJSON, withObject, (.:), (.:?), (.=), object, toJSON, parseJSON)
import           Data.Text  (Text)
import           Data.Time  (UTCTime)
import           GHC.Generics (Generic)

data ArtistSocialLinksDTO = ArtistSocialLinksDTO
  { aslSpotify    :: Maybe Text
  , aslInstagram  :: Maybe Text
  , aslTwitter    :: Maybe Text
  , aslYoutube    :: Maybe Text
  , aslSoundcloud :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistSocialLinksDTO where
  toJSON ArtistSocialLinksDTO{..} = object
    [ "spotify"    .= aslSpotify
    , "instagram"  .= aslInstagram
    , "twitter"    .= aslTwitter
    , "youtube"    .= aslYoutube
    , "soundcloud" .= aslSoundcloud
    ]

instance FromJSON ArtistSocialLinksDTO where
  parseJSON = withObject "ArtistSocialLinksDTO" $ \o ->
    ArtistSocialLinksDTO
      <$> o .:? "spotify"
      <*> o .:? "instagram"
      <*> o .:? "twitter"
      <*> o .:? "youtube"
      <*> o .:? "soundcloud"

data ArtistDTO = ArtistDTO
  { artistId       :: Maybe Text
  , artistName     :: Text
  , artistGenres   :: [Text]
  , artistBio      :: Maybe Text
  , artistAvatarUrl :: Maybe Text
  , artistSocialLinks :: Maybe ArtistSocialLinksDTO
  } deriving (Show, Eq, Generic)
instance ToJSON ArtistDTO
instance FromJSON ArtistDTO

data ArtistFollowerDTO = ArtistFollowerDTO
  { afFollowId         :: Maybe Text
  , afArtistId         :: Maybe Text
  , afFollowerPartyId  :: Text
  , afCreatedAt        :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowerDTO where
  toJSON ArtistFollowerDTO{..} = object
    [ "followId" .= afFollowId
    , "artistId" .= afArtistId
    , "followerPartyId" .= afFollowerPartyId
    , "createdAt" .= afCreatedAt
    ]

instance FromJSON ArtistFollowerDTO where
  parseJSON = withObject "ArtistFollowerDTO" $ \o ->
    ArtistFollowerDTO
      <$> o .:? "followId"
      <*> o .:? "artistId"
      <*> o .:  "followerPartyId"
      <*> o .:? "createdAt"

data ArtistFollowRequest = ArtistFollowRequest
  { afrFollowerPartyId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ArtistFollowRequest where
  toJSON ArtistFollowRequest{..} = object
    [ "followerPartyId" .= afrFollowerPartyId
    ]

instance FromJSON ArtistFollowRequest where
  parseJSON = withObject "ArtistFollowRequest" $ \o ->
    ArtistFollowRequest
      <$> o .: "followerPartyId"

data VenueDTO = VenueDTO
  { venueId       :: Maybe Text
  , venueName     :: Text
  , venueAddress  :: Maybe Text
  , venueCity     :: Maybe Text
  , venueCountry  :: Maybe Text
  , venueLat      :: Maybe Double
  , venueLng      :: Maybe Double
  , venueCapacity :: Maybe Int
  , venueContact  :: Maybe Text
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

data RsvpDTO = RsvpDTO
  { rsvpId        :: Maybe Text
  , rsvpEventId   :: Text
  , rsvpPartyId   :: Text
  , rsvpStatus    :: Text  -- "Accepted", "Declined", "Maybe"
  , rsvpCreatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON RsvpDTO
instance FromJSON RsvpDTO

data InvitationDTO = InvitationDTO
  { invitationId         :: Maybe Text
  , invitationEventId    :: Maybe Text
  , invitationFromPartyId :: Maybe Text
  , invitationToPartyId  :: Text
  , invitationStatus     :: Maybe Text
  , invitationMessage    :: Maybe Text
  , invitationCreatedAt  :: Maybe UTCTime
  , invitationUpdatedAt  :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
instance ToJSON InvitationDTO
instance FromJSON InvitationDTO
