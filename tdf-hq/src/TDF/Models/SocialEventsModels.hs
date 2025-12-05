{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Persistent model stubs for the Social Events feature.
--
-- These are intentionally conservative stubs to be adapted to the project's
-- existing Persistent conventions (UUID primary keys, naming, migrations).
-- TODO: Adjust types and sql settings to match `TDF.Models` conventions.
module TDF.Models.SocialEventsModels
  ( migrateSocialEvents
  ) where

import Database.Persist.TH
import Database.Persist.Types
import Database.Persist.Sql (SqlPersistT)
import Data.Aeson (Value)
import Data.Time (UTCTime)

-- NOTE:
-- - If your project uses UUID primary keys, update the entities to include
--   `Id UUID` or equivalent sql settings. The examples below use default
--   Persistent primary keys for simplicity.
-- - For PostgreSQL arrays or JSONB fields, add `sqltype` attributes as needed.

share [mkPersist sqlSettings, mkMigrate "migrateSocialEvents"] [persistLowerCase|
ArtistProfile
    partyId Text Maybe
    name Text
    bio Text Maybe
    avatarUrl Text Maybe
    genres [Text] Maybe sqltype=text[]
    socialLinks Value Maybe sqltype=jsonb
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show

Venue
    name Text
    address Text Maybe
    city Text Maybe
    country Text Maybe
    latitude Double Maybe
    longitude Double Maybe
    capacity Int Maybe
    contact Value Maybe sqltype=jsonb
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show

SocialEvent
    organizerPartyId Text Maybe
    title Text
    description Text Maybe
    venueId VenueId Maybe
    startTime UTCTime
    endTime UTCTime
    priceCents Int Maybe
    capacity Int Maybe
    metadata Value Maybe sqltype=jsonb
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show

EventArtist
    eventId SocialEventId
    artistId ArtistProfileId
    role Text Maybe
    Primary eventId artistId
    deriving Show

EventRsvp
    eventId SocialEventId
    partyId Text
    status Text
    metadata Value Maybe sqltype=jsonb
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show

EventInvitation
    eventId SocialEventId
    fromPartyId Text Maybe
    toPartyId Text Maybe
    status Text Maybe
    message Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show

ArtistGenre
    artistId ArtistProfileId
    genre Text
    Primary artistId genre
    deriving Show
|]

-- Expose the migration name to be run by the application's migration runner.
migrateSocialEvents :: [Migration]
migrateSocialEvents = migrateAll
