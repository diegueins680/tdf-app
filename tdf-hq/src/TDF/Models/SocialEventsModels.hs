{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Models.SocialEventsModels
  ( migrateSocialEvents
  ) where

import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateSocialEvents"] [persistLowerCase|
ArtistProfile
    partyId Text Maybe
    name Text
    bio Text Maybe
    avatarUrl Text Maybe
    genres [Text] Maybe sqltype=text[]
    socialLinks Text Maybe
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
    contact Text Maybe
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
    metadata Text Maybe
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
    metadata Text Maybe
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
