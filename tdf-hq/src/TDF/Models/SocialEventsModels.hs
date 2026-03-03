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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TDF.Models.SocialEventsModels where

import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           GHC.Generics       (Generic)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateSocialEvents"] [persistLowerCase|
ArtistProfile sql=social_artist_profile
    partyId Text Maybe
    name Text
    bio Text Maybe
    avatarUrl Text Maybe
    genres [Text] Maybe sqltype=text[]
    socialLinks Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

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
    deriving Show Generic

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
    deriving Show Generic

EventArtist
    eventId SocialEventId
    artistId ArtistProfileId
    role Text Maybe
    Primary eventId artistId
    deriving Show Generic

EventRsvp
    eventId SocialEventId
    partyId Text
    status Text
    metadata Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventInvitation
    eventId SocialEventId
    fromPartyId Text Maybe
    toPartyId Text Maybe
    status Text Maybe
    message Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

ArtistGenre
    artistId ArtistProfileId
    genre Text
    Primary artistId genre
    deriving Show Generic

ArtistFollow
    artistId ArtistProfileId
    followerPartyId Text
    createdAt UTCTime default=now()
    Primary artistId followerPartyId
    deriving Show Generic

EventTicketTier
    eventId SocialEventId
    code Text
    name Text
    description Text Maybe
    priceCents Int
    currency Text
    quantityTotal Int
    quantitySold Int
    salesStart UTCTime Maybe
    salesEnd UTCTime Maybe
    isActive Bool
    position Int Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventTicketTierCode eventId code
    deriving Show Generic

EventTicketOrder
    eventId SocialEventId
    tierId EventTicketTierId
    buyerPartyId Text Maybe
    buyerName Text Maybe
    buyerEmail Text Maybe
    quantity Int
    amountCents Int
    currency Text
    status Text
    metadata Text Maybe
    purchasedAt UTCTime
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventTicket
    eventId SocialEventId
    tierRefId EventTicketTierId
    orderRefId EventTicketOrderId
    holderName Text Maybe
    holderEmail Text Maybe
    code Text
    status Text
    checkedInAt UTCTime Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventTicketCode code
    deriving Show Generic
|]
