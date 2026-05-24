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

EventMoment
    eventId SocialEventId
    authorPartyId Text Maybe
    authorName Text
    caption Text Maybe
    mediaUrl Text
    mediaType Text
    mediaWidth Int Maybe
    mediaHeight Int Maybe
    mediaDurationMs Int Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventMomentReaction
    momentId EventMomentId
    reaction Text
    reactorPartyId Text
    createdAt UTCTime default=now()
    Primary momentId reaction reactorPartyId
    deriving Show Generic

EventMomentComment
    momentId EventMomentId
    authorPartyId Text Maybe
    authorName Text
    body Text
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
    enableWaitlist Bool default=False
    allowTransfers Bool default=True
    refundPolicy Text default='full'
    refundDeadline UTCTime Maybe
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
    stripePaymentIntentId Text Maybe
    promoCodeId PromoCodeId Maybe
    originalAmountCents Int Maybe
    paymentMethod Text Maybe
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
    currentHolderPartyId Text Maybe
    currentHolderEmail Text Maybe
    currentHolderName Text Maybe
    originalHolderPartyId Text Maybe
    transferHistory Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventTicketCode code
    deriving Show Generic

EventBudgetLine
    eventId SocialEventId
    code Text
    name Text
    lineType Text
    category Text
    plannedCents Int
    notes Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventBudgetLineCode eventId code
    deriving Show Generic

EventFinanceEntry
    eventId SocialEventId
    budgetLineId EventBudgetLineId Maybe
    direction Text
    source Text
    category Text
    concept Text
    amountCents Int
    currency Text
    status Text
    externalRef Text Maybe
    notes Text Maybe
    metadata Text Maybe
    occurredAt UTCTime
    recordedByPartyId Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

PromoCode sql=promo_code
    eventId SocialEventId Maybe
    code Text
    description Text Maybe
    discountType Text
    discountValue Int
    currency Text default='USD'
    maxRedemptions Int Maybe
    currentRedemptions Int default=0
    validFrom UTCTime Maybe
    validUntil UTCTime Maybe
    tierIds Text Maybe
    minPurchaseAmountCents Int Maybe
    isActive Bool default=True
    createdByPartyId Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniquePromoCode code
    deriving Show Generic

PromoCodeRedemption sql=promo_code_redemption
    promoCodeId PromoCodeId
    orderId EventTicketOrderId
    discountAmountCents Int
    redeemedAt UTCTime default=now()
    deriving Show Generic

TicketRefundRequest sql=ticket_refund_request
    orderId EventTicketOrderId
    requestedByPartyId Text Maybe
    reason Text Maybe
    amountCents Int
    status Text default='pending'
    approvedByPartyId Text Maybe
    approvedAt UTCTime Maybe
    rejectionReason Text Maybe
    stripeRefundId Text Maybe
    processedAt UTCTime Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

TicketTransfer sql=ticket_transfer
    ticketId EventTicketId
    fromPartyId Text Maybe
    toPartyId Text Maybe
    toEmail Text Maybe
    toName Text Maybe
    status Text default='pending'
    transferCode Text
    message Text Maybe
    expiresAt UTCTime Maybe
    acceptedAt UTCTime Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueTicketTransferCode transferCode
    deriving Show Generic

EventWaitlist sql=event_waitlist
    eventId SocialEventId
    tierId EventTicketTierId Maybe
    partyId Text Maybe
    email Text
    name Text Maybe
    quantity Int default=1
    status Text default='active'
    priority Int default=0
    notifiedAt UTCTime Maybe
    expiresAt UTCTime Maybe
    convertedOrderId EventTicketOrderId Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

StripePaymentIntent sql=stripe_payment_intent
    orderId EventTicketOrderId
    stripePaymentIntentId Text
    stripeClientSecret Text
    amountCents Int
    currency Text default='USD'
    status Text
    metadata Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueStripePaymentIntent stripePaymentIntentId
    deriving Show Generic

StripeWebhookEvent sql=stripe_webhook_event
    stripeEventId Text
    eventType Text
    payload Text
    processedAt UTCTime default=now()
    UniqueStripeWebhookEvent stripeEventId
    deriving Show Generic

TicketQRCode sql=ticket_qr_code
    ticketId EventTicketId
    qrData Text
    qrImageUrl Text Maybe
    generatedAt UTCTime default=now()
    UniqueTicketQRCode ticketId
    deriving Show Generic
|]
