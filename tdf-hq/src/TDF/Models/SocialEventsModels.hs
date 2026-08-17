{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Models.SocialEventsModels where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Persist.TH
import GHC.Generics (Generic)
import TDF.UUIDInstances ()

share
    [mkPersist sqlSettings, mkMigrate "migrateSocialEvents"]
    [persistLowerCase|
ArtistProfile sql=social_artist_profile
    partyId Text Maybe
    name Text
    bio Text Maybe
    avatarUrl Text Maybe
    genres [Text] Maybe sqltype=text[]
    socialLinks Text Maybe
    countryCode Text Maybe
    countryId UUID Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

Venue
    name Text
    address Text Maybe
    city Text Maybe
    country Text Maybe
    countryCode Text Maybe
    countryId UUID Maybe
    cityId UUID Maybe
    timezone Text Maybe
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
    eventTypeId UUID Maybe
    workflowStateId UUID Maybe
    timezone Text Maybe
    startTime UTCTime
    endTime UTCTime
    priceCents Int Maybe
    currencyId UUID Maybe
    capacity Int Maybe
    metadata Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

ExternalVenueRef sql=external_venue_ref
    provider Text
    externalId Text
    venueId VenueId
    lastSeenAt UTCTime
    UniqueExternalVenueRef provider externalId
    deriving Show Generic

ExternalArtistRef sql=external_artist_ref
    provider Text
    externalId Text
    artistId ArtistProfileId
    lastSeenAt UTCTime
    UniqueExternalArtistRef provider externalId
    deriving Show Generic

ExternalEventRef sql=external_event_ref
    provider Text
    externalId Text
    eventId SocialEventId
    city Text
    countryCode Text Maybe
    sourceUrl Text Maybe
    priceCents Int Maybe
    currency Text Maybe
    lastSeenAt UTCTime
    missingRuns Int default=0
    sourceStatus Text default='active'
    UniqueExternalEventRef provider externalId
    deriving Show Generic

ExternalEventDiscoveryRun sql=external_event_discovery_run
    provider Text
    runDate Day
    scheduledFor UTCTime Maybe
    status Text
    citiesCount Int
    eventsSeen Int
    eventsCreated Int
    eventsUpdated Int
    venuesCreated Int
    artistsCreated Int
    errorMessage Text Maybe
    startedAt UTCTime
    finishedAt UTCTime Maybe
    UniqueExternalEventDiscoverySlot provider scheduledFor !force
    deriving Show Generic

EventCity sql=event_city
    name Text
    normalizedName Text
    countryCode Text
    timeZone Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventCity normalizedName countryCode
    deriving Show Generic

EventCitySubscription sql=event_city_subscription
    partyId Text
    cityId EventCityId
    createdAt UTCTime default=now()
    UniqueEventCitySubscription partyId cityId
    deriving Show Generic

EventDiscoverySource sql=event_discovery_source
    sourceKey Text
    name Text
    sourceType Text
    feedUrl Text Maybe
    cityId EventCityId Maybe
    enabled Bool default=TRUE
    priority Int default=100
    configuration Text Maybe
    etag Text Maybe
    lastModified Text Maybe
    consecutiveFailures Int default=0
    lastSuccessAt UTCTime Maybe
    lastError Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventDiscoverySource sourceKey
    deriving Show Generic

EventResearchPilotControl sql=event_research_pilot_control
    controlKey Text
    approved Bool default=FALSE
    approvedAt UTCTime Maybe
    approvedByPartyId Text Maybe
    approvalReference Text Maybe
    maxActiveCandidates Int default=20
    updatedAt UTCTime default=now()
    UniqueEventResearchPilotControl controlKey
    deriving Show Generic

EventResearchPilotAudit sql=event_research_pilot_audit
    controlId EventResearchPilotControlId
    approved Bool
    approvedByPartyId Text
    approvalReference Text
    createdAt UTCTime default=now()
    deriving Show Generic

EventResearchRun sql=event_research_run
    runKey Text
    status Text
    reconciliation Bool default=FALSE
    checkpoint Text Maybe
    counters Text default='{}'
    errorSummary Text Maybe
    startedAt UTCTime
    updatedAt UTCTime
    finishedAt UTCTime Maybe
    createdByPartyId Text
    UniqueEventResearchRun runKey
    deriving Show Generic

EventResearchCandidate sql=event_research_candidate
    provider Text
    externalId Text
    runId EventResearchRunId
    sourceId EventDiscoverySourceId Maybe
    eventId SocialEventId Maybe
    reviewState Text
    title Text
    startTime UTCTime Maybe
    endTime UTCTime Maybe
    timezone Text
    venueName Text Maybe
    city Text Maybe
    province Text Maybe
    countryCode Text
    sourceUrl Text
    infoUrl Text Maybe
    purchaseUrl Text Maybe
    payload Text
    evidence Text
    confidence Text
    managedFields Text default='[]'
    contentHash Text
    verifiedAt UTCTime
    isPilot Bool default=TRUE
    createdAt UTCTime
    updatedAt UTCTime
    UniqueEventResearchCandidate provider externalId
    deriving Show Generic

EventResearchChange sql=event_research_change
    runId EventResearchRunId
    candidateId EventResearchCandidateId Maybe
    eventId SocialEventId Maybe
    action Text
    beforeValue Text Maybe
    afterValue Text Maybe
    sourceUrl Text
    confidence Text
    consultedAt UTCTime
    externalId Text
    result Text
    dedupeKey Text
    createdAt UTCTime
    UniqueEventResearchChange dedupeKey
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
    Id UUID default=gen_random_uuid()
    momentId EventMomentId
    reactionTypeId UUID Maybe
    reaction Text Maybe
    reactorPartyId Text
    createdAt UTCTime default=now()
    UniqueEventMomentReaction momentId reactionTypeId reactorPartyId !force
    deriving Show Generic

EventMomentComment
    momentId EventMomentId
    authorPartyId Text Maybe
    authorName Text
    body Text
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventLiveBroadcast
    eventId SocialEventId
    artistId ArtistProfileId
    broadcasterPartyId Text
    broadcasterName Text
    title Text
    description Text Maybe
    status Text
    playbackUrl Text Maybe
    ingestUrl Text Maybe
    whipUrl Text Maybe
    streamKey Text Maybe
    viewerCount Int
    startedAt UTCTime
    endedAt UTCTime Maybe
    lastHeartbeatAt UTCTime
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

ArtistGenre
    artistId ArtistProfileId
    genre Text
    genreId UUID Maybe
    Primary artistId genre
    deriving Show Generic

-- Canonical relationship used by all new artist-profile writes. ArtistGenre
-- remains migration evidence only until every historical row is reviewed.
ArtistGenreMembership sql=artist_genre_membership
    artistId ArtistProfileId
    genreId UUID
    sortOrder Int default=0
    createdAt UTCTime default=now()
    Primary artistId genreId
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
    currencyId UUID Maybe
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
    checkoutIdempotencyKey Text Maybe
    purchasedAt UTCTime
    stripePaymentIntentId Text Maybe
    promoCodeId PromoCodeId Maybe
    originalAmountCents Int Maybe
    paymentMethod Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventTicketCheckout buyerPartyId checkoutIdempotencyKey !force
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

EventLogisticsPlan
    eventId SocialEventId
    timezone Text default='UTC'
    defaultTravelMode Text default='drive'
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventLogisticsPlan eventId
    deriving Show Generic

EventLogisticsMember
    eventId SocialEventId
    partyId Text
    memberRole Text
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    UniqueEventLogisticsMember eventId partyId
    deriving Show Generic

EventLogisticsPlace
    eventId SocialEventId
    venueId VenueId Maybe
    label Text
    placeType Text
    address Text Maybe
    googlePlaceId Text Maybe
    latitude Double
    longitude Double
    instructions Text Maybe
    contactName Text Maybe
    contactPhone Text Maybe
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventLogisticsActivity
    eventId SocialEventId
    activityType Text
    title Text
    notes Text Maybe
    startTime UTCTime
    endTime UTCTime Maybe
    placeId EventLogisticsPlaceId Maybe
    originPlaceId EventLogisticsPlaceId Maybe
    destinationPlaceId EventLogisticsPlaceId Maybe
    travelMode Text Maybe
    bufferMinutes Int Maybe
    priority Text
    status Text
    version Int default=1
    createdByPartyId Text
    createdAt UTCTime default=now()
    updatedAt UTCTime default=now()
    deriving Show Generic

EventLogisticsAssignment
    activityId EventLogisticsActivityId
    partyId Text Maybe
    externalName Text Maybe
    externalPhone Text Maybe
    externalEmail Text Maybe
    createdAt UTCTime default=now()
    deriving Show Generic

EventLogisticsDependency
    activityId EventLogisticsActivityId
    dependsOnActivityId EventLogisticsActivityId
    createdAt UTCTime default=now()
    UniqueEventLogisticsDependency activityId dependsOnActivityId
    deriving Show Generic

EventRouteVerification
    activityId EventLogisticsActivityId
    activityVersion Int
    provider Text
    travelMode Text
    departureTime UTCTime
    durationSeconds Int Maybe
    staticDurationSeconds Int Maybe
    distanceMeters Int Maybe
    bufferSeconds Int
    allocatedSeconds Int
    verdict Text
    encodedPolyline Text Maybe
    errorMessage Text Maybe
    checkpoint Text Maybe
    verifiedAt UTCTime
    deriving Show Generic

EventLogisticsAlertDelivery
    activityId EventLogisticsActivityId
    activityVersion Int
    checkpoint Text
    recipientPartyId Text
    channel Text
    deliveredAt UTCTime
    UniqueEventLogisticsAlert activityId activityVersion checkpoint recipientPartyId channel
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
