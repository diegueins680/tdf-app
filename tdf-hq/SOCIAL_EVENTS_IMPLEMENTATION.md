# Social Event Calendar - Backend Implementation Guide

## Overview
This document outlines the backend API requirements for the social event calendar feature in TDF-mobile. The feature enables users to create and discover social events, manage artist profiles, venues with geolocation, and RSVP functionality.

## Database Models

### 1. SocialEvent Model
```haskell
data SocialEventGeneric backend = SocialEvent
  { socialEventId :: SocialEventId
  , socialEventTitle :: Text
  , socialEventDescription :: Maybe Text
  , socialEventStartDateTime :: UTCTime
  , socialEventEndDateTime :: UTCTime
  , socialEventVenueId :: VenueId
  , socialEventCreatedBy :: PartyId
  , socialEventTicketPrice :: Maybe Int64 -- in cents
  , socialEventTicketUrl :: Maybe Text
  , socialEventIsPublic :: Bool
  , socialEventCreatedAt :: UTCTime
  , socialEventUpdatedAt :: UTCTime
  } deriving (Generic, Show)

-- Also need junction table: EventArtists
-- SocialEventId -> ArtistProfileId (many-to-many)
```

### 2. ArtistProfile Model
```haskell
data ArtistProfileGeneric backend = ArtistProfile
  { artistProfileId :: ArtistProfileId
  , artistProfilePartyId :: PartyId -- Link to existing Party model
  , artistProfileName :: Text
  , artistProfileBio :: Maybe Text
  , artistProfileImageUrl :: Maybe Text
  , artistProfileInstagramHandle :: Maybe Text
  , artistProfileSpotifyUrl :: Maybe Text
  , artistProfileCreatedAt :: UTCTime
  , artistProfileUpdatedAt :: UTCTime
  } deriving (Generic, Show)

-- Also need junction table: ArtistGenres
-- ArtistProfileId -> Genre (many-to-many)
```

### 3. Venue Model
```haskell
data VenueGeneric backend = Venue
  { venueId :: VenueId
  , venueName :: Text
  , venueAddress :: Text
  , venueCity :: Text
  , venueState :: Maybe Text
  , venueZipCode :: Maybe Text
  , venueLatitude :: Double
  , venueLongitude :: Double
  , venueCapacity :: Maybe Int
  , venueImageUrl :: Maybe Text
  , venuePhoneNumber :: Maybe Text
  , venueWebsite :: Maybe Text
  , venueCreatedAt :: UTCTime
  , venueUpdatedAt :: UTCTime
  } deriving (Generic, Show)
```

### 4. EventRSVP Model
```haskell
data RSVPStatusEnum = Going | Interested | NotGoing | None
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "RSVPStatusEnum"

data EventRSVPGeneric backend = EventRSVP
  { eventRSVPId :: EventRSVPId
  , eventRSVPEventId :: SocialEventId
  , eventRSVPUserId :: PartyId
  , eventRSVPStatus :: RSVPStatusEnum
  , eventRSVPCreatedAt :: UTCTime
  , eventRSVPUpdatedAt :: UTCTime
  } deriving (Generic, Show)
```

### 5. EventInvitation Model
```haskell
data InvitationStatusEnum = Pending | Accepted | Declined | Expired
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "InvitationStatusEnum"

data EventInvitationGeneric backend = EventInvitation
  { eventInvitationId :: EventInvitationId
  , eventInvitationEventId :: SocialEventId
  , eventInvitationFromUserId :: PartyId
  , eventInvitationToUserId :: PartyId
  , eventInvitationStatus :: InvitationStatusEnum
  , eventInvitationCreatedAt :: UTCTime
  , eventInvitationUpdatedAt :: UTCTime
  } deriving (Generic, Show)
```

## DTO Models

### Events API Responses
```haskell
data SocialEventDTO = SocialEventDTO
  { id :: Text
  , title :: Text
  , description :: Maybe Text
  , startDateTime :: UTCTime
  , endDateTime :: UTCTime
  , venue :: VenueDTO
  , artists :: [ArtistProfileDTO]
  , ticketPrice :: Maybe Int64
  , ticketUrl :: Maybe Text
  , isPublic :: Bool
  , rsvpCount :: Int
  } deriving (Generic, Show)

data SocialEventCreatePayload = SocialEventCreatePayload
  { title :: Text
  , description :: Maybe Text
  , startDateTime :: UTCTime
  , endDateTime :: UTCTime
  , venueId :: Text
  , artistIds :: [Text]
  , ticketPrice :: Maybe Int64
  , ticketUrl :: Maybe Text
  , isPublic :: Bool
  } deriving (Generic, Show)

data VenueDTO = VenueDTO
  { id :: Text
  , name :: Text
  , address :: Text
  , city :: Text
  , state :: Maybe Text
  , zipCode :: Maybe Text
  , latitude :: Double
  , longitude :: Double
  , capacity :: Maybe Int
  , imageUrl :: Maybe Text
  , phoneNumber :: Maybe Text
  , website :: Maybe Text
  } deriving (Generic, Show)

data ArtistProfileDTO = ArtistProfileDTO
  { id :: Text
  , partyId :: Text
  , name :: Text
  , bio :: Maybe Text
  , imageUrl :: Maybe Text
  , genres :: [Text]
  , instagramHandle :: Maybe Text
  , spotifyUrl :: Maybe Text
  } deriving (Generic, Show)
```

## API Endpoints

### Events
- `GET /api/v1/events` - List events with filters (city, artistId, upcomingOnly)
- `GET /api/v1/events/:id` - Get event details
- `POST /api/v1/events` - Create event (authenticated)
- `PUT /api/v1/events/:id` - Update event (owner only)
- `DELETE /api/v1/events/:id` - Delete event (owner only)

### Event RSVPs
- `GET /api/v1/events/:id/rsvps` - Get RSVP list for event
- `POST /api/v1/events/:id/rsvp` - RSVP to event (authenticated)
- `PUT /api/v1/events/:id/rsvp` - Update RSVP status (authenticated)
- `DELETE /api/v1/events/:id/rsvp` - Remove RSVP (authenticated)

### Event Invitations
- `GET /api/v1/events/:id/invitations` - Get invitations for event
- `POST /api/v1/events/:id/invitations` - Send invitation (authenticated)
- `POST /api/v1/invitations/:id/accept` - Accept invitation (authenticated)
- `POST /api/v1/invitations/:id/decline` - Decline invitation (authenticated)

### Artists
- `GET /api/v1/artists` - List artists with search/filters
- `GET /api/v1/artists/:id` - Get artist profile
- `GET /api/v1/artists/party/:partyId` - Get artist profile by party
- `POST /api/v1/artists` - Create artist profile (authenticated)
- `PUT /api/v1/artists/:id` - Update artist profile (owner only)
- `DELETE /api/v1/artists/:id` - Delete artist profile (owner only)
- `GET /api/v1/artists/search?query=...` - Search artists by name
- `GET /api/v1/artists/search?genre=...` - Search artists by genre

### Venues
- `GET /api/v1/venues` - List venues with filters (city, coordinates, radius)
- `GET /api/v1/venues/:id` - Get venue details
- `POST /api/v1/venues` - Create venue (authenticated)
- `PUT /api/v1/venues/:id` - Update venue (owner/admin)
- `DELETE /api/v1/venues/:id` - Delete venue (owner/admin)
- `GET /api/v1/venues/search?query=...` - Search venues by name

## Query Parameters

### Events List
- `city` (string) - Filter by city
- `artistId` (string) - Filter by artist performing
- `upcomingOnly` (boolean) - Show only future events
- `skip` (int) - Pagination offset
- `limit` (int) - Pagination limit

### Venues List
- `city` (string) - Filter by city
- `nearCoords` (object) - { lat: number, lng: number }
- `radiusKm` (number) - Search radius in kilometers
- `query` (string) - Text search

### Artists Search
- `query` (string) - Search by name
- `genre` (string) - Filter by genre
- `skip` (int) - Pagination offset
- `limit` (int) - Pagination limit

## Implementation Checklist

### Database Layer (TDF.DB)
- [ ] Create migration for SocialEvent table
- [ ] Create migration for ArtistProfile table
- [ ] Create migration for Venue table
- [ ] Create migration for EventRSVP table
- [ ] Create migration for EventInvitation table
- [ ] Create migration for EventArtists junction table
- [ ] Create migration for ArtistGenres junction table

### Models (TDF.Models)
- [ ] Define RSVPStatusEnum
- [ ] Define InvitationStatusEnum
- [ ] Define SocialEvent model with Persistent
- [ ] Define ArtistProfile model with Persistent
- [ ] Define Venue model with Persistent
- [ ] Define EventRSVP model with Persistent
- [ ] Define EventInvitation model with Persistent

### DTOs (TDF.DTO)
- [ ] Create SocialEventDTO
- [ ] Create SocialEventCreatePayload
- [ ] Create SocialEventUpdatePayload
- [ ] Create VenueDTO
- [ ] Create VenueCreatePayload
- [ ] Create ArtistProfileDTO
- [ ] Create ArtistProfileCreatePayload
- [ ] Create EventRSVPDTO
- [ ] Create EventInvitationDTO

### API Routes (TDF.API)
- [ ] Create SocialEventsAPI type
- [ ] Create VenuesAPI type
- [ ] Create ArtistsAPI type
- [ ] Create EventRSVPsAPI type
- [ ] Create EventInvitationsAPI type

### Handlers (TDF.Server or TDF.Handlers)
- [ ] Implement events CRUD handlers
- [ ] Implement events list with filters
- [ ] Implement RSVP handlers
- [ ] Implement invitation handlers
- [ ] Implement artist CRUD handlers
- [ ] Implement artist search handlers
- [ ] Implement venue CRUD handlers
- [ ] Implement venue geolocation search

### OpenAPI Generation
- [ ] Run OpenAPI generation after handlers are implemented
- [ ] Update `docs/openapi/lessons-and-receipts.yaml`
- [ ] Regenerate TypeScript clients with `npm run generate:api:ui` and `npm run generate:api:mobile`

## Authentication & Authorization
- All creation endpoints require authentication (authenticated user ID from JWT)
- Event owners can update/delete their own events
- Artist profiles linked to authenticated user
- Venue management: admin or venue creator
- Public events visible to all; private events only to invited users

## Performance Considerations
- Use database indexes on (city, startDateTime) for event listing
- Use indexes on (latitude, longitude) for venue geolocation queries
- Cache popular events and venues
- Implement pagination for large result sets (limit 50 items per page)

## Error Handling
- 400 Bad Request - Invalid input
- 401 Unauthorized - Missing authentication
- 403 Forbidden - Insufficient permissions
- 404 Not Found - Resource not found
- 409 Conflict - Business logic violation (e.g., duplicate artist in event)
- 500 Internal Server Error - Server error
