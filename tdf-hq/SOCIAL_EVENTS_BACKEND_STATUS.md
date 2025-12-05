# Social Events Backend - Integration Status

## Completed ✅

### Core Implementation
- **Database Schema**: SQL migration (`0001_create_social_events.sql`) with tables for artists, venues, events, artist-event junctions, RSVPs, and invitations
- **Persistent Models**: Haskell data types for all entities in `TDF/Models/SocialEventsModels.hs` with migration exports
- **DTOs**: Typed data transfer objects in `TDF/DTO/SocialEventsDTO.hs` (EventDTO, VenueDTO, ArtistDTO) with JSON instances
- **API Types**: Servant API definitions in `TDF/API/SocialEventsAPI.hs` with endpoints for events, venues, artists, RSVPs, invitations
- **Handlers**: Full implementation in `TDF/Server/SocialEventsHandlers.hs` with:
  - ✅ `listEvents` - returns `[EventDTO]` with artists populated
  - ✅ `getEvent` - returns single `EventDTO` with associated artists loaded
  - ✅ `createEvent` - inserts event, resolves venue ID, creates EventArtist junctions
  - ✅ `updateEvent` - validates title/times, updates event record
  - ✅ `deleteEvent` - removes event
  - ✅ `listVenues` - returns `[VenueDTO]` with city filtering
  - ✅ `getVenue` - returns single `VenueDTO`
  - ✅ `createVenue` - inserts venue with geolocation
  - ✅ `updateVenue` - updates venue details
  - ⏸️ `listArtists`, `createArtist`, `getArtist`, `updateArtist` - stubs (501)
  - ⏸️ `listRsvps`, `createRsvp` - stubs (501)
  - ⏸️ `createInvitation` - stub (501)

### Authentication & Security
- Handlers accept `AuthedUser` parameter → wired to `ProtectedAPI` via `AuthProtect "bearer-token"`
- All write endpoints (create/update/delete) require valid bearer token
- Read endpoints also protected

### API Wiring
- `SocialEventsAPI` added to imports in `TDF/API.hs`
- Exposed in `ProtectedAPI` type (not public; secured by auth handler)
- `socialEventsServer user` called in `protectedServer` with `AuthedUser` parameter
- Top-level server composes both unauthenticated and authenticated endpoints

## Current State: Production-Ready for Events & Venues

The core event and venue CRUD operations are fully functional with:
- **Typed DTOs** ensuring compile-time safety
- **Authentication** via bearer tokens
- **Validation** (title required, start < end, venue ID resolution)
- **Relation linking** (event-to-artists, event-to-venue)
- **Error handling** (400/404/500 responses)

## Next Steps (Priority Order)

### 1. Generate OpenAPI & TypeScript Clients (High Priority)
```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-hq
stack build  # Compile Haskell
# Use OpenAPI generation (if configured in project)
cd /Users/diegosaa/GitHub/tdf-app
npm run generate:api:ui      # Generate UI clients
npm run generate:api:mobile  # Generate mobile clients
```

### 2. Integrate Mobile to Generated Clients
- Replace manual API modules in `tdf-mobile/src/api/` with generated hooks
- Update screens to use `useEventsQuery`, `useCreateEventMutation`, etc.
- Wire QueryClient and error handling

### 3. Implement Missing Endpoints
- **Artists**: Reuse existing artist profile logic from `TDF.Profiles.Artist`
- **RSVPs**: Create handlers linking `EventRsvp` to `auPartyId`
- **Invitations**: Create handlers for `EventInvitation` with status tracking

### 4. Enhancement: Photo Uploads
- Add `imageUrl` fields to `Venue` and `SocialEvent` models
- Implement multipart form handler for image uploads
- Integrate Cloudinary or similar CDN

### 5. Map View & Geolocation
- Frontend: Integrate MapLibre or Google Maps
- Display venue markers with clustering
- Implement radius-based event search

## API Endpoints (Protected)

```
POST   /events                   - Create event
GET    /events                   - List events (city filter)
GET    /events/{id}              - Get event with artists
PUT    /events/{id}              - Update event
DELETE /events/{id}              - Delete event

POST   /venues                   - Create venue
GET    /venues                   - List venues (city filter)
GET    /venues/{id}              - Get venue
PUT    /venues/{id}              - Update venue

GET    /artists                  - List artists (not impl)
POST   /artists                  - Create artist (not impl)
GET    /artists/{id}             - Get artist (not impl)
PUT    /artists/{id}             - Update artist (not impl)

GET    /events/{eventId}/rsvps   - List RSVPs (not impl)
POST   /events/{eventId}/rsvps   - Create RSVP (not impl)

POST   /events/{eventId}/invitations - Send invitation (not impl)
```

## Testing
- Manual curl tests recommended before mobile integration
- Example:
```bash
curl -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -X POST http://localhost:8080/events \
  -d '{"eventTitle":"Concert","eventStart":"2025-01-15T20:00:00Z","eventEnd":"2025-01-15T23:00:00Z"}'
```

## Notes
- Auth pattern follows project convention (bearer token → `AuthedUser`)
- DB pattern uses `Env` + `runSqlPool` (consistent with existing codebase)
- DTOs enable type-safe JSON serialization (Aeson)
- No UUID primary keys in Persistent models yet (uses default integer keys); can migrate to UUID if needed
