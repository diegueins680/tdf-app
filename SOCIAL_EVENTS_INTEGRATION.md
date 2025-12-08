# Social Event Calendar - Developer Integration Guide

## Quick Reference for Backend Integration

### Phase 1: Database Setup (Week 1-2)

**Haskell Backend - Database Migration**

1. Create `src/TDF/DB/SocialEvents.hs` with migrations:
   ```haskell
   -- SocialEvent table
   share [mkPersist sqlSettings, mkMigrate "migrateSocialEvents"] [persistLowerCase|
     SocialEvent sql=social_events
       title Text
       description (Maybe Text)
       startDateTime UTCTime
       endDateTime UTCTime
       venueId VenueId
       createdBy PartyId
       ticketPrice (Maybe Int64)
       ticketUrl (Maybe Text)
       isPublic Bool
       createdAt UTCTime default=now()
       updatedAt UTCTime default=now()
       deriving Show Generic
   |]
   ```

2. Create junction tables:
   - `event_artists` (SocialEventId <-> ArtistProfileId)
   - `artist_genres` (ArtistProfileId <-> Genre Text)

3. Update `Database.persist.Sqlite` migrations config

### Phase 2: Types & DTOs (Week 1-2)

**Backend - Update Models**

1. Add enums to `TDF.Models`:
   ```haskell
   data RSVPStatusEnum = Going | Interested | NotGoing | None
   data InvitationStatusEnum = Pending | Accepted | Declined | Expired
   ```

2. Add models:
   - ArtistProfile
   - Venue
   - EventRSVP
   - EventInvitation

3. Create DTOs in `TDF.DTO`:
   - SocialEventDTO
   - SocialEventCreatePayload
   - VenueDTO
   - ArtistProfileDTO
   - EventRSVPDTO

### Phase 3: API Endpoints (Week 2-3)

**Backend - Implement Handlers**

1. Create `TDF.API.SocialEvents` with type definition:
   ```haskell
   type SocialEventsAPI =
       "events" :> Get '[JSON] [SocialEventDTO]
     :<|> "events" :> ReqBody '[JSON] SocialEventCreatePayload :> Post '[JSON] SocialEventDTO
     :<|> "events" :> Capture "id" Text :> Get '[JSON] SocialEventDTO
     -- ... more endpoints
   ```

2. Create handlers in `TDF.Server`:
   - `listEvents` - with filtering by city, artist, upcoming
   - `createEvent` - with transaction for artist associations
   - `getEvent`, `updateEvent`, `deleteEvent`
   - RSVP handlers
   - Invitation handlers

3. Create artist and venue handlers similarly

### Phase 4: OpenAPI & Client Generation (Week 3)

**Backend**

1. Run OpenAPI generation after implementing all endpoints:
   ```bash
   cd tdf-hq
   make generate-openapi
   ```

2. Verify `docs/openapi/lessons-and-receipts.yaml` includes new endpoints

**Frontend**

3. Regenerate TypeScript clients:
   ```bash
   cd tdf-app
   npm run generate:api:mobile
   npm run generate:api:ui
   ```

4. Update API imports in mobile app to use generated clients instead of manual APIs

### Phase 5: Testing & Deployment (Week 4+)

## API Implementation Priority Order

### High Priority (Core Features)
1. Events CRUD + list with filters
2. Venue CRUD + list with geolocation
3. Artist profiles CRUD + list
4. Event RSVP functionality
5. Event invitations

### Medium Priority (Enhanced Features)
6. Search functionality (artists, venues, events)
7. RSVP analytics (count by status)
8. User-specific queries (my events, my artist profile, etc.)

### Nice-to-Have
9. Event recommendations
10. Venue ratings
11. Social graph (following artists/users)

## Testing Endpoints with cURL

```bash
# List events
curl -X GET "http://localhost:8080/api/v1/events?city=Quito"

# Create event (authenticated)
curl -X POST "http://localhost:8080/api/v1/events" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Summer Concert",
    "startDateTime": "2024-06-15T19:00:00Z",
    "endDateTime": "2024-06-15T22:00:00Z",
    "venueId": "venue-123",
    "artistIds": ["artist-1", "artist-2"],
    "ticketPrice": 5000,
    "isPublic": true
  }'

# Get venue list with geolocation
curl -X GET "http://localhost:8080/api/v1/venues?nearCoords=0.35,-78.51&radiusKm=5"

# Create artist profile
curl -X POST "http://localhost:8080/api/v1/artists" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "John Doe",
    "bio": "Electronic music producer",
    "genres": ["Electronic", "Ambient"],
    "instagramHandle": "@johndoe",
    "spotifyUrl": "https://open.spotify.com/artist/..."
  }'

# RSVP to event
curl -X POST "http://localhost:8080/api/v1/events/{eventId}/rsvp" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"status": "Going"}'
```

## Database Query Examples

### Get upcoming events in a city
```sql
SELECT e.* FROM social_events e
JOIN venues v ON e.venue_id = v.id
WHERE v.city = 'Quito'
AND e.start_date_time > NOW()
ORDER BY e.start_date_time ASC
LIMIT 50;
```

### Find venues near coordinates
```sql
SELECT v.* FROM venues v
WHERE 
  (ACOS(
    SIN(RADIANS(v.latitude)) * SIN(RADIANS(0.35)) +
    COS(RADIANS(v.latitude)) * COS(RADIANS(0.35)) *
    COS(RADIANS(v.longitude) - RADIANS(-78.51))
  ) * 6371) <= 5
ORDER BY (ACOS(...)) ASC;
```

### Get artist profiles by genre
```sql
SELECT ap.* FROM artist_profiles ap
JOIN artist_genres ag ON ap.id = ag.artist_profile_id
WHERE ag.genre = 'Electronic'
ORDER BY ap.created_at DESC;
```

## Common Issues & Solutions

### Issue: Mobile app not connecting to backend
- **Solution**: Verify `EXPO_PUBLIC_API_BASE` environment variable matches backend URL
- Check CORS configuration in backend (should already be permissive in dev)

### Issue: Geolocation queries too slow
- **Solution**: Add database indexes:
  ```sql
  CREATE INDEX idx_venues_city ON venues(city);
  CREATE INDEX idx_venues_coords ON venues(latitude, longitude);
  CREATE INDEX idx_events_start_time ON social_events(start_date_time);
  ```

### Issue: Artist/Venue associations not updating
- **Solution**: Ensure junction tables (event_artists, artist_genres) are properly updated when creating/updating entities

### Issue: RSVP count mismatch
- **Solution**: Verify RSVP logic handles status transitions (Going -> NotGoing, etc.) correctly - should update existing RSVP, not create new one

## Performance Optimization Tips

1. **Caching**
   - Cache popular events (top 100 events)
   - Cache frequently accessed venues by city
   - Cache artist profiles

2. **Pagination**
   - Always paginate large result sets (max 50 items)
   - Use offset/limit pattern or cursor-based pagination

3. **Query Optimization**
   - Eager-load related entities (venue, artists) with event queries
   - Use database projections for list views (don't fetch full event data)
   - Pre-calculate RSVP counts as denormalized field

4. **Indexes**
   - (city, start_time) for events
   - (latitude, longitude) for venues
   - (event_id, user_id) for RSVPs (unique constraint)

## Monitoring & Logging

Add logging for:
- Event creation/deletion
- RSVP changes
- Geolocation queries (slow queries)
- API errors and validation failures

Example:
```haskell
logInfo $ "Event created: " <> eventTitle <> " by " <> show userId
```

## Mobile App Testing Checklist

After backend is deployed:

- [ ] Login and authenticate
- [ ] Create event with all fields
- [ ] View event details
- [ ] RSVP to event (all statuses)
- [ ] Create artist profile
- [ ] Search for events by city
- [ ] Search for artists by genre
- [ ] Discover venues by location
- [ ] View venue details and events
- [ ] Send event invitation to friend
- [ ] Accept/decline invitation
- [ ] Edit event details
- [ ] Delete event
- [ ] Edit artist profile
- [ ] Create venue

## Next Steps

1. Schedule backend development sprint
2. Set up test database
3. Implement Phase 1 (database)
4. Test with cURL
5. Implement Phase 2-3 (types and endpoints)
6. Generate OpenAPI and clients
7. Test mobile integration
8. Deploy to staging
9. Run full QA cycle
10. Deploy to production
