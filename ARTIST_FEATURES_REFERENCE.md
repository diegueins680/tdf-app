# Artist Features - Quick Reference

## Overview
The Social Events platform includes artist profile management, enabling musicians to showcase their work and perform at events.

## Artist Data Model

### Core Fields (Database)
```haskell
-- From tdf-hq/src/TDF/Models.hs
data Artist = Artist
  { artistId :: ArtistId
  , artistName :: Text              -- Artist/band name
  , artistBio :: Maybe Text         -- Biography
  , artistGenres :: [Text]          -- Music genres (jazz, rock, etc.)
  , artistImageUrl :: Maybe Text    -- Profile photo
  , artistWebsiteUrl :: Maybe Text  -- Official website
  , artistCreatedAt :: UTCTime
  , artistUpdatedAt :: UTCTime
  } deriving (Show, Read, Eq, Ord, Generic)
```

### Frontend Type (Mobile/Web)
```typescript
// From tdf-mobile/src/types/index.ts
export interface ArtistProfile {
  id: ID;
  name: string;
  bio?: string;
  genres: string[];
  imageUrl?: string;
  websiteUrl?: string;
  socialLinks?: {
    spotify?: string;
    instagram?: string;
    twitter?: string;
    youtube?: string;
    soundcloud?: string;
  };
  stats?: {
    followersCount: number;
    eventsCount: number;
  };
}
```

## Current Implementation Status

### ✅ Completed
- Database model and schema
- Artist-Event junction table (EventArtist)
- Artist population in event queries
- Type definitions on mobile

### 🟡 In Progress (Stubbed - 501 responses)
- Create artist profile endpoint
- List artists endpoint
- Get artist profile endpoint
- Update artist profile endpoint
- Search artists by name/genre

### ❌ Not Yet Started
- Artist portfolio/discography
- Streaming platform links (Spotify, YouTube, SoundCloud)
- Photo upload for profiles
- Artist analytics and following
- Direct messaging between artists/fans
- Collaboration/featured artist marking

## Key Use Cases

### 1. Artist Discovers Platform
- Artist signs up / creates profile
- Adds bio, genres, links to music platforms
- Uploads profile photo

### 2. Event Creator Books Artists
- Searches for artists by name or genre
- Views artist profile, listens to samples
- Books artist for event
- Event appears on artist's profile

### 3. Fan Follows Artist
- Discovers artist through events or search
- Follows/saves artist
- Gets notifications of new events
- Accesses artist's music links

### 4. Artist Manages Calendar
- Views all upcoming bookings
- Accepts/declines booking requests
- Updates availability
- Sees performance history

## Testing Artist Features

### Unit Test: Artist Creation
```bash
# Backend
curl -X POST http://localhost:8080/artists \
  -H "Authorization: Bearer test-token" \
  -H "Content-Type: application/json" \
  -d '{
    "artistName": "The Jazz Collective",
    "artistBio": "Modern jazz ensemble",
    "artistGenres": ["jazz", "fusion"],
    "artistWebsiteUrl": "https://example.com"
  }'

# Expected: 201 Created with artistId
```

### Integration Test: Artist in Event
```bash
# 1. Create artist
# 2. Create event with that artist
# 3. GET /events/{id} should include artist in eventArtists array
# 4. Verify eventArtists contains:
#    - artistId
#    - artistName
#    - artistGenres
```

### Mobile Test: Artist Selection
1. Open CreateEvent form
2. Tap "Select Artists"
3. See list populated from backend
4. Search by artist name
5. Multiple selection works
6. Selected artists display in summary

## Next Phase: Implementation Roadmap

### Step 1: Implement Artist CRUD (4-6 hours)
- [ ] Create `TDF.Server.ArtistsHandlers` with full CRUD
- [ ] Add handlers to API routes
- [ ] Type-safe DTOs (ArtistDTO)
- [ ] Authentication checks (can only edit own profile)

```haskell
-- Handler signature pattern
artistsServer :: AuthedUser -> ServerT ArtistsAPI AppM
```

- [ ] Test with curl commands
- [ ] Verify artists appear in event creation form

### Step 2: Artist Search & Filtering (2-3 hours)
- [ ] Backend: Search by name, genre
- [ ] Mobile: Update Artists.searchByName() implementation
- [ ] UI: Genre pills/filters in artist selection modal
- [ ] Test: Search returns correct results

### Step 3: Artist Profile Pages (4-5 hours)
- [ ] Mobile: `app/artistDetail.tsx` screen
- [ ] Display: Bio, genres, upcoming events
- [ ] Actions: Follow, message, share (placeholders)
- [ ] Navigation: Link from event detail and search

### Step 4: Photo Uploads (3-4 hours)
- [ ] Backend: S3/Cloudinary integration for images
- [ ] Mobile: Image picker for profile photo
- [ ] API: POST endpoint for photo upload
- [ ] Display: Artist photos in events and profiles

### Step 5: Streaming Links (2-3 hours)
- [ ] Backend: Store socialLinks in artist profile
- [ ] Mobile: Display Spotify, YouTube, etc. buttons
- [ ] Action: Deep link to external platforms
- [ ] Test: Links open correctly in apps/browser

## Artist-Focused Features Ideas

### High Priority (Artist-Centric)
- **Public Artist Profile**: One-sheet with bio, photos, music links, upcoming shows
- **EPK (Electronic Press Kit)**: Auto-generated PDF with artist info for promoters
- **Booking Request**: Promoters can request artist for events
- **Performance Calendar**: Show to fans, bookings appear here
- **Streaming Stats**: Display play counts if connected to Spotify API

### Medium Priority
- **Collaboration Tools**: Mark featured artists on tracks
- **Media Gallery**: Photos from past performances
- **Fan Comments**: Fans leave messages on artist profiles
- **Direct Messaging**: Artists communicate with promoters/fans
- **Analytics**: Concerts per month, fan growth, reach

### Lower Priority (Future)
- **Release Calendar**: Track upcoming album drops
- **Merchandise Integration**: Link to artist's merch store
- **Ticket Revenue Tracking**: See income from ticket sales
- **Tour Planning**: Optimize routing for multi-city tours
- **Press Kit Generation**: Export as PDF for media/promoters

## Code Patterns to Follow

### Backend (Haskell)
```haskell
-- 1. Handler with authentication
artistsServer :: AuthedUser -> ServerT ArtistsAPI AppM

-- 2. Extract user ID from AuthedUser
let userId = authUserId authedUser

-- 3. Query database with Persistent
artists <- runSqlPool
  (selectList [ArtistCreatedBy ==. userId] [Asc ArtistName])
  (appConnPool env)

-- 4. Map to DTO for response
return $ map toArtistDTO artists

-- 5. Pattern for authorization check
when (authorId /= userId) $
  throwError err403 { errBody = "Unauthorized" }
```

### Mobile (TypeScript)
```typescript
// 1. Use mapping layer for consistency
export const Artists = {
  list: async (filters?: ArtistFilters) => {
    const results = await get<any[]>('/artists', filters);
    return results.map(mapBackendArtistToFrontend);
  },

  // 2. Create with proper type safety
  create: async (body: ArtistCreate) => {
    const response = await post<any>('/artists', body);
    return mapBackendArtistToFrontend(response);
  }
};

// 3. Use in components
const { data: artists } = useQuery({
  queryKey: ['artists'],
  queryFn: () => Artists.list()
});
```

## File Locations

**Backend**:
- Models: `tdf-hq/src/TDF/Models.hs` (Artist entity)
- Database: `tdf-hq/config/models` (Persistent schema)
- Handlers: `tdf-hq/src/TDF/Server/` (ArtistsHandlers.hs - to be created)
- Routes: `tdf-hq/src/TDF/API/ArtistsAPI.hs` (to be created)

**Mobile**:
- Types: `tdf-mobile/src/types/index.ts` (ArtistProfile interface)
- API: `tdf-mobile/src/api/artists.ts` (API client - exists, needs implementation)
- Screens: `tdf-mobile/app/` (artistDetail.tsx - to be created)
- Components: `tdf-mobile/components/` (ArtistCard.tsx, ArtistSelector.tsx, etc.)

## Related Documentation

- See `SOCIAL_EVENTS_INTEGRATION_STATUS.md` for full context
- See `SOCIAL_EVENTS_TESTING_GUIDE.md` for testing procedures
- See `tdf-hq/SOCIAL_EVENTS_BACKEND_STATUS.md` for backend details

## Questions?

Refer to the comprehensive guides in the main repository:
1. `SOCIAL_EVENTS_SESSION_SUMMARY.md` - Overview of current state
2. `SOCIAL_EVENTS_INTEGRATION_STATUS.md` - Full architecture
3. `SOCIAL_EVENTS_TESTING_GUIDE.md` - How to test features
