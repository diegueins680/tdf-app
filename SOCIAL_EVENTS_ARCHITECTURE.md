# Social Event Calendar - Architecture Overview

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         Mobile App (React Native)               │
│                          (tdf-mobile)                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │              Navigation (Expo Router)                    │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────────────┐   │  │
│  │  │ Events   │  │ Venues   │  │ Artist Profiles     │   │  │
│  │  │ Discovery│  │ Explorer │  │ Management          │   │  │
│  │  └──────────┘  └──────────┘  └──────────────────────┘   │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────────────┐   │  │
│  │  │Create    │  │Create    │  │User Profile &       │   │  │
│  │  │Event     │  │Venue     │  │Preferences          │   │  │
│  │  └──────────┘  └──────────┘  └──────────────────────┘   │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              ▲                                  │
│                              │                                  │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │            React Query (Server State)                    │  │
│  │      useQuery | useMutation | useInfiniteQuery           │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              ▲                                  │
│                              │                                  │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │              API Client Layer (TypeScript)               │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────────┐     │  │
│  │  │ Events API │  │ Venues API │  │ Artists API    │     │  │
│  │  │ - list()   │  │ - list()   │  │ - getById()    │     │  │
│  │  │ - create() │  │ - create() │  │ - create()     │     │  │
│  │  │ - rsvp()   │  │ - getById()│  │ - update()     │     │  │
│  │  │ - invite() │  │ - update() │  │ - search()     │     │  │
│  │  └────────────┘  └────────────┘  └────────────────┘     │  │
│  └──────────────────────────────────────────────────────────┘  │
│                              ▲                                  │
│                    HTTP/REST │ JSON                            │
│                              │                                  │
└──────────────────────────────┼──────────────────────────────────┘
                               │
                               │
┌──────────────────────────────┼──────────────────────────────────┐
│                              ▼                                  │
│                    Haskell Backend (Servant)                   │
│                          (tdf-hq)                              │
├──────────────────────────────────────────────────────────────┤
│                                                                │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │           HTTP API Layer (TDF.API)                       │ │
│  │  ┌────────────────┐  ┌────────────────┐                 │ │
│  │  │ SocialEventsAPI│  │ VenuesAPI      │                 │ │
│  │  │ - GET /events  │  │ - GET /venues  │ ...             │ │
│  │  │ - POST /events │  │ - POST /venues │                 │ │
│  │  │ - etc.         │  │ - etc.         │                 │ │
│  │  └────────────────┘  └────────────────┘                 │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              ▲                                 │
│                              │                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │         Handler Layer (TDF.Server)                       │ │
│  │  ┌──────────────┐  ┌──────────────┐                     │ │
│  │  │ Events       │  │ Venues       │                     │ │
│  │  │ - listEvents │  │ - listVenues │ ...                 │ │
│  │  │ - createEvt  │  │ - createVenue│                     │ │
│  │  │ - rsvpHandler│  │ - searchByLoc│                     │ │
│  │  └──────────────┘  └──────────────┘                     │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              ▲                                 │
│                              │                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │        Database Layer (TDF.DB)                           │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │ │
│  │  │ SQL Queries  │  │ Transactions │  │ Indexes      │  │ │
│  │  │ - SELECT     │  │ - BEGIN/END  │  │ - (city,     │  │ │
│  │  │ - INSERT     │  │ - ROLLBACK   │  │    time)     │  │ │
│  │  │ - UPDATE     │  │              │  │ - (lat,lng)  │  │ │
│  │  │ - DELETE     │  │              │  │              │  │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘  │ │
│  └──────────────────────────────────────────────────────────┘ │
│                              ▲                                 │
│                              │                                 │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │         ORM Layer (Persistent)                           │ │
│  │  ┌──────────────────────────────────────────────────────┐ │
│  │  │ Models                                               │ │
│  │  │ - SocialEvent, ArtistProfile, Venue, etc.           │ │
│  │  │ - EntityDef, EntityField, PersistEntity             │ │
│  │  └──────────────────────────────────────────────────────┘ │
│  └──────────────────────────────────────────────────────────┘ │
│                              ▲                                 │
│                  SQL │ Binary Protocol                        │
│                              │                                 │
└──────────────────────────────┼────────────────────────────────┘
                               │
                               │
                    ┌──────────────────────┐
                    │  PostgreSQL Database │
                    │                      │
                    │ Tables:              │
                    │ - social_events      │
                    │ - artist_profiles    │
                    │ - venues             │
                    │ - event_rsvps        │
                    │ - event_invitations  │
                    │ - event_artists (jt) │
                    │ - artist_genres (jt) │
                    └──────────────────────┘
```

## Data Flow: Creating an Event

```
User Input (Mobile)
    │
    ▼
EventCreateForm Component
    │
    │ title, date, venue, artists, etc.
    │
    ▼
useMutation (createEvent)
    │
    │ mutation.mutate({...})
    │
    ▼
Events.create() API Client
    │
    │ POST /api/v1/events
    │ Content-Type: application/json
    │ Authorization: Bearer {token}
    │
    ▼
Backend HTTP Server (Servant)
    │
    ▼
Handler: createEvent
    │
    │ - Validate input
    │ - Check authorization
    │
    ▼
Database Transaction
    │
    ├─ INSERT INTO social_events (title, ...)
    │ Returns: event_id
    │
    ├─ FOR EACH artist_id:
    │  │ INSERT INTO event_artists (event_id, artist_id)
    │  └─ Returns: row_count
    │
    └─ COMMIT
    │
    ▼
Backend Response
    │
    │ 200 OK
    │ { id, title, ... }
    │
    ▼
Mobile Receives Response
    │
    ▼
React Query Cache Update
    │
    │ - Invalidate queryKey: ['events']
    │ - Refetch events list
    │
    ▼
Component Re-render
    │
    ▼
Show Success Alert
    │
    ▼
Navigate to Event Detail
```

## Data Flow: Searching Venues by Location

```
User Opens VenueExplorer Screen
    │
    ▼
Request Location Permission
    │
    ▼
Get Current Coordinates
    │ (expo-location)
    │ latitude, longitude
    │
    ▼
useQuery Setup
    │
    │ queryKey: ['venues-nearby', lat, lng, radius]
    │
    ▼
Venues.list({nearCoords: {lat, lng}, radiusKm: 5})
    │
    │ GET /api/v1/venues?nearCoords=0.35,-78.51&radiusKm=5
    │
    ▼
Backend Handler: listVenues
    │
    │ - Extract near_coords (0.35, -78.51)
    │ - Extract radius_km (5)
    │
    ▼
Database Query (Geolocation Search)
    │
    │ SELECT * FROM venues v
    │ WHERE distance(
    │   point(v.latitude, v.longitude),
    │   point(0.35, -78.51)
    │ ) <= 5 km
    │ ORDER BY distance ASC
    │
    ▼
Backend Response
    │
    │ 200 OK
    │ [{id, name, lat, lng, city}, ...]
    │
    ▼
Mobile Receives Response
    │
    ▼
React Query Caches Result
    │
    ▼
Component useMemo: Calculate Distances
    │
    │ venues.map(v => ({
    │   ...v,
    │   distance: haversine(userLat, userLng, v.lat, v.lng)
    │ }))
    │
    ▼
Component useMemo: Sort by Distance
    │
    │ venues.sort((a, b) => a.distance - b.distance)
    │
    ▼
FlatList Renders Venues
    │
    │ VenueItem (distance from user)
    │
    ▼
User Can Select Venue
    │
    ▼
Navigate to VenueDetail or CreateEvent
```

## Component Hierarchy

```
RootLayout (_layout.tsx)
│
├─ StatusBar
│
└─ AppProviders
   ├─ SafeAreaProvider
   └─ QueryClientProvider
      │
      └─ Tabs Layout
         │
         ├─ Events Tab (stack)
         │  ├─ Events Screen
         │  ├─ Event Detail
         │  ├─ Create Event
         │  │  ├─ Venue Selection Modal
         │  │  │  └─ VenueItem (list)
         │  │  └─ Artist Selection Modal
         │  │     └─ ArtistItem (list)
         │  └─ Event RSVP Modal
         │
         ├─ Venues Tab (stack)
         │  ├─ Venue Explorer
         │  │  └─ VenueItem (FlatList)
         │  └─ Venue Detail
         │
         ├─ Artists Tab (stack)
         │  ├─ Artist Directory
         │  └─ Artist Detail
         │
         ├─ User Profile Tab
         │  ├─ Profile Info
         │  ├─ Artist Profile Section
         │  └─ Events Section
         │
         └─ Other Tabs...
```

## State Management Pattern

```
Query Request
    │
    ▼
React Query Hook (useQuery)
    │
    ├─ Check cache
    │  │
    │  ├─ Cache Hit? Return cached data
    │  │
    │  └─ Cache Miss? Fetch from API
    │
    ▼
API Client (events.ts, venues.ts, etc.)
    │
    ├─ Build URL with query params
    │ GET /api/v1/events?city=Quito&skip=0&limit=50
    │
    ├─ Add Authorization header
    │ Authorization: Bearer {token}
    │
    └─ Make HTTP request
       │
       ▼
    Backend Response
       │
       ├─ 200 OK → Parse JSON → Return data
       │
       ├─ 400 Bad Request → Throw error
       │
       ├─ 401 Unauthorized → Handle auth error
       │
       └─ 500 Server Error → Retry with backoff
       │
       ▼
    React Query updates cache
       │
       ├─ Set data state
       │ isLoading: false
       │ data: [...]
       │
       └─ Trigger component re-render
       │
       ▼
    Component Renders with Data
       │
       └─ Display events, venues, or artists
```

## Mutation Pattern

```
User Interaction (click Create)
    │
    ▼
useMutation Setup
    │
    │ const mutation = useMutation({
    │   mutationFn: (data) => API.create(data),
    │   onSuccess: () => invalidateCache(),
    │   onError: () => showAlert(error)
    │ })
    │
    ▼
mutation.mutate({...payload})
    │
    ├─ isLoading: true
    │ (show loading indicator)
    │
    ▼
API Call (POST /api/v1/events)
    │
    ▼
Backend Processing
    │
    ├─ Validate
    ├─ Authorize
    ├─ Save to DB
    └─ Return response
    │
    ▼
Response Received
    │
    ├─ Success (200) → onSuccess callback
    │  │
    │  ├─ Invalidate ['events'] query
    │  │
    │  ├─ Refetch events list
    │  │
    │  ├─ Show success alert
    │  │
    │  └─ Navigate away
    │
    └─ Error (400+) → onError callback
       │
       ├─ Extract error message
       │
       ├─ Show error alert
       │
       └─ Stay on form (user can retry)
```

## Performance Optimization Pattern

```
Expensive Computation
    │
    ▼
useMemo Hook
    │
    ├─ Memoize result
    ├─ Check dependencies [var1, var2]
    │
    └─ Re-compute only if dependencies change
    │
    ▼
Memoized Result
    │
    └─ Prevent unnecessary re-renders


Component Render
    │
    ▼
memo() Wrapper
    │
    ├─ Check props (shallow comparison)
    ├─ Props changed? Re-render
    └─ Props same? Skip re-render
    │
    ▼
Optimized Component


Event Handler
    │
    ▼
useCallback Hook
    │
    ├─ Memoize function
    ├─ Check dependencies [var1, var2]
    │
    └─ Return same function reference if deps unchanged
    │
    ▼
Callback Reference
    │
    └─ Pass to child without triggering re-render
```

## Error Handling Flow

```
API Request
    │
    ▼
Network Error?
    ├─ No → Continue
    └─ Yes → Retry with backoff
         │
         └─ After 3 retries → Show error alert
    │
    ▼
HTTP Status
    │
    ├─ 200 OK → Return data
    │
    ├─ 4xx Client Error
    │  │
    │  ├─ 400 Bad Request → Validation error
    │  │  └─ Show form error
    │  │
    │  ├─ 401 Unauthorized → Auth error
    │  │  └─ Redirect to login
    │  │
    │  └─ 404 Not Found → Resource not found
    │     └─ Show empty state
    │
    └─ 5xx Server Error
       │
       ├─ 500 Internal Error
       └─ 503 Service Unavailable
          │
          └─ Retry with exponential backoff
             │
             └─ If persists → Show error alert
```

## Authentication Flow

```
App Launch
    │
    ▼
Check Stored Token
    │
    ├─ Token exists? → Validate
    │  │
    │  └─ Valid? → Set auth context
    │
    └─ No token? → Show login screen
    │
    ▼
API Request
    │
    ├─ Add Authorization header
    │ Authorization: Bearer {token}
    │
    ▼
Backend Verification
    │
    ├─ Valid token? → Process request
    │
    └─ Invalid/expired token?
       │
       ├─ Return 401
       │
       └─ Mobile catches error
          │
          ├─ Clear stored token
          ├─ Clear auth context
          └─ Redirect to login
```
