# Social Events Feature - Quick Testing Guide

## Prerequisites

```bash
# Clone and setup workspace
cd /Users/diegosaa/GitHub/tdf-app

# Ensure git submodules are initialized
git submodule update --init --recursive

# For backend testing, ensure Docker is running
# For mobile testing, ensure Expo Go app is available on device/emulator
```

## Testing Workflow

### Step 1: Start Backend

```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-hq

# Build and start PostgreSQL + Haskell API
make up

# Wait for logs to show "Server listening on http://0.0.0.0:8080"
make logs

# In another terminal, seed test data
make seed

# Expected output:
# curl: (7) Failed to connect... (if seed endpoint not responding yet)
# API seed successful! Created test parties, events, venues

# Verify API is responding
curl http://localhost:8080/health
# Should return 200 OK
```

### Step 2: Test Backend Endpoints (without auth for now)

```bash
# List all events
curl http://localhost:8080/events

# Should return JSON array like:
# [
#   {
#     "eventId": 1,
#     "eventTitle": "Jazz Night",
#     "eventStart": "2024-02-15T19:00:00Z",
#     ...
#   }
# ]

# Get single event
curl http://localhost:8080/events/1

# List venues
curl http://localhost:8080/venues

# Get single venue
curl http://localhost:8080/venues/1

# Create venue (would need bearer token in production, test in dev mode)
curl -X POST http://localhost:8080/venues \
  -H "Content-Type: application/json" \
  -d '{
    "venueName": "The Blue Room",
    "venueAddress": "123 Main St",
    "venueCity": "New York",
    "venueCountry": "US",
    "venueLat": 40.7128,
    "venueLng": -74.0060,
    "venueCapacity": 500
  }'
```

### Step 3: Start Mobile App

```bash
cd /Users/diegosaa/GitHub/tdf-app/tdf-mobile

# Install dependencies (if not already done)
npm install

# Start Expo dev server
npm start

# Options:
# - Press 'i' to open iOS Simulator
# - Press 'a' to open Android Emulator
# - Scan QR code with Expo Go app on physical device
```

### Step 4: Test Mobile Features

#### 4a. View Events List
1. Navigate to **Events** tab (bottom navigation)
2. Should see list of events from backend
3. Each event shows: Title, Date/Time, Venue, Artist count
4. Try filtering by city in search box

**Expected Result**: Events populate from backend, city filter works

#### 4b. Create Venue
1. Tap **Create Venue** button or navigate to `createVenue` screen
2. Fill in form:
   - Name: "Test Venue"
   - Address: "456 Test Ave"
   - City: "Los Angeles"
   - Latitude: 34.0522
   - Longitude: -118.2437
   - Capacity: 300
3. Tap **Create Venue** button

**Expected Result**: 
- Success alert appears
- New venue appears in venue selector on next create event
- Backend POST /venues was called with proper structure

#### 4c. Create Event
1. Navigate to CreateEvent screen or tap **+ Create** in Events tab
2. Fill in form:
   - Title: "Test Jazz Event"
   - Description: "A test event"
   - Start Time: Tomorrow at 7 PM
   - End Time: Tomorrow at 10 PM
   - Select Venue: (from dropdown)
   - Select Artists: (from dropdown or create new)
   - Price: 29.99
   - Toggle: Make event public
3. Tap **Create Event** button

**Expected Result**:
- Success alert appears
- Event appears in events list
- Backend POST /events called with:
  - `eventTitle: "Test Jazz Event"`
  - `eventStart: "2024-02-XX..."`
  - `eventPriceCents: 2999` (29.99 dollars → 2999 cents)

#### 4d. View Event Detail
1. Tap on any event in the list
2. Should see full event details:
   - Title, description
   - Date, time, duration
   - Venue name with city
   - Artists performing
   - Ticket price (displayed as dollars)
   - RSVP buttons (currently placeholders)

**Expected Result**: All event data displays correctly

### Step 5: Verify Data Mapping

**Mobile → Backend Transformation (CreateEvent form):**

Monitor network traffic (Xcode debugger or Charles proxy) or check backend logs:

```
Frontend sends:  { "title": "Test", "startTime": "2024-02-15T19:00:00Z", "ticketPrice": 2999 }
Backend receives: { "eventTitle": "Test", "eventStart": "2024-02-15T19:00:00Z", "eventPriceCents": 2999 }
Database stores: SocialEvent row with eventTitle, eventStart, eventPriceCents

Backend returns: EventDTO with renamed fields
Frontend maps: eventTitle → title, eventStart → startTime
Frontend displays: "Test event starting at 7:00 PM for $29.99"
```

### Step 6: Check Logs

**Backend logs** (see errors, query timing):
```bash
# From tdf-hq directory
make logs

# Look for:
# - [INFO] POST /events 200
# - [INFO] SELECT * FROM social_event ...
# - [ERROR] any DB connection issues
```

**Mobile logs** (see API calls, data transformations):
```bash
# In Expo dev terminal, enable dev logging:
# Or check console in Xcode if running iOS Simulator

# Look for:
# - API requests being sent
# - Data mapping transformations
# - Query cache updates
```

## Troubleshooting

### Issue: Backend won't start
```bash
# Check if port 8080 is already in use
lsof -i :8080

# Check Docker is running
docker ps

# Rebuild containers
make clean
make up
```

### Issue: PostgreSQL connection fails
```bash
# Verify Docker network
docker network ls

# Check DB service health
docker ps | grep db

# Check DB logs
docker logs tdf-db

# Reset completely
make clean
docker system prune -a
make up
```

### Issue: Mobile can't reach backend
```bash
# Verify backend URL in mobile .env
cat tdf-mobile/.env

# Should have: EXPO_PUBLIC_API_BASE=http://localhost:8080

# On Android Emulator, use: http://10.0.2.2:8080 instead of localhost
# On iOS Simulator, localhost works fine

# Test connectivity
curl -v http://localhost:8080/health
```

### Issue: CreateEvent fails silently
```bash
# Check mobile console for error:
# Common: "venueId required" — need to select venue first
# Common: "artistIds required" — need to select at least one artist
# Common: "end time must be after start time"

# Check backend response:
# Look for 400/422 validation errors in server logs
```

### Issue: Venue/Event data shows wrong format
```bash
# Verify mapping layer is in place:
grep "mapBackendEventToFrontend" tdf-mobile/src/api/events.ts

# Check data transformation:
# - eventId should map to id
# - eventTitle should map to title
# - eventStart should map to startTime
# - eventPriceCents should stay as cents (multiply by 100 when storing)
```

## Expected Behavior Checklist

- [ ] Backend `/events` endpoint returns JSON array
- [ ] Backend `/venues` endpoint returns JSON array
- [ ] Mobile Events tab populates from backend
- [ ] City filter in mobile works
- [ ] CreateVenue form submits successfully
- [ ] CreateEvent form submits successfully
- [ ] Event appears in list immediately after creation
- [ ] Event detail view shows all data correctly
- [ ] Currency displays as dollars (29.99) but stores as cents
- [ ] Artists are linked correctly to events
- [ ] Venue is linked correctly to events

## Performance Expectations

- Events list load: < 2 seconds
- Event detail load: < 1 second
- Create event: < 3 seconds
- List query count: All events ≈ < 100 records initially

## Next: Advanced Testing

Once basic flow works, test:
1. **RSVP Flow** (currently stubbed, needs backend implementation)
2. **Artist Management** (currently stubbed, needs backend implementation)
3. **Invitations** (currently stubbed, needs backend implementation)
4. **Search/Filtering** (backend filters work; mobile search UI needs more options)
5. **Error Scenarios** (invalid dates, missing required fields, duplicate venues)
6. **Pagination** (test with 100+ events)
7. **Real-time Sync** (refresh events list after creation)

## Reference Files

- Backend status: `/tdf-hq/SOCIAL_EVENTS_BACKEND_STATUS.md`
- Integration overview: `/SOCIAL_EVENTS_INTEGRATION_STATUS.md`
- API routes: `tdf-hq/src/TDF/API/SocialEventsAPI.hs`
- Mobile API clients: `tdf-mobile/src/api/events.ts`, `tdf-mobile/src/api/venues.ts`
- Type definitions: `tdf-mobile/src/types/index.ts`
