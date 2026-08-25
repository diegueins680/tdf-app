# Backend Changes Needed for Artist Fans List Feature

## Overview
The frontend has been implemented to display a paginated list of fans on each artist's public profile page. The backend now needs to implement the corresponding API endpoint.

## Required Endpoint

**URL**: `GET /fans/artists/{artistId}/fans`

**Query Parameters**:
- `page` (number, default: 1) - Page number
- `pageSize` (number, default: 5) - Number of items per page

**Response Type**: `ArtistFansResponse`

```typescript
{
  items: ArtistFanDTO[];
  page: number;
  pageSize: number;
  total: number;
}
```

Where `ArtistFanDTO` is:
```typescript
{
  afFanId: number;           // The fan's party ID
  afDisplayName: string;     // Fan's display name
  afAvatarUrl: string | null; // Fan's avatar URL (from fan_profile.avatar_url)
  afFollowedAt: string;      // ISO timestamp of when they followed
}
```

## Haskell Implementation Steps

### 1. Add DTOs to TDF/DTO.hs

Add these data types after the existing `FanFollowDTO`:

```haskell
data ArtistFanDTO = ArtistFanDTO
  { afFanId       :: Int64
  , afDisplayName :: Text
  , afAvatarUrl   :: Maybe Text
  , afFollowedAt  :: UTCTime
  } deriving (Show, Generic)
instance ToJSON ArtistFanDTO

data ArtistFansResponse = ArtistFansResponse
  { afrItems    :: [ArtistFanDTO]
  , afrPage     :: Int
  , afrPageSize :: Int
  , afrTotal    :: Int
  } deriving (Show, Generic)
instance ToJSON ArtistFansResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = dtoCamelDrop 3 }
```

### 2. Add Handler Function

Add this handler function (likely in a Server module or in TDF/ServerFanClub.hs):

```haskell
fanArtistsPublicGetFans :: Int64 -> Maybe Int -> Maybe Int -> AppM ArtistFansResponse
fanArtistsPublicGetFans rawArtistId mPage mPageSize = do
  artistKey <- either throwError pure (validateFanClubArtistPathId rawArtistId)
  let page = max 1 (fromMaybe 1 mPage)
      pageSize = min 100 (max 1 (fromMaybe 5 mPageSize))
      offset = (page - 1) * pageSize

  runDB $ do
    -- Count total followers
    total <- count [M.FanFollowArtistPartyId ==. artistKey]

    -- Fetch paginated followers with their profiles
    follows <- selectList
      [M.FanFollowArtistPartyId ==. artistKey]
      [Desc M.FanFollowCreatedAt, OffsetBy offset, LimitTo pageSize]

    -- Build DTOs
    fans <- forM follows $ \(Entity _ follow) -> do
      let fanPartyId = fanFollowFanPartyId follow
      mParty <- selectFirst [M.PartyId ==. fanPartyId] []
      mProfile <- selectFirst [M.FanProfileFanPartyId ==. fanPartyId] []

      let displayName = case mParty of
            Just (Entity _ p) -> M.partyDisplayName p
            Nothing -> "Fan"
          avatarUrl = mProfile >>= fanProfileAvatarUrl . entityVal

      pure ArtistFanDTO
        { afFanId = fromSqlKey fanPartyId
        , afDisplayName = displayName
        , afAvatarUrl = avatarUrl
        , afFollowedAt = fanFollowCreatedAt follow
        }

    pure ArtistFansResponse
      { afrItems = fans
      , afrPage = page
      , afrPageSize = pageSize
      , afrTotal = fromIntegral total
      }
```

### 3. Add API Route

Find where the fans/artists routes are defined and add:

```haskell
type FansPublicAPI =
       "fans" :> "artists" :> Get '[JSON] [ArtistProfileDTO]
  :<|> "fans" :> "artists" :> Capture "artistId" Int64 :> Get '[JSON] ArtistProfileDTO
  :<|> "fans" :> "artists" :> Capture "artistId" Int64 :> "releases" :> Get '[JSON] [ArtistReleaseDTO]
  :<|> "fans" :> "artists" :> Capture "artistId" Int64 :> "fans"
         :> QueryParam "page" Int
         :> QueryParam "pageSize" Int
         :> Get '[JSON] ArtistFansResponse  -- NEW ROUTE
```

And wire it up in the handler:

```haskell
fansPublicHandlers =
       listPublicArtists
  :<|> getPublicArtist
  :<|> getPublicArtistReleases
  :<|> fanArtistsPublicGetFans  -- NEW HANDLER
```

## Database Schema Reference

The implementation uses these existing tables:

- **fan_follow**: Stores fan-artist relationships
  - fan_party_id (FK to party)
  - artist_party_id (FK to party)
  - created_at

- **fan_profile**: Stores fan profile information
  - fan_party_id (FK to party)
  - display_name
  - avatar_url
  - bio, city, favorite_genres

- **party**: Stores basic party information
  - party_id
  - display_name

## Testing

Once implemented, test with:
```bash
curl 'http://localhost:8080/fans/artists/123/fans?page=1&pageSize=5'
```

Expected response:
```json
{
  "items": [
    {
      "afFanId": 456,
      "afDisplayName": "Juan Pérez",
      "afAvatarUrl": "https://example.com/avatar.jpg",
      "afFollowedAt": "2024-05-20T10:30:00Z"
    }
  ],
  "page": 1,
  "pageSize": 5,
  "total": 9
}
```

## Notes

- The endpoint is public (no authentication required)
- Fans are ordered by most recent followers first (DESC on created_at)
- Page size is capped at 100 to prevent abuse
- The frontend defaults to 5 fans per page for optimal UX
