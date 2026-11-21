# Fan artist recommendations (COMMUNITY-FAN-RECOMMENDATION-001)

`GET /fans/me/recommendations?limit=N` answers the artists a signed-in fan may want to
follow next. It is read-only and sits behind `AuthProtect "bearer-token"` with the other
`/fans/me` routes.

## Rule

Item-to-item collaborative filtering over `fan_follow`:

- Two artists are similar when the same fans follow both. For a followed artist `s` and a
  candidate `c`: `similarity(s, c) = shared(s, c) / sqrt(followers(s) * followers(c))`
  (cosine of the two follower sets). The fan's own rows are not evidence.
- A candidate's score is the sum of its similarities to every artist the fan follows.
  Dividing by the follower counts keeps an artist everybody follows from being
  recommended to everyone.
- Candidates are ordered by score, then follower count, then artist id. Each names up to
  three followed artists that contributed most (`arecSeedArtistIds`).
- When that yields fewer than `limit` artists, the list is completed with the most
  followed artists that have at least one follower (`arecReason = popular`, score 0).
  A fan without follows therefore receives only `popular` entries.
- `limit` defaults to 6 and is clamped to 1..24.

## Boundaries

- **Visibility.** Only publicly listed artists (ART-PUBLIC-VISIBILITY-001) are returned,
  and only they count as evidence: a hidden artist the fan follows is never named.
- **Exclusions.** Artists the fan already follows and the fan's own party are never
  returned.
- **Follow policy.** Where the social-v2 follow policy is installed, an artist the
  caller may not follow (a block in either direction, a closed profile, an organisation
  or an inactive account, by `social_v2_profile_eligible`, the predicate `followArtist`
  applies) is excluded before ranking and before the limit, so the list never offers a
  follow that would be refused. Before that migration the predicate is absent and public
  listing alone decides, as it does for follows.
- **Privacy.** The handler reads the caller's follows and aggregate counts. No other
  fan's identity is read or returned. Follower lists of public artists are already
  public (`GET /fans/artists/{artistId}/fans`), so the answer discloses nothing those
  lists do not.
- **Consistency.** The three reads share one transaction at the pool's default
  isolation. A shared count larger than a follower count read an instant later is
  clamped, so one followed artist contributes at most 1.
- **Authorization.** `requireFanAccess` (Fan or Customer role, coherent grants), as for
  `GET /fans/me/follows`.

## Implementation and evidence

| Part | Where |
|---|---|
| Ranking (pure) and SQL | `tdf-hq/src/TDF/Fans/ArtistRecommendations.hs` |
| Handler | `fanListRecommendations` in `tdf-hq/src/TDF/Server.hs` |
| Contract | `listFanArtistRecommendations` in `tdf-hq/docs/openapi/api.yaml` |
| Web | `tdf-hq-ui/src/features/fans/RecommendedArtists.tsx` |
| Ranking properties | `tdf-hq/test/TDF/Fans/ArtistRecommendationsSpec.hs` |
| SQL on PostgreSQL | `scripts/test-fan-recommendations-postgres.sh` |

## Open for the owner

- Only follows are evidence. Plays (`music_playback_event`), favourites and RSVPs are
  not used yet.
- One shared fan is enough to recommend. A minimum-support threshold would trade
  coverage for stability on a small catalog.
- Scores are computed on every request. That is adequate while `fan_follow` is small; a
  precomputed similarity table becomes worthwhile when it is not.
- No HTTP-level test runs the route against a fully migrated database.
- Mobile does not call the route.
