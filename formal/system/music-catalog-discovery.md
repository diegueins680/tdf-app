# Authorized recording-title search and release pagination

**Boundary:** `MEDIA-MUSIC-DISCOVERY-001`, a focused part of universal audio P1.
This contract describes the existing public music-release endpoint and its web
catalog. It does not establish universal search, a complete canonical catalog,
or delivery authorization for every historical audio context.

## Inputs, state and assumptions

The backend accepts a search query, optional artist ID, requested limit/offset
and an edge-country value. `requirePublicFeature` and
`Assets.trustedEdgeTerritory` remain authoritative. The handler clamps limit to
1–100 and offset to at least zero. Its query string is trimmed and supplied to
the database as parameters. `directory_normalize_text` is the existing directory
normalizer; it is a deployment prerequisite already declared by the migration
manifest, not a new search service or extension.

Database state consists of `music_public_release`, release tracks, canonical
recordings, assets and existing publication/availability/rights predicates.
These predicates and the trusted edge-country boundary are trusted dependencies
of this increment; executing this query is not a fresh proof of them.

The client keeps query and requested page in URL parameters. A valid page is a
positive safe integer no greater than
`floor(Number.MAX_SAFE_INTEGER / 24)`; other values use page one. It requests
25 authorized release summaries at offset `(page - 1) * 24`, displays at most
24 summaries and uses the extra row solely to enable the next-page action.
The existing API client's optional pagination object preserves old callers.

## Retrieval contract

1. Existing release-title and display-artist matching remain available. Added
   recording-title matching uses `music_recording.canonical_title` through the
   release version's real `music_release_track` relationship.
2. A recording-title match requires a `stream_audio` or `preview_audio` asset
   for the same recording/version for which
   `music_public_asset_accessible(asset_id, territory)` is true at query time.
   Release visibility alone cannot authorize a match on an otherwise hidden
   track's title. A mixed-territory release may be visible through one track
   while another track's title remains unavailable as a search signal.
3. Public-release publication visibility, existing availability time/territory
   constraints and optional artist filtering apply to the result. This does
   not strengthen the existing release-title or artist branches into a claim
   that every displayed release is fully playable for every principal.
4. Search uses normalized Latin accent/case/spacing matching and a literal
   case-insensitive substring branch for non-Latin titles. An empty normalized
   term does not become a match-all condition. Percent and underscore in raw
   input are literal text; SQL parameters do not become executable query text.
   This is partial matching, not typo tolerance or bilingual language analysis.
5. `EXISTS` preserves one result row per release when several assets or tracks
   match. A shared recording in two authorized releases produces two release
   cards with their own stable URLs, not a newly merged release identity.
6. Results retain `published_at DESC, id` ordering and existing response fields.
   No matched-track title, media URL, credential, or private metadata field is
   added to the response.

## Client transitions and concurrency

`(query, page) -> request -> loading -> results | explicit failure`

Query/page are deferred together. Search edits reset page to one; page changes
preserve the query and unrelated URL parameters. Forward/back navigation uses
URL history. Query-cache identity includes the deferred query and page;
superseded requests receive cancellation signals, and a late response from an
older query cannot replace the current query's visible results.

During a deferred transition, old results are withheld. Pagination controls are
disabled while their request is pending. A failed request exposes an explicit
retry action and prevents moving next using stale success data. An empty page
after page one retains the previous-page action. Each visible page deduplicates
release IDs defensively; offsets advance by 24, not the sentinel count of 25.

Selection follows existing stable release URLs into the release page and the
global-player actions owned by `MEDIA-MUSIC-PLAYER-001`. This increment does not
add track-level cards or a new playback engine.

Ordering and cross-page coverage assume a stable authorized result set between
requests. Concurrent publication, withdrawal or rights changes can move rows
across offset boundaries; a snapshot cursor and global cross-page deduplication
are not implemented. The result cache is not proof that rights still hold:
backend media authorization remains mandatory when a listener selects audio.

## Verification obligations and status

| Obligation | Mechanism | Classification and boundary |
| --- | --- | --- |
| Canonical recording title finds an otherwise differently named release | Real PostgreSQL, shipped handler SQL literal, submission-valid fixture and publication functions | TESTED at the source identities recorded in `catalog-track-search.md`; rerun required when the query changes. |
| Private, withdrawn, scheduled/embargoed and territory-excluded fixtures stay outside recording-title results | Same PostgreSQL runner, including a visible mixed-territory release | Finite security controls; no universal authorization proof. |
| Hidden title cannot reveal a visible release through an unauthorized track | Authorized negative/positive fixtures and deliberate removal of the asset-accessibility predicate | Mutation control must expose the hidden title; passing an ordinary query alone is insufficient. |
| Matching renditions do not duplicate release rows; limit/offset and artist filter remain active | PostgreSQL controls | TESTED finite fixtures; does not establish performance or cross-request snapshot consistency. |
| Release 25 is reachable and URL/back navigation preserve context | Rendered `MusicCatalogPage.test.tsx` with deterministic API fixtures | Component regression test, not real HTTP or browser playback. |
| Changed query resets page and rejects stale data | Deferred request/cancellation component regression | React/query-library behavior under controlled promise ordering; not all possible scheduler interleavings. |
| Existing API calls remain compatible; pagination is bounded and cancellable | `musicReleases.pagination.test.ts` | API-client contract test with mocked transport. |
| Target deployment, p95 SLO, full HTTP handler binding and physical assistive technology | Separate release/performance obligations | UNVERIFIED by these tests. |

`scripts/test-music-catalog-search.mjs` extracts the SQL literal from
`TDF.Server.MusicRelease.listPublicReleases`, substitutes fixture parameter
values and executes it on an owned disposable database. It verifies the literal
query's behavior and parameter-placeholder count. It does **not** execute the
compiled Haskell handler, prove the real Haskell parameter ordering, exercise
HTTP routing or establish authorization of an actual production principal.
The normalizer is extracted from its authoritative migration, not reimplemented
in the fixture. The deliberate fixture adaptations and gate-removal mutant are
test-only; production triggers and rights gates are not disabled.

There is no formal model or mathematical proof for this increment. Its
requirement remains `PARTIAL`: deterministic SQL, security and client contracts
cover a limited set of obligations, while compilation, current-head hosted
checks, independent review and deployment are separate release evidence.

Reproduce the focused checks:

```sh
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/pages/MusicCatalogPage.test.tsx src/api/musicReleases.pagination.test.ts \
  --watch=false --reporters=default
TDF_MUSIC_WORKER_TEST_DATABASE_URL=postgresql://USER@127.0.0.1:PORT/tdf_music_search_test \
  node scripts/test-music-catalog-search.mjs
```

The backend CI job invokes the PostgreSQL runner after existing worker recovery
checks; the normal frontend suite includes the component/API tests. A configured
CI step is not a hosted pass. Existing immutable migration SQL, master files,
release identities and delivery credentials are unchanged by this increment.

## Release and remaining scope

Rollback uses the prior application source; no schema/data rollback or backfill
is introduced. The normalizer/public-view/media-accessibility dependencies must
exist on the target. Release smoke tests must find a known authorized recording
by title, navigate beyond the first page, follow a stable card URL, select its
track through the global engine, and verify a protected-title negative control
in an appropriate safe environment.

`AC-003 / MEDIA-002 -> MEDIA-MUSIC-DISCOVERY-001 -> listPublicReleases +
musicReleases.listPublic + MusicCatalogPage -> PostgreSQL/API/component tests ->
current candidate CI -> independent review -> release receipt`.

Universal `/buscar`, typo tolerance, aliases, collaborators/credits, genre,
track-context titles differing from the canonical recording title, authorized
lyrics, track-level cards, non-release audio, historical ingestion and search
index reconciliation remain incomplete. Pagination removes the first-24-card
UI limit; it does not establish that every eligible audio asset is indexed.
