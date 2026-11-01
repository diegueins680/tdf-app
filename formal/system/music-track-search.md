# Authorized track discovery and exact selection

Boundary: `MEDIA-MUSIC-TRACK-SEARCH-001`, a scoped part of universal audio P1.
This extends canonical release audio into `/buscar`. It does not establish
coverage of all historical, social, event, podcast or other audio sources.

## System and assumptions

The database remains the existing published-release view, release-track and
recording relationships, assets and `music_public_asset_accessible`. Public
feature gating and `Assets.trustedEdgeTerritory` remain authoritative. Their
implementation is a dependency, not a newly proven policy. The search endpoint
does not issue delivery credentials; the existing persistent engine authorizes
only the selected source and retains its generation guards and preview policy.

The browser has one authoritative engine in this application context. Current
rendered controls receive ordinary user actions; arbitrary injected scripts,
cross-tab ownership and constant-time database execution are outside this model.
Offset page coverage assumes no intervening catalog mutation.

## Retrieval and authorization obligations

`GET /music/search` trims q, accepts at most 200 Unicode characters, limit 1–100
(default 13) and nonnegative offset. Blank q returns an empty array. Typed
parameter parsing is separate from handler validation. The public flag remains
required even for blank queries.

Text matching narrows current public track contexts inside the database. It
uses the existing normalized accent/case matching and literal non-Latin
substring matching for recording titles, track/release artists and release
titles. Percent/underscore or SQL-looking input is data, never executable SQL.
This is partial matching, not typo tolerance or full bilingual analysis.

For every context, the source aggregate requires that the asset belongs to the
same recording and release version and satisfies the existing delivery predicate
for the trusted territory. Zero-source contexts are excluded before recording
deduplication, pagination or response projection, regardless of which text
field matched. A release visible through track A cannot expose unauthorized
track B through a broad artist/release-title query.

One stable authorized matching context is selected per recording using
publication time, release identity and track ordering. Renditions cannot
duplicate a hit. Concurrent rights/publication changes can change later offset
pages; no snapshot or immutable authorization promise is made.

Only exact track/recording/release/version IDs, stable release slug/title,
recording title/artist/duration and eligible asset IDs/roles/media types are
projected. Technical metadata is restricted to numeric bitrate_kbps and
loudness_lufs. No storage location, master reference, uploader, arbitrary
technical blob, delivery URL or credential is returned. Successful results and
handler errors carry private/no-store; pre-handler parse failures are outside
that handler-header obligation.

## Client transitions and safety

State is `(draft query, committed query, page, request key, pending/result/error,
selected context, existing engine generation)`.

1. The music section mounts only at `/buscar`, not `/inicio`. It does not alter
   directory entity kinds, filters, maps, favorites or saved-search semantics.
2. Editing the draft hides old actionable music rows immediately. Requests use
   AbortSignal and query/page identity, without previous-query placeholder data.
   Older response arrival cannot replace the current query's visible hits.
3. Query changes derive page one before rendering. Each page fetches 13 rows,
   displays 12 and uses the extra authorized row only to enable Next. Music
   pagination remains independent from directory pagination and filters.
4. Loading, empty and service-unavailable states are distinct. An unavailable or
   disabled music API is not reported as an observed empty catalog. Directory
   emptiness retains its legacy translated title; an active music query adds a
   separate directory-scope clarifier. Malformed or out-of-contract music wire
   data becomes a music-only error before rendering. Closed schema-field lists
   reject unexpected fields; the directory remains independently usable.
5. An explicit Play click synchronously converts the exact hit into deferred
   source descriptors and dispatches the existing player event. No release-detail
   lookup, first-track guess, per-result signing or artwork request precedes it.
6. Results arrival, retry, pagination and navigation do not issue Play commands.
   Release navigation preserves the authoritative engine. No second audio
   element, provider, persistence store or player session is introduced.
7. Permission revocation after discovery is enforced by fresh selected-source
   delivery authorization. Denial remains a failure of that selected recording,
   never a substitution of a different recording.
8. A nonzero saved position for a rendition/recovery source is restored only
   after that source has metadata. Source identity and selection generation
   reject obsolete preparation; preparation timeupdate zero cannot replace the
   intended position. A newer seek, including zero, replaces the queued target.
   Play is issued only after restoration and only if current user intent still
   permits it; explicit pause, terminal failure and unmount cannot be undone by
   late metadata. Initial zero-position startup keeps its existing path.

These obligations refine PLAYER-002/003/005/006 and MEDIA-002/003 within the
stated boundaries. They do not prove those invariants for the entire platform.

## Verification classification and limits

The executable finite design model is
`music-track-search-model.mjs`, with replayable adversarial traces and deliberate
mutants in `scripts/__tests__/music-track-search-model.test.mjs`. Its report must
state finite generation/event bounds and distinguish bounded safety exploration
and conditional drain reachability from unconditional liveness. It is not
extracted implementation or a refinement theorem.

Actual SQL/binding controls execute through
`scripts/test-music-track-search.mjs` on an owned PostgreSQL fixture. That fixture
uses 14 selected historical migrations plus the actual normalizer/publication
functions; it is not the full-schema or compiled-handler proof. Component tests
cover exact selection, stale data, no autoplay, query/page transitions and
independent errors, wire validation and metadata-position preparation. These
event-driven unit tests do not prove the observed WebKit clock stall is repaired;
the unchanged real advancing-clock assertion remains a fresh hosted gate. Native tests use real AAC/FLAC clocks with synthetic API and
authorization responses, not actual production media delivery.

Compiled HTTP routing, full-schema fixtures, final model bounds, native outcomes,
CI and release status are recorded separately in
`docs/music-releases/track-search-delivery-2026-10-26.md`. Until that evidence is
available, those obligations are UNVERIFIED. Production activation, real CDN
objects/revocation, physical devices, full WCAG 2.2 AA and reference SLOs remain
separate release gates. No whole-system PROVEN classification is claimed.
