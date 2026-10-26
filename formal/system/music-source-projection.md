# Authorized source references in public music and saved collections

Requirement: `MEDIA-MUSIC-SOURCES-001`, scoped to the `sources` arrays returned by
four existing web API projections. Source qualification is recorded at commit
`ade78b5ab11ba297cf419a96012032ebf289820d`. This is an executable contract tested on finite
PostgreSQL fixtures, not a mathematical proof of media authorization or bytes.

## Boundary and property

The affected handlers are `getPublicRelease`, `listFavorites`, `listPlaylists` and
`listPlaybackHistory` in `TDF.Server.MusicRelease`. Each already selects ready
stream/preview assets for a recording and published release version and excludes
stale preview ranges. The correction additionally requires
`music_public_asset_accessible(asset.id, territory)` for each returned source.

For a projection evaluated against database snapshot `D` and the handler's
trusted territory `T`, the intended property is:

`returnedSource(s, D, T) -> music_public_asset_accessible_D(s.assetId, T) = true`

The same predicate is used by `getPublicMusicAssetAccess` before issuing a public
delivery credential. This creates agreement at the projection snapshot; it does
not authorize future playback from cached metadata. A later selection must still
call the asset-access endpoint, which rechecks the then-current policy. Issued
credentials retain their existing expiry/cache bounds; this change does not
revoke transferred bytes or previously issued URLs.

The new filter concerns public listening availability, not owner/editor preview
or paid-download entitlements. Those existing authorization paths are separate.

## Assumptions and inputs

- Existing public feature gates, authenticated saved-collection handlers,
  `currentPartyId`, and trusted edge-territory derivation remain authoritative.
- Existing public-release, publication, availability and preview predicates are
  trusted dependencies. Their implementation is not proved by calling them.
- The same territory value is bound to the source predicate and the handler's
  existing availability checks, using the actual positional parameter contract.
- A ready database asset need not exist in storage or decode successfully. Storage
  consistency, codecs, MIME, rights documentation and media transfer are outside
  this projection contract.

The request inputs are the public release slug or authenticated principal and
trusted edge-country context. Database inputs are the existing release view,
recording/track links, rendition metadata, preview ranges, availability rules,
favorites, playlists and listening history. No schema or response-field addition
is needed.

## Preserved behavior

1. Eligible full and current-preview references remain available. A preview-only
   policy emits its current authorized preview without advertising denied full
   renditions. A full policy can emit both authorized roles.
2. A track excluded by territory or withdrawal yields no authorized sources, even
   if another track keeps the containing release visible. Unknown territory
   cannot establish a territory-specific grant; existing Worldwide rules retain
   their prior semantics.
3. Source objects retain their existing `assetId`, `role`, `mediaType` and
   `technicalMetadata` shape and order. They do not issue a delivery URL or
   credential. Returning an asset ID is not itself authorization.
4. Existing track/release identities, titles, personal saved rows, ordering and
   positional fields remain. In saved collections, `available` still indicates
   that the existing query resolved a public version; it does **not** guarantee
   a nonempty source array, storage readiness or successful playback.
5. The existing per-account saved-row predicates remain intact. This increment
   does not make personal favorites, playlists or history public.
6. Public detail track metadata, artwork metadata and availability-rule fields
   are not redacted or redefined. Filtering source references does not prove the
   privacy of all metadata on a mixed-territory release.

Each projection is read-only. Publication, rights or territory may change between
projection and access authorization. No cross-request snapshot or distributed
transaction is introduced. Cached source arrays cannot replace backend access
authorization, and an empty array does not remove a saved collection entry.

## Verification obligations

The runner `scripts/test-music-source-projection.mjs` extracts the actual
four SQL literals, executes them with the authoritative delivery predicate and
normal publication transitions in an owned disposable database, and checks:

- Preview-only and full listening policies, valid preview ranges and stale-preview
  exclusion, mixed-territory tracks, unknown-territory behavior and withdrawal.
- Every emitted fixture source independently satisfies the real delivery-policy
  function for the requesting territory.
- Saved-row identities, account isolation and unavailable-row semantics remain.
- Removing only the new source predicate exposes denied full references in each
  affected projection, so the regression has a meaningful negative control.
- The actual handler parameter list agrees with the extracted query's positional
  binding contract. Source inspection/extraction is distinct from executing a
  compiled Haskell handler or making real authenticated HTTP requests.

The completed source-owner receipt records ten expected baseline agreement
failures and a passing current run across all four SQL surfaces, including four
policy-removal mutants and eight omitted/reordered Haskell argument controls.
All 17 CI-selection tests passed; a dedicated backend CI step executes the new
runner and runner-only changes select real PostgreSQL backend validation.

The binding parser extracts the list adjacent to each actual SQL literal,
normalizes whitespace, rejects forms outside its three whitelisted expressions,
asserts the expected positional contract and uses those extracted expressions to
derive fixture parameters. This is a trusted lexical translation, not Haskell
evaluation, a type proof or execution through Persistent/Servant/HTTP.

The recorded execution uses Node 24.13.1, PostgreSQL server 16.13 and psql client
18.2. It applies the runner's relevant real migration subset; it is not a complete
production-schema rehearsal. The random database was removed, the owned cluster
stopped and absence of its listener verified. Reported source, fixture and log
hashes are checked by the delivery report. All named results are `TESTED` finite
controls; no property is classified `PROVEN`. SQL predicate agreement is not a
proof of byte delivery, whole-system authorization, rights or storage existence.

Reproduce the SQL controls:

```sh
TDF_MUSIC_WORKER_TEST_DATABASE_URL=postgresql://USER@127.0.0.1:PORT/tdf_music_source_test \
  node scripts/test-music-source-projection.mjs
```

The existing disposable-database helper must own and clean only its random test
database. Production, external storage and real customer data are not test inputs.
Full handler compilation, HTTP binding, browser playback, cache behavior,
representative-catalog query plans and production deployment require separate
evidence. Existing stale-credential bounds are unchanged, not newly verified.

## Traceability, release and rollback

`MEDIA-002/003 projection agreement -> MEDIA-MUSIC-SOURCES-001 -> four source
projections -> real PostgreSQL/parameter controls -> current-candidate CI ->
independent review -> target-environment acceptance`.

This focused main-targeted increment is independent of the playback, catalog
search/pagination and legacy-provider actions candidates. It introduces no
migration, processing job, backfill, master rewrite, payment change or new
infrastructure. Application rollback restores the previous query implementation
without reversing data; that rollback would restore the known overly broad
source metadata projection and therefore is a mitigation tradeoff, not a privacy
improvement.

The requirement remains `PARTIAL`: scoped controls passed, while compiled
Stack/HTTP, target performance and release gates remain separate obligations.
This source filter does not establish universal audio discovery,
complete private-metadata isolation, native decoder success, immediate revocation
of issued delivery credentials or production acceptance.

## Full canonical financial-main qualification

The next normal composition retains protected main
`78fa351b469c1d2031df3232204dd722ac480e3b`: all145 requirement objects,
including the posted-ledger-header contract, remain exact. The original scoped
source-projection requirement is unchanged, for146 total. All1339 current-main
catalog decisions, client/player code, operations, dependencies and pinned Mobile
source are retained. The four-predicate handler patch is byte identical to744e75.

The existing SQL runner now prepares the complete schema using the canonical
production fixtures and migration renderer, rather than its older14-migration
subset. It checks all222 receipts, replay without history changes, the actual
delivery-policy function before/after fixtures, and final canonical readiness.
The shared random-database owner and connection guards are unchanged.

At source`7187e17af15a46495aa6ebf388717eed6322f4d7`, a separate main78fa
handler baseline reproduces ten policy-agreement failures on that complete
schema. The candidate passes all four SQL projections, four removed-filter
controls and eight omitted/reordered argument controls. Both random databases
were removed and the owned loopback cluster stopped. No compiled HTTP, media
byte, performance or production result is inferred from this SQL qualification.

The first complete-schema attempt failed before policy checks because its
synthetic other-party row omitted mandatory`is_org`; the fixture now explicitly
supplies`FALSE` and`NOW()`, with the real constraint retained. Failure evidence
is preserved. The older744 CI worker-cleanup timeout is recorded separately;
its unchanged worker test must pass in the new final-head CI, without relaxed
timeouts, skipped cases or approval inheritance. The evidence packet is
`reports/music-source-projection-main621-evidence-20261026`.

A separate actual-Git fixture reproduces rejection when the scrubbed environment
loses CI exact-checkout trust configuration, and success with that configuration.
Only the read-only Git identity call now receives HOME; SQL and renderer children
retain the unchanged scrubbed environment. No wildcard or globaloperator trust
setting is introduced. The complete SQL qualification was repeated after this
runner correction, with its new source identity retained.
