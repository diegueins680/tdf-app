# Community discovery and navigation

Product authority: the October 13, 2026 request for Community Search, Global Artist
Discovery & Navigation Scroll Restoration. This record distinguishes implementation
from local, hosted and production verification. A local pass is not a release.

## Architecture and causes

The web client uses React, React Router DOM 7.18, TanStack Query 5, Material UI,
Vite, Jest and Playwright. The API is Haskell/Servant/Persistent with PostgreSQL.
Cloudflare Pages serves the web; the current API deployment procedure is
`ops/hetzner/README.md`, not the retained Fly scripts.

`/fans` is Comunidad. Its legacy `/fans/artists` catalog downloaded artists but
provided no prominent name search. `/buscar` used `/directory/search`, whose
published directory records are distinct from legacy `artist_profile` records.
The historical backfill did not continuously create directory profiles for new
legacy artists. The October 7 derived-listing migration operates on existing
directory profiles; it does not fill this gap. Global search also implicitly
selected/stored a city. The authenticated header's command palette searched
sections rather than community entities; the public mobile search entry lived
inside the overflow menu.

The protected AppShell preserves a constrained `main` scrolling element across
routes. Public pages scroll the document. CSS overflow alone does not reliably
identify the scrolling element: an unconstrained public main can compute to
`overflow: auto`. There was no central history restoration policy. The artist
editor also scrolled into view on mount; that automatic effect is removed.

## Search contract

Both surfaces call the existing `/directory/search` endpoint through
`useDirectorySearch`. Requests use a 300 ms configurable debounce, TanStack
query keys, abort signals, retained results while fetching, 20-item cursor pages,
and a 30 minute inactive cache for returning to loaded discovery ranges.
Community query, profile kind, city and genre are URL parameters. Clear-search
preserves unrelated URL parameters. Global search no longer chooses a city
without the visitor selecting one.

The server projects existing public directory documents plus legacy artists
which have neither a directory subject identity nor a legacy directory link.
Any directory identity, including private, unpublished, moderated or merged,
suppresses its legacy fallback. No private contact, legal identity, credential,
account-existence or administrative enrichment fields are selected. Legacy
results use stable `/artista/{partyId}` destinations; directory profiles retain
`/directorio/{slug}`. A legacy result has an `artist-{partyId}` search identifier,
not a directory UUID; it does not offer UUID-only directory favorites.

`entityType=artist` selects existing artist, band and project profile kinds;
`profileKind` supports artist, band, project and person. Other global categories
remain those already supported by the service. Registered people appear only
through published public directory profiles. Draft legacy bands are not silently
published. Sorting remains relevance; no unsupported recent-activity promises
or fabricated verification badges are introduced.

The existing database normalizer handles case, Latin accents and whitespace.
Exact names, prefixes and partial names have separate relevance tiers ahead of
bounded quality/activity signals. Existing directory full-text, taxonomy alias
and trigram matching remain available. Legacy candidates use indexed name and
profile-text substring branches before metadata projection; short queries and
empty browsing can still require scans. Typo tolerance is supported by the
existing directory similarity function; the legacy candidate prefilter requires
a text match. This is an explicit limitation, not claimed universal fuzzy search.

Migration `2026-10-13_community_search_indexes.sql` adds two expression GIN
indexes using the existing `pg_trgm` extension and normalizer. It changes no
profile publication state and creates no external service or parallel index
store. When pg_trgm is unavailable, the existing normalization/matching remains
functional without those indexes. All SQL parameters remain Persistent-bound.

## Navigation contract

A single `NavigationScrollRestoration` lives above route Suspense boundaries.
AppShell marks its scroll container explicitly; public routes use the document.
Path-changing PUSH/REPLACE starts at zero. POP restores the history-entry key's
position, scoped by pathname to avoid collisions between fresh document loads,
with a bounded session-storage map across refresh. Query-only changes
preserve in-page position. Refresh without a saved position retains native
behavior. Anchors wait for their DOM target through rendering observers and use
scroll margins including the measured public sticky-header height. Resize/Mutation observers retry restoration for asynchronous
layout, not a timer. User scrolling or keyboard input ends pending restoration.
Outgoing position is captured from scroll/click events before the route DOM is
replaced. Inline mutations do not trigger a navigation reset.

Existing post-auth routing already defaults to Comunidad and validates return
paths. Its authorization/open-redirect checks are reused; the added regression
covers the default, a safe artist fragment and a protocol-relative rejection.

## Verification boundaries

- SQL regression: `TDF_DISCOVERY_TEST_DATABASE_URL=postgresql://127.0.0.1:55492/tdf_discovery_test python3 scripts/test-community-search.py`.
  Requires an isolated migrated database. Fixture writes are transaction-rolled
  back. Executes the SQL extracted from the shipped Haskell expressions, not a
  reimplementation of relevance. It does not exercise the HTTP handler.
- Component regressions: CommunityDiscovery, DirectorySearchPage, FanHubPage
  audit/onboarding and loginRouting Jest suites.
- Browser regression: `node node_modules/@playwright/test/cli.js test --config=playwright.discovery.config.mjs`.
  Controlled public API fixtures; desktop Chrome, Pixel 7, 360px Android and
  iPhone 13 WebKit configurations. This is browser integration, not real-device
  or live-backend E2E. Runs a fixed build to avoid development reloads changing
  history entries during assertions.
- Haskell validation: Stack's project GHC, targeting Server.Directory and API.
- CI, review, merge, deployment and production checks must be recorded separately
  in the final delivery report. The public production search was read before
  implementation and returned three directory profiles; that is baseline
  evidence, not verification of these changes.

The existing FanHub release feed/editor still consumes the legacy catalog for
its separate feed and management behavior. Discovery never filters that dataset
in the browser. Physical Redmi/iPhone devices and installed PWA behavior require
separate device evidence; viewport emulation does not establish those outcomes.

## Historical delivery evidence and release gates

| Requirement | Root cause / previous limitation | Implementation | Files changed | Tests | Verification |
|---|---|---|---|---|---|
| Community search | Legacy feed lacked prominent query controls | Debounced server search, filters, pagination and recoverable states | `CommunityDiscovery.tsx`, `useDirectorySearch.ts`, `FanHubPage.tsx` | Jest cancellation, URL filters, links, errors; PostgreSQL matching | Implemented; automatically tested locally; not production verified |
| Global artist search | Directory documents omitted unmapped legacy artists; implicit city hid results | Shared public projection, artist category, canonical links, explicit city choice | `Server/Directory.hs`, `API/Directory.hs`, `DirectorySearchPage.tsx`, generated clients/OpenAPI | SQL ranking/privacy/band/project/pagination/facets; 26 Global Search tests | Implemented; automatically tested locally; not production verified |
| Search state preservation | Local state and URL replacement could lose history intent | URL query/filter state and retained query pages | `useDirectorySearch.ts`, `CommunityDiscovery.tsx`, `DirectorySearchPage.tsx` | Component and browser discovery/history scenarios | Implemented; automatically tested locally |
| New-route scroll reset | Public document and persistent AppShell main use different scroll roots | Central router policy with explicit container selection | `NavigationScrollRestoration.tsx`, `AppShell.tsx`, `App.tsx` | Browser assertions for both scroll roots | Implemented; automatically tested locally |
| Browser history restoration | No per-entry restoration; async layout can override positions | Bounded history-key positions and rendering observers with user handoff | `NavigationScrollRestoration.tsx`, `RouteLoadingFallback.tsx` | Back/Forward, refresh and async-container scenarios | Implemented; automatically tested locally |
| Deep-link navigation | Mount effects competed with intended anchors | Fragment precedence, late-target observation and measured header offsets | `NavigationScrollRestoration.tsx`, `navigation.css`, `PublicBranding.tsx` | Late anchors, sticky offsets, Records user-handoff regression | Implemented; automatically tested locally |
| Mobile/responsive compatibility | Mobile search entry was hidden; no narrow discovery layout | Visible search affordances and responsive cards/filters | `PublicBranding.tsx`, `TopBar.tsx`, `CommunityDiscovery.tsx` | Desktop, Pixel 7, 834px tablet, 360px Android, iPhone WebKit; Firefox integration | Browser-emulated automation; physical devices/PWA installation unverified |
| Accessibility | Missing search landmark, labels and result status | Named form, live status, keyboard controls, clear/retry and reserved image geometry | `CommunityDiscovery.tsx`, navigation controls, `navigation.css` | Scoped discovery axe and selected whole-page browser axe assertions | Automatically tested locally; no whole-site WCAG or unrelated form-focus certification |


Local evidence includes actual migrated PostgreSQL query tests and a targeted
Stack/GHC compile of the directory module. Generated API conformance and 61
release-helper tests passed. UI typecheck and lint passed. The initial full UI
run passed 3,211 tests and failed five; a focused 139-test rerun passed all of
those failures after correcting an assertion for the new AbortSignal argument.
The final integrated six-suite search/navigation run passed all 113 tests,
including recoverable errors. This does not claim a fresh full-suite pass on
the final merged main revision.

PR: https://github.com/diegueins680/tdf-app/pull/576. Required branch policy includes
one independent approving review, strict required checks and serialized
`main-integration-admission`. These gates are not bypassed. Candidate changes
are not represented as merged or production-deployed. Public production
`/fans` and `/buscar` returned HTTP200 during read-only smoke checks; those are
availability checks of the existing release, not functional verification of this
candidate. Preview deployments are separate from production deployment.

Remaining evidence boundaries: physical Redmi/iPhone and installed PWA behavior,
full-page accessibility review, form-validation focus across unrelated forms,
production-sized query plans/load testing, live-backend browser journeys,
protected hosted CI, independent review and post-release functional smoke tests.
The new indexes are transactional with bounded lock/statement timeouts; large
production tables still require the existing deployment rehearsal and lock review.

Hosted CI initially rejected two stale catalog successor references; both now
point to the 203-entry migration manifest and all six provenance tests pass
locally. An earlier external Datadog production-web test checked the legacy
`tdf-app.pages.dev/` URL and received HTTP301 while expecting HTTP200. Its API
health check passed. After integrating main, Datadog, the PostgreSQL foundation,
social persistence, UI quality, migration and production-migration checks passed
at `a34d7a2bb25e90b13ce1ea82d0e699eab5542443`.

That hosted run identified three integration corrections: keep persona and
discovery browser artifacts as separate uploads; synchronize the mobile generated
client with the optional search fields; and register `profileKind` in the compiled
API declaration snapshot. The snapshot was compared with the retained artifact
from the actual CI binary: its only difference was the new optional query
parameter. Local CI-pipeline tests (28) and compiled-surface tests (12) pass.
Fresh hosted checks are required for the resulting commit.

The generated native client update is tracked in companion PR
https://github.com/diegueins680/TDF-mobile/pull/161. Its 97 suites / 625 tests and
`release:check` passed locally. Expo started and returned `packager-status:running`
on the isolated local port; it was stopped after validation. This is not a
native-device, installed-PWA or store-release claim.

The subsequent main integration preserves all 203 current-main migrations and
appends the unchanged community index migration, for 204 entries. Mobile main's
chat-thread notification destination is also retained; its six affected tests
pass. Its routing allowlist successor is explicitly reviewed in the catalog
decisions instead of silently dropping a new main feature.

Broader browser integration initially passed 477 cases, skipped 22 and failed
six. Three failures assumed an implicit Quito filter; the test now explicitly
selects Quito and retains the URL/history assertions. Login destination and
open-redirect rejection use separate fresh browser contexts. Storage-denial
tests wait for the mocked suggestion response before reload. Records waits for
the route to mount before handing anchor restoration to simulated user input.
With clean locked dependencies, the affected rerun passed 32 of 33 cases; the
remaining Records fixture then passed all five browser configurations after the
mount-readiness correction. Assertions were retained, including whole-page axe
checks, exact profile destinations, image fallback counts and request bounds.
Borrowed dependency symlinks produced an invalid local theme/contrast run; that
environment was replaced with a clean `npm ci` installation before this evidence.
The dedicated discovery/navigation matrix was then rebuilt with those locked
dependencies and passed all 20 cases across desktop, tablet, Pixel 7, 360px
Android and iPhone WebKit configurations.

Deployment also has an independent infrastructure blocker: `ops/hetzner/README.md`
states that the guarded routine release executor remains unfinished. The retired
`scripts/production-release.mjs` rejects before provider or database actions.
Preparing artifacts is not deployment; this task does not bypass that boundary.

## Historical October 14 continuation

Hosted CI for `fa349d854` completed with all 3,236 UI tests across 294 suites,
488 browser journeys (22 skipped), ten rollout checks and all 20 discovery
cases passing. Backend, API-contract and production-migration checks passed.
Mobile quality rejected notification-generator drift: Mobile main already
contained chat_thread while this parent’s canonical web resolver did not. The
web resolver now preserves that merged behavior, validates positive safe thread
IDs and uses the fixed chat fallback. Generator equality, both generated API
clients and 35 notification/navigation component tests pass locally.

Main PR544’s index-declaration repair is integrated. The unchanged community
index migration follows main’s 204 entries, for 205 total. The actual release
contract loader validated all 205 at `59da704e48e89dd80a95671f724c46ccdbbee1c9`,
including community introduction `08f3bcf50319983193d37ccec87d1c664c8ef21d`.
Fresh CI/review is required after the integration push. No candidate deployment
or production feature verification is claimed.

The continuation also passes 67 catalog/release-helper tests. Read-only production
availability checks on October 14 (local time) returned HTTP200 for `/fans` and
`/buscar`; these remain checks of the existing release, not this candidate.


## Current consolidation contract

The authoritative migration sequence is `scripts/production-migrations.json`.
This candidate preserves current main's 204 entries and appends the community
search indexes and the October 14 event-alias resynchronization migration, in
that order (206 entries). Their original introduction commits remain ancestors.
Earlier counts and test snapshots above describe historical candidates only.

The alias repair from PR586 refreshes event projections when a genre alias changes,
including old and new normalized names or genre identities. Its rollback removes
its trigger/function before the older event-search rollback; the canonical
migration test exercises retry, rollback, reapply and the stale-index negative
control. The community indexes change no data or public visibility and remain
compatible with the previous application. An application rollback may retain them;
removing them is optional performance cleanup, not a data rollback prerequisite.

Mobile is pinned to `57e6c3f9612b5dbd0bc8a55b80b9c9a79a880677`, whose tree equals
Mobile main `5cafe319f040034be717ee1674b66fe308425e70` at this audit. Web and native
clients are generated from this parent's OpenAPI, including optional `profileKind`.
The ticket prerequisite PR545 is retained by full merge ancestry.

Catalog decisions preserve their original inline retirement record. Repeated
nested copies of the migration review are replaced with hashes and file/entry
references to retained source commits, as documented in the catalog README.
The review references are evidence; the migration manifest remains the authority
for execution order. No production deployment or new feature activation is
established by this consolidation.


`DiscoveryFallbackPrivacy.tla` covers the privacy boundary in one SQL snapshot:
three subject-identity states and presence/absence of a legacy link. Any directory
subject identity or legacy link suppresses fallback, even if that identity is
private. Two deliberate mutations admit private subjects or ignore a link and
must violate the named safety invariant. Actual PostgreSQL controls separately
remove each shipped SQL predicate and must expose the synthetic hidden artist.
These finite checks do not prove arbitrary SQL translation, cache revocation,
concurrency across publication changes, liveness or complete application privacy.

The canonical directory migration runner now owns its actual server process,
allocates an available loopback port, and applies the existing current-handler
schema prerequisites after its historical migration tests. Its public backfill
control is isolated from subsequent exact HTTP facet fixtures. Earlier fixture
failures are retained as regression evidence; assertions were not weakened.
