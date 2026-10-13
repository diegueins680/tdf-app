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
position, with a bounded session-storage map across refresh. Query-only changes
preserve in-page position. Refresh without a saved position retains native
behavior. Anchors wait for their DOM target through rendering observers and use
scroll margins. Resize/Mutation observers retry restoration for asynchronous
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

## Delivery evidence and remaining release gates

| Requirement | Previous limitation / cause | Implementation and primary files | Evidence |
|---|---|---|---|
| Community search | Legacy feed catalog had no prominent query control | `CommunityDiscovery.tsx`, `useDirectorySearch.ts`, `FanHubPage.tsx` | Jest debounce/cancellation, URL filters, canonical links, error recovery; PostgreSQL matching cases |
| Global artist search | Published directory documents omitted unmapped legacy artists; implicit city hid results | `Server/Directory.hs`, `DirectorySearchPage.tsx`, API/OpenAPI types | Real SQL ranking, privacy, band/project, pagination and facet assertions; component tests |
| Search state preservation | Local state and unconditional URL replacement could lose history intent | Shared query cache, URL query/filter state; integrated main's URL-driven global search | Component regressions and browser discovery/history scenario |
| New-route scroll reset | Public document and protected persistent main used different scroll roots | `NavigationScrollRestoration.tsx`, `AppShell.tsx`, `App.tsx` | Browser assertions for both scrolling roots |
| Browser history restoration | No centralized per-entry restoration; async layout/native anchoring could override position | Bounded history-key positions, rendering observers and restoration handoff on user input | Back/Forward, refresh and async-container browser scenarios; final matrix status recorded in PR |
| Deep-link navigation | Mount effects and generic scroll resets competed with intended anchors | Fragment precedence, late-target observers, `navigation.css` scroll margins | Delayed fragment target and 16px offset assertions |
| Mobile/responsive compatibility | Search entry hidden in mobile menu; no narrow discovery layout | Public/header search affordances and responsive MUI cards/filters | Chromium desktop/Pixel 7/834px tablet/360px Android and iPhone 13 WebKit configurations; no physical-device claim |
| Accessibility | Missing search landmark/status and discoverable keyboard entry | Named search form, labels, live status, retry/clear controls, bounded image geometry | Scoped axe serious/critical checks plus component/browser interactions; not a whole-site WCAG certification |

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
locally. The external Datadog production-web test checks the legacy
`tdf-app.pages.dev/` URL and received HTTP301 while expecting HTTP200. Its API
health check passed. This external failure is not hidden or reclassified as a
success for this PR.
