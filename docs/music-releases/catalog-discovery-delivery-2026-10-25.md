# Music catalog discovery delivery increment

This P1 candidate combines recording-title search with usable release catalog
pagination. It is stacked on the universal playback candidate and has not been
established as merged or deployed by this document.

The combined starting commit is `99ca66b07a7c9ef851d2dfb80199025b3307fc19`:
pagination commit `a90cb2e9a`, normal merge of playback head `6e61811c6`, and
recording-search integration `99ca66b07`. The original search baseline and
source hashes in [catalog-track-search.md](catalog-track-search.md) remain
historical execution identities. The combined catalog page has pagination in
addition to the original search-label change; its new hash must not be relabeled
as an input to the earlier SQL run.

Listeners can search `/musica` using a recording title even when the release
title differs, then reach releases beyond the first 24 cards. Search/page URL
parameters survive navigation; a changed query resets the page. Recording-title
matches reuse the existing asset-accessibility predicate and retain the existing
release-level response and ordering. Full behavior, assumptions and precise
test boundaries are in
[music-catalog-discovery.md](../../formal/system/music-catalog-discovery.md).

The focused requirement `MEDIA-MUSIC-DISCOVERY-001` owns the query, client/page,
their tests and the dedicated CI invocation. The initial integration retained
140 foundation requirement objects plus the discovery requirement; the latest
foundation refresh below retains 142 objects plus that unchanged discovery
requirement, for 143 total. Inventory and traceability are regenerated from
actual sources and the unchanged pinned Mobile checkout; these generated
mappings establish freshness and coverage, not test success.

Evidence to attach to the exact candidate before merge is the focused frontend
test run, current handler SQL regression with all four groups and its negative
control, compilation/type/lint gates, required hosted checks and independent
review. The original recording-search run passed four runtime groups on owned
PostgreSQL 16; that result retains its original source hashes. No p95 latency,
production upload-to-playback acceptance, full HTTP handler binding or physical
device/screen-reader acceptance is claimed here.

This search/pagination change adds no migration, asset backfill, infrastructure
or alteration to original masters. Application rollback restores the previous
query/page implementation.
The target must already contain the declared directory normalizer and existing
music publication/delivery predicates. Production verification and normal
protected integration remain required before claiming delivery.

Remaining P1 work includes universal `/buscar` projection, richer authorized
metadata/typo retrieval, non-release sources and historical reconciliation.
Offset pagination assumes stable results between requests; concurrent
publication or withdrawal may move rows between pages.

## Integrated execution evidence

Combined UI/API/player regression66/66 passed in six suites after preserving the new search label and waiting for the library fixture query to enable its playback button. TypeScript and Vite build passed; the initial bundle is334831 gzip bytes with five preloads. The exact integrated handler and SQL runner passed all four PostgreSQL groups on server16.13; the receipt binds handlerfb6257e6 and runner070bc9f4 before/after, owned databases are removed and the55571listener is stopped. These are SQL-literal controls, not compiled HTTP binding acceptance.

The new actual-browser discovery journeys passed4/4 in desktop Chromium and Pixel7 with synthetic API metadata, zero retries/skips and no media-method substitution. They verify keyboard pagination to records25and49,44px controls, detail/back navigation, query reset and URL/deep-link persistence. The browser served entryindex-nxqzNTxl.js (SHA2563b8c53c329d3c3d747576635c943f9d8fe3659417fd8beffa3bada41149448b7). Metadata/SQL and browser evidence have distinct boundaries. Versioned report `reports/music-catalog-discovery-2026-10-25.json` records the source and receipt identities. Required exact-head hosted checks and fresh review remain pending.

The preceding foundation refresh merged playback
`464d49dcdf9b72c6670cb4bb6ea1716d6b7e3429`, including the clocked Linux audio
sink and main's canary integration, as
`0a0dd901471653fc874175ebb401d4580fb0419b`. Its 140 foundation requirements
plus discovery made 141 total. The handler/SQL regression inputs remain exactly
the integrated PostgreSQL execution inputs.

## Latest foundation reconciliation

The new normal merge takes playback foundation
`70e4e2e2679ac607ee15af9967e56aac5a0f58f0`, which contains protected main
`1c9c833cc1ba62b47822fca3c20e14e86cf28daa` with the completed directory-alert
and logout-intent integrations. Every one of that foundation's 142 requirement
objects is retained field-for-field. The original `MEDIA-MUSIC-DISCOVERY-001`
object from `0a0dd9014` is appended unchanged, making 143 total. This preserves
all fields of `AUTH-LOGOUT-001`, `DIR-SAVED-SEARCH-001` and the player requirement,
including its native-audio test wrapper mapping. Only requirements and generated
inventory/traceability conflicted; the merged sources are not rewritten by this
metadata reconciliation.

The actual pinned Mobile source remains
`d832bb3057dd03d44c0f35eea05be9f36320429d`. Main's 1,330 catalog decisions and
the nine audio decisions retained by the playback foundation are preserved;
this discovery refresh adds no catalog decision. The recording-search handler
and SQL-runner hashes remain `fb6257e60b9c506e856ef7827946fbeb166ba9b4f0d5227fa9c0fa91c0bb17fc`
and `070bc9f4770b80966328b164bbbd29a7ecc623eb0f4f2c0612e9c24542abd6a2`.

The preceding `0a0dd9014` candidate completed
[hosted CI run 40266955707](https://github.com/diegueins680/tdf-app/actions/runs/40266955707):

- UI: 3,413 tests in 303 suites passed.
- Canonical backend build and tests: 4,039 examples, zero failures, 21 pending.
- Recording-search PostgreSQL controls: all four groups passed, including the
  missing-authorization mutation control.
- Native audio browser suite: 40 passed.
- Persona browser suite: 524 passed, 22 skipped; rollout checks: 10 passed;
  discovery checks: 25 passed.

These are results for that preceding head. Pending examples and skipped browser
tests are not passes, and the new merged candidate requires its own relevant
qualification, hosted checks and independent review. The local Stack build was
**cancelled with exit 130** after the preceding candidate's hosted canonical
build and tests passed; it is not a local build pass and provides no current
merged-candidate qualification. No production deployment or business write is
established by this refresh. Exact result boundaries remain recorded in
`reports/music-catalog-discovery-2026-10-25.json`.

## Journal integration baseline

The following normal refresh retains playback `7de7c292753565ae6ea64c57dd9aa4b97e330b6d` and protected main `3af768dacd95c1223975c62da3ce8088bed89b47` (release-journal #618). All 142 foundation requirement objects remain exact plus the unchanged discovery requirement. This refresh changes no audio/search/page implementation; 106 combined player/catalog/client/logout regressions in eight suites passed after the prior privacy integration. Exact new-head CI and review remain required.
