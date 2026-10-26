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
their tests and the dedicated CI invocation. All 140 inherited requirement
objects are preserved unchanged in this increment. Inventory and traceability
are regenerated from actual sources and the unchanged pinned Mobile checkout;
these generated mappings establish freshness and coverage, not test success.

Evidence to attach to the exact candidate before merge is the focused frontend
test run, current handler SQL regression with all four groups and its negative
control, compilation/type/lint gates, required hosted checks and independent
review. The original recording-search run passed four runtime groups on owned
PostgreSQL 16; that result retains its original source hashes. No p95 latency,
production upload-to-playback acceptance, full HTTP handler binding or physical
device/screen-reader acceptance is claimed here.

There is no migration, asset backfill, new infrastructure or change to original
masters. Application rollback restores the previous query/page implementation.
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
