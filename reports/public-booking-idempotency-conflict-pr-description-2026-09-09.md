# Draft PR: make public tentative bookings idempotent and conflict-safe

## Problem

The unauthenticated no-price booking compatibility route had no durable idempotency contract. A retry after a timeout or dropped response could create another tentative booking. PostgreSQL already prevented overlapping resource allocations, but the handler did not translate its `23P01` exclusion failure into the documented `409` response.

## Changes

- Require and validate `Idempotency-Key` for `POST /bookings/public`.
- Add an additive PII-free replay table containing only key, normalized-request SHA-256, booking reference, and timestamp.
- Serialize equal keys, replay equal payloads, reject changed payloads with `409`, and avoid duplicate engineer notification.
- Run guest Party creation, booking/resource writes, and replay receipt in one transaction.
- Map the existing allocation exclusion failure to `409` after rollback.
- Preserve payload-scoped keys across ordinary retry and same-tab reload/back using a sessionStorage name derived only from a Web Crypto SHA-256 digest.
- Update both known first-party callers, OpenAPI, generated web/mobile types, production migration manifest, CI migration rehearsal, Jest, Hspec/build evidence, and desktop/phone Playwright retry coverage.
- Repair one pre-existing migration-manifest ancestry pointer to the byte-identical introducing commit in current history so the guarded local release planner can evaluate the stack.

## Evidence

- Audit: `reports/public-booking-idempotency-conflict-audit-2026-09-09.md`
- Focused web Jest: 3 suites / 28 tests passed.
- Focused web ESLint, web TypeScript, mobile lint, and mobile TypeScript passed.
- UI production build passed; bundle guard: 5 preloads / 413,015 gzip bytes initial JavaScript.
- Backend focused Hspec: `PublicBookingReq` 3/3 and `ensurePartyRecord` 1/1 passed; production and 184-module test executable compiled/linked.
- New PostgreSQL 17 idempotency migration rehearsal passed.
- Existing PostgreSQL 17 service-booking rehearsal passed and rejected an overlapping legacy allocation.
- Production-release suite passed 49/49.
- The guarded release planner passed in non-mutating dry-run mode with an empty command list after repairing the pre-existing manifest ancestry pointer.
- The strict catalog-list gate initially found two new fingerprints and one stale fingerprint; the reviewed metadata was updated, and the exact local discovery/policy commands now pass.
- Playwright retry journey passed 2/2 on desktop and phone; the first synthetic `503` and second successful POST used the exact same key.
- Mobile contract commit `57abd23c0522133274e5bc2aa1a7e524d94dd62b` is available on its pushed feature branch.
- Mobile draft PR: https://github.com/diegueins680/TDF-mobile/pull/71.

## Risks and compatibility

- The endpoint now requires a 16–128 character visible-ASCII key. Known generated and first-party clients are updated; unknown external consumers were not inventoried and will receive documented `400` until migrated.
- Browser storage is only a key-continuity aid. Server/database state is authoritative, and no contact payload is stored in the browser key name or replay table.
- Exact handler behavior was compiled and source-reviewed but not invoked through a configured PostgreSQL-backed HTTP process. Migration invariants and the client retry are verified independently.
- No price, payment, cancellation, role, permission, feature flag, customer message, analytics payload, or production data changed.
- No conversion, Web Vitals, provider, SMTP, staging, or production claim is made.

## Migration and rollback

Apply `tdf-hq/sql/2026-09-09_public_booking_tentative_idempotency.sql` through the reviewed production manifest before serving this code. It is additive and idempotent.

The supplied rollback intentionally retains the replay table. Older application code ignores it; deleting it would allow a historical retry key to create a duplicate booking. Roll back application traffic first and define any later retention/drop policy separately.

## Remaining gaps

- Add a barrier-synchronized PostgreSQL-backed HTTP integration for equal retry, changed payload, and overlapping resource requests.
- Inventory external callers before rollout.
- Add privacy-reviewed exactly-once booking funnel measurement.
- Runtime-exercise the Domo legacy fallback retry.
- Complete screen-reader, keyboard/focus, zoom, offline, and physical-device validation.

No merge or deployment is requested by this draft.
