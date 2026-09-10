# TDF public-booking HTTP concurrency audit — 2026-09-09

## Outcome

This bounded transaction-integrity batch closes the highest-priority verification gap from `reports/public-booking-idempotency-conflict-audit-2026-09-09.md`: the real Servant handler now runs in CI against the tested backend executable and a disposable PostgreSQL 17 database.

That runtime test exposed one high-confidence defect in the preceding implementation. Resource availability was resolved before the endpoint acquired its idempotency lock and inspected the durable replay receipt. A request retried after the original transaction committed could therefore receive `409` because its own booking made the room unavailable, instead of receiving the original `200` booking response. A changed payload under the same key could also receive the generic room-conflict reason instead of the idempotency-specific conflict.

Resource resolution now happens inside the same database transaction, after the per-key advisory lock and replay-receipt lookup. A durable matching receipt returns the original booking before availability is considered. A new request retains availability validation, deterministic resource locking, the PostgreSQL exclusion constraint, and transactional rollback.

The final HTTP test verifies concurrent equal requests, a post-commit replay, changed-payload key reuse, concurrent different-key room conflict, and database cardinality. No visual, copy, pricing, payment, cancellation, permission, role, analytics, schema, generated-client, mobile, external-provider, or production-data change is included.

## Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
|---|---|---|---|
| Repository read/write | Available | Work was made in the existing isolated worktree on `feature/public-booking-http-concurrency-20260909` | The dirty primary checkout remained untouched |
| Dedicated baseline | Available | Clean baseline `f3454a9ce857eae8b7658d322fa3790d1617a176` was recorded before branching | This batch is reviewable on top of the published idempotency contract |
| Candidate backend | Available | The production executable and 184-module test executable rebuilt after the handler change | The HTTP test invokes current compiled code, not a mock handler |
| PostgreSQL | Available | A self-cleaning `pgvector/pgvector:pg17` container received the production-schema fixture and complete reviewed migration batch | Handler behavior and committed row cardinality are exercised against the production database engine |
| HTTP/runtime | Available | The compiled Warp/Servant process reported healthy and received loopback `POST /bookings/public` requests | Exact HTTP statuses and response booking IDs are verified |
| Concurrency observation | Available | A test-only two-second booking trigger created a bounded overlap and `pg_locks` observed a blocked advisory-lock waiter | The equal-key result is not presented as merely sequential replay |
| External communications | Disabled | The test starts the server with `env -i`, no provider credentials, no required engineer, and discovery/enrichment/logistics workers disabled | No email, WhatsApp, payment-provider, or customer communication is attempted |
| Test-target guard | Available | The harness accepts loopback or the CI `postgres` service only and refuses a database name without `_test` | The script is not a production mutation path |
| Browser/native device | Not needed for this batch | No user-visible surface or native implementation changed | Earlier desktop/phone evidence remains applicable; no new screenshot is claimed |
| GitHub | Available | Implementation commit `2c6b8f39b4252634c42b22d6401ece2e8dc4866b` was pushed before this report | Draft review and exact-head CI are supported |
| Production | Not used | Only synthetic loopback requests and disposable database writes occurred | No production or staging validation is claimed |

## Baseline and methodology

- Root baseline: `f3454a9ce857eae8b7658d322fa3790d1617a176`.
- Preceding implementation: `2da5c939baca7cdeec8aba623a83e9b42440f2e3`.
- Environment: macOS host; current compiled Haskell backend; synthetic HTTP payloads; disposable PostgreSQL 17; reviewed production-schema migration fixture.
- Methods: handler/data-flow trace, historical-gap revalidation, real endpoint reproduction, controlled concurrency, exact status/body/booking-ID assertions, persisted-row cardinality checks, focused Hspec, shell syntax validation, CI contract tests, and strict catalog audit.
- Evidence boundary: this is synthetic integration evidence, not production validation, field telemetry, user research, payment verification, or external message delivery.

## Coverage matrix

| Route/state | Role | Device/runtime | Inspection method | Status |
|---|---|---|---|---|
| `POST /bookings/public`, two concurrent equal keys/payloads | Anonymous prospective customer | Real loopback HTTP; compiled backend; PostgreSQL 17 | Release gate, observed advisory waiter, response/body/row checks | **Verified:** `200/200`, identical booking ID, one booking/receipt/Party/resource/allocation |
| Same request after original commit | Returning anonymous requester | Real loopback HTTP | Third request after both initial responses | **Verified:** `200`, original booking ID |
| Same key with changed normalized input | Anonymous prospective customer | Real loopback HTTP | Status and exact reason assertion | **Verified:** `409` idempotency-specific response, no changed-input Party or booking |
| Same room/time with two different keys | Two anonymous prospective customers | Concurrent real HTTP | Release gate, statuses, database cardinality | **Verified:** one `200`, one `409`, one booking/receipt/Party/resource/allocation |
| Guest security records | Anonymous customer | PostgreSQL 17 | Party-bound table counts | **Verified:** zero `user_credential` and zero `party_security_role` rows |
| Production-schema automatic migrations | System | Disposable PostgreSQL 17 | Existing production entrypoint verifier | **Verified:** complete batch applied and idempotent before HTTP test |
| Notification deduplication | Engineer | No engineer/provider configured | Source behavior only | **Unchanged; not externally exercised** |
| Browser form, Domo fallback, mobile | Customer | UI/native | No visible or contract change | **Not rerun in this batch** |
| Process crash between commit and response | Anonymous customer | Fault injection | Not implemented | **Unverified**; post-commit replay semantics are verified without killing the process |
| Staging, production, providers, analytics | Customers/operators | External systems | Not invoked | **Skipped by scope/safety** |

## Historical finding revalidation

Historical reports remain unchanged.

| Finding/gap | Current classification | Evidence |
|---|---|---|
| Full PostgreSQL-backed HTTP replay/concurrency was unavailable | **Superseded; now verified locally and enforced in CI** | New guarded harness starts the compiled backend and asserts responses plus persisted rows |
| `PB-IDEMP-01` equal-key replay | **Partially resolved in the preceding batch; ordering defect found and fixed here** | Pre-fix runtime assertion: `Post-commit equal-key replay response: expected '200', got '409'`; final harness passes |
| `PB-CONFLICT-01` resource exclusion maps to `409` | **Verified through real HTTP** | Concurrent different-key overlap produced exactly one `200` and one `409`, with loser writes rolled back |
| `PB-GUEST-01` no undisclosed credential/role creation | **Verified through real HTTP and PostgreSQL** | Test Party IDs have zero credential and security-role rows |
| Production migration availability | **Verified** | Existing production-schema verifier passed before every endpoint test run |

## Finding PB-REPLAY-ORDER-01 — committed replay checked availability first

- Journey/role: anonymous customer retrying after a timeout, lost response, reload, or back navigation.
- Reproduction: create a resource-backed tentative booking, wait for commit, then resend the same key and payload.
- Expected: `200` with the original booking ID and no new write.
- Baseline actual: `409` because `resolveResourcesForBooking` saw the room occupied by the request's own committed booking before `createPublicTentativeBookingTransaction` inspected the replay receipt.
- Evidence/severity/confidence: real compiled HTTP/backend/PostgreSQL run; **high severity**, **high confidence**. Production frequency remains unknown.
- Cause: resource resolution and availability ran in an earlier independent `runDB` call outside the key-serialized transaction.
- Remedy: pass the resource-resolution database action into the transaction; execute it only in the no-receipt `createNew` branch after the lock and receipt lookup.
- Effort/dependencies: small backend control-flow change; no migration, contract, or dependency.
- Acceptance: concurrent equal requests return one booking; post-commit identical replay returns it; changed payload gets the idempotency-specific `409`; resource losers leave no Party, booking, relation, allocation, or receipt; all checks run on the tested CI executable.
- Status: **implemented and locally verified**; hosted exact-head CI pending at report creation.

## Engineering and safety rationale

The replay receipt remains the correctness boundary. Moving resource resolution inside the transaction ensures a matching receipt wins before mutable availability is consulted, while a new request still validates the current availability and takes deterministic row locks before writing. Different keys remain protected by the existing PostgreSQL range-exclusion constraint.

The test delay trigger exists only in the disposable test database. It is not a product migration and is never installed in application environments. It makes the overlap long enough to observe a blocked advisory-lock waiter and is discarded with the local container or CI service.

The harness refuses non-loopback databases except GitHub's `postgres` service hostname under `CI=true`, then independently requires the resolved database name to end in `_test`. It refuses pre-existing fixture markers instead of deleting or overwriting them. The candidate backend receives a clean environment containing only the required test settings, so host credentials cannot leak into email, payment, social, or messaging providers.

## Verification performed

- Pre-fix `npm run test:public-booking-http-concurrency`: **failed as intended** with `Post-commit equal-key replay response: expected '200', got '409'` after the production-schema migration verifier passed.
- `stack test --test-arguments='--match=ensurePartyRecord'`: **passed**, 1 example / 0 failures after rebuilding/linking the production and 184-module test executables. Existing Cabal missing-module, name-shadowing, unused-import, partial-function, and linker warnings remain.
- Final `npm run test:public-booking-http-concurrency`: **passed**. Results: equal-key `200/200`, post-commit replay `200` with the same ID, changed payload `409` with the stable idempotency reason, overlapping different keys `200/409`, exact row counts, zero guest credentials/roles.
- The final integration run also passed the existing production-schema automatic migration/idempotency verifier before starting endpoint assertions.
- `npm run test:ci-pipeline`: **passed**, 18/18 tests, including script-to-backend scope selection, exact tested-binary wiring, clean environment, target-host guard, and `_test` database guard.
- `npm run test:catalog-list-audit`: **passed**, 1/1.
- `npm run audit:catalog-lists`: **passed**, no unreviewed candidate.
- `sh -n scripts/test-public-booking-http-concurrency.sh`: **passed**.
- `git diff --check` and staged diff check: **passed** for the implementation commit.

No test was disabled, assertion weakened, timeout raised in product code, or empty collection counted as a pass.

## Changed files

- `tdf-hq/src/TDF/Server.hs` — makes replay lookup authoritative before resource availability.
- `scripts/test-public-booking-http-concurrency.sh` — guarded local/CI HTTP and PostgreSQL integration harness.
- `.github/workflows/ci.yml` — runs the harness after compiling/testing the backend and applying reviewed migrations.
- `scripts/ci-change-scope.mjs` — future harness-only changes select backend CI.
- `scripts/__tests__/ci-change-scope.test.mjs` — scope regression.
- `scripts/__tests__/ci-pipeline.test.mjs` — exact executable/database/safety wiring regression.
- `package.json` — reproducible local command.
- this report and the associated draft-PR description.

No screenshot was created because the change has no visible state; the earlier desktop/phone booking screenshots remain the applicable UI evidence.

## Deferred work and next batch

1. **External caller inventory and rollout** — impact: consumers missing the required header receive `400`; dependency: traffic/API ownership; acceptance: every active caller is migrated or explicitly retired before rollout.
2. **Domo fallback runtime** — impact: the no-authoritative-quote caller remains source/type verified; acceptance: real browser/runtime retry reuses one key and never claims payment or a hold.
3. **Crash-after-commit fault injection** — impact: stronger evidence for lost-response recovery; acceptance: terminate the test connection/process after commit, restart, retry, and receive the original booking without extra rows.
4. **Engineer notification replay** — impact: duplicate internal communications remain source-controlled rather than delivery-sink verified; acceptance: a configured local sink receives one notification for a created booking and none for replays.
5. **Booking/onboarding measurement and broader accessibility** — analytics, screen-reader, physical-device, field performance, and production validation remain deferred as documented previously.

## Handoff status

- Branch: `feature/public-booking-http-concurrency-20260909`.
- Base: `feature/public-booking-idempotency-conflicts-20260909` at `f3454a9ce857eae8b7658d322fa3790d1617a176`.
- Implementation commit: `2c6b8f39b4252634c42b22d6401ece2e8dc4866b` (pushed).
- Draft PR: https://github.com/diegueins680/tdf-app/pull/324.
- Hosted exact-head checks: running at report commit; final status is recorded in the execution handoff.

No merge, production deployment, production mutation, real transaction, or customer communication was performed.
