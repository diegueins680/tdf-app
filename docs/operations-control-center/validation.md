# Validation and acceptance evidence

Validation date: 2026-08-11. Environment: isolated feature worktree, PostgreSQL 16 disposable Docker database, local Haskell/Node toolchains. No provider production credentials were used.

## Executed evidence

- Main operations migration applied successfully after the current Persistent schema and applied a second time with `ON_ERROR_STOP=1`.
- Registration capture created one domain event/outbox record; processing created one thread and event. Reprocessing produced zero additional visible effects.
- Changing the source registration to paid/cancelled added history to the same thread, resolved the thread, and did not rewrite the source status.
- Backfill dry-run reported eligibility without insertion. Apply produced one item; an interrupted/rerun path reported zero remaining and did not duplicate its thread.
- A persisted inbound WhatsApp event with external ID `ops-wa-test-1` processed `(1,0,0)`, produced one correlated conversation thread and one provider event, and projected zero message-text metadata fields.
- OpenAPI generation succeeded for web and mobile after the contract change.
- Web TypeScript typecheck, production build, full lint (zero errors; 72 pre-existing warnings), and all 128 Jest suites / 1,547 tests succeeded. The operations component test includes an axe scan with no serious/critical findings; existing React `act` warnings remain test-harness noise.
- Mobile TypeScript typecheck, lint with zero warnings, public Expo configuration, all 40 Jest suites / 209 tests, and the two operations suites (three tests) succeeded.
- The Haskell executable/test target compiled all 139 modules and the full backend suite passed: 2,289 examples, zero failures. Operations lifecycle/RBAC tests include 200 QuickCheck cases; priority/SLA policy also passed. Existing compiler warnings outside the operations modules remain.
- Feature-registry audit passed with 116 features, 129 web routes, and 36 mobile routes. The backfill shell and HTTP load harness passed syntax validation.
- The non-destructive rollback produced `t|t|t|t`: every provider and organization disabled, immutable evidence table retained, and all capture triggers absent. Reapplying the migration recreated capture and the SQL acceptance test passed again.

## Acceptance mapping

| # | Scenario | Evidence |
| --- | --- | --- |
| 1 | Registration → one thread and operational/business flow | capture trigger, correlation constraint, source link/quick actions; full payment provider activation is credential-dependent |
| 2 | Registration replay no duplicate | executed DB replay evidence; unique event/correlation keys |
| 3 | Booking conflict urgent/no unsafe approval | conflict priority metadata and existing booking conflict guard; separate source command |
| 4 | Transfer receipt verification | receipt capture, Accounting/Reception scope, existing payment verification source action |
| 5 | Duplicate gateway webhook | provider event uniqueness plus existing Stripe/payment idempotency |
| 6 | Overdue invoice SLA escalation | invoice capture/backfill plus 50/80/100/150 SLA function |
| 7 | Failed outbound retry/failure queue | persisted attempt/failure model; provider dispatcher activation remains provider-dependent |
| 8 | Authorized dead-letter replay | Manager endpoint, retry state, reason/request audit, idempotent projection |
| 9 | Refund second person/no self approval | API role guards, DB self-check, tested model classification |
| 10 | SRI authorized immutability/note workflow | existing invoice immutability plus dual-approval classification; direct offline SRI conformance remains disabled and documented |
| 11 | Teacher/Engineer financial restriction | model property/unit cases and server projection filters |
| 12 | Organization/branch isolation | membership predicate on every API/stream path; DB/API integration staging test required before enablement |
| 13 | Immutable audit | append-only trigger plus atomic command audit |
| 14 | Resolve/archive does not mutate source | separate operations update and executed source-status check |
| 15 | Backfill interruption/rerun | executed dry-run/apply/rerun evidence |
| 16 | Real-time reconnect | monotonic replay API, web two-second delta invalidation; browser E2E required in deployed canary |
| 17 | Mobile push/deep links authorization | encrypted token API, opaque link design, route/API guard; APNs/FCM credentials required |
| 18 | Search/views/bulk/pagination persisted | real REST API, cursor pagination, saved-view table, versioned bulk calls; no production mocks |
| 19 | Spanish/English | web locale catalogs and mobile bilingual operational states |
| 20 | No production mock fallback | implementation calls API only; mocks confined to test files |

Items described as credential/deployed-canary dependent are not represented as production-verified. The PR must stay draft until those environment-specific checks and representative HTTP load tests are attached.

## Performance evidence and method

`tdf-hq/sql/benchmark_admin_operations_control_center.sql` ran on the disposable PostgreSQL 16 Docker database with 10,000 synthetic domain events and rolled the entire fixture back. Final implementation results:

| Measurement | Result |
| --- | ---: |
| Atomic event + outbox capture | 10,000 in 5.184 s (about 1,929/s) |
| Single-worker full projection | 10,000 in 57.597 s (about 174/s) |
| Filtered full-text inbox, 50 rows, 100 samples | p50 179.729 ms; p95 190.732 ms; max 207.977 ms |
| Reconciliation | 10,000 events = 10,000 processed outbox records = 10,000 threads = 10,000 thread events |

The measured database inbox p95 is below 500 ms and capture exceeds the 250 events/s target on this local environment. A single worker does not sustain a continuous 250 events/s projection rate; the implementation permits multiple replicas to drain independent aggregates using `SKIP LOCKED` while preserving per-aggregate predecessor ordering. Initial load is covered by one worker; the two-year continuous-rate target requires at least two worker replicas and must be confirmed by the canary HTTP test. This is not represented as end-to-end compliance.

`scripts/operations-load-test.mjs` measures authenticated inbox and quick-action p95 plus replay visibility using real persisted API data. Run against a staging organization with representative indexes/cardinality:

```sh
TDF_OPS_BASE_URL=https://staging.example \
TDF_OPS_TOKEN='…' \
TDF_OPS_WORK_ITEM_ID='…' \
TDF_OPS_EXPECTED_VERSION=7 \
node scripts/operations-load-test.mjs
```

The harness fails if inbox p95 ≥500 ms, quick action p95 ≥750 ms, visibility p95 ≥3000 ms, any request fails, or a replay produces a duplicate effect. No authenticated canary token/environment was available in this implementation session, so HTTP quick-action and post-commit visibility figures remain a rollout gate rather than an asserted result.
