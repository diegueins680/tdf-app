# Canonical refund accounting: implementation and verification

Date: 2026-09-16 UTC. This is a bounded continuation, not completion of the entire
payment platform. Dependency: [draft #396](https://github.com/diegueins680/tdf-app/pull/396).
Decision: [ADR 0129](../adr/0129-atomic-canonical-refund-accounting.md).

## Capability and repository checkpoint

Worktree: `/Users/diegosaa/GitHub/tdf-app-payment-checkout`. The original dirty
`tdf-app` worktree was not changed. Instructions and workflow doctor were read/run;
doctor reported 16 OK, two missing daily-memory warnings, no errors, and a clean
branch. GitHub reads, fetch, local branch creation and commits succeeded.

Started from #396 at `20566eab9e99685983beac359f2462386e59f8f7` and merged the
observed latest default `main`, `7eafd3f479817f267ba0de6592c3e16ea920f4ae`, in
`a3be3fc12a96c3992daba95b3f0d5df1a9d3ed3d`. That merge introduced no file changes:
the signing-secret example cleanup was already inherited. No secret-shaped old
diff or credential value was printed. No GitHub PR was merged by this work.

Reviewed the accessible open root and mobile PR metadata and new payment-core
diffs. #331 was observed at `790b239760206cec0c88e2aae29838fa4b4c3fa6`; its newer
contact-flow, completion-capability, manual-transfer and environment-summary
repairs are not integrated here. Mobile #83 was observed at
`337d46f25a8b7c7c688aa3657ffb599ecb4c47c7`, on an older base than mobile #82.
Do not replace the current #82 submodule and lose its query/evidence contracts.
These branches add no held-refund completion implementation to reuse. A separate
integration PR must preserve both histories and regenerate from canonical OpenAPI.

GitHub #396 checks were read again: final-head UI quality succeeded at
2026-09-16T05:01:58Z, and backend quality succeeded at 05:53:41Z. These are parent
CI results, not CI results for this increment:
[UI job](https://github.com/diegueins680/tdf-app/actions/runs/35057555894/job/104670890423),
[backend job](https://github.com/diegueins680/tdf-app/actions/runs/35057555894/job/104670890405).

This increment did not re-run staging discovery or provider credentials checks.
The last staging inspector evidence remains 2026-09-16T03:12:53.786Z: public web/API
health accessible, hosting authentication unavailable, deployed SHA unknown and
provider qualification false. That is historical evidence, not a fresh staging
result. No sandbox, staging deployment, production operation or real transaction
was performed. See [operator runbooks](operator-runbooks.md) for activation gates.

## Findings and changes

The existing refund completion path left canonical intent status/totals/history
unchanged, despite completing checkout/ledger/receipt records. Two real database
regressions directly showed `captured/0/0 history` after successful full or combined
partial refunds. Other new tests demonstrated acceptance of canonical balance
drift, a canonical dispute, and an invalid transition correlation.

`advanceCanonicalRefund` now verifies the original succeeded payment, environment,
merchant, currency, checkout state and refund aggregates while holding the relevant
rows. It checks the intent's immutable snapshot, captured amount and sum of prior
succeeded refunds for that attempt, then invokes the canonical state machine before
the existing financial writes. Any SQL failure rolls the transaction back. No new
provider-specific accounting path is introduced.

The canonical refund sum now uses `Integer` for its comparison before accepting an
`Int64` result. The red counterexample was a captured maximum `Int64`, one minor
unit already refunded, and a new maximum refund; the old transition returned
`-9223372036854775808` refunded. Deterministic and full-range generated tests cover
the repaired boundary. No floating-point money or changed API numeric type is used.

No API, web, mobile, generated client, environment configuration, activation flag,
SQL migration or historical data rewrite is part of this increment. The existing
service PayPal completion caller uses the repaired shared store. Tests exercise
the real store with synthetic PayPal-labelled evidence, not PayPal HTTP.

## Executed verification

Environment: local macOS, Node 24.8.0, PostgreSQL 16.10; Stack resolver `lts-24.42`,
GHC 9.10.3. All credentials/data in database tests were synthetic; isolated clusters
were Unix-socket-only with empty initial databases named `tdf_provider_retry_test`.
No system GHC/Cabal, production database, live provider or provider SDK sandbox was used.

Source points:

- Baseline: `20566eab9e99685983beac359f2462386e59f8f7`.
- Initial red unit tests: `ab02c8b1f0748c482642f06565e8775c5b3d2377`.
- Red database tests (correct existing event-name assertions):
  `cd2044abf47ea9ca1cb7e98c5c89ccefbba59a70`.
- Fixed runtime and database tests: `0d8dc52655f1eb5b485cae500f476ed5188a2a7d`.
- Final test source: `6723bd8436e33d36c604cfba376a2d90f912436e` adds only another
  pure canonical money property; runtime and database cases remain byte-identical.

Commands below use the root unless marked `tdf-hq`. Pipelines used `set -o pipefail`
and `2>&1 | tee LOG`; full final output additionally used `| tail -12` to bound the
console, not the saved log. Timestamps are completed-log modification times in UTC.

| Command / source | Completed UTC | Outcome / evidence |
| --- | --- | --- |
| `npm run ai:doctor`, baseline | Turn checkpoint | 16 OK, 2 warnings, 0 errors; exit 0 |
| `stack test --fast --rerun-tests --test-arguments='--match=refund-safety'`, baseline, `tdf-hq` | 05:22:05.112Z | 3 examples, 0 failures; 100 generated cases; exit 0; `/private/tmp/tdf-payment-refund-reconcile-baseline-20260916.log` |
| Same focused command, initial red, `tdf-hq` | 15:07:09.889Z | 4 examples, 1 expected failure; seed 1922308137; exit 1; `/private/tmp/tdf-refund-reconcile-red-unit-20260916.log` |
| Disposable PG start, restricted sandbox | 15:06:55.647Z | Denied shared-memory attachment; no tests; exit 1. Approved retry started successfully; retained synthetic `server.log`. |
| `TDF_PROVIDER_RETRY_DATABASE_URL=RED_URL sh scripts/test-provider-retry-runtime.sh`, red DB source | 15:18:15.999Z | 231 examples, 6 expected failures, 15.3786s; seed 1568803708; exit 1; `/private/tmp/tdf-refund-reconcile-red-db-20260916.log` |
| `stack test --fast --rerun-tests --test-arguments='--match=refund-safety --seed=1922308137'`, fixed, `tdf-hq` | 15:20:15.309Z | 4 examples, 0 failures; 100 generated cases; exit 0; `/private/tmp/tdf-refund-reconcile-green-unit-20260916.log` |
| `npm run quality:repo`, fixed | 15:20:51.128Z | 232 tests passed, exit 0; formal audit: 9770 findings, 0 critical/errors, 382 warnings, 9388 info; `/private/tmp/tdf-refund-reconcile-repo-quality-20260916.log` |
| `TDF_PROVIDER_RETRY_DATABASE_URL=GREEN_URL sh scripts/test-provider-retry-runtime.sh`, fixed | 16:16:03.698Z | 231 examples, 0 failures, 12.4426s; exit 0; `/private/tmp/tdf-refund-reconcile-green-db-20260916.log` |
| `stack test --fast --rerun-tests`, fixed, `tdf-hq` | 16:17:23.720Z | 2631 examples, 0 failures, 21.2694s; exit 0; `/private/tmp/tdf-refund-reconcile-full-20260916.log` |
| `stack test --fast --rerun-tests`, final test source, `tdf-hq` | 16:19:29.567Z | 2632 examples, 0 failures, 24.0836s; exit 0; both refund money properties ran 100 generated cases; `/private/tmp/tdf-refund-reconcile-final-full-20260916.log` |
| `npm run quality:repo`, final test source | 16:23:58.131Z | 232 tests passed; same formal totals as the fixed run; exit 0; `/private/tmp/tdf-refund-reconcile-final-repo-quality-20260916.log` |

Catalog regeneration, both commands exit 0 at final source:

```sh
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output docs/catalog-persistence/reports/static-list-inventory.json
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --format csv --output docs/catalog-persistence/reports/list-consumer-matrix.csv
```

JSON generated at 16:20:08.072Z; CSV written at 16:20:52.148Z: 1433 scanned files,
1135 reviewed candidates. Decisions are byte-identical to #396; no new review entry
or exception was added. Both completed processes returned exit 0, with no console
output. The empty tee files are not used as completion-timestamp evidence.
`git diff --check`, 64 local documentation-link target checks, generated web/mobile
byte comparison and unchanged-surface diff checks passed. An initial ad-hoc Node
comparison had a parenthesis syntax error before reading files; the corrected
read-only comparison succeeded. It was not an application/test failure.

`RED_URL` was
`postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-refund-reconcile-red-pg.ZyF50t`;
`GREEN_URL` used `/private/tmp/tdf-refund-reconcile-green-pg.Zen36Z` instead.
Both clusters used `initdb -D DIR/data -U postgres --auth=trust --no-locale -E UTF8`,
`pg_ctl -D DIR/data -l DIR/server.log -o "-k DIR -h ''" start`, and
`createdb -h DIR -U postgres tdf_provider_retry_test`, with PostgreSQL 16 binaries
under `/usr/local/opt/postgresql@16/bin`. These URLs contain no secret passwords.

The harness validates an empty disposable target before writes, applies the real
migration chain, exercises query-recovery repeat/empty rollback/reapply and operator
flag retention, and runs all provider-runtime cases. The green run also passed its
final used-history rollback-refusal/readability check; the red run stopped at the
expected test failure before that final check. No new migration was introduced.

Both servers were stopped with explicit `pg_ctl -D DIR/data -m fast stop` after
testing. Restricted stop attempts were denied; approved retries returned exit 0.
Independent status checks returned `no server running` for both. Files and logs
remain recoverable; nothing was deleted. Existing compiler/linker warnings and
advisory formal findings remain visible; no quality threshold was weakened.

The transaction rollback test intentionally raises an SQL error *after* the real
completion call in the same transaction, checks that the intent, refund, receipt
and ledger reverted, then successfully retries completion. It is not a provider
timeout or an injected ledger-internal crash. Concurrent tests use real database
connections and synthetic verified observations, never parallel refund POSTs.
The legacy-null-intent branch is retained by code inspection; these new refund
fixtures use bound canonical intents. No historical-null refund backfill or full
legacy completion replay was newly exercised here.

## Official source check and next recovery boundary

Access date: 2026-09-16 UTC. Official source, high confidence in API mechanics:
[PayPal GET refund](https://developer.paypal.com/api/payments/v2/refunds-get) and
[PayPal maintained Payments v2 OpenAPI, version 2.12](https://raw.githubusercontent.com/paypal/paypal-rest-api-specifications/main/openapi/payments_payment_v2.json).
The documented lookup is `GET /v2/payments/refunds/{refund_id}` with OAuth. Its
example includes an `up` relation to the original capture; statuses include
`COMPLETED`, `PENDING`, `FAILED`, `CANCELLED`. This does not establish TDF merchant
availability, credential health or successful sandbox execution. No pricing,
regulatory or Ecuador portfolio claim was refreshed in this increment.

The next implementation must query only an already-known immutable refund ID
under the original merchant/environment, validate identity/capture/amount/currency,
then apply verified completion through this repaired store. Do not follow response
URLs blindly, retry the original POST, treat 404/age as proof of no refund, or
release funds from status text without a separately reviewed finality policy.

Still unimplemented: that provider GET adapter and authenticated operator action;
unknown-ID discovery; audited legacy-failed resolution; historical canonical
backfill; refund/dispute coordination; remaining tax, seller and fee allocations;
SRI issuance; updated #331/#83 integration. They are unfinished implementation,
not tasks falsely attributed solely to missing credentials.

Still externally unverified: merchant refund capability, sandbox account/secret
aliases and webhook registration, staging hosting authentication/deployed SHA,
qualified sandbox/refund tests, staged activation and legal/accounting sign-off.
Required hosting names remain `FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` or
an authorized local Fly login. Do not supply credential values in code or reports.

## Rollout, rollback and review

Merge order: existing canonical/payment stack, #393, #396, then this accounting
increment. No migrations/backfill accompany this PR. Before activation, reconcile
existing checkout and per-attempt succeeded-refund aggregates against stored
totals. Treat mismatches as exceptions for evidence-backed review, not authority
for direct SQL repair. Keep provider execution disabled until recovery is qualified.

An application rollback preserves new intent/refund/history rows, but an old binary
can reintroduce divergent totals. Disable affected refund commands, drain old
writers, preserve reservations and financial evidence, and prefer a forward fix.
Do not erase successful refunds or decrement balances to make reconciliation pass.

No new browser/mobile test, screenshot, device run, provider sandbox test, staging
test or deployment was executed. Their surfaces and contracts were unchanged;
the earlier local full-web timeout/interruption record remains in
[the #396 evidence](refund-execution-safety-2026-09-16.md), not replaced by a mock.
