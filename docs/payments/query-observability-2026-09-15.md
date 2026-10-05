# Payment-query observability: verification and handoff

Date: 2026-09-15 UTC / America/Guayaquil. This continues the payment platform with
a read-only admin report, not provider activation or overall completion. See
[ADR 0122](../adr/0122-read-only-payment-query-observability.md).

## Access and repository review

Resumed clean root worktree `tdf-app-payment-checkout` at #371,
`4ccaaa190c4ae6b1d041d8eabd7eed8fd5327c5e`. Fetched remote refs, reviewed all open
PR metadata and payment remote branches. At the initial inspection, default `main` was
`73edd77a36c8dcc73e5217303c62376ae684853b` and is an ancestor of this branch.
No newer payment implementation duplicated the query-report work. The only
new fetch update was unrelated social audit documentation. At the initial
#371 inspection around 14:21 UTC, completed checks were successful and build,
backend, UI, mobile, persona and migration checks were still running; there
were no human reviews. No pending check is claimed as passed.

Created root `codex/payment-recovery-observability-20260915` after #371.
The original dirty `tdf-app` worktree was not modified. Root runtime/API/test
commit: `a1a6350f852a01eb6134ccb8ae742e5a9b12ac9f`.
Web/contracts commit: `9fddf58037c1780402fe5393d1d4694e005d765e`;
API-test lint correction: `0ffe9d666debb12bc5492fb978e951b9dd5988a0`.
The final runtime verification below uses this committed source; subsequent
changes are catalog review and documentation only. At 15:02 UTC, #371 still had
backend CI in progress; all other reported checks had succeeded. Parent CI is
not evidence that this new branch's CI passed.
At final verification (15:06 UTC), GitHub `main` had advanced to
`52b32e4c17b3b0485c66e666137f85d44cc962d4` through unrelated artwork PR #369
(merged 14:33:52 UTC). Its six changed files were inspected: no payment runtime
overlap, but the catalog review file has additional entries. This dependent PR
preserves its #371 base rather than importing unrelated artwork changes into the
review diff. Reconcile the catalog entries without dropping either set when
updating the parent stack to main; rerun the catalog gate on the combined tree.

Fetched and inspected mobile branches and open PRs. Existing payment contract
#79 at `9c86c459081b32b47579b9c95c8514029225ad75` depends on the canonical ticket
flow #78 and RSVP #64; generated-only #76 remains superseded. Social contract
#80 (`52093b3b48252ed9f53a0c535945a212b8b1af9a`) is separately dependent on root
#367, not a query-report implementation. Its generated-file overlap must be
regenerated from the combined reviewed OpenAPI after those dependencies merge.

The existing mobile payment lineage is not an ancestor/descendant of latest
mobile `main` (`d4a1b17c733f57f912d36e7e396f6869cfd18c8a`); that divergence includes
pending RSVP/payment work versus merged onboarding/merch work. This contract-only
continuation preserves #79/#78, not a destructive switch of the root gitlink to
bare mobile main. It does not claim that mobile main was reconciled. Created
`codex/payment-query-observability-contract-20260915` from #79. The generated
change adds 127 lines with no deletions or native runtime changes. Review the
mobile merge topology before integrating this stack; do not overwrite #80's
unmerged social contract with a payment-only schema snapshot.
Mobile commit `d84fc3196202f2ac2468f78816b3c98fb4a22144` was pushed and verified
using `git ls-remote`. Draft [mobile PR #81](https://github.com/diegueins680/TDF-mobile/pull/81)
was created and its exact head, base #79 branch, and draft state verified at
15:03 UTC. No mobile checks were reported at that inspection.

GitHub CLI initially hit a sandbox connection error; an approved outside-sandbox
read succeeded. Mobile fetch likewise succeeded outside the sandbox after DNS
was blocked inside. No secret values were read or printed.

## Staging qualification remains blocked

Read-only inspector from #344 worktree/head
`0081b6b03bd58716090c57d936f31a364e944522` returned exit 1 at
**2026-09-15T14:22:31.992Z**:

- `tdf-hq-studio-audit-staging.fly.dev`: HTTP 200, health/database OK.
- `tdf-studio-audit-staging-web.fly.dev`: HTTP 200, health OK.
- All six hosting status/config/secret-name checks:
  `hosting_authentication_unavailable`.
- `sourceCommit=null`, `providerQualified=false`, no alternate hosting configured.

The report artifact is metadata only. No staging deployment or provider test was
attempted. Restore approved app-scoped `FLY_STAGING_API_TOKEN` /
`FLY_STAGING_WEB_TOKEN` access or authorized local Fly login, then recheck exact
deployed SHA and provider sandbox readiness. Health alone is not qualification.

## Implementation and security

The additive strict-admin GET report defaults to sandbox, bounds page size and
offset, performs read-only SQL with per-statement timeouts, and does not reserve
budgets, claim jobs, send provider HTTP or change any payment. It reports schema
availability and the database flag separately from operational rows. Errors and
unknown diagnostics are redacted. Successful report and database-error responses carry `no-store`;
pre-report authorization/filter errors contain no operational rows.

The existing admin page now includes a responsive Spanish/English panel with
explicit environment/status filters, pagination, loading/error/missing-schema
states and freshness timestamps. It hides stale or mismatched-environment rows.
Original operation outcome is distinct from job completion. No manual retry,
reset, charge, refund, void, seller payout or provider-activation control was added.
The shared OpenAPI generated types cover both web and mobile; no new wire state
was inserted into existing checkout responses.

The first catalog audit caught new fingerprint `4080f92d3868198965e6`: the
14-token `cpqLastOutcome` redaction enum. Reviewed against the worker/store
emitters and exact Haskell projection, then classified as a fixed technical
security output allowlist. Arbitrary stored text maps to `unrecognized`.
No audit gate or existing review decision was disabled/removed. No migration
manifest or SQL checksum changed.

After tracking the new UI files, the audit identified one additional fingerprint,
`072c90f7e568f32fe021`: the JSX options combining sandbox/production with the five
recovery-job states. Reviewed against `validateProviderQueryFilters`, the recovery
table's CHECK constraint and OpenAPI; classified as fixed technical protocol
filters, not editable business catalogs. An earlier passing scan before those
files were tracked is not used as the final audit evidence.

## Executed checks

Environment: macOS, Node 24.8.0 locally, Stack lts-24.42/GHC 9.10.3, PostgreSQL
16.10 native private Unix socket, Jest/jsdom. CI uses its own pinned environment.
All provider credentials, responses and database rows in tests are synthetic.

| Command | Source / observed UTC time | Observed outcome |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 22` | Baseline #371, before edits; observed 14:23:45 | 119 examples, zero failures; 16.3428 s; exit 0. |
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/pages/CommerceProviderEventsPage.test.tsx` | Baseline #371; observed 14:23:45 | Two tests passed; 11.951 s; exit 0. |
| `cd tdf-mobile && npm run typecheck` | Baseline #79 before client generation; observed by 14:31:55 | Exit 2: missing Expo/native dependencies and unresolved `expo/tsconfig.base`, not a successful mobile baseline. |
| `cd tdf-mobile && npm ci --no-audit --no-fund` | Locked #79 manifest; completed before 14:37:29 | Exit 0; 1,405 packages installed. Existing peer/deprecation warnings retained. No package/lock change, deployment or audit result claimed. |
| `cd tdf-mobile && npm run typecheck` | Same baseline after dependency install, before generation; observed 14:39:45 | Exit 0. |
| `npm run generate:api` | First attempt before mobile install | Web generated; mobile was explicitly skipped by readiness check. Not counted as complete client generation. |
| `npm run generate:api && cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts` | Final schema including no-store header; observed 14:39:45 | Exit 0. Both clients generated with openapi-typescript 7.10.1 and byte-identical. |
| `cd tdf-mobile && npm run typecheck` | Updated generated contract; observed 14:41:44 | Exit 0. No native device/Expo execution claimed. |
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/components/payments/ProviderQueryRecoveryPanel.test.tsx src/api/commerceOperations.test.ts src/pages/CommerceProviderEventsPage.test.tsx` | New panel/API tests; observed 14:39:45 | Three suites, 11 tests passed; 18.691 s; exit 0. |
| `cd tdf-hq-ui && npm run typecheck` | Updated web source; observed 14:39:45 | Exit 0. |
| `node --test scripts/__tests__/payment-query-report-contract.test.mjs` | After CJS import correction; observed 14:44:31 | Three tests passed; 1799.377027 ms; exit 0. First attempt failed on a named import from the installed CommonJS YAML package; no application assertion ran on that attempt. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-query-report-catalog-audit.json` | Initial report 14:41:22.004 | Exit 1; one new unreviewed diagnostic enum, zero stale decisions. Follow-up result must be recorded separately. |

Final-source checks (all times UTC on 2026-09-15):

| Command | Source / observed time | Observed outcome |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tee /private/tmp/tdf-query-report-focused-backend.log \| tail -n 22` | Intermediate build started during edits; observed 14:48:01 | 123 examples, zero failures; 15.8481 s; exit 0. Supplementary only; final full/DB runs below verify the completed source. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tee /private/tmp/tdf-query-report-full-backend.log \| tail -n 18` | Final backend source; log inspected 15:01:03 | 2,596 examples, zero failures; 34.6076 s; suite reported passed. Shell exit output was not retained, so a separate final run follows. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tee /private/tmp/tdf-query-report-full-backend-final.log \| tail -n 18` | `0ffe9d6`; observed 15:03:09 | 2,596 examples, zero failures; 24.4156 s; exit 0. No credentialed provider or database-backed suite is implied by this default run. |
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/components/payments/ProviderQueryRecoveryPanel.test.tsx src/api/commerceOperations.test.ts src/pages/CommerceProviderEventsPage.test.tsx src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | `9fddf58`; observed 14:52:32 | Five suites, 24 tests passed; 64.418 s; exit 0. Only subsequent API-test change was adding `await` below. |
| Scoped ESLint command below | First run observed 14:53:25 | Exit 1, two `no-floating-promises` violations in new API tests. Corrected with async/await in `0ffe9d6`, not a relaxed rule. An earlier incorrect executable path exited 127 before lint ran. |
| Scoped ESLint command below, then `npm test -- --runTestsByPath src/api/commerceOperations.test.ts` | `0ffe9d6`; re-run observed 15:02 UTC | Exit 0, zero lint warnings; two API tests passed; 3.678 s. |
| `npm run quality:repo` (captured in `/private/tmp/tdf-query-report-repo-quality.log`) | `9fddf58`; observed 14:54:00 | Exit 0. Nine Node groups total 157 tests, zero failures; formal audit passed and generated-artifact diff guard passed. Existing warnings retained. |
| PostgreSQL harness command below | `0ffe9d6`; observed 14:56:56 | 133 examples, zero failures; 19.0567 s; complete harness exit 0, including migration repetition and rollback assertions. |
| `git diff --check` | Runtime commits plus handoff edits; observed 15:02:39 | Exit 0. |
| `node --test scripts/__tests__/catalog-list-audit.test.mjs scripts/__tests__/payment-query-report-contract.test.mjs` | Final catalog review; observed 15:05:23 | Five tests, zero failures; 2726.822958 ms; exit 0. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-query-report-catalog-tracked.json` | All source files tracked, before second review | Exit 1, one unreviewed UI filter list, zero stale decisions. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-query-report-catalog-reviewed.json` | Final report generated 15:04:36.948; completion observed 15:05:23 | Exit 0; 1,422 files, 1,128 candidates, zero unreviewed and zero stale decisions. |

An earlier combined Node invocation reported 58 passing tests at 14:48:01
(1651.629454 ms, exit 0), but its full argument list was not retained in the
handoff. It is supplementary, not the reproducible final gate evidence above.
The early successful catalog output `/tmp/tdf-query-report-catalog-final.json`
preceded tracking the new UI files and is likewise not the final gate result.
Setup commands below reproduce the verified private-cluster parameters; original
flag ordering was not retained. No result is claimed for an unexecuted provider,
staging or native-device test.

Scoped ESLint (cwd `tdf-hq-ui`):

```sh
../node_modules/.bin/eslint src/components/payments/ProviderQueryRecoveryPanel.tsx src/components/payments/ProviderQueryRecoveryPanel.test.tsx src/api/commerceOperations.ts src/api/commerceOperations.test.ts src/pages/CommerceProviderEventsPage.tsx src/pages/CommerceProviderEventsPage.test.tsx --max-warnings=0
```

### Isolated PostgreSQL evidence and cleanup

Created a task-owned private temporary cluster at
`/private/tmp/tdf-payment-query-report-pg.7OnkTZ`, initialized with PostgreSQL
16.10, UTF-8/C locale, and local trust authentication under a mode-0700 parent.
Started only its Unix socket with `listen_addresses=''`; no TCP listener or
existing development/production database was used. Commands, with the exact
task-specific paths, were:

```sh
/usr/local/opt/postgresql@16/bin/initdb -D /private/tmp/tdf-payment-query-report-pg.7OnkTZ/data -U postgres --auth=trust --encoding=UTF8 --locale=C
/usr/local/opt/postgresql@16/bin/pg_ctl -D /private/tmp/tdf-payment-query-report-pg.7OnkTZ/data -l /private/tmp/tdf-payment-query-report-pg.7OnkTZ/postgres.log -o "-k /private/tmp/tdf-payment-query-report-pg.7OnkTZ -c listen_addresses=''" -w start
/usr/local/opt/postgresql@16/bin/createdb -h /private/tmp/tdf-payment-query-report-pg.7OnkTZ -U postgres tdf_provider_retry_test
```

Initialization/start/database creation exited 0, observed by 14:37:29. The
unchanged harness requires an empty schema and the exact test database name:

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-query-report-pg.7OnkTZ' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-query-report-postgres.log | tail -n 155
```

Executed with approved local socket access. Covers missing-versus-empty schema,
SQL-error redaction, strict role/filter checks, environment isolation, pagination,
read-only evidence, bounded table-lock timeout, arbitrary diagnostic redaction,
completed-query versus confirmed-no-charge operation, and existing provider
concurrency/financial regressions. It reapplies the parent recovery migration,
validates empty rollback preserving an operator flag, reapplies again, then
checks populated rollback refusal and retained history. No new SQL migration
was introduced by this PR.

Stopped only this cluster with
`/usr/local/opt/postgresql@16/bin/pg_ctl -D /private/tmp/tdf-payment-query-report-pg.7OnkTZ/data -m fast -w stop` after the
suite. Exact-path `pg_ctl status` returned `no server running` (expected exit 3)
at 15:01 UTC. The stopped cluster and logs are retained for inspection, not
deleted. This run did not use or restart Docker; the earlier #371 Docker
cleanup limitation remains recorded in that increment's handoff.

## Rollout, remaining work and review order

Deploy backend before UI only in an authorized environment. An old API or missing
schema must produce an unavailable warning, not a verified empty queue. Compare
original operation status with the job state; never treat a completed query as a
capture. The database flag shown here is only one activation prerequisite.
No migration/backfill is added. Roll back the UI/endpoint without deleting or
resetting historical jobs. See [operator runbooks](operator-runbooks.md).

Root depends on #371 after the existing canonical/core/provider/reconciliation
stack; mobile contract depends on #79 after #78/#64. Keep both draft/unmerged.
The current mobile/main and #80 schema overlaps require coordinated regeneration
when the reviewed lineages are merged. Mobile push/PR evidence is recorded above;
the root draft PR records its verified head and final catalog result separately.

Still unexecuted: credentialed provider sandbox/E2E, authenticated staging report,
production-sized read-load testing, complete production-schema rehearsal, browser
screenshots and native device tests. Safe manual dead-letter recovery, unknown
resource reconciliation, audit-detail navigation, late-paid fulfillment, remaining
adapters and business flows, and compliant marketplace settlements/payouts remain
unfinished. Merchant onboarding/contracts, secrets, webhook registration and
Ecuadorian legal/accounting/PCI review remain explicit activation gates.
