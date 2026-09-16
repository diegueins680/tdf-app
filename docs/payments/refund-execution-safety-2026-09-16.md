# Refund execution safety — 2026-09-16 UTC

Continuation after [#393](https://github.com/diegueins680/tdf-app/pull/393).
Design, threat/state model, compatibility and official references:
[ADR 0128](../adr/0128-refund-execution-fence.md). This is not completion or
activation of the entire payment platform.

## Access and baseline

Started from clean isolated worktree `tdf-app-payment-checkout`, parent
`ed064a7549f93bdd9239c5781706400c6fad5aa7`. The original dirty worktree is untouched.
`npm run ai:doctor` returned 16 OK, two missing-daily-memory warnings, zero errors.
Default main remains `3763a407146874a09095e26303a999f5a348afd1`, already an ancestor.
Fetched remote history and reviewed open root/mobile PR metadata and issues.
Restricted GitHub issue/mobile/CI reads failed to connect; approved retries
succeeded. No open issue or PR contained this refund execution repair.

#331 advanced to `79715507b5ddf6368145ff6f75244c5f0398a472`. Reviewed its contact
checkout, direct-create capability qualification and environment-separated
financial-summary changes. Its API/UI contract addition is paired with mobile
[#83](https://github.com/diegueins680/tdf-mobile/pull/83),
`80aed785d4b1783d08c209a10d411379f1b8bdb3`. They do not modify RefundStore or the
service refund handler. They are **not integrated or retested in this branch**;
combine them with the later payment API/mobile stack in a separate integration
increment before declaring the entire stack current. Do not replace the #82
submodule with #83 and lose later generated recovery/evidence contracts.
Mobile remains clean at #82 `33720ef45b0565005c4b54b0e0c106cc93831613`.

At 04:42 UTC, mobile #83 advanced again to
`337d46f25a8b7c7c688aa3657ffb599ecb4c47c7`. Its inspected complete four-line diff
adds both `cpiEnvironment` and `cacEnvironment` in the generated contract. It
still contains no refund-execution implementation. The first path-filtered
`gh pr diff` command was rejected by the CLI; the corrected full PR diff succeeded.
The moving upstream work remains deliberately outside the frozen refund test tree.

At 2026-09-16 04:14 UTC, #393 CI on the exact parent head had successful build,
catalog, repository, UI, persona, API-contract, production-migration and Cloudflare
checks; backend-quality was still in progress. Mobile/API-runtime/migration
checks marked skipped are not evidence of those suites executing.

The clean-parent baseline command, run from `tdf-hq`, passed with 13 examples,
zero failures, exit 0, 0.0474 seconds; log completed 2026-09-16T04:00:54.981Z:

```sh
stack test --fast --rerun-tests --test-arguments='--match=refund'
```

Log: `/private/tmp/tdf-payment-refund-baseline-20260916.log`.

## Test boundaries

Local macOS, Node 24.8.0, Stack lts-24.42/GHC 9.10.3, PostgreSQL 16.10. Synthetic reviewer IDs,
minor-unit amounts and provider references only. Tests exercise the real store,
locks, migrations and ledger constraints. They do not exercise PayPal HTTP,
credentials, merchant approval, TLS, refunds in a real sandbox, hosted workers,
mobile screens or a deployed staging application. Web verification is separately
identified below as mocked DOM tests, not browser/provider end-to-end execution.

The first disposable database uses Unix sockets only, no TCP listener:
`/private/tmp/tdf-payment-refund-pg.ybiv1O`. Created with `mktemp -d`; initialized
using PostgreSQL 16 `initdb -U postgres --auth=trust --no-locale -E UTF8`, and
started with `pg_ctl -o "-k /private/tmp/tdf-payment-refund-pg.ybiv1O -h ''"`.
Database name: `tdf_provider_retry_test`. Trust authentication applies only to
this synthetic disposable cluster, not staging or production.

The initial restricted harness exited 1 after its connection check; **no tests
executed**, and it performed no schema writes. Log:
`/private/tmp/tdf-payment-refund-red-db-20260916.log`. The approved retry uses the
same empty owned database and retains separate evidence. The harness checks
database identity and emptiness before applying any migration.

## Executed regression evidence

Regression-only commit: `e9ac411f0d2763eae3ffdef9c3c074fcb6ecc23c`.
Fixed runtime/test source: `5b80af6b4d3ba1367f49d7d464f720bc19d278fb`.
Local log completion timestamps below are filesystem last-write times; exit codes
are independently observed from the process tools.

The red PostgreSQL command completed at 2026-09-16T04:15:04.646Z with **223
examples, six failures**, exit 1, 23.0957 seconds, seed `902065472`:

```sh
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-refund-pg.ybiv1O' sh scripts/test-provider-retry-runtime.sh
```

Log: `/private/tmp/tdf-payment-refund-red-db-approved-20260916.log`.
Eight simultaneous claims returned eight execution permits; ordinary replays
and separately approved replays also acquired another permit. Ambiguous outcomes
became `failed`; legacy failed rows were reissued; verified completion after an
uncertain outcome was rejected. The existing 215 database examples passed. The
harness stopped at the expected red suite; its final nonempty rollback refusal
check did not execute on this run. The earlier restricted connection-only run
completed at 2026-09-16T04:08:05.149Z, exit 1, with no tests or schema writes.

The red pure command completed at 2026-09-16T04:15:58.469Z with **three examples,
two failures**, exit 1, 0.0168 seconds, seed `180116136`:

```sh
stack test --fast --rerun-tests --test-arguments='--match=refund-safety'
```

Log: `/private/tmp/tdf-payment-refund-red-unit-20260916.log`. The deterministic
Int64 overflow case wrongly returned success. A whole-range QuickCheck oracle
found an independent counterexample after 66 cases. Small normal-balance tests
had not covered this behavior.

The fixed tests also cover immutable pending references, pre-execution cancellation,
failed line reservations while another line remains available, and concurrent
partial completions. They retain the original red assertions; fixture factoring
adds two-line coverage. No quality threshold or financial constraint was weakened.

### Backend and database verification

At fixed backend source `5b80af6b4d3ba1367f49d7d464f720bc19d278fb`:

| Command / scope | UTC completion | Result | Local log |
|---|---|---|---|
| Fresh PostgreSQL harness below | 04:21:44.335 | 227 examples, zero failures; 26.2938 seconds; exit 0 | `/private/tmp/tdf-payment-refund-green-db-20260916.log` |
| `stack test --fast --rerun-tests --test-arguments='--match=refund-safety --seed=180116136'` | 04:21:53.413 | Three examples, zero failures; 100 QuickCheck cases; 0.0017 seconds; exit 0 | `/private/tmp/tdf-payment-refund-green-unit-20260916.log` |
| `stack test --fast --rerun-tests` | 04:22:27.614 | 2,630 examples, zero failures; 27.0723 seconds; exit 0 | `/private/tmp/tdf-payment-refund-full-20260916.log` |
| `npm run quality:repo` | 04:20:19.629 | 232 tests, zero failures; exit 0; formal audit zero critical/errors | `/private/tmp/tdf-payment-refund-quality-20260916.log` |

Times are on 2026-09-16 UTC. Stack commands run in `tdf-hq`; repository/harness
commands run from the worktree root. The three Stack/harness commands were
sequenced with `set -e -o pipefail`; the observed combined exit was zero. Database
configuration applied only to the harness, not the subsequent full backend suite.

```sh
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-refund-green-pg.unlrWO' sh scripts/test-provider-retry-runtime.sh
```

The fresh green cluster `/private/tmp/tdf-payment-refund-green-pg.unlrWO` used the
same initialization settings and no TCP listener. The unchanged harness applied
the real migration chain; verified repeated query-recovery migration, empty
rollback with operator-flag preservation, and reapplication; ran 227 store tests;
then verified that rollback refuses used history and keeps it readable. No new
migration or backfill is introduced. This is the bounded provider-runtime schema,
not a new full production-schema rehearsal.

Both owned clusters were stopped using PostgreSQL 16 `pg_ctl -D <exact cluster>/data
-m fast stop`, exit 0. Independent status checks at 04:23 UTC reported no server
running. Synthetic data/logs are retained locally; nothing was deleted. No other
process or database was stopped.

### Administrative interface alignment

Post-backend inspection found that merch administration still exposed cancel for
historical `failed` refunds and inferred no financial movement from a disabled
execution flag. These were existing UI mismatches, repaired in this increment.
No API/DTO/generated-client change is needed. The mobile submodule is unchanged.

Baseline `npm test -- --runInBand --runTestsByPath src/pages/MerchAdminPage.test.tsx`
in `tdf-hq-ui` passed three tests, exit 0, 10.588 seconds, completed
2026-09-16T04:24:28.260Z. Log:
`/private/tmp/tdf-payment-refund-web-baseline-20260916.log`.

Regression-only commit `bdfafd96486ce04bbe6190938fc6d8f5f8341f6b` ran
`npm test --prefix tdf-hq-ui -- --runInBand --runTestsByPath
src/pages/MerchAdminPage.test.tsx` from the root: nine tests, four failures,
exit 1, 8.610 seconds. Red output was observed in the execution tool transcript,
not saved as a separate log. Both English/Spanish held-state cases failed;
the rendered legacy failed row still contained the cancellation control and
the unsupported no-money-moved claim. Pre-execution controls remained present.
Execution was observed between 04:24 and 04:26 UTC; no more precise finish
timestamp was retained for this unsaved red run.

The UI fix, `28f0652a503dcc6e2c602633e2f3b8f327e6ee01`, removes those controls for
held refunds, explains reconciliation, and uses evidence-based copy for unavailable
execution. Backend sources are unchanged from `5b80af6b4`. Added DOM accessibility
checks for both locales. The same targeted command passed nine tests, exit 0,
11.591 seconds, completed 2026-09-16T04:26:16.896Z. Log:
`/private/tmp/tdf-payment-refund-web-green-20260916.log`. These are mocked React DOM
tests, including serious/critical axe checks, not browser/device screenshots.

At the final runtime/UI source, `npm run quality:repo` passed again with 232 tests,
zero failures, exit 0, completed 2026-09-16T04:28:20.382Z. Formal audit: 9,765
findings, zero critical/errors, 378 advisory warnings and 9,387 info. The warnings
were not suppressed or reclassified. Log:
`/private/tmp/tdf-payment-refund-final-repo-quality-20260916.log`.

Both final inventory commands passed with exit 0, 1,433 scanned files and 1,135
reviewed candidates. The JSON generated timestamp is 2026-09-16T04:28:16.683Z.
The decision file is unchanged; no review entries or gate exceptions were added:

```sh
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output docs/catalog-persistence/reports/static-list-inventory.json
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --format csv --output docs/catalog-persistence/reports/list-consumer-matrix.csv
```

The same pair also passed on the backend-only intermediate tree at 04:22 UTC;
the later runs replace those generated reports after the UI repair. Relative
documentation links, `git diff --check` and byte equality of existing web/mobile
generated contracts were checked. API contracts and mobile source did not change.

### Full web gate: retained first-run failure

`npm run quality:ui` ran from the root at unchanged final runtime/UI source.
ESLint (zero-warning threshold) and TypeScript passed. The full Jest stage exited
1 with **216 suites: 215 passed, one failed; 2,055 tests: 2,052 passed, three
failed**, 484.766 seconds. Log completed 2026-09-16T04:39:23.328Z:
`/private/tmp/tdf-payment-refund-ui-quality-20260916.log`. The build stage was not
executed by this failed composite command.

The failures are in `src/__tests__/MarketplacePage.test.tsx`: the inclusive rental
date test exceeded the existing five-second timeout, followed by saved-buyer
restoration and contact-checkout review assertions. The component, marketplace
test file and Jest configuration are byte-unchanged from parent #393. The test
output also reports unwrapped React `act` updates. This is evidence of a failure,
not yet proof of its cause; no timeout, assertion or quality gate was weakened.
The new refund-admin suite passed during the full run.

An unchanged isolated replay,
`npm test -- --runInBand --runTestsByPath src/__tests__/MarketplacePage.test.tsx`
from `tdf-hq-ui`, passed all ten cases, exit 0, 8.911 seconds, completed
2026-09-16T04:40:45.313Z. Log:
`/private/tmp/tdf-payment-refund-marketplace-replay-20260916.log`. The rental case
took 813 ms; the existing React `act` warnings remained. This supports an
intermittent timing/isolation risk, not a proved cause or a fix for that risk.

One full unchanged rerun used `npm test -- --runInBand`, with a subsequent
`npm run build` guarded by `set -e -o pipefail`. During that rerun, the unchanged
`AdminUsersPage` suite hit two five-second timeouts and subsequent assertions;
it had passed in the first full run. At 04:46 UTC, `uptime` reported load averages
108.44/54.83/40.17; the approved `sysctl -n hw.ncpu` check reported 16 CPUs.
At 04:47, load remained 50.75/51.08/40.30. Resource contention is a plausible
contributor, not a proved sole cause. No other user's processes were inspected
or stopped, and no timeouts or assertions were weakened.

After independent CI verification below succeeded, the redundant local rerun
was intentionally stopped with `kill -TERM 68164`. Its PID, 23:41:12 local start
time and exact `tdf-app-payment-checkout/tdf-hq-ui` working directory were checked
first. The owning shell returned **143** at 04:52 UTC. This is an interrupted
run, not a full passing run or a complete failed-suite count. Partial log:
`/private/tmp/tdf-payment-refund-web-full-replay-20260916.log`, last write
2026-09-16T04:52:34.644Z. The chained local build was not executed. No third
full-suite attempt was made to seek a green result; local timing/isolation
reliability remains an explicit follow-up issue.

### Independent CI verification

The [UI quality job](https://github.com/diegueins680/tdf-app/actions/runs/35055962490/job/104666174473)
was verified through GitHub metadata and filtered test/build logs, not inferred
from local targeted tests. PR head:
`f19fda5922fafe1e1290ec316a37c7c7a30da98b`. Actual default PR merge checkout,
reported by `git log -1 --format=%H` in CI:
`1db6adcb9d1ccd76e21e957d7e56bcb7c158c03d`.
GitHub Git API verified both commits have tree
`19a060001c748824024d5069ed5f6c53a3bcee58`; the merge parents are exact parent
#393 and `f19fda592`. Later delivery edits change documentation only.

Environment: GitHub Actions `ubuntu-latest`, configured Node 22, `npm ci`, then
the unchanged `npm run quality:ui` command from `.github/workflows/ci.yml`.
Job started 2026-09-16T04:32:21Z and completed **success** at 04:38:38Z:

- ESLint with `--max-warnings=0` and TypeScript passed.
- At 04:37:51.846Z, **216/216 suites and 2,055/2,055 tests passed**, zero failures,
  202.804 seconds. This includes the complete marketplace and refund admin suites.
- `tsc --noEmit -p tsconfig.app.json && vite build && node scripts/check-initial-bundle.mjs`
  passed; Vite finished at 04:38:35.159Z in 15.88 seconds. The bundle check reported
  five preloads and 314,884 gzip bytes of initial JavaScript at 04:38:35.395Z.

This is CI mock/unit/DOM/build evidence, not a provider sandbox, deployed staging,
refund-specific browser run or native-device test. The general persona-web-e2e
job also reported success in metadata; its individual scenarios were not inspected
as refund qualification. Local failures above are retained despite CI success.

## Delivery and review order

Opened and independently verified **draft
[#396](https://github.com/diegueins680/tdf-app/pull/396)**, head branch
`codex/payment-refund-execution-safety-20260916`, base
`codex/payment-worker-log-safety-20260915` (#393). Initial published head
`f19fda5922fafe1e1290ec316a37c7c7a30da98b` matched both PR metadata and `git ls-remote`.
No GitHub PR was merged and no manual deployment/provider operation was run.

Focused commits:

1. `e9ac411f0` — failing store/money regressions.
2. `5b80af6b4` — execution fence, reservation/money repair and extended DB tests.
3. `bdfafd964` — failing admin-control regressions.
4. `28f0652a5` — admin controls and evidence-based copy.
5. `f19fda592` — source-backed design, catalog reports and initial evidence.
6. Final documentation-only delivery — retained local failures/interruption and
   independent full CI verification; runtime/UI source remains unchanged.

Review/merge order remains the existing payment stack through #393, then #396.
Integrate the separately moving #331/mobile #83 work before complete-stack
qualification. Do not enable refund/provider flags on this draft. At the last
verified initial-head CI snapshot (04:48 UTC), build, catalog, repo/UI quality,
persona, API-contract and production-migration checks were successful;
backend-quality remained in progress. Skipped mobile/API-runtime/migration jobs
were not exercised by CI. Final documentation pushes may start new checks;
the earlier successful job is not a claim about those new runs.

## Remaining activation blockers

- The staging inspector last ran successfully as an inspection on 2026-09-16
  03:12 UTC but returned readiness failure: API/web health were reachable; all
  hosting metadata/config/secret-name checks lacked authentication. No new
  staging qualification is claimed here. Supply authorized staging-only Fly
  access using existing `FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` names or
  an authorized local login, then verify deployed SHA and sandbox bindings.
- Merchant sandbox credentials/contracts and authorized webhook registration
  remain unverified. Never enable a refund/provider flag on these local results.
- Implement and qualify a read-only original-refund lookup and an independently
  approved, audited resolution path for held and legacy-failed rows. Known-ID
  lookup is documented by PayPal; unknown-ID discovery still needs original
  capture/request correlation and provider support. Never infer absence from a
  timeout, HTTP 404, elapsed retention or a generic failure label.
- Canonical-intent refund projection, refund audit completeness, financial
  allocations outside service storefront, SRI issuance, guest/admin/native UX,
  and end-to-end refund HTTP/webhook recovery are not qualified by these tests.
  Existing PayPal refund/reversal webhook handling records reconciliation
  exceptions; it does not complete this missing refund-recovery workflow.
- Drain old handler processes for any future rollout. No schema changes are
  needed, but an older binary retains the unsafe execution/reservation behavior.
  Rollback requires disabling affected refund commands and retaining holds.
- Legal/accounting/PCI review and all broader market/production gates in the
  platform audit remain mandatory. No production action or real refund is run.
