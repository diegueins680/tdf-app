# Payment stack integration and completion qualification — 2026-09-15

Continuation after draft [#385](https://github.com/diegueins680/tdf-app/pull/385).
This is not platform completion or activation. Policy, security and primary
references: [ADR 0126](../adr/0126-checkout-completion-capability-gates.md).

## Access and source review

The isolated worktree started clean at
`a64ea1b580faab59e547e3b307f74b0313d58866`; the original dirty worktree was not
modified. Refreshed default-branch history, all open root/mobile PR metadata,
payment heads, issues and #385 CI. The latest observed main was
`0784958ecb8916106ab21ae3121f50893b71aefc` (#376 Instagram lifecycle repair).
Payment core #331 advanced to `a8d31238aaef8e919551c74010d6f8f820ac231a`, including
completion-capability repair `be010ac276c2809e55678d83cea917a141087b3b` and main
integration. The remaining payment heads were unchanged. No duplicate provider
implementation was started. New social/event drafts remain separate; open
issues #128/#130 still concern onboarding and RSVP behavior.

`npm run ai:doctor` passed: 16 OK, two missing-daily-memory warnings, no errors;
GitHub authenticated and the worktree was clean. Root/mobile contracts remained
byte-identical and mobile stayed pinned at draft #82's
`33720ef45b0565005c4b54b0e0c106cc93831613`. No mobile files or contracts changed.
At the initial CI refresh, all reported #385 checks were successful or skipped
except backend-quality still running. That is parent CI evidence only.
At the final fetch on 2026-09-16 UTC, main and #331 still had those exact heads;
#385's backend-quality and the other named required checks had succeeded
(migration-tests was skipped). This is not CI evidence for the new branch.

## Reused work and merge integrity

Created `codex/payment-stack-integration-20260915` from #385 after the local
baseline. Integrated the complete updated #331 branch in `f9ed87ed5`, preserving
its source history. Its only conflict was the appended catalog-decision list:
retained the payment decisions and the upstream artwork decisions, checked JSON
syntax and unique IDs. No whole-side replacement or bulk decision builder was
used. Integrated latest main in `8466e1f6e` without conflicts, retaining #369
artwork, #370 read-only messaging checks and #376's later lifecycle repair.

The merged code retains the later exact-method API and original-intent replay
logic while applying #331's Datafast confirmation and PayPal completion gates.
Upstream login-test and production-migration-order regressions were preserved.
All three exact heads—#385, updated #331 and observed main—were verified as
ancestors using `git merge-base --is-ancestor`. These are **local integration
commits**, not merges of GitHub PRs or production releases.

Added six real-PostgreSQL integration cases for Datafast continuation without a
fictional capture capability, missing Datafast/PayPal completion evidence,
original-intent preservation, runtime credential withdrawal, environment isolation
and every marketplace requirement. Three additional cases exposed that bank,
DeUna and PayPhone wallet remained advertised without verified status lookup.
The first combined run returned 215 examples / three failures, all the new
query-backed route checks; the other 212 passed. The small shared-policy repair
now requires `server_verification` for these three methods. One pure regression
checks the resulting minimum requirements and idempotent additive normalization.

The generated catalog JSON/CSV inventory is refreshed against the combined tree;
its size reflects the previously separate payment/main consumers. Existing
review decisions and the fail-on-unreviewed/stale/duplicate gate are retained.
No new editable catalog authority, provider record or feature activation is added.

## Operator / rollout procedure

1. Review the core repair #331 and the full dependent payment stack through #385,
   then this integration branch. Its merge history includes observed main; do not
   replay old generated snapshots or drop the union of reviewed catalog entries.
   Mobile #82 remains the required unchanged gitlink; preserve separate mobile
   #80 social-contract additions during future lineage reconciliation.
2. Check the method's exact environment/account and completion-capability
   evidence. Cards/bank/DeUna/PayPhone require verified server lookup; PayPal
   requires verified capture. Preserve all additional caller and marketplace
   restrictions. Merely documented capabilities must not be marked verified to
   make a button appear.
3. Confirm runtime configuration agrees with the intended environment. A missing
   method is not an instruction to reveal credentials, bypass a gate or retry an
   existing ambiguous attempt. Resume only that original attempt and reconcile it.
4. Run credentialed sandbox completion, cancellation, timeout, replay, webhook and
   settlement checks before activation. Record real environment/account aliases,
   immutable deployed SHA and provider evidence separately from these mocks.
5. Rollback changes application behavior only, not data. Disable new affected
   checkout methods before removing these completion gates; retain original-
   operation recovery, holds, provider references and ledger/history records.

No new SQL migration, backfill, API shape, generated client, UI control or native
mobile screen. Existing migration dependencies are exercised by the DB harness.
No refund, payout, recurring mandate, live transaction or production deployment
is implemented or performed by this increment.

## Staging recheck and remaining blockers

From the unchanged #344 staging worktree at `0081b6b03bd58716090c57d936f31a364e944522`,
`node scripts/inspect-payment-staging.mjs` exited 1 at
**2026-09-15T20:25:56.923Z**. Approved unrestricted retry also exited 1 at
**2026-09-15T20:27:48.678Z**. Both public staging endpoints returned HTTP 200;
API health/database were OK, web health was OK. The web endpoint's absent DB
health field is not evidence of a database failure. All six hosting status,
configuration and secret-name checks reported `hosting_authentication_unavailable`.
`sourceCommit=null`, `providerQualified=false`; no alternative host configured.

No credential values were read/displayed; no deployment or provider call occurred.
The staging worktree's existing untracked sanitized inspector artifacts were
retained and not committed. Restore authorized `FLY_STAGING_API_TOKEN` /
`FLY_STAGING_WEB_TOKEN` access or local Fly login, then qualify deployed SHA,
sandbox account aliases/contracts/secret names and registered webhooks. Setup is
authorized by the user but cannot proceed through unavailable hosting authority.

Real provider tests, qualified staging, native/device QA, production-scale
performance, legal/accounting/PCI review, compliant seller settlements/payouts,
remaining recurring/refund/void/release flows, legacy late-approval handling and
full product fulfillment remain incomplete. No local pass is substituted for
any of those external or financial qualifications.

## Test and commit evidence

Environment: local macOS, Node 24.8.0, Stack lts-24.42/GHC 9.10.3, PostgreSQL
16.10. All provider data/configuration is synthetic. No provider HTTP is used by
the new database-backed availability tests. Dates below are UTC; the document
date is the local Ecuador work date. Log timestamps are last-write times, not
inferred deployment or provider-execution times.

### Executed checks

| UTC completion / evidence time | Source | Check | Actual result |
|---|---|---|---|
| 2026-09-15 19:35:37 | Parent `a64ea1b58` | Backend provider baseline | 123 examples, zero failures; exit 0 |
| 2026-09-15 19:35:31 | Parent `a64ea1b58` | Four web payment suites | 43 tests, zero failures; exit 0 |
| 2026-09-15 19:38:54 | Integrated web `8466e1f6e` | Seven payment/login/artwork suites | 59 tests, zero failures; exit 0 |
| 2026-09-15 19:40:01 | Integrated source before new DB cases | Repository quality baseline | 232 tests, zero failures; exit 0 |
| 2026-09-15 19:40:49 | Integrated catalog | Fail-on-unreviewed inventory | 1,430 files, 1,135 reviewed candidates; no unreviewed/stale decisions; exit 0 |
| 2026-09-15 20:27:10 | Unchanged integrated web | Production web build, including typecheck/preload budget | Exit 0; Vite 37.88 seconds; existing large-chunk advisory retained |
| 2026-09-15 20:29:52 | `bfdf06d6b` regression cases, before fix | Disposable PostgreSQL harness | 215 examples, three failures; exit 1. All three new query-backed capability cases failed; post-test populated rollback check was not reached |
| 2026-09-15 20:33:01 | `158318524` | Full default backend | 2,603 examples, one failure; exit 1. See retained failure below |
| 2026-09-15 20:35:26 | Frozen runtime `158318524`, generated inventory only | JSON/CSV catalog refresh | Exit 0; same 1,430 files / 1,135 candidates; no decisions weakened |
| 2026-09-16 02:40:54 | `158318524` | First post-fix fresh PostgreSQL test log | 215 examples, zero failures. Final harness exit was not retained in the resumed tool transcript; repeated below for definitive whole-harness evidence |
| 2026-09-16 02:41:26 | `158318524` | Focused onboarding recheck | Four examples, zero failures in log; no source modification |
| 2026-09-16 02:45:55 | `158318524` | Full backend, original failing seed | 2,603 examples, zero failures; exit 0; 23.3367 seconds |
| 2026-09-16 02:46:11 | `158318524` plus documentation/inventory | Repository quality repeat | 232 tests, zero failures; exit 0. Formal analysis: 9,763 findings, zero critical/errors, 377 warnings and 9,386 information findings; existing gates unchanged |
| 2026-09-16 02:46:51 | Unchanged mobile `33720ef45` | Provider-neutral mobile checkout unit suite | Three tests, zero failures; exit 0; not native/device or provider QA |
| 2026-09-16 02:47:30 | Frozen runtime/test source `158318524` | Definitive fresh PostgreSQL harness | 215 examples, zero failures; **whole harness exit 0**, including repeat migration, empty rollback/operator-seed retention, reapply, populated rollback refusal and readable history; 15.8327 seconds for Hspec |

Additional completed checks: mobile `npm --prefix tdf-mobile run typecheck`
exited 0 (observed 2026-09-15 20:29–20:30 UTC, no separate timestamped log);
`cmp` of web/mobile generated types and `git diff --check` exited 0. Two new
documents' relative links were checked. No screenshot or native manual QA was
produced. The first restricted DB harness attempt failed connection validation
before any write; the approved local-socket retry produced the red cases above.

The original unrelated failure was
`TDF.Server helpers / sessionServer / requires Party-bound, in-window
access-request evidence and stays idempotent`, at `test/TDF/ServerSpec.hs:5451`,
expected `False` but got `True`, seed `1116919930`. The fixture and implementation
use wall-clock time for the evidence window. Neither `ServerSpec.hs`, `Server.hs`
nor `ServerAuth.hs` was changed by this increment. Focused and full same-seed
reruns passed unchanged; the cause was **not reproduced or proven fixed**. Retain
this as test-instability risk, not a reason to weaken assertions or hide the first
failure. Seed reproduction alone does not control the wall clock.

### Exact commands and local logs

Run from the isolated worktree unless a directory is specified. Full local logs
are under `/private/tmp/tdf-payment-stack-integration-`; they are local evidence,
not uploaded CI artifacts. Suffixes are listed below. Commands used `set -o
pipefail` with `tee` where a log is listed, so observed exit codes include the
underlying check. No real provider credentials were loaded by these tests.

```sh
# cwd: tdf-hq; baseline-backend-20260915.log
stack test --fast --rerun-tests --test-arguments='--match=provider'

# baseline-web-20260915.log
npm --prefix tdf-hq-ui test -- --runTestsByPath \
  src/components/payments/ReconciliationEvidencePanel.test.tsx \
  src/components/payments/HostedProviderCheckout.test.tsx \
  src/api/paymentCapabilities.test.ts \
  src/__tests__/DatafastReturnPage.test.tsx

# web-20260915.log; the four paths above plus these three paths
npm --prefix tdf-hq-ui test -- --runTestsByPath \
  src/components/payments/ReconciliationEvidencePanel.test.tsx \
  src/components/payments/HostedProviderCheckout.test.tsx \
  src/api/paymentCapabilities.test.ts \
  src/__tests__/DatafastReturnPage.test.tsx \
  src/pages/LoginPage.test.tsx \
  src/features/releases/ReleaseArtwork.test.tsx \
  src/features/releases/resolveReleaseArtwork.test.ts

# web-build-20260915.log; repo-quality-20260915.log / quality-recheck-20260916.log
npm --prefix tdf-hq-ui run build
npm run quality:repo

# cwd: tdf-hq; full-backend-20260915.log (initial failure)
stack test --fast --rerun-tests
# session-recheck-20260916.log; full-recheck-20260916.log
stack test --fast --rerun-tests --test-arguments='--match=Party-bound'
stack test --fast --rerun-tests --test-arguments='--seed=1116919930'

# mobile-20260916.log
npm --prefix tdf-mobile test -- --runTestsByPath __tests__/providerNeutralCheckout.test.ts
npm --prefix tdf-mobile run typecheck
cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts
git diff --check

node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed --output docs/catalog-persistence/reports/static-list-inventory.json
node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed --format csv \
  --output docs/catalog-persistence/reports/list-consumer-matrix.csv

# db-proof-20260916.log; synthetic local-only database, no credentials
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-integration-proof-pg.dlQwqC' \
  sh scripts/test-provider-retry-runtime.sh
```

The first DB regression and first post-fix harness used the same command with
socket directories `/private/tmp/tdf-payment-integration-pg.X2mzLm` and
`/private/tmp/tdf-payment-integration-final-pg.w5HPMZ`, respectively; logs are
`db-verified-20260915.log` and `db-final-20260915.log`. The restricted failure log
is `db-20260915.log`. The definitive fresh cluster was created with `mktemp -d
/private/tmp/tdf-payment-integration-proof-pg.XXXXXX`, then PostgreSQL 16
`initdb -D <exact-directory>/data -U postgres --auth=trust --no-locale -E UTF8`,
`pg_ctl -D <exact-directory>/data -l <exact-directory>/server.log -o
"-k <exact-directory> -h ''" start`, and `createdb -h <exact-directory> -U postgres
tdf_provider_retry_test`. All three clusters used Unix sockets without TCP
listeners and were stopped with `pg_ctl -D <exact-directory>/data stop -m fast`.
Their disposable synthetic data and logs were retained; nothing was deleted.
At 2026-09-16 02:49 UTC, `pg_ctl -D <exact-directory>/data status` reported
`no server running` with expected exit 3 for each exact cluster directory.

### Commit and review order

This draft depends on #385, includes updated #331 and observed main, and changes
no mobile gitlink. Review order is the existing stack through #385, then this
integration increment. No PR was merged.

1. `f9ed87ed54beb2eb635fa3fe9ac1aa53b80d76e1`: integrate upstream core repair and
   preserve the reviewed catalog-decision union.
2. `8466e1f6e7b04947b1c3056759132e376f9fc7ff`: preserve current main history.
3. `3425905b0bc7ea270bb87e39bb9d702c7e1a82bd`: six durable completion tests.
4. `bfdf06d6bd15ec7ddb2c81fd8c07f94df1f02a7d`: three failing hosted-method cases.
5. `15831852417ea03e5d2db6204ffc463c3e4401e8`: shared completion-gate repair and
   pure normalization regression; the final runtime/test source tested above.
6. `2670b7d21`: regenerate JSON/CSV catalog inventories using the unchanged
   reviewed-decision gate.

The remaining commits contain generated catalog inventories and this evidence,
ADR and runbook documentation only; they do not modify the tested runtime.

Delivery verified at 2026-09-16 02:52 UTC: pushed
`codex/payment-stack-integration-20260915` and opened draft
[#389](https://github.com/diegueins680/tdf-app/pull/389), base
`codex/payment-reconciliation-evidence-view-20260915` (#385). Both `git ls-remote`
and GitHub PR metadata returned `7cb3876674e380c7efce9c69849d8090e1febdd1`, the
documentation commit after the tested source and catalog commit. PR metadata
confirmed OPEN and draft. CI was queued/in progress, not claimed passed. This
delivery note is a subsequent documentation-only commit. No provider transaction,
production deployment, PR merge or new mobile PR was performed.
