# Capture replay integrity — 2026-09-15

Continuation after draft [#374](https://github.com/diegueins680/tdf-app/pull/374),
not completion or activation of the payment platform. See
[ADR 0123](../adr/0123-idempotent-capture-evidence-and-binding.md).

## Access, dependencies and source

Started with a clean isolated `tdf-app-payment-checkout` worktree at
`c19c4f5d87abdb5a9b3e935e79af2238fdb99c43`. The original dirty worktree was not
changed. Fetched root/mobile refs, inspected open PRs, open issues and payment
branches. Root main remained `52b32e4c17b3b0485c66e666137f85d44cc962d4` (#369
artwork); its catalog-review overlap remains documented in the parent handoff.
This dependent branch preserves #374's reviewed lineage; it does not claim that
the entire pending stack has been merged into latest main.

New upstream payment branch updates were inspected: #332 at `536964de6` and
#334 at `70823741b` propagate the prior validated foundation and refresh catalog
reports, not capture-replay repairs. Commit `25416b43e` also adds migration-order
and login-test coverage; preserve those additions when merging the parent stack.
No existing payment implementation was replaced or duplicated. Open issues #128
and #130 concern onboarding/RSVP broadcasting, not this defect.

Mobile remains exactly draft [#81](https://github.com/diegueins680/TDF-mobile/pull/81)
at `d84fc3196202f2ac2468f78816b3c98fb4a22144`. Its payment ancestry still diverges
from mobile main. #80's refreshed social/onboarding generated contract at
`c1832c5` was inspected and is unrelated; combined generation remains a merge
dependency, not authorization to overwrite either schema. No mobile file, gitlink,
OpenAPI contract or web implementation changed in this increment. Mobile fetch
failed with sandbox DNS restrictions, then succeeded with approved network access.

Created `codex/payment-capture-replay-integrity-20260915` after #374.

| Commit | Scope |
|---|---|
| `4a88878d046b36d46361998739fc31fb5951a55a` | Reproduce timestamp and receipt-identity defects for all four online providers. |
| `03d563c18d7f0ce5339f0e7f0023d55c0c037dc6` | Shared capture/binding repair, expanded regression cases and real manual-evidence migration in the disposable harness. |
| `b2ab560b250fd11ac7c0f3aa98b6dd8f5307ab03` | Add the existing canonical bank-account/capability migration missing from the test harness. Final tested source; later changes are documentation only. |

At 15:29 UTC, #374 had no reviews, all reported non-backend checks passed, and
backend CI was still running. That parent evidence is not new-branch CI evidence.
The same backend-pending state was observed again at 15:48 UTC.

## Staging remains unqualified

`node scripts/inspect-payment-staging.mjs` from the #344 worktree returned exit 1,
report time **2026-09-15T15:18:48.933Z**. Both exact public Fly staging endpoints
returned HTTP 200; the API health/database checks succeeded. All six hosting
status/config/secret-name checks returned `hosting_authentication_unavailable`.
`sourceCommit=null`, `providerQualified=false`, no alternate hosting configured.

No secret values were read or printed, no hosting resource was created, and no
staging/provider transaction or deployment was attempted. Restore approved
`FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` access or authorized local Fly
login, then verify deployed SHA, sandbox aliases, merchant contracts and webhook
registration. Public health is not proof of payment readiness.

## Repair and compatibility

The shared capture function now acknowledges an already-paid capture only when
the successful attempt, full checkout payment, posted capture ledger and exact
receipt agree. Receipt identity includes amount/currency, receipt number, provider
adapter and external resource. Replays return `Right False` without rewriting
timestamps, refund/dispute state, receipts or financial history. Original voided
receipts remain historical evidence, not newly issued or valid fiscal receipts.
Incomplete or conflicting history fails closed for reconciliation; no automatic
backfill is performed.

Binding verifies and locks the checkout/attempt pair before writing. It can only
advance created → customer action/processing or customer action → processing.
Exact replays, processing regressions and terminal attempt observations do not
rewrite attempt/checkout state. Cross-checkout binding requests are rejected
before creating another resource or audit event. A new resource may still add
its immutable binding/audit without changing a terminal attempt.

Datafast, PayPal, PlaceToPay and PayPhone use this common boundary. The existing
independently approved bank-transfer path shares the same capture finalizer;
customer evidence alone remains insufficient. No new cash, crypto, marketplace,
refund, payout or subscription method is implemented or enabled.

New positive payments on expired/canceled checkouts remain rejected. A replay
after an existing capture is distinct from a first late payment: this patch does
not reacquire inventory, reopen fulfillment, post suspense accounting or execute
a refund. Product handlers still own their fulfillment side effects; no complete
provider-to-fulfillment sandbox claim is made.

No production migration, manifest edit, backfill or historical-reference rewrite.
The test harness adds the **existing** manual-evidence migration and a minimal
synthetic ORM party-ID dependency; it does not replace the actual commerce tables
or their guards. This is not a complete production-schema rehearsal.

## Verification record

Environment: local macOS, Node 24.8.0, Stack lts-24.42/GHC 9.10.3, PostgreSQL
16.10 over private Unix sockets. Every payment/account/reviewer/receipt fixture
is synthetic. No provider TLS, merchant account or real bank payment is involved.
Existing compiler/linker warnings and quality gates were retained.

| Command | Source / observed UTC time | Outcome |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tee /private/tmp/tdf-payment-next-baseline-20260915.log \| tail -n 18` | Clean parent #374; observed by 15:21:07 | 123 examples, zero failures, 15.8924 s, exit 0. |
| Full database harness below, first cluster | Regression source `4a88878d0`, before runtime repair; observed 15:31:01 | 141 examples, **8 expected regression failures**, 16.9870 s, exit 1. Existing 133 cases passed; later rollback assertions did not run because the suite failed. |
| Focused Stack command below | Repair before added manual test; observed 15:38 UTC | Compiled, but exit 1 due incorrect `--match` quoting (`unexpected argument`). No test assertion ran; not a passing test. |
| Catalog command below, initial report | Generated 15:38:40.089; observed 15:41:43 | Exit 0; 1,422 files / 1,128 candidates, zero unreviewed. Final tracked-source scan is recorded separately. |
| `sh -n scripts/test-provider-retry-runtime.sh` | `03d563c18`; observed after 15:41:43 | Exit 0. |
| `cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts` | Unchanged generated clients | Exit 0. Generation was not needed or rerun. |
| `git diff --check` | Each implementation stage | Exit 0. |
| Full database harness below, second cluster | `03d563c18`; observed 15:46:08 | 170 examples, one failure, 20.3522 s, exit 1. All 36 new online-provider cases and the existing 133 passed. Bank fixture lacked the real account/capability migration, so its route correctly failed closed. No rollback assertions ran after this failure. |
| `psql` application and bank-filtered Stack command below | `b2ab560b2`; observed 15:48:09 | Migration application exit 0; two examples, zero failures, 0.3808 s, Stack exit 0. Independent-review rejection and approved manual replay now verified. |
| Final full database harness below, third fresh cluster | `b2ab560b2`; observed 15:50:30 | **170 examples, zero failures**, 26.5395 s, complete harness exit 0. Includes repeated recovery migration, empty rollback preserving operator flag, populated rollback refusal and retained history readback. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tee /private/tmp/tdf-capture-replay-full-backend-20260915.log \| tail -n 18` | `b2ab560b2`; observed 15:49 UTC | **2,596 examples, zero failures**, 31.3536 s, exit 0. Default run excludes the separately executed DB cases. |
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | Unchanged web source; observed 15:45:11 | Two suites, **13 tests passed**, 50.557 s, exit 0. Mocked web APIs, not a browser/provider sandbox. |
| `set -o pipefail; npm run quality:repo 2>&1 \| tee /private/tmp/tdf-capture-replay-repo-quality-20260915.log \| tail -n 20` | Runtime source `03d563c18`; observed 15:46:08 | Exit 0; nine groups totaling **157 Node tests**, zero failures; formal audit and generated-artifact diff guard passed. Later source change is one harness prerequisite. |
| Final catalog command below | Report generated 15:47:33.144; completion observed 15:48:09 | Exit 0; 1,422 files / 1,128 candidates, **zero unreviewed / stale**. All scanned source matches final code; the last `.sh`-only prerequisite correction is outside the scanner's supported extensions. No catalog decisions or gate changes needed. |

All final checks above passed. The reproduced eight failures, command-quoting
failure and missing fixture prerequisite remain documented rather than hidden.
Read-only diagnosis initially selected a nonexistent `capability.enabled` column;
the corrected query returned zero synthetic bank capability rows, explaining the
fixture failure. No production data was read or changed by that diagnosis.

Exact database commands (cwd root):

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-pg.vcxlFW' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-capture-replay-red-20260915.log | tail -n 95
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-final-pg.WHwBmN' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-capture-replay-full-db-20260915.log | tail -n 100
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-verified-pg.Jk5UHN' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-capture-replay-verified-db-20260915.log | tail -n 75
```

The failed focused invocation (cwd `tdf-hq`; retained to explain the failure):

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-pg.vcxlFW' PGOPTIONS='-c statement_timeout=15000 -c lock_timeout=10000' stack test --fast --rerun-tests --test-arguments='--match="verified capture replay integrity" +RTS -N2 -RTS' 2>&1 | tee /private/tmp/tdf-capture-replay-focused-20260915.log | tail -n 70
```

Initial catalog command (cwd root):

```sh
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /private/tmp/tdf-capture-replay-catalog-20260915.json
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /private/tmp/tdf-capture-replay-catalog-final-20260915.json
```

Bank prerequisite diagnosis/verification applied the repository migration with
this command from root:

```sh
/usr/local/opt/postgresql@16/bin/psql 'postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-final-pg.WHwBmN' -X -q -v ON_ERROR_STOP=1 -f tdf-hq/sql/2026-09-11_manual_bank_provider_activation.sql
```

Then from `tdf-hq`:

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-capture-replay-final-pg.WHwBmN' PGOPTIONS='-c statement_timeout=15000 -c lock_timeout=10000' stack test --fast --rerun-tests --test-arguments='--match=bank +RTS -N2 -RTS' 2>&1 | tee /private/tmp/tdf-capture-replay-bank-20260915.log | tail -n 25
```

All three private clusters were created with `mktemp -d`, `initdb -U postgres
--auth=trust --encoding=UTF8 --locale=C`, `pg_ctl` socket directory pinned to the
respective mode-0700 parent and `listen_addresses=''`, then `createdb -U postgres
tdf_provider_retry_test` on that socket. Initialization/start/database creation
exited 0 (first cluster observed 15:24:04; second 15:38:41; third 15:49 UTC). The harness
validates the exact database name and empty schema before writes. No existing
development database or Docker daemon was modified. The first cluster was stopped
with exact-path `pg_ctl -D /private/tmp/tdf-capture-replay-pg.vcxlFW/data -m fast -w stop`
(exit 0, observed 15:49:07). Exact-path stop commands for
`/private/tmp/tdf-capture-replay-final-pg.WHwBmN/data` and
`/private/tmp/tdf-capture-replay-verified-pg.Jk5UHN/data` both exited 0 at 15:51:41.
`pg_ctl -D <exact data path> status` returned `no server running` (expected exit 3)
for all three at 15:52:26. Test data/logs are retained, not deleted. Both local
ADR/handoff links resolved and final `git diff --check` passed at that inspection.
GitHub confirmed default main still at `52b32e4c17b3b0485c66e666137f85d44cc962d4`.

## Rollout, remaining gates and review order

Review after #374, preserving the parent core/provider/UI/migration order and
mobile #81 pin. No additional native PR is needed for unchanged contracts.
Drain older verification/callback writers before any authorized rollout; rolling
back reintroduces binding regression and timestamp rewriting. Preserve incomplete
legacy evidence for explicit operator reconciliation rather than deleting or
manufacturing it.

Still unexecuted: credentialed provider sandbox/E2E, authenticated staging,
real refunds/chargebacks/settlements, complete production-schema rehearsal,
native device tests and screenshots. Late-payment financial quarantine and
fulfillment recovery, unknown-resource reconciliation, remaining business flows,
seller payouts and legal/accounting/PCI approvals remain outstanding. No
certification, production readiness or whole-platform completion is asserted.
