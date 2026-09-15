# No-charge replay and cancellation compatibility: verification

Date: 2026-09-15 UTC / Ecuador. Scope: preserve terminal financial history during
PlaceToPay/PayPhone reconciliation and correctly classify new PayPhone status-2
observations. See [ADR 0121](../adr/0121-history-preserving-no-charge-reconciliation.md).
This is not completion or activation of the entire payment platform.

## Current repository and access

Resumed clean isolated `tdf-app-payment-checkout` at #363,
`139a45102c4b715cc8762b5653d0a2db061b167c`. Read repository/backend guidance,
reviewed payment remote branches, open PRs and #363 checks/comments/reviews.
There were no human reviews of #363; completed active checks passed, while
`backend-quality` remained in progress at the initial inspection around
06:25 UTC. Preview health/deployments are not payment staging qualification.
At 14:15:20 UTC the parent head was still `139a451...`, all its reported
non-skipped checks had completed successfully, and `main` remained at the
commit below. Parent checks are not CI evidence for this new branch.

`git fetch origin` succeeded. Default `main` had advanced to
`73edd77a36c8dcc73e5217303c62376ae684853b` through #333. Inspected its work and
the updated payment-core branch at `e39abf5a4ce2151e03dbd91051ba508295081efb`:
the reviewed baseline, migration metadata and catalog reconciliation are not a
competing cancellation/replay implementation. Other open new branches addressed
social queries, task APIs and onboarding/follow continuity.

Created `codex/payphone-cancellation-compatibility-20260915` from the updated
payment-core branch (already containing latest `main`), then locally merged the
#363 recovery stack. Integration commit
**`1d4acf7420ff532922a2c9623d1b854e3a76dbe6`** preserves both lineages. The only
content conflict was the migration-manifest catalog decision. Its new fingerprint
was reviewed against the combined manifest, retaining the explicit technical
allowlist and review evidence. No remote PR was merged by this work.

The resulting manifest has **107 entries**: source comparison confirmed all
102 latest-main entries and all 105 upstream payment-core entries remain in
order with unchanged metadata, and all 104 worker-branch entries remain present
with unchanged metadata. No SQL file, recorded checksum or introducing commit
was rewritten for this cancellation fix. The original dirty main worktree was
not modified; the existing mobile submodule reference from #363 was preserved.

The dependent PR diff includes already reviewed baseline changes from #333/
`e39abf5...`; those are integration ancestry, not newly authored social/UI/CI
work. Review the substantive payment repair separately at
`1d4acf742...` → `e0714bbbe5d4ac6ff77a5fa86471bd05778b88c9`, then the focused
web test `9ca3c9086d121743f57fc154ac70f878c83f4d0b` and posted-ledger negative
test `a902ca23a4fa4e16fa2e7f9c7c766cf173ea9ffa`. These three commits were recorded
at 06:46:52, 06:50:10 and 06:54:47 UTC respectively. Later changes are documentation.

## Staging and official-source refresh

Read-only `node scripts/inspect-payment-staging.mjs` from the #344 worktree,
head `0081b6b03bd58716090c57d936f31a364e944522`, returned exit 1 at
**2026-09-15T06:25:28.752Z**:

- API `https://tdf-hq-studio-audit-staging.fly.dev`: HTTP 200, health/database OK.
- Web `https://tdf-studio-audit-staging-web.fly.dev`: HTTP 200, health OK; not
  evidence of an API/payment test.
- All six hosting status/config/secret-name checks reported
  `hosting_authentication_unavailable`; no alternate hosting was configured.
- `sourceCommit=null`, `providerQualified=false`. No credential values were
  retrieved, new environment created, or staging deployment/payment attempted.

Restore approved app-scoped hosting access through `FLY_STAGING_API_TOKEN` /
`FLY_STAGING_WEB_TOKEN` or authorized local Fly login, then rerun inspection.
Genuine provider sandbox accounts, method/site configuration, contracts and
webhook registration must still be qualified. Authentication failure does not
establish its cause or prove a token expired. Presence of a secret is not proof
of a valid sandbox or authority to send a production transaction.

The official [PayPhone Sale contract](https://docs.payphone.app/api-sale), its
[notification reference](https://docs.payphone.app/notificacion-externa), and
[PostgreSQL locking documentation](https://www.postgresql.org/docs/16/explicit-locking.html)
were consulted on 2026-09-15. ADR 0121 records the inconsistent canceled/rejected
wording and the distinction between a terminal code and a specific decline or
cancellation reason. No fresh exhaustive market, pricing or legal review is
claimed by this targeted repair.

## Implemented behavior

Runtime/test commit **`e0714bbbe5d4ac6ff77a5fa86471bd05778b88c9`**:

- New authenticated PayPhone code-2 queries produce `AdapterCancelled`, still
  requiring exact amount/currency/resource/reference matching.
- The shared no-charge path locks and validates the bound original intent and
  attempt. Coherent terminal no-charge history is preserved unchanged, including
  legacy failed/canceled combinations and original error labels/timestamps.
- Replays do not append another payment failure audit or modify checkout state.
  A newer processing or paid attempt on that checkout is unaffected.
- A first verified cancellation records a canceled intent and attempt. The
  checkout remains retryable if active, rather than canceling the product order.
- Authorized/captured/refunded money, posted ledger evidence or incoherent
  terminal states are rejected, never silently repaired or cleared.
- Existing operation terminal-conflict checks, caller-owned savepoint rollback,
  job fencing and shared query budget remain intact. No new provider mutation
  or customer cancellation endpoint was added.

The public session still reports `confirmed_no_charge`; existing web and mobile
clients need no new wire enum. The focused return-page test confirms neutral
Spanish copy and removal of only the PayPhone recovery key, retaining another
provider's key. No screenshots or device tests were produced.

## Local test record

Environment: macOS, Stack `lts-24.42` / GHC 9.10.3, PostgreSQL 16.10 (Homebrew)
in a fresh private Unix-socket cluster for the successful database run, and
Jest/jsdom for web tests. The initial Docker PostgreSQL run was interrupted as
described below. Provider aliases,
credentials and responses are synthetic. Database tests execute real adapter
parsing, runtime qualification, store transitions, capture ledger and receipt
logic with injected transport, never a real provider request.

| Command | Source / observed UTC time, 2026-09-15 | Outcome |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Original #363 `139a451...`; completed before 06:27:39 | 118 examples, zero failures, 15.6543 seconds; exit 0. DB environment unset. |
| `node --test scripts/__tests__/production-release.test.mjs scripts/__tests__/catalog-list-audit.test.mjs` | Integrated baseline `1d4acf742...`; completed before 06:30:07 | 55 tests passed, zero failures, 1153.3561 ms; exit 0. No release executed. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payphone-cancellation-baseline-audit.json` | Integrated baseline; report 06:29:22.410 | Exit 0; 1,418 files, 1,126 candidates, zero unreviewed/stale decisions. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| rg --line-buffered -A20 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling\|Test suite'` | Integrated baseline `1d4acf742...`, before runtime/test edits; completion observed 06:37:04 | 118 examples, zero failures, 17.1706 seconds; exit 0. Compiled the upstream baseline with the payment stack. DB environment unset. |
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | Integrated baseline, before web test addition; completed before 06:34:08 | Two suites, 12 tests passed; 9.603 seconds; exit 0. No screenshots, browser or provider network. |
| `git diff --check` | Repeated through implementation/test commits | Exit 0. |

Final implementation results (runtime and test source through `a902ca23a...`):

| Command | Observed UTC time, 2026-09-15 | Outcome |
|---|---|---|
| `cd tdf-hq-ui && npm test -- --runTestsByPath src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | Completion observed by 13:42:34 | Two suites, **13 tests passed**, zero failures; 105.457 seconds; exit 0. Includes the new cancellation recovery test. |
| `cd tdf-hq-ui && npm run typecheck` | Separate rerun completed before 13:45:23 | Exit 0; TypeScript emitted no errors. The earlier combined typecheck/Jest command is not used as sole evidence of typecheck success. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payphone-cancellation-final-audit.json` | Report generated 13:51:14.982; exit observed before 14:12:01 | Exit 0; **1,418 files, 1,126 candidates**, zero unreviewed/stale decisions. |
| Native PostgreSQL harness command below | Completion observed 14:12:51 | **127 examples, zero failures**, 9.4149 seconds; exit 0 including post-suite nonempty rollback refusal and history readback. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tee /private/tmp/tdf-payphone-no-charge-full-backend.log \| tail -n 85` | Started 14:13:02; completion observed 14:14:14 | **2,592 examples, zero failures**, 28.1933 seconds; exit 0. Provider-retry DB environment unset; this does not subsume the separate PostgreSQL suite. |
| Node inline relative Markdown target check for ADR 0121 and this report | Final result observed 14:15:20 | Five relative targets resolved, including the ADR's return link; exit 0. Earlier four-target check also passed at 14:12:51. |

Successful database command, executed outside the filesystem/network sandbox
after the sandbox denied the local socket:

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-no-charge-pg.Cep9YB' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-payphone-no-charge-native-db-escalated.log | tail -n 155
```

This task-created cluster used `initdb -U postgres -A trust --no-locale
--encoding=UTF8`, a mode-0700 temporary parent directory, and
`pg_ctl ... -o "-k /private/tmp/tdf-payment-no-charge-pg.Cep9YB -c listen_addresses=''"`.
It did not expose a TCP listener. The database was newly created as
`tdf_provider_retry_test`; the unchanged harness additionally requires the exact
database name and an empty schema before any migration. It applies the reviewed
payment fixture/migrations, repeats the recovery migration, tests empty rollback
with operator-flag retention, reapplies it, runs the real SQL suite, then verifies
nonempty rollback refuses and history remains readable. No existing database was
cleared or redirected. `pg_ctl -D /private/tmp/tdf-payment-no-charge-pg.Cep9YB/data
-m fast -w stop` completed with exit 0 at 14:13:02. The stopped synthetic cluster
and local test log remain in the temporary directory; no production data is present.

The final full backend run also covers the new PayPhone code-2 parser test,
existing Datafast/PayPal behavior, and the latest integrated baseline. It does
not exercise credentials, a real provider TLS request or deployed fulfillment.

Interrupted/unsuccessful attempts are not counted as passing tests:

- Initial `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 |
  tail -n 135` compiled and started the Docker-backed suite but stalled in its
  first recovery example. Docker container-status requests both inside and
  outside the sandbox eventually returned HTTP 500, and a five-second connection
  probe to the task's local test port timed out. At about 13:45 UTC, only the
  identified test child PID 24732 was terminated with SIGTERM; Stack reported
  `ExitFailure (-15)`, harness exit 1. This is an interrupted infrastructure run,
  not a successful suite or a demonstrated application assertion failure.
- The harness attempted its owned-container cleanup, but suppresses Docker
  cleanup errors. Removal of `tdf-provider-retry-test-1485` is **unverified** while
  Docker is unavailable. Once Docker recovers, inspect that exact task-owned
  container and remove it if still present; do not restart Docker or remove other
  projects' containers merely for this test. No Docker daemon restart was done.
- The first native harness attempt inside the sandbox exited 1 with database
  connection unavailable. A direct socket probe showed `Operation not permitted`;
  the approved outside-sandbox probe then succeeded. The successful rerun above
  used the same fresh empty database, with no safety check disabled.

Unchanged compiler/linker warnings are not suppressed. The optional database
suite is separate from the default backend suite; focused test counts overlap
the full suite and must not be summed as independent coverage.

New database cases include concurrent duplicate cancellation; failed-history
compatibility; stale declined classification after new cancellation; original
no-charge replay after a cross-provider replacement is processing or genuinely
captured through the canonical ledger/receipt path; actual callback query and
fenced-worker execution; incoherent terminal history; and all three positive
financial counters, plus a posted-capture ledger even when deliberately corrupted
test intent counters are zero. No database guard or ledger row is disabled or
deleted by that negative test. Historical declined fixtures intentionally reconstruct the
old typed adapter result; they are not results from the corrected PayPhone parser.

## Rollout, rollback and remaining work

This repair needs no new schema or backfill. Preserve the complete integrated
migration manifest and previous immutable financial/provider references. Before
an authorized rollout, pause/drain old callback and scheduled consumers, retain
disabled provider/account/worker gates, deploy the reviewed stack, and perform
credentialed sandbox replay/concurrency checks before activation. Old binaries
do not have this no-charge replay guard; mixed writers can still exhibit the
defect. An image rollback alone does not repair previously changed records.
Keep evidence and use a reviewed forward repair if historical inconsistencies
are discovered. Never bulk relabel all failed PayPhone intents as canceled.

Operators should compare unresolved operations, original intent/attempt labels,
posted capture totals, receipts, and open reconciliation exceptions before and
after rollout. A no-charge result never proves an unrelated replacement payment
failed. Use the original attempt/provider reference for support investigations;
do not request tokens, hosted URLs, decrypted callbacks or raw card/provider
payloads in tickets. See the [operator runbooks](operator-runbooks.md).

Review after **#331 → #332 → #334 → #340 → #343 → #347 → #350 → #353 → #358 →
#363**, incorporating the latest reviewed baseline described above. Staging #344
remains independent after #343. No PR merge or production deployment is performed.

Still unexecuted: credentialed sandbox/E2E, staging payments, device/mobile tests,
full production-schema rehearsal of this integrated baseline, and live
refund/void/recurring/settlement/payout flows. Qualified merchant contracts,
credentials, exact method mappings, webhook registration and Ecuadorian legal,
accounting and PCI review remain activation dependencies. Unknown-resource
ambiguous creates, late-paid fulfillment policy, safe recovery administration,
remaining adapter/business-flow implementation and compliant seller payouts
are separate unfinished work. No mock result substitutes for those requirements.
