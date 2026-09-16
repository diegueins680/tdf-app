# Payment worker diagnostic safety — 2026-09-15

Continuation after [#389](https://github.com/diegueins680/tdf-app/pull/389), not
completion or activation of the payment platform. Design and primary references:
[ADR 0127](../adr/0127-payment-worker-diagnostic-boundaries.md).

## Access, baseline and dependencies

Started from clean isolated worktree `tdf-app-payment-checkout` at
`824e1e9aeaa99d399c31bc5755c4967c03cb17fc`. The original dirty worktree was not
changed. Fetched default/history/remote payment refs, reviewed open root/mobile
PR metadata and issues. Existing payment heads including #331 were unchanged;
no pending worker-diagnostic repair was found. New event/social work remains
separate. Mobile stays clean at #82 `33720ef45b0565005c4b54b0e0c106cc93831613`;
its generated contract and native screens are unchanged.

Main advanced to `3763a407146874a09095e26303a999f5a348afd1` (#355 social formal
audit). Created `codex/payment-worker-log-safety-20260915` after #389 and locally
integrated that complete history. Only the generated catalog JSON/CSV conflicted;
regenerate them against the combined tree using the existing decisions and
fail-on-unreviewed gate. No catalog authority or review decision changes are
authorized by this diagnostic repair. No GitHub PR is merged.

The first local merge validation ran before report generation completed, failed
on conflict markers/invalid JSON, and the shell incorrectly continued to a local
commit. After both generation commands completed with exit 0, JSON/marker/diff
checks passed and that **unpublished, agent-owned** merge commit was amended to
`da81213085db2a98dc0fce89bff18ee9280e1b84`. No conflicted report was pushed.
The generated inventory recorded 1,431 files and 1,135 reviewed candidates at
2026-09-16T03:10:38.954Z. Both exact parent #389 and observed main were verified
as ancestors. Subsequent commit `925b42932a05c980a5d3d46641d20f8ce63cc4cc`
contains the behavior-preserving iteration seams and failing regression cases.

Initial #389 checks were successful or skipped, except backend-quality still
running. This is parent CI evidence only. Restricted `npm run ai:doctor` returned
15 OK/three warnings/zero errors, including an apparent GitHub-authentication
warning; the approved unrestricted retry authenticated successfully and returned
16 OK/two missing-daily-memory warnings/zero errors. Polling remains disabled.

The clean-parent provider baseline command in `tdf-hq` passed with 126 examples,
zero failures and exit 0 (17.8928 seconds; log completed
2026-09-16T03:01:11.897Z):

```sh
stack test --fast --rerun-tests --test-arguments='--match=provider'
```

Local log: `/private/tmp/tdf-payment-continuation-baseline-20260916.log`.

## Implementation and threat model

Two workers rendered arbitrary exception details into logs despite a misleading
redaction function name. Tests exercise the actual production iteration boundary
using synthetic markers only; no secret values, real card numbers or provider
requests are needed. Fixed failure events eliminate the exception-data path.

The provider-event, query-recovery and merchandise expiry loops also isolate
diagnostic sink failures. A committed tick is not rerun because its log failed;
normal loop timing, qualification and lease recovery remain unchanged. Async
cancellation propagates through both tick and sink boundaries. Idle behavior and
integer-only information counters remain compatible. This does not make durable
financial audit writes best-effort or suppress payment-state validation failures.

Covered threats: sensitive logging, log injection, error-renderer failures,
logging-destination availability and accidental duplicate work after a logging
failure. Not covered by this increment: all application/driver/hosting log paths,
historical incident assessment, external log-service security, production worker
liveness, credentials, real provider behavior or legal/PCI certification.

## Verification

Environment: local macOS; Node 24.8.0; Stack lts-24.42/GHC 9.10.3. Test actions
and exception contents are synthetic; this is not merchant sandbox or staging
payment execution. Runtime/test source for the first post-fix results:
`e19de03981b4d91fcd7f017aef14dfef93f16edf`. UTC timestamps below are local log
last-write times; process exit codes were observed separately through the tools.

Retained pre-fix evidence:

- At 2026-09-16T03:33:52.020Z, the first test command completed compilation but
  exited 1 because `--test-arguments='--match=payment worker logging boundary'`
  split the pattern into unexpected arguments. **No tests executed** in that
  invocation. Local log: `/private/tmp/tdf-payment-worker-logging-red-20260916.log`.
- The corrected command, `stack test --fast --rerun-tests
  --test-arguments='--match=logging'`, on unchanged `925b42932` ran 24 examples:
  **10 failures**, exit 1, 0.0645 seconds, seed `1572381602`, completed at
  2026-09-16T03:34:27.613Z. The failures demonstrated malformed/sensitive error
  output, exception-renderer evaluation and logging-sink exceptions escaping
  the workers. Cancellation/idle checks and the already-fixed query failure
  message passed. Log: `/private/tmp/tdf-payment-worker-logging-red-focused-20260916.log`.
- `e19de03981b4d91fcd7f017aef14dfef93f16edf` replaces the vulnerable output and
  isolates synchronous diagnostic-sink failures. Source was frozen at this
  commit for the initial complete verification below. A final formatting-only
  commit, `9e28364fa3d24d6d91fa5d7405723b39ba69c32c`, wraps four fixed diagnostic
  literals to follow the source style; the test source, tick logic and SQL remain
  unchanged. Final-source repeats are recorded separately below.

| UTC completion | Check | Actual result |
|---|---|---|
| 2026-09-16 03:38:22 | Targeted worker boundary, original seed `1572381602` | Exit 0; 24 examples, zero failures, 0.0335 seconds; three QuickCheck properties passed 100 generated cases each |
| 2026-09-16 03:39:17 | Full default backend | Exit 0; 2,627 examples, zero failures, 41.0251 seconds; includes unchanged Datafast/PayPal and other existing unit regressions |
| 2026-09-16 03:38:07 | Repository quality | Exit 0; 232 Node tests, zero failures; formal audit 9,764 findings, zero critical/errors, 377 warnings and 9,387 information findings |
| 2026-09-16 03:40:25 | Fresh PostgreSQL 16.10 payment harness at `e19de0398` | **Whole harness exit 0**; 215 examples, zero failures, 25.1495 seconds; repeat migration, empty rollback/operator-seed retention, reapply, used-history rollback refusal and readable history |
| 2026-09-16 03:40:10 | Catalog JSON/CSV at `e19de0398` | Both commands exit 0; 1,432 files, 1,135 reviewed candidates; no unreviewed/stale decisions or authority changes |
| 2026-09-16 03:46:38 | Final `9e28364fa` targeted repeat, same command/seed | Exit 0; 24 examples, zero failures; 0.0462 seconds, three properties with 100 cases each |
| 2026-09-16 03:47:31 | Final `9e28364fa` full backend repeat | Exit 0; 2,627 examples, zero failures; 38.5840 seconds |
| 2026-09-16 03:46:13 | Final source repository quality repeat | Exit 0; same 232 tests and formal counts above; no weakened gates |
| 2026-09-16 03:48:36 | Final catalog JSON/CSV repeat | Both commands exit 0; 1,432 files, 1,135 reviewed candidates; decision file unchanged |

The earlier onboarding evidence-window failure in #389 did not recur in this
full run; no onboarding code or assertion was changed. Existing compiler
partial-function, unused-binding, missing-Cabal-module and linker advisories
remain visible; compilation succeeded. No dependency or quality gate was weakened.

Exact post-fix commands (local logs prefixed
`/private/tmp/tdf-payment-worker-logging-`):

```sh
# cwd: tdf-hq; green-20260916.log, then full-20260916.log
set -e -o pipefail
stack test --fast --rerun-tests --test-arguments='--match=logging --seed=1572381602'
stack test --fast --rerun-tests

# cwd: repository root; quality-20260916.log
npm run quality:repo
cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts
git diff --check

node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed --output docs/catalog-persistence/reports/static-list-inventory.json
node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed --format csv \
  --output docs/catalog-persistence/reports/list-consumer-matrix.csv

# db-20260916.log; disposable local-only DB, synthetic provider evidence
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-payment-worker-log-pg.LIlCbg' \
  sh scripts/test-provider-retry-runtime.sh
```

Logged commands used `tee` with `pipefail`; logs remain local, not uploaded CI
artifacts. Generated web/mobile types compare byte-identically and whitespace
checks passed. Web/mobile production builds, UI/native/device/manual QA, external
log-service failure, real process shutdown and credentialed provider sandbox
flows were not run in this increment. There is no UI/client change or screenshot.
Final repeats used the identical commands above with local log suffixes
`final-targeted-20260916.log`, `final-full-20260916.log` and
`final-quality-20260916.log`. The database harness was run at the behavioral-fix
commit `e19de0398`, not rerun after the fixed-string formatting cleanup; the
later full/targeted source checks are explicitly distinguished here.

The PostgreSQL cluster was created under a `mktemp -d
/private/tmp/tdf-payment-worker-log-pg.XXXXXX` directory. Commands used the
PostgreSQL 16 binaries under `/usr/local/opt/postgresql@16/bin`:

```sh
initdb -D /private/tmp/tdf-payment-worker-log-pg.LIlCbg/data -U postgres --auth=trust --no-locale -E UTF8
pg_ctl -D /private/tmp/tdf-payment-worker-log-pg.LIlCbg/data \
  -l /private/tmp/tdf-payment-worker-log-pg.LIlCbg/server.log \
  -o "-k /private/tmp/tdf-payment-worker-log-pg.LIlCbg -h ''" start
createdb -h /private/tmp/tdf-payment-worker-log-pg.LIlCbg -U postgres tdf_provider_retry_test
pg_ctl -D /private/tmp/tdf-payment-worker-log-pg.LIlCbg/data stop -m fast
pg_ctl -D /private/tmp/tdf-payment-worker-log-pg.LIlCbg/data status
```

It listened only on a private Unix socket, not TCP. The first restricted stop
attempt was denied permission to signal its PID; the approved unrestricted retry
stopped the exact cluster with exit 0. At 03:42 UTC, a separate status check
reported `no server running`. Synthetic data/logs were retained; nothing was
deleted. This was the only database cluster created by this increment.

## Staging and remaining blockers

The read-only staging inspector at unchanged #344 commit
`0081b6b03bd58716090c57d936f31a364e944522` returned exit 1 at
2026-09-16T03:11:28.042Z. Both public staging health endpoints returned HTTP 200;
API database health was OK. All six hosting status/configuration/secret-name
checks reported `hosting_authentication_unavailable`, `sourceCommit=null` and
`providerQualified=false`; no alternative host configured. Web's absent DB-health
field is not evidence of a database failure. The approved unrestricted retry
also returned exit 1 at **2026-09-16T03:12:53.786Z**, with the same six hosting
failures, healthy public endpoints and unqualified provider/deployment state.

Restore authorized `FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` or local Fly
login, then verify deployed SHA, sandbox merchant aliases/contracts, secret names
and registered webhooks. No credential values were read/displayed and no hosting
resource was created, deployed or removed. Existing sanitized untracked inspector
artifacts were preserved outside this branch.

Provider sandbox and qualified staging flows, native/device QA, legacy
Datafast/PayPal late-approval handling, remaining product fulfillment, recurring,
refund/void, settlement and compliant seller-payout execution remain open. The
Ecuador commercial market matrix is not refreshed by this source-level security
repair. Required legal/accounting/PCI and production-activation reviews remain.

## Runbook and rollback

Use the operational procedure in ADR 0127 and existing strict-admin payment
queue/reconciliation views. A log line never proves a charge, settlement or safe
fallback. Preserve original attempts, financial history and payment holds.
No SQL migration, backfill, API/client change, provider activation or new mobile
PR. Review after #389 and the existing dependent payment stack. Roll back only
application code with the diagnostic exposure risk understood; do not clear
queues or rewrite historical records. Prefer a forward repair.

## Final source review and reused security cleanup

The final remote refresh retained main `3763a4071`, core #331 `a8d31238a` and
#389 `824e1e9ae`. #389 backend CI was still in progress at that read; no failure
or completion is inferred. The fetch also discovered
[#392](https://github.com/diegueins680/tdf-app/pull/392), commit
`2455951713967642de9b429c5a790ceec6cb1fa2`: a one-line documentation replacement
in `STRIPE_WEBHOOK_SETUP.md`. A read-only check emitted only line counts and
booleans: the change replaces a signing-secret-shaped token with a placeholder,
and the surrounding text is unchanged. No credential-like value was printed,
copied into evidence, sent to a provider or tested for validity.

Preserved that existing commit in local merge `90156e57c36e034ec97b26d7b88cf4023a9d6284`;
its only changed file was the setup document, and the tested Haskell source
remains byte-identical to `9e28364fa`. No GitHub PR was merged. Review #392 with
this stack rather than duplicating its patch. This does not make Stripe eligible
for Ecuador, activate an integration, rotate a credential or remove Git history.
Do not use the old published example. If it was ever installed, an authorized
operator must assess exposure and rotate it through the appropriate provider
workflow; production access and history rewriting are not authorized here.

## Commit and delivery order

Base: #389 (`codex/payment-stack-integration-20260915`). The existing payment
stack through #389 precedes this draft; preserve the reviewed main history and
the #392 documentation cleanup. No new mobile PR or gitlink change.

1. `da81213085db2a98dc0fce89bff18ee9280e1b84`: integrate observed main and valid
   generated reports; supersedes the unpublished premature merge commit.
2. `925b42932a05c980a5d3d46641d20f8ce63cc4cc`: iteration seams and 24 regressions.
3. `e19de03981b4d91fcd7f017aef14dfef93f16edf`: diagnostic security/reliability fix.
4. `9e28364fa3d24d6d91fa5d7405723b39ba69c32c`: fixed-literal formatting only;
   final runtime/test source verified above.
5. `90156e57c36e034ec97b26d7b88cf4023a9d6284`: reuse #392 without rewriting it.

Subsequent commits contain inventory and evidence/runbook documentation only.
`0c795b1b6c97e1267eb6363b8be495249e0136b6` records the final inventory;
`5e83368cc9248d310a29d7c8e013bd3835cb9acb` records evidence and runbooks.

At **2026-09-16 03:52 UTC**, `git ls-remote` and GitHub metadata independently
confirmed pushed head `5e83368cc9248d310a29d7c8e013bd3835cb9acb` on
`codex/payment-worker-log-safety-20260915` and OPEN draft
[#393](https://github.com/diegueins680/tdf-app/pull/393), based on #389's branch.
CI was in progress, with no failed check reported at that read; it is not claimed
green. This delivery note is a subsequent documentation-only commit. The worktree
was clean and runtime/test files matched `9e28364fa`. No provider transaction,
qualified staging deployment, production mutation or GitHub PR merge occurred.
