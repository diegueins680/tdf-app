# Missed-callback query recovery: evidence and runbook

Date: 2026-09-15 UTC; work began 2026-09-14 Ecuador and continued after midnight.
This increment implements an independent PlaceToPay/PayPhone recovery worker,
disabled by default. It does **not** complete the overall payment platform.
Design, state flow, threat controls and limits are in
[ADR 0120](../adr/0120-durable-provider-query-recovery.md).

## Repository and access evidence

Started from clean isolated `tdf-app-payment-checkout`, #358 head
`a5ba14f0be4e4f6da1df666b31dd6ae343afedab`, on new branch
`codex/payment-reconciliation-worker-20260915`. The original dirty worktree was
untouched. Initial network inspection failed under sandbox DNS restrictions;
the approved retry fetched origin and read GitHub metadata. Default `main`
remained `17a33eca11d585d84435af85340beece9b51d14e`, already an ancestor of the
dependent payment stack. No payment branch with a competing worker was found.

Inspected payment remote refs, open PRs #354–#358 and #358 comments/checks.
Completed parent checks were successful; its
[backend job](https://github.com/diegueins680/tdf-app/actions/runs/34927356611/job/104248175155)
was still running at initial inspection. No human review was present. Rechecked
#333's files at unchanged `5bceb329f034cdfadf077f8f2889aa50bd402c7c`:
its CI/catalog/onboarding/migration work does not implement this recovery worker.
Unmerged work was not treated as default-branch functionality. Parent check
results do not establish hosted results for this increment.

Final remote refresh completed before 05:12:36 UTC with `main` and #358 unchanged.
#358's `backend-quality`, migration, contract, catalog, repository, persona and
aggregate quality checks had then passed; UI/mobile/API-contract-test jobs were
skipped by the parent change scope. This remains parent-only evidence.

Read current official PlaceToPay notification/session, PayPhone Sale and
PostgreSQL locking documentation on 2026-09-15 UTC; exact sources and confidence
are linked in ADR 0120. This was a targeted contract refresh, not a fresh
exhaustive market or legal review. The broader dated findings and remaining
portfolio qualifications remain in the
[Ecuador platform audit](ecuador-payment-platform-audit-2026-09-11.md).

## Staging recheck: not qualified

`node scripts/inspect-payment-staging.mjs`, run read-only from the #344 worktree
at `0081b6b03bd58716090c57d936f31a364e944522`, reported
**2026-09-15T04:10:47.410Z**, exit 1:

- `https://tdf-hq-studio-audit-staging.fly.dev`: HTTP 200, health/database OK.
- `https://tdf-studio-audit-staging-web.fly.dev`: HTTP 200, health OK; not proof
  of a payment/API database test.
- All six hosting status/configuration/secret-name checks:
  `hosting_authentication_unavailable`; alternate hosting unavailable.
- `providerQualified=false`, `sourceCommit=null`. Deployed source, merchant
  credentials/contracts and actual provider flows remain unverified.

No secret values were retrieved, resources created or deployments made.
Authentication failure does not prove token expiry. Shortest path: restore
approved app-scoped access via `FLY_STAGING_API_TOKEN` and
`FLY_STAGING_WEB_TOKEN`, or an authorized local Fly login; rerun the inspector;
then qualify the exact app source and provider sandbox aliases before a reviewed
staging deployment/test. A public health response is insufficient authority to
provision, reconfigure or send payments from that environment.

## Implementation and compatibility

Migration commit **`b9875ff176c35f446c64d53dccadfd15b839cda4`**
(`2026-09-14T23:45:50-05:00`) adds jobs, leases, shared query budgets and exact
false environment flags. Runtime/test/manifest commit
**`ddfc19eb9247bf539a676755986cb5e5369d8f3b`**
(`2026-09-15T00:06:42-05:00`) integrates boot, real adapter queries, final lease
checks, atomic financial/job outcome and 35 additional database cases.
`7f8633050a5123559823ccdcd6fc6c1923a7d22a` corrects only the catalog decision
schema marker; it changes no runtime/test/migration behavior. Later delivery
documentation is separate.

Only known ambiguous create operations with matching immutable bindings are
eligible. No historical reference is rewritten. New jobs are not inbox events.
Provider responses/credentials never enter job records or logs. Budget and
retry delays use database time. Retry exhaustion and query mismatch produce
operator review, not another charge or an invented no-charge result.

The migration is registered after provider execution. Its manifest introducing
commit is an ancestor and its committed SQL matches the working file exactly.
The existing CI backend harness already runs this database suite, so no quality
gate was removed or skipped. The catalog decision update reviews three fixed
technical protocol candidates (job states, terminal subset, private error sum
type) and refreshes the changed migration-manifest fingerprint. No scanner,
threshold, database authority or dynamic payment-method catalog is bypassed.

No API/OpenAPI/generated client, web/mobile/admin code changed in this increment.
Existing consumers still read canonical payment states. Job administration is
restricted to the read-only procedure below plus existing reconciliation
exception handling; new retry/resolution controls remain follow-up work.

## Local test record

Environment: local macOS, Stack `lts-24.42` / GHC 9.10.3, disposable PostgreSQL 16.
All provider identities, credentials and responses in tests are synthetic;
injected transport does not contact a provider. The suite exercises actual
runtime configuration, adapter request/parsing, database stores, locks and
financial state. It is neither a credentialed sandbox nor a staging test.

| Command | Source / observed UTC time on 2026-09-15 | Result |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Baseline `a5ba14f...`; started around 04:10, completed before 04:13 | 118 examples, zero failures, 17.0548 seconds; exit 0. Optional DB environment unset. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| rg --line-buffered -A25 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.Provider\|Test suite'` | Intermediate runtime source, before new DB tests; completion observed before 04:50 | Runtime and test compilation completed; 118 examples, zero failures, 16.1128 seconds; exit 0. Not new DB coverage. |
| `set -o pipefail; ./scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 110` | Initial attempt around 04:50 | Exit 126: script is not executable; used its documented `sh` invocation. No DB tests ran. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 110` | Restricted attempt, completed before 04:55 | Exit 126: Docker socket denied by sandbox. No DB tests ran; approved retry followed. |
| Same `sh` command above | Approved disposable DB; intermediate uncommitted worker/tests; completed 05:06:27 | 107 examples, two failures, 51.0872 seconds; exit 1. Both were an ambiguous `last_error_code` column in the new test assertion, fixed in `ddfc19eb...`. Not a provider failure. Subsequent nonempty rollback check did not run after this failed suite. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 125` | Final executable source `ddfc19eb...`, catalog-only follow-up `7f863305...`; approved isolated DB; started before 05:07:07, completion observed 05:14:40 | **107 examples, zero failures**, 52.0003 seconds; exit 0. Includes repeat apply, empty rollback/reapply, operator-owned flag preservation and nonempty rollback refusal/read-back. |
| `node --test scripts/__tests__/production-release.test.mjs scripts/__tests__/production-entrypoint.test.mjs` | Uncommitted source containing final manifest at `b9875ff...`; completed before 05:03:07 | 60 tests passed, zero failures; 37958.21035 ms; exit 0. No deployment performed. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payment-query-recovery-audit.json` | Intermediate source; report 05:04:32.177, completion observed before 05:06:27 | Exit 1: four unreviewed candidates and one stale manifest decision; 1,416 files, 1,125 candidates. Explicit technical review entries were then added; scanner/gate unchanged. |
| Same audit with `--output /tmp/tdf-payment-query-recovery-final-audit.json` | `ddfc19eb...`; completion observed 05:09:26 | Exit 1 before scanning: new technical decisions used a model-specific marker instead of required `technical_constant_allowlist`. Fixed by `7f863305...`; no gate change. |
| Same final audit command | `7f863305...`; report 05:15:45.420, completion observed 05:15:59 | Exit 0; **1,416 files / 1,125 candidates / zero unreviewed or stale decisions**. |
| `node --test scripts/__tests__/catalog-list-audit.test.mjs` | `7f863305...`; completion observed 05:12:36 | One test passed, zero failures; 2355.177161 ms; exit 0. |
| `set -o pipefail; SOURCE_COMMIT=7f8633050a5123559823ccdcd6fc6c1923a7d22a node scripts/render-production-migration-batch.mjs \| wc -l` | `7f863305...`; completion observed 05:14:40 | Rendered 34,590 lines, exit 0. SQL was not executed against production or staging. |
| `git merge-base --is-ancestor b9875ff176c35f446c64d53dccadfd15b839cda4 HEAD` and Node comparison of `git show <introducedBy>:<path>` against manifest SQL | Before 05:06:27 | Exit 0; introducing commit and SQL agree. |
| `git diff --check`, `sh -n scripts/test-provider-retry-runtime.sh` | Repeated during implementation through final code commit | Exit 0. |
| Node local Markdown target check in ADR 0120 and this report | Observed before 05:11:20 | Four relative links resolved; exit 0. External links were separately read from official sources. |
| `docker ps -a --filter name=tdf-provider-retry-test --format '{{.Names}} {{.Status}}'` | After successful harness completion, before 05:16 UTC | Exit 0; no matching containers remain. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| rg --line-buffered -A25 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.Provider\|Test suite'` | `7f863305...`, DB environment unset; started after 05:14:40, completed before 05:17 UTC | **2,589 examples, zero failures**, 44.6360 seconds; exit 0. Focused 118 examples overlap this suite; the 107 optional DB cases ran separately. |

Only completed results are listed. Other worktrees were compiling concurrently;
their processes/work were not interrupted. Compiler/linker warnings were not
suppressed. The harness removed only its disposable synthetic databases and
containers; no user, staging or production data was removed.

Local Markdown checker (read-only, run from repository root):

```sh
node -e 'const fs=require("node:fs");const path=require("node:path");let count=0;for(const f of ["docs/adr/0120-durable-provider-query-recovery.md","docs/payments/query-recovery-2026-09-15.md"]){for(const m of fs.readFileSync(f,"utf8").matchAll(/\]\(([^)]+)\)/g)){if(m[1].startsWith("http"))continue;const p=path.resolve(path.dirname(f),m[1].split("#")[0]);if(!fs.existsSync(p))throw new Error("Missing target: "+p);count++;}}console.log("Verified "+count+" local document links");'
```

The disposable harness applies real checkout/intent/provider migrations atop a
guarded test-only ORM base. It repeat-applies the new migration, exercises empty
rollback/reapply and preservation of an operator-edited false flag before Hspec.
After successful Hspec it must refuse rollback of a used queue/budget and prove
history remains readable. It removes only its own synthetic database/container.
This is not the complete production-schema or fulfillment rehearsal.

New cases cover missed callbacks without fake inbox evidence, exact disabled
gates, missing runtime credentials/account authority, unknown-resource exclusion,
replica concurrency, shared callback budget, backoff/redaction, expired and
superseded lease rejection, both kill-switch rechecks, atomic rollback when job
completion fails, callback/query reordering, retry exhaustion/crashed final lease,
money mismatch, late-paid/expired rejection, immutable terminal history and
invalid/live-lease state changes. The 72 inherited database cases retain Datafast,
PayPal, callback identity, replay, query and financial atomicity regressions.

## Authorized future sandbox rollout and operator procedure

1. Review dependent PRs in order, apply the additive migration before new callback
   binaries, and leave both new database flags and the process switch disabled.
   Drain old query consumers: old callback binaries do not share the new budget.
2. Establish exact `COMMERCE_CHECKOUT_ENV=sandbox`, qualified merchant alias,
   contract, USD settlement and sandbox credential evidence. Required secret and
   configuration names are in the [operator runbooks](operator-runbooks.md).
   Presence is not validation; do not print values or blanket-enable accounts.
3. Confirm all consumers sharing those provider credentials use the same quota
   authority, or obtain a reviewed cross-system coordination arrangement.
   Public PayPhone documentation does not specify enforcement scope. Six
   reservations/minute here is not a guarantee about outside integrations.
4. Only under authorized sandbox qualification, enable the exact sandbox
   `checkout.provider_query_recovery` flag and process switch, then restart the
   intended worker process. Production stays disabled. No activation command
   was executed in this work.
5. Exercise a known pending sandbox resource while withholding its callback;
   observe initial delay, one authoritative query, terminal/pending outcome,
   matching ledger/receipt and no fabricated inbox record. Repeat concurrent
   callbacks, process termination, expired leases, incorrect bindings, provider
   timeout, quota contention, revocation and late-paid/expired review. Record
   source SHA, account alias, environment, UTC time and sanitized correlation IDs.
6. Before/after compare paid checkout/intent/capture amounts, ledger totals,
   receipts, unresolved operations, inbox retries and open exceptions. Never
   equate `completed` job with settlement or `dead_letter` with a failed charge.

Authorized operators can inspect aggregate queue health without customer or
provider payload data. This query is read-only and does not repair any row:

```sql
BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5s';
SELECT operation.provider, operation.environment, job.status,
       count(*) AS jobs,
       min(job.next_attempt_at) AS earliest_due,
       count(*) FILTER (WHERE job.status = 'processing'
         AND job.lease_expires_at <= clock_timestamp()) AS expired_leases,
       max(job.attempt_count) AS highest_attempt_count
  FROM commerce_provider_query_job job
  JOIN commerce_provider_operation operation ON operation.id = job.operation_id
 GROUP BY operation.provider, operation.environment, job.status;
SELECT provider, environment, exception_type, count(*) AS open_exceptions
  FROM commerce_reconciliation_exception
 WHERE status = 'open' AND exception_type = 'scheduled_query_requires_review'
 GROUP BY provider, environment, exception_type;
COMMIT;
```

For an exception, an authorized operator must reconcile the original resource,
checkout, payment attempt and canonical financial history. Use the correlation
`provider-query-job:<operation UUID>` to locate immutable audit. Do not copy
tokens, hosted URLs, decrypted callbacks or raw provider responses into tickets.
There is no automatic dead-letter reset, refund, payout or terminal-history edit.
Late paid/expired fulfillment and contradictory provider evidence need separately
reviewed product, financial and support decisions.

To stop recovery, disable the exact environment database flag and process switch.
The final database authority check fences revocations at its transaction boundary
(see ADR); do not promise to cancel a commit already holding its authority locks.
Preserve schema and history on image rollback. Empty rollback is permitted only
after stopping both callback and scheduled query consumers and verifying both
new tables are unused. Never clear rows to make rollback pass. Prior callback
code lacks the distributed budget, so it cannot run alongside an enabled worker
under a claimed shared quota guarantee.

## Blockers, unexecuted tests and review order

Review order: **#331 → #332 → #334 → #340 → #343 → #347 → #350 → #353 → #358
→ this worker**. Staging #344 is an independent sibling after #343. No PR is
merged, no protections bypassed and no production action is authorized here.

Unexecuted: credentialed provider sandbox/E2E and staging payments, full
production-schema rehearsal for this new migration, new web/mobile tests,
actual settlements/refunds/voids/mandates/payouts and production deployment.
No actual provider/account has been qualified by these mock/database results.

Remaining work includes history-compatible PayPhone cancellation classification,
job administration and safe reviewed reprocessing, unknown-resource ambiguous
create recovery, explicit late-payment/fulfillment policy, Datafast/PayPal's
remaining canonical adapter/business-flow work, subscriptions/mandates, taxes,
SRI credit notes, disputes, settlements and contracted connected-seller payouts.
Merchant contracts, real sandbox credentials, exact site-method mappings,
webhook registration, hosting authorization and Ecuadorian legal/accounting/PCI
review remain human/provider dependencies. Internal balances are not a compliant
custody or marketplace payout mechanism by themselves.

Remote push, draft PR and hosted results are reported only after read-back
verification; a local commit or parent preview is not evidence of those actions.
