# Provider reconciliation atomicity — verification

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. This continuation repairs the shared
PlaceToPay/PayPhone query-application boundary. It does not complete the overall
payment platform or implement the independent missed-callback worker.
See [ADR 0119](../adr/0119-atomic-provider-query-application.md).

## Access and branch review

Started from clean isolated worktree `tdf-app-payment-checkout`, draft #353 at
`86a8ce38cd131bfcb1b3e3035fd9a3e3aea1d503`. Initial `git fetch origin` failed
under sandbox DNS restrictions; the approved network retry succeeded. Default
`main` remained `17a33eca11d585d84435af85340beece9b51d14e`, already an ancestor.
The temporary local branch name `codex/payment-reconciliation-worker-20260914`
was renamed to `codex/payment-reconciliation-atomicity-20260914` when inspection
identified this prerequisite. No remote worker branch or implementation is claimed.

Rechecked open PRs, payment branches, #353 comments/reviews and #333's changed
files at head `5bceb329f034cdfadf077f8f2889aa50bd402c7c`. No human review of
#353 was present. Inspected #351 (`192f9578665109e52d28a96c772e3307bdc684e5`)
and #352 (`a1895105c6e15e1f95b4b72fd6218b34a311f6e6`) bodies/files. Those
separate event/storefront branches register existing migrations and correct an
expiry fixture; they do not implement this reconciliation boundary. Their work
was not copied or treated as merged. The original dirty main worktree was not
modified.

At 03:15 UTC, #353's completed active hosted checks were successful; its
[backend job](https://github.com/diegueins680/tdf-app/actions/runs/34923941944/job/104237915497)
was still in progress, started 03:10:29 UTC. Its automatic web preview is not
payment staging qualification. No hosted result for this new change is inferred
from parent checks.

## Staging recheck

Ran `node scripts/inspect-payment-staging.mjs` read-only from the #344 worktree,
head `0081b6b03bd58716090c57d936f31a364e944522`. Report timestamp
**2026-09-15T03:15:13.311Z**, exit 1:

- API `https://tdf-hq-studio-audit-staging.fly.dev`: HTTP 200, health/database OK.
- Web `https://tdf-studio-audit-staging-web.fly.dev`: HTTP 200, health OK. The
  web response is not evidence of an API database/payment test.
- All six hosting status/configuration/secret-name inspections returned
  `hosting_authentication_unavailable`; no alternate hosting was configured.
- `providerQualified=false`, `sourceCommit=null`. Deployed SHA, credentials,
  contracts and provider flows remain unqualified. Authentication failure does
  not establish the cause or prove token expiry.

No secret values were retrieved/displayed and no resources were created,
deployed or reconfigured. Restore approved app-scoped hosting access through
`FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` or authorized local Fly login,
then rerun the inspector before a separately reviewed staging deployment.

## Tests and evidence

Environment: local macOS, Stack `lts-24.42` / GHC 9.10.3, PostgreSQL 16 in a
dedicated disposable container. Provider fixtures, merchant aliases and
credentials are synthetic. The injected query transport never contacts a
provider; the tests still use the real inbox store/decryption, callback trust
validation, adapter query construction/parsing and PostgreSQL financial path.

Implementation/test commit: `9294566721236e9ee5b644213db24c2ba2a5ca75`
(2026-09-14T22:36:01-05:00). Test-only follow-ups
`874e45b1f59c9ce2b2562d845313b9d1d16a0620` remove a new Hspec-name shadow and
`bb54a2232bad46344b58a4dd91103d84431fcb85` repair fixture dependencies and inbox
claiming. Documentation is separate. Test results are recorded
below only after completion; mocked contract tests do not qualify a sandbox.

| Command | Source/time (UTC 2026-09-15) | Observed result |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Baseline #353; started around 03:16, completion observed before 03:17:09 | 118 examples, 0 failures; 16.1446 seconds; exit 0. DB environment unset. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| rg --line-buffered -A25 -B3 'error:\|Error:\|warning:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.Provider\|Test suite'` | Intermediate uncommitted source; completion observed before 03:35 | Exit 1 during compilation: a new fixture omitted the operation-claim encryption-key argument. Fixed before the implementation commit; not a provider/test pass. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 110` | Initial restricted attempt, before 03:35:17 | Exit 126: sandbox denied the Docker socket. No database tests ran. Approved escalation reran the same isolated harness. |
| Same focused Stack command as the intermediate build above | Started before 03:35; compiled `929456672...`; completion observed before 03:51:23 | 118 examples, 0 failures; 16.9829 seconds, exit 0. These inherited focused tests do not execute the optional new database cases. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 110` | Approved disposable PostgreSQL run started before 03:35:17; test-name cleanup during build; completion observed before 03:51:23 | 72 examples, 16 failures; 18.5935 seconds, exit 1. The fixtures loaded inbox data before claiming and attempted ticket capture without the required runtime tables. No real payment failed; these were isolated test-fixture failures. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payment-reconciliation-atomicity-audit.json` | Report 03:41:52.197, before ledger-fixture follow-up | Exit 0; 1,413 files / 1,122 candidates, all with review decisions. No scanner/decision changes. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 120` | Final `bb54a2232...`; approved local PostgreSQL 16, started around 03:53; completion observed before 03:59:15 | 72 examples, 0 failures; 30.8132 seconds, exit 0. Includes 27 new reconciliation cases and 45 inherited cases. |
| `set -o pipefail; ./scripts/test-provider-execution-runtime-migration.sh 2>&1 \| tail -n 20` | Restricted initial attempt, before 03:59:15 | Exit 126: sandbox Docker-socket denial; no tests ran. Retried with approved local access. |
| `sh -n scripts/test-provider-retry-runtime.sh`, `git diff --check` | Final source, observed before 03:56:08; whitespace repeatedly checked | Exit 0. |
| Local Markdown link check using Node `fs.existsSync` on relative links in ADR 0119 and this report | Observed before 03:56:08 | Exit 0; two local links resolved. External sources were separately opened in the official-source browser. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| rg --line-buffered -A25 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.Provider\|Test suite'` | Final `bb54a2232...`, DB environment unset; started before 03:59:15, completion observed 04:01:32 | 2,589 examples, 0 failures; 54.6890 seconds, exit 0. The focused 118 examples overlap this suite. |
| `set -o pipefail; ./scripts/test-provider-execution-runtime-migration.sh 2>&1 \| tail -n 20` | Approved local PostgreSQL 16; final `bb54a2232...`; started before 03:59:15, completion observed 04:01:32 | Exit 0, “Provider execution migration tests passed”. Inherited repeat application, immutable-reference/trust checks, nonempty rollback refusal, empty rollback, operator-owned flag preservation and reapply. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payment-reconciliation-atomicity-final-audit.json` | Final `bb54a2232...`; report 04:01:12.272, completion observed 04:01:32 | Exit 0; 1,414 files / 1,122 candidates / zero unreviewed. No gate, scanner or catalog-decision changes. |

Final `git fetch origin` completed before 03:56:08, with `main` unchanged at the
SHA above. All new executable/test changes are in `bb54a2232...`; subsequent
changes are documentation only. Existing missing-home-module/linker/deprecation
and inherited partial-list warnings were not suppressed. The new Hspec shadow
warning was removed in the test-only follow-up.

Post-test container read-back used `docker ps -a --filter name=tdf-provider-retry-test
--filter name=tdf-provider-execution-migration --format '{{.Names}} {{.Status}}'`:
exit 0, no rows, observed after 04:01:32. The harnesses removed only their own
disposable synthetic databases/containers; no user, staging or production data
was deleted. Final local-doc checker command (run from repository root):

```sh
node -e 'const fs=require("node:fs"); const path=require("node:path"); const files=["docs/adr/0119-atomic-provider-query-application.md","docs/payments/reconciliation-atomicity-2026-09-14.md"]; let n=0; for(const f of files){for(const m of fs.readFileSync(f,"utf8").matchAll(/\]\(([^)]+)\)/g)){if(m[1].startsWith("http"))continue; const p=path.resolve(path.dirname(f),m[1].split("#")[0]); if(!fs.existsSync(p))throw new Error("Missing local doc target: "+p); n++;}} console.log("Verified "+n+" local document links");'
```

The new database cases cover caller rollback, savepoint survival, caller lock
retention using independent connections/NOWAIT, concurrent success idempotency,
terminal-state reordering/conflicts, altered money/bindings, SQL failure,
injected `ThreadKilled` after application, unknown status, cancellation, unsupported mutations,
persisted-callback query status, response observation time, transport redaction,
query mismatch exceptions and invalid stored callback trust. The fixtures do not
test a real TLS handshake, sandbox, end-to-end browser/device flow or job lease.

No production SQL/migration, public API, generated client, web, mobile or
administrative source changed. The database harness now includes a guarded,
test-only ORM-base fixture adapted from the existing rental migration harness,
then applies the real checkout-trigger compatibility and sale/rental runtime
migrations. Capture-ledger queries reference rental tables even for TDF-owned
service revenue. New paid fixtures therefore use a service checkout; they do
not pretend to fulfill tickets without fee/seat snapshots. Existing ticket
identity/retry fixtures remain unchanged. No runtime/commerce trigger or money
constraint is stubbed or disabled. This is not a full production-schema rehearsal
or ticket/booking fulfillment test. Existing gates/thresholds are preserved.

Inherited migrations seed some production-labeled feature flags inside this
dedicated local database; this is synthetic fixture setup, not provider or
production activation. No production database or secret is involved.

## Remaining blockers and delivery order

Review after **#331 → #332 → #334 → #340 → #343 → #347 → #350 → #353**.
Staging #344 is an independent sibling after #343. No merge or production
deployment is authorized. Remote push/PR and final results are recorded when
verified, not inferred from local commits.

Not implemented in this increment: independent missed-callback reconciliation,
shared status-query rate budgets, lease fencing/job administration, unknown-ID
create recovery, automated late-paid/expired fulfillment, settlement/recurring/
refund/void/payout execution, and the remaining broader payment-domain work.
The precise next worker safety requirements are in ADR 0119. No worker flag was
introduced or enabled. Existing provider accounts remain subject to their
disabled/qualification gates.

Unexecuted: credentialed provider sandbox/end-to-end and staging payment tests,
new web/mobile tests, real invoices/settlements/refunds/payouts, production data
changes and deployment. Qualified sandbox accounts, exact contracted method
mappings, webhook registration/SHA-256 coordination, authorized hosting access
and legal/accounting/PCI review remain required before activation. Local mocks
cannot substitute for any of these steps.
