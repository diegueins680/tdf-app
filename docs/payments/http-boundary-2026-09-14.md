# Payment HTTP boundary — implementation and verification

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. This is a bounded continuation of the
payment stack, not completion of the entire platform. Architecture, limits,
source links and rollback are in [ADR 0117](../adr/0117-shared-bounded-payment-transport.md).

## Access and branch review

- Started from clean isolated worktree `tdf-app-payment-checkout`, on draft #347
  at `6bd0c9316f1816ccfc1873363787a99f33ff72e0`. Created dependent branch
  `codex/payment-http-boundary-20260914`.
- `git fetch origin` succeeded. Default `main` remained
  `17a33eca11d585d84435af85340beece9b51d14e`, already an ancestor of this stack.
- Rechecked all open PR names/bases and the active payment #347 checks. Event
  #345/#346 and CI #333 have separate work; #333's latest file list was inspected
  and its code is not duplicated here. The original dirty worktree is untouched.
- GitHub repository secret **names only** were listed. Dedicated
  `FLY_STAGING_API_TOKEN` and `FLY_STAGING_WEB_TOKEN` remain absent. No hosting or
  payment credential values were retrieved, printed or committed. Prior staging
  metadata-access failures remain documented in [#344](https://github.com/diegueins680/tdf-app/pull/344).
  This continuation has not newly verified a staging deployment or merchant account.

## Changes and coverage

The shared boundary now serves active Datafast create/status calls and PayPal
OAuth/order/capture/query/refund/webhook verification, plus PlaceToPay/PayPhone
session and reconciliation requests. Applicable products inherit these fixes
through existing helpers; no web/mobile/API schema change is necessary.

Tests cover provider destination validation, parsed-host tampering, redacted URL
and response errors, exact-size/oversized bodies, non-2xx handling, HTTP redirects,
malformed HTTP/gzip, stale-connection POST retry suppression, a stalled response
body, PayPal OAuth safety, stable create/capture request IDs and capture binding.
All tokens, names, order IDs and HTTP replies in these tests are synthetic. The
custom connection cannot contact a provider; no TLS or provider success is implied.

## Executed checks

Environment: macOS, repository Stack resolver `lts-24.42` / GHC 9.10.3. Use the
recorded source hash below rather than assuming results apply to later commits.
Catalog audits used local Node `v24.8.0`, rechecked before delivery.
`stack ls dependencies | rg '^http-client'` completed successfully before
01:49:46 UTC: `http-client 0.7.19`, `http-client-tls 0.3.6.4`.

| Command | UTC evidence / source | Result |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Baseline #347; started after 01:13:25, completion observed before 01:21:49 | **93 examples, 0 failures**, 0.7982 seconds; exit 0. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| tail -n 45` | Intermediate worktree, before 01:26:56 | Compile failed: `hostAddress` is not exported by the installed `Request` API. No test ran. Removed that unsupported field check; host/port/TLS/Host-header checks remain. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider HTTP transport boundary' 2>&1 \| tail -n 55` | Intermediate implementation; completion observed before 01:45:32 | Build exited 1 before test execution. Final tail omitted the originating diagnostic. Inspection identified an invalid new test assertion requiring `Eq CookieJar`; corrected to a presence check, without weakening the cookie policy. The subsequent run also uses an unambiguous no-space Hspec filter. |
| `cd tdf-hq && stack exec ghci -- -ignore-dot-ghci -v0 -e ':m + Network.HTTP.Client' -e ':i CookieJar' -e ':t makeConnection'` | Before 01:49:46 | Exit 0; confirmed installed `CookieJar` instances lack `Eq` and checked the synthetic connection constructor signature. Diagnostic inspection, not a test pass. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payment-http-audit.json` | Intermediate code; report 01:33:08.870 | Exit 0; 1,413 files / 1,122 candidates; zero unreviewed or stale decisions. |
| Same catalog command with `--output /tmp/tdf-payment-http-final-audit.json` | Started 01:43:12 on production-code commit `74ccf95942202a82c3ac67418ac91a0eea058951`; report 01:49:02.617. Later changes were test assertions/local variable naming and documentation only. | Exit 0; same counts, zero unreviewed or stale decisions. No audit rules/decisions changed. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| rg --line-buffered -A 35 -B 3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.ProviderRetrySpec\|Test suite'` | Started before 01:45:32; final corrected test module recompiled on `1c27d2d8561f1effa10e2bea7d12193249f36961`; completion observed 01:56:21 | **112 examples, 0 failures**, 16.3503 seconds; exit 0. Includes 19 new transport examples, not sandbox tests. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| tail -n 28` | Same final code/test commit; started after 01:56:21, completion observed 01:57:40 | **2,583 examples, 0 failures**, 27.1996 seconds; exit 0. No database-test environment was set; PostgreSQL checks are separate. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 50` | Same final code/test commit; approved disposable PostgreSQL 16 command running by 01:57:40; completion observed 01:58:27 | **36 examples, 0 failures**, 5.9495 seconds; exit 0. Existing schema setup, replay/legacy compatibility, concurrency and alternate-provider isolation tests all passed. No provider HTTP call. |
| `git diff --check` | Final source and documentation; checked through 01:58:27 | Exit 0. |

The full backend build and tests passed on final code/test commit
`1c27d2d8561f1effa10e2bea7d12193249f36961`, following production code
`74ccf95942202a82c3ac67418ac91a0eea058951`. Existing warnings were not suppressed.
The new synthetic connection harness uses the public, deprecated `closeManager`
for deterministic test cleanup; production uses the persistent shared manager.
Web/native-device, real TLS/provider sandbox and staging payment tests were not
executed for this internal backend change. Running or mocked checks are never
recorded as provider sandbox success.

Before delivery, `docker ps -a --filter name=tdf-provider-retry --format
'{{.Names}} {{.Status}}'` exited 0 with no rows: the harness removed its disposable
container. Final `git fetch origin` succeeded and default `main` remained the
same recorded commit. Neither check accessed production payment data.

## Delivery and review order

Code commit: `74ccf95942202a82c3ac67418ac91a0eea058951`. Corrected final test commit:
`1c27d2d8561f1effa10e2bea7d12193249f36961`. Documentation is a separate follow-up.
Required order: **#331 → #332 → #334 → #340 → #343 → #347 → this branch**.
Staging #344 remains an independent sibling after #343. No merge, force-push,
production deployment or provider activation is authorized by this sequence.

Parent #347's complete active GitHub checks passed, verified at 01:53:24 UTC.
Its [backend job](https://github.com/diegueins680/tdf-app/actions/runs/34914993269/job/104210658364)
ran 00:53:19–01:50:21 UTC and concluded success on parent
`6bd0c9316f1816ccfc1873363787a99f33ff72e0`. That result is **not** represented as
verification of this dependent transport patch.

## Required human/operational steps

1. Review the stack through #347, then this dependent transport change. #344 is a
   sibling staging qualification PR; do not merge or deploy automatically.
2. Restore approved staging hosting authentication using app-scoped secrets or an
   authorized local login. Qualify deployed code, provider environment and exact
   secret names without exporting values.
3. Obtain active sandbox merchant accounts and registered callbacks. Run the
   credentialed provider test matrix before any flag activation. No new account,
   contract, refund permission or marketplace payout authority is inferred.
4. Reconcile ambiguous results against original references. Keep pending orders
   blocked from alternate-provider payment until authoritative no-charge evidence.

No migration, production transaction, provider activation, production deployment,
historical-data purge or PR merge was performed in this continuation.
