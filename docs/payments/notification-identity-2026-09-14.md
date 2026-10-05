# PlaceToPay notification identity — verification

Date: 2026-09-14 Ecuador / 2026-09-15 UTC. This bounded continuation implements
forward replay deduplication, not the complete payment platform. See
[ADR 0118](../adr/0118-signed-payment-notification-identity.md) for the algorithm,
official sources, threat model, compatibility limits and rollback.

## Access and repository review

Started from clean isolated `tdf-app-payment-checkout`, draft #350 at
`aac15a69e9bc2e6de01652282d9da4012a0af067`. `git fetch origin` succeeded;
default `main` remained `17a33eca11d585d84435af85340beece9b51d14e`, already an
ancestor of the stack. Created `codex/payment-notification-identity-20260914`.
Rechecked open PRs, payment remote branches and #350 reviews/comments (no human
review was present). Inspected new #348/#349 descriptions/files and #333's file
list; no competing callback implementation was found. #349 reports full-schema
rehearsal failures on its separate branch; #333 includes migration-manifest work.
Neither is duplicated or silently treated as merged by this change.

Staging inspector `node scripts/inspect-payment-staging.mjs` ran read-only from
the #344 worktree on `0081b6b03bd58716090c57d936f31a364e944522`. Its report at
**2026-09-15T02:11:08.780Z**, exit 1, recorded:

- API `https://tdf-hq-studio-audit-staging.fly.dev`: HTTP 200, health/database OK.
- Web `https://tdf-studio-audit-staging-web.fly.dev`: HTTP 200, health OK; a web
  health response does not establish a database test.
- All six app status/configuration/secret-name inspections:
  `hosting_authentication_unavailable`. No alternate hosting was configured.
- `providerQualified=false`, `sourceCommit=null`: deployed image/version and
  payment accounts were not qualified. This is not proof of token expiration.

No credential values were retrieved or displayed. Restore approved app-scoped
hosting authentication (`FLY_STAGING_API_TOKEN`, `FLY_STAGING_WEB_TOKEN`, or an
authorized local Fly login), then rerun inspection before proposing deployment.
The existing staging remains unqualified; no resources were created or deployed.

At 02:25–02:26 UTC, #344's active hosted checks were all successful, including
its [backend job](https://github.com/diegueins680/tdf-app/actions/runs/34914860999/job/104210334939).
The manual staging-inspection job was skipped. These CI results do **not** prove
hosting access, sandbox transactions or provider qualification. #350's backend
job was still in progress at the 02:23 UTC check. Automatic web previews are not
payment staging qualification.

The #344 backend job ran 00:51:44–02:07:15 UTC and concluded success. Rechecking
#350's backend at 02:57 UTC still showed in progress (started 02:02:07 UTC); no
hosted pass for this dependent identity change is claimed. Final `git fetch
origin` completed before 03:00:02 UTC with `main` unchanged at the SHA above.

## Test environment and executed checks

Local macOS, Stack `lts-24.42` / GHC 9.10.3; Node `v24.8.0`. Test callbacks,
secrets, merchant aliases and provider responses are synthetic. No provider
request, sandbox transaction, real TLS handshake or production data is used.

Implementation commit: `9c1002424e61176db5f1dc9ecb614fe8a8ee13c4`
(2026-09-14T21:21:10-05:00). Test-source commit
`93ae918b76a54db4700f0331aa8b8bcf568b7deb` (2026-09-14T21:31:02-05:00) adds
explicit reordered/escaped fields and avoids new partial-list warnings. Final
test-only cleanup `f93a70f1e3740685dceb70e757caead562dbd7d6`
(2026-09-14T21:47:31-05:00) renames a local count that shadowed an Hspec hook.
Documentation is separate. The environment-scoping case writes synthetic
production-labelled rows only inside the disposable local database, never a
production environment.

| Command | Source/time (UTC 2026-09-15) | Observed result |
|---|---|---|
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests --test-arguments='--match=provider' 2>&1 \| tail -n 25` | Baseline #350; started 02:11, completion observed before 02:16 | 112 examples, 0 failures; 19.7206 seconds, exit 0. |
| `cd tdf-hq && stack exec ghci -- -ignore-dot-ghci -v0 -e ':m + Data.Aeson.KeyMap' -e ':t adjust' -e ':t filterWithKey'` | Intermediate uncommitted test helper; completion observed 02:19–02:20 | Diagnostic exited 1: installed KeyMap has no `adjust`. Corrected the helper to `mapWithKey` before committing. This was not a test-suite result. |
| `cd tdf-hq && set -o pipefail; stack test --fast --test-arguments='--match=provider' 2>&1 \| rg --line-buffered -A30 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.Provider\|Test suite'` | Started 02:19 on intermediate source, recompiled test module on `93ae918b...`; completion observed 02:46:37 | 118 examples, 0 failures; 19.7509 seconds, exit 0. Six new pure identity examples; remaining examples inherited. |
| `node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /tmp/tdf-payment-notification-identity-audit.json` | Started 02:22 on implementation commit; report 02:36:21.152, completion observed 02:38:28. Later source changes only extended test variants. | Exit 0; 1,413 files / 1,122 candidates / zero unreviewed. No gate or catalog decision changes. |
| `git diff --check` | Repeatedly through 02:27 | Exit 0. |
| `set -o pipefail; ./scripts/test-provider-execution-runtime-migration.sh 2>&1 \| tail -n 20` | Approved disposable PostgreSQL 16; running by 02:46:07 on `93ae918b...`, unchanged SQL/harness through `f93a70f1...`; completion observed 02:49:09 | Exit 0, “Provider execution migration tests passed”. Repeat application, trust/reference tampering rejection, non-empty rollback refusal, empty rollback, operator-flag preservation and reapplication. |
| `cd tdf-hq && set -o pipefail; stack test --fast --rerun-tests 2>&1 \| rg --line-buffered -A30 -B3 'error:\|Error:\|examples,\|Failures:\|Finished in\|Compiling TDF.Commerce.ProviderRetrySpec\|Test suite'` | Final `f93a70f1...`; started after 02:47:31, completion observed before 02:58 | 2,589 examples, 0 failures; 104.5880 seconds, exit 0. Database-test environment unset; database cases are separate. |
| `set -o pipefail; sh scripts/test-provider-retry-runtime.sh 2>&1 \| tail -n 50` | Final `f93a70f1...`; approved disposable PostgreSQL 16 harness running by 02:58:41, completion observed 03:02:18 | 45 examples, 0 failures; 33.8249 seconds, exit 0. Includes nine new identity/handler/database cases and 36 inherited cases; no provider request. |

Migration-container read-back at 02:49:09 with `docker ps -a --filter
name=tdf-provider-execution-migration --format '{{.Names}} {{.Status}}'` returned
no rows (exit 0). Only the harness's synthetic container was removed. This was
the existing focused provider-execution fixture, not the full production-schema
rehearsal reported by #349 on its separate branch.

Callback-container read-back after 03:02:18 with `docker ps -a --filter
name=tdf-provider-retry --format '{{.Names}} {{.Status}}'` also returned no rows,
exit 0. Both harnesses removed only their own disposable test containers.

The final full backend build/test and disposable PostgreSQL callback suite passed.
Existing warnings were not suppressed; new partial-list/shadowing warnings were
removed in test-only follow-ups. The 118-example focused suite overlaps the full
backend suite; it is not 118 additional independent tests.

## Coverage and boundaries

Six added pure examples cover the stable digest golden, unsigned/format variants,
signed-field separation, unambiguous tuple identity, malformed inputs and the
separation between identity and authentication. Nine added database examples
cover untrusted-path rejection, concurrent deduplication/claims, terminal replay,
environment/merchant scoping, out-of-order signed states, immutable conflicts,
legacy uppercase projection, bounded historical cutover and the real Servant
handler's original-body signature check. The handler test invokes the application
handler with PostgreSQL, **not** a listening HTTP server or provider callback.

Existing PayPal/PayPhone minimization, retry/reconciliation, Datafast recovery and
shared transport cases remain in the same suites. No SQL, API DTO, generated
client, frontend or mobile source changed. No new database migration is required;
the database harness applied and exercised the inherited schema.

Unexecuted: credentialed provider sandbox/end-to-end tests, staging payments,
native-device/browser tests, new settlement/recurring/payout workflows and a
production migration or rollout. Historical ciphertext is neither inspected nor
rewritten outside explicitly synthetic local test fixtures.

## Delivery and next gates

Review order: **#331 → #332 → #334 → #340 → #343 → #347 → #350 → this branch**.
Staging #344 remains an independent sibling after #343. No merge or production
deployment is authorized. Remote delivery is recorded in the draft PR metadata;
this source report does not equate branch publication with a hosted CI pass.

Required next steps: restore staging authentication; obtain and qualify real
merchant sandbox credentials/method mappings; confirm account SHA-256 activation
and register HTTPS callbacks; execute interrupted/duplicate/out-of-order
reconciliation cases before activation. Preserve original payment attempts and
never infer no-charge evidence from callback identity or a transport failure.
Historical cleanup, recurring `internalReference` notifications, refund/void and
settlement execution, legal/accounting/PCI and compliant seller payouts remain
explicitly outside this increment and are not marked complete.
