# Payment stack integration — 2026-09-16

## Scope and pinned inputs

Integration only; no provider activation, real provider HTTP, deployment, live
transaction, GitHub merge, or production data change.

- Downstream root: draft #408, `365561aecff57e76d38cb0e98217a34f1d642ff4`.
- Upstream payment core: draft #331, pinned
  `35a92e04b1f3f1a7def3f9c126d19cde3d00b5c3`. This contains default main
  `d4bd497df6979f639dcd112e4f60b57f3526b9e0` (verified ancestry).
- Mobile downstream: draft #85, `b9222df77d0a187cca4359278704362ec17fb3af`.
- Mobile main: `2db83046eaf2fccb473f1230dd2433bf9364ec31`.
- Mobile upstream contract integration: `6093556c6f35037cdd4b2071bdc0401bf1d5ad5a`,
  containing #83's `337d46f25a8b7c7c688aa3657ffb599ecb4c47c7` and requester-name
  contract changes. Explicitly fetched and inspected, not inferred from a gitlink.

The upstream branch advanced during inspection; this integration deliberately
pins the inspected revision. Subsequent upstream commits are not claimed tested.
The original dirty `tdf-app` worktree was not edited.

## Reconciled behavior

Keep the downstream method-specific completion/query gates, hosted PlaceToPay
and PayPhone executors, callback protection, canonical capture/refund accounting,
held-refund admin recovery, and immutable provider references. Add upstream:

- Exact nonblank approved merchant binding at attempt creation.
- Recurring capability requirement for subscription routes.
- Merchandise transfer instructions scoped to merchandise; other flows require
  generic commerce instructions.
- Contact-only marketplace coordination creates an unpaid request, not a bank
  transfer, payment evidence, or an invented custody/payout mechanism.
- Environment-separated intent and amount-component aggregation, with visible
  sandbox/production labels on every financial summary family. Older responses
  without the additive fields display an explicit unknown environment.
- CI-service PostgreSQL audit fixture, without exporting a job-wide libpq password.

Both TypeScript clients are generated from the reconciled OpenAPI and compared
byte-for-byte. Mobile main also introduced consumers of the existing Haskell
`ExperimentAssignmentDTO`/`ExperimentExposureResult`; their DTO schemas are now
preserved in canonical OpenAPI. **No experiment endpoints were found in the
current backend routing or added by this integration.** Those upstream mobile
calls remain an unrelated functional dependency, not a verified API capability.

Mobile's newer artist-create test expected broader permission than the root
registry authorizes. Preserve canonical Artist/Admin permission and assert that
Fan can view but cannot create. Do not grant a role solely to make a mobile test
pass. Self-service rollout needs the corresponding reviewed backend policy.
Native route declarations were regenerated locally with Expo's installed
generator; they are ignored development artifacts. The merchandise preference
subject type now derives from the generated API instead of duplicating its enum.

## Refund migration decision and activation gate

Reuse upstream `2026-09-16_payment_intent_refund_sync.sql` unchanged, preserving
its introduction and SQL contents. Keep every prior migration in order and use
main's corrected introduction ancestry. The merged manifest has 108 entries.

Application completion advances the locked canonical intent first, then updates
the refund. The new AFTER-status trigger totals verified succeeded refunds. When
that total already equals the canonical balance it does not append another
history event. Regression assertions count **both** application and SQL refund
event types, so a duplicate history write cannot hide behind a different name.
The 249 real-PostgreSQL regressions run with this new trigger installed, covering
concurrent partial completions, replay, ambiguous holds, drift, disputes, and
transaction rollback.

The migration also explicitly backfills already-succeeded, intent-bound refunds.
It validates provider, checkout, environment, currency, merchant, succeeded
attempt, completed timestamp, and nonblank external refund reference. It rejects
decreasing or over-capture totals and invalid states; it preserves disputed or
chargeback state when updating historical refunded amounts. Unbound legacy
attempts remain untouched. This is internal-record reconciliation, **not new
verification from the provider**. Application recovery still refuses a new refund
completion on a disputed intent; that policy is not relaxed by historical repair.

Before deployment, a qualified operator must inventory affected intent/refund IDs
in an approved environment, compare immutable provider evidence and ledger,
receipt, checkout and settlement totals, rehearse on a production-shaped copy,
review every disagreement, and preserve a backup. Historical provenance must not
be inferred solely from an old `succeeded` label. No production-shaped historical
dataset was available in this integration. Production activation remains blocked
on that review and the existing legal/accounting and merchant qualification gates.

Rollback requires stopped refund writers and uses the upstream rollback script to
remove only the synchronization trigger/functions. It deliberately **retains**
money and audit history; it is not a destructive financial backfill reversal.
Reverting app code alone does not reverse the financial backfill. Never restore
older balances over provider-confirmed refunds.

## Verification environment and commands

All tests below are local or mocked, not provider sandbox or staging evidence.
Host macOS x86_64; Stack lts-24.42/GHC 9.10.3; installed npm lockfile dependencies;
native PostgreSQL 16 on `127.0.0.1:56419`. Synthetic fixture databases only. Native
cluster directory: `/private/tmp/tdf-payment-stack-native-pg.ykLIUh`. No secret
values were read or recorded. Timestamps below are UTC log modification times;
test runners also report durations. Source is the pinned inputs plus the merge
and integration edits in this PR, not an already-deployed commit.

| Check | Exact command / directory | Result and UTC completion evidence |
| --- | --- | --- |
| Baseline held-refund | `stack test --fast --rerun-tests --test-arguments='--match=held-refund'`, `tdf-hq` | 33 examples, 0 failures; 18:05:54; `tdf-payment-stack-sync-baseline-20260916.log` |
| Full backend | `stack test --fast --rerun-tests`, `tdf-hq` | 2,674 examples, 0 failures; 18:36:12; `tdf-payment-stack-integration-backend-20260916.log` |
| Combined provider/refund DB | `TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@127.0.0.1:56419/tdf_provider_retry_test' sh scripts/test-provider-retry-runtime.sh`, root | 249 examples, 0 failures; 19:27:21; `tdf-payment-stack-native-refund-approved-20260916.log` |
| Merchant/contact/report DB | `TDF_PAYMENT_AUDIT_DATABASE_URL='postgresql://postgres@127.0.0.1:56419/tdf_payment_audit_test' COMMERCE_CHECKOUT_ENV=sandbox stack test --fast --test-arguments='--match=payment-audit --fail-on=empty'`, `tdf-hq` | 4 examples, 0 failures; 19:52:14; `tdf-payment-stack-native-audit-approved-20260916.log` |
| Migrations/backfill/rollback | `TDF_CANONICAL_PAYMENT_DATABASE_URL='postgresql://postgres@127.0.0.1:56419/tdf_canonical_payment_test' sh scripts/test-canonical-payment-lifecycle-migration.sh`, root | PASS; 19:55:11; `tdf-payment-stack-native-canonical-20260916.log`; expected constraint/rollback rejection messages are asserted negative cases |
| Native runner negative guards | Repeat the preceding command on the populated DB; separately append `?host=example.invalid` to its URL | Both refuse with exit 1 before writes; existing DB retained |
| Mobile lint + full tests | `npm run lint && npm test`, `tdf-mobile` | Lint pass, 458 tests / 78 suites pass; 19:54:38; `tdf-payment-stack-mobile-rerun-20260916.log` |
| Types | `npm run typecheck`, root | Web and mobile pass after canonical schema regeneration; command then entered the separately recorded first mobile test run |
| Repo quality | `npm run quality:repo`, root | PASS; formal audit 0 critical/errors (390 warnings, 9,424 info); output recorded in execution transcript |

Log basenames above are under `/private/tmp/`. They are local evidence, not hosted
artifacts. Initial native attempts were sandbox-denied; approved reruns above
actually connected and passed. Docker attempts stalled at container creation,
were interrupted, and are **not** migration successes; cleanup clients also
stalled, so Docker-container absence has not been verified. Native fixture setup
checked an empty target before applying `payment_audit_fixture.sql`.

The first full web run failed: 216 suites passed, the 626-test course-admin suite
had 623 failures, 1,466 tests passed overall. Its log also reported a temporary
disk-full error. An isolated course-suite run reproduced a 15-second timeout and
cascading overlapping React `act` failures. The first failing case then passed
alone without changing code (1 pass, 625 intentionally unselected tests, 8.495s).
Do not represent that focused run as a full-suite pass. Full rerun status is
recorded below once completed. No timeouts or assertions were relaxed.

The first mobile integration run had one failing policy assertion (457 passed).
The canonical-permission reconciliation above fixed it; the full 458-test rerun
passed. Earlier local typechecks exposed the missing DTOs and stale route types;
they are failures, not successful verification.

## Remaining external qualification

Existing provider and staging qualification blockers remain as documented in
`held-refund-query-2026-09-16.md`. No new staging access is claimed. The most recent
prior read-only staging check reported healthy public HTTP endpoints but missing
hosting authentication (`FLY_STAGING_API_TOKEN`, `FLY_STAGING_WEB_TOKEN` aliases
or usable authorized Fly login), no verified deployed source, and unqualified
merchant credentials/contracts. Do not substitute production secrets. No native
device checkout, provider sandbox, staging deployment, or real refund was run.

## Review and rollback order

Review updated #331 through the existing dependent root chain ending at #408,
then this integration. Mobile main and #83's integrated ancestry plus #85 precede
the mobile integration; publish its exact commit before the root gitlink.
No PR was merged. Keep all provider execution/recovery flags disabled pending
the runbooks' sandbox, account, contract and environment evidence.

## Published integration and supplemental checks

- Root implementation merge: `60fe0aea06b9153b953005290ecc8ba9659c04aa`;
  [draft #414](https://github.com/diegueins680/tdf-app/pull/414), base #408.
- Mobile final code: `ff77c2060034762f69aed81473c8f94a25684d7e`;
  [draft #88](https://github.com/diegueins680/TDF-mobile/pull/88), base #85.
  Both remote heads, draft status and bases were read back after publication.
- Migration ID/path order comparison: all 107 downstream, 106 upstream and 102
  default-main entries retained in the 108-entry merged manifest. Existing SQL
  migrations are unchanged; only the new forward/rollback pair is added.
- Strict catalog JSON/CSV audits: PASS, 1,458 scanned files / 1,137 candidates,
  zero unreviewed and zero stale decisions. Regeneration preserves reviewed
  security/business classifications; deriving the new mobile subject type
  eliminates the redundant handwritten enum.
- `npm run test:production-release && npm run test:ci-pipeline`: 60 + 23 tests
  PASS at the implementation merge; output in the execution transcript.
- `npm run lint`, `tdf-hq-ui`: PASS, zero warnings; command output in
  `/private/tmp/tdf-payment-stack-web-lint-20260916.log`. Exit 0 was observed before
  2026-09-16T20:06:07Z (the log timestamp reflects startup, not completion).
- `npm run build`, `tdf-hq-ui`: PASS at 19:58:03Z; TypeScript/Vite plus bundle
  gate, five preloads / 314,065 gzip initial JS bytes. Existing chunk-size warning
  remains; no budget was relaxed. Log: `tdf-payment-stack-web-build-20260916.log`.
- `npm test -- --runTestsByPath src/pages/CourseRegistrationsAdminPage.test.tsx`,
  `tdf-hq-ui`: unchanged complete rerun PASS, 626 tests / one suite, 169.535s,
  19:59:08Z. Log: `tdf-payment-stack-course-rerun-20260916.log`.
- `npm test -- --runTestsByPath src/components/payments/HeldRefundRecoveryPanel.test.tsx src/pages/CommerceProviderEventsPage.test.tsx src/api/commerceOperations.test.ts src/pages/AccessRequestsPage.test.tsx`,
  `tdf-hq-ui`: 41 tests / four suites PASS, 26.413s, 19:59:45Z. Log:
  `tdf-payment-stack-focused-web-20260916.log`.
- Native PostgreSQL shutdown completed; `pg_ctl status` subsequently reported
  no server running. Fixture files were retained, not erased.

The latest full-web rerun (`npm test`, `tdf-hq-ui`) and hosted CI were still
pending when these supplemental results were assembled. The following final
verification entry, when present, supersedes only that pending status and does
not erase the earlier failed attempts.

### Final hosted web evidence and local interruption

The hosted [web-quality job](https://github.com/diegueins680/tdf-app/actions/runs/35144435997/job/104956908672)
passed at **2026-09-16T20:12:35Z**. Its actual log records **217 suites / 2,089
tests passed**, 203.867s, at 20:11:49Z, followed by a successful build/bundle gate
(five preloads, 314,884 gzip initial bytes). The workflow API confirms
`head_sha=60fe0aea06b9153b953005290ecc8ba9659c04aa`; this is the tested code
integration, not a different branch or mock provider environment.

The same [CI run](https://github.com/diegueins680/tdf-app/actions/runs/35144435997)
also reported successful mobile quality, API contracts, migration tests,
production-migration contract checks, repository quality, and persona browser
journeys. Catalog authority passed in its separate workflow. Backend quality
was still in progress at the last observation; do not call the aggregate run
complete. These browser journeys do not constitute provider sandbox checkout.
Automatic preview checks reported success, but no preview payment flow or
deployed source/environment qualification was performed by this integration.

After verifying the complete hosted result, stop the duplicate slow local web
run: exit **143**, last log write **20:18:57Z**. It had recorded 82 passing and
four failing suites, with timeout failures; it is **not a local full-suite pass**.
Keep `/private/tmp/tdf-payment-stack-web-final-20260916.log` alongside the earlier
failed logs. No test assertions, timeout thresholds, or quality gates were
relaxed. The subsequent root commit only records this evidence; new hosted
checks on that documentation-only head may still be pending.
