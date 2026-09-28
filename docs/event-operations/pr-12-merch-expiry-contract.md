# PR draft: Verify the existing canonical checkout expiry contract

## Scope and dependency

Base: `fix/event-schema-migration-dependencies`, draft PR 351 at
`192f9578665109e52d28a96c772e3307bdc684e5`. Branch: `test/merch-expiry-checkout-contract`.
This closes the owning storefront test discrepancy documented by that prerequisite PR. It does
not implement event-specific payments, change a provider, or alter any production SQL/checksum.

EO-033–036/EO-041–044/EO-055–058 trace to the [executable expiry contract](merch-expiry-test-contract.md),
MX-01–05 and `tdf-hq/test/integration/merch_checkout_expiry_assertions.sql`. The existing finite
`ReservationRace`, `ContractPayment` and `OperationalLiveness` abstractions remain applicable at
their documented bounds; SQL timestamp/count/projection checks are concrete tests, not an
unbounded proof or complete implementation of event booking/finance.

## Evidence for correcting the expectation

The old owning test expected one result because only one stock reservation survived the competing
requests. However, the fixture has **two** eligible expiring checkouts. The unchanged production
function explicitly returns the number of updated checkouts, and the unchanged worker publishes
that number as `expiredCheckouts`. The original script failed again on this branch before its
expiry assertion was replaced (exit 1); the prior PR's traced run established `released=2` versus
`test 2 = 1`. This is a stale test expectation, not a reason to rewrite checksum-pinned business SQL.

The replacement does more than adjust a scalar. It asserts the fixture shape, the instant one
microsecond before expiry, the exact expiry boundary, both failed order projections, exactly one
released inventory/canonical hold unit, preservation of the paid checkout and four consumed units,
complete projection/history equality on same/later retries, and rejection of a fresh reservation
on an expired checkout. Eight table snapshots include versions, timestamps and payment/audit
evidence. Statement results are captured before checking postconditions; null counts fail closed.
Only the exact expected eligibility exception is accepted by the negative reservation test.

The existing downstream stock, commission, refund, dispute, settlement, privacy, evidence-preserving
rollback, clean rollback and reapply assertions are unchanged. The temporary snapshot function
lives only in `pg_temp` of the disposable test session. No helper or test data enters production.

## CI integration

The owning storefront migration script was not selected or invoked by the migration CI job.
Two regressions first failed on this omission, then passed after adding:

- Exact path selection for the owning runner and its SQL assertion file in `ci-change-scope.mjs`.
- An invocation of the existing runner in `migration-tests`, with no `continue-on-error` or gate bypass.

The existing workflow's all-scope fallback for pipeline edits, Stack requirements and aggregate
checks remain unchanged. Hosted checks must execute on the published head before merge readiness
can be claimed; local workflow assertions are not hosted CI results.

## Verification checkpoint

- Original `./scripts/test-artist-merch-storefronts-migration.sh`: exit 1 at the stale expiry count.
- Corrected full owning suite: **exit 0** on disposable PostgreSQL 16, including MX-01–05 and all
  previously unreachable refund/dispute/settlement/rollback/reapply checks. No live provider calls.
- `npm run test:ci-pipeline`: **23/23 passed** after both new regressions had reproduced the old gaps.
- `sh -n scripts/test-artist-merch-storefronts-migration.sh`: passed.
- `git diff --cached --check`: passed; 45 relative documentation links resolved and read-back
  confirmed no owned storefront test containers remain. Production SQL, backend modules and the
  manifest have no diff from PR 351.
- Final `npm run quality:repo`: **exit 0**, including internship 8, loop 42, heuristic audit tests 4,
  release 61, CI contracts 23, visual-artifact tests 2 and persona-program tests 3. The latter
  validate metadata/program contracts, not newly captured screenshots or browser persona journeys.
- Staged `npm run verify:formal`: exit 0, 0 critical/errors and 354 advisory warnings. This is the
  repository heuristic audit, not a substitute for the separately executed TLC/Alloy checks.
- Pinned TLC/Alloy rerun: **exit 0**; all positive TLC configurations, all 16 expected mutation
  counterexamples, one SAT Alloy scenario and eight UNSAT assertions completed within the unchanged
  documented finite scopes. No model, fairness assumption or production implementation was changed.
- Parent `stack test tdf-hq --fast --no-run-tests --no-terminal`: still running; a compile-only
  command is not evidence of a full runtime test pass.

The formal rerun uses the unchanged documented scopes and the same pinned tools as the parent:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

## Rollback, security and limitations

Reverting this test/CI/documentation increment is code-only rollback; no schema or user data needs
rollback. Flags and provider credentials are unchanged. The parent full-schema pass remains valid
evidence at its own checkpoint, not a fresh full-schema run in this PR.

The fixture does not model all checkout states, simultaneous expiry workers, live provider behavior
or late verified-payment/expiry races. The latter needs a separate stock/financial reconciliation
review before provider activation; evidence-gated `paid` transitions alone do not prove resource
availability after expiration. Existing broader web/mobile/offline and event hiring/settlement
gaps remain. Legal/accounting and sandbox provider reviews are still mandatory.

No merge, deployment, production SQL, credential change, payment activation, live charge/refund/
payout or branch-protection override was performed.
