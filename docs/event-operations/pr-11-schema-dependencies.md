# PR draft: Restore canonical schema migration dependencies

## Scope and dependency

Base: `test/event-operations-schema-rehearsal`, draft PR 349 at
`cf591e1faa975b8023c6c06892fb50a51ba06dff`. Branch: `fix/event-schema-migration-dependencies`.
EO-008/EO-041–044/EO-055–056/EO-058–061: repair the authoritative manifest omissions exposed by
the strict full-schema rehearsal. This is compatibility work, not a second event/payment system
or completion of event settlement. No new business SQL, API, UI, provider adapter or permission.

The manifest registers four **existing, unchanged** migrations:

| Migration | Dependency / reason | Immutable source anchor |
|---|---|---|
| `2026-09-07_artist_merch_storefronts` | After directory/checkout/refund foundations, before `2026-09-08_merch_reputation`, which requires its six canonical tables | `8bca78e512577e74a6404290188742a226430856` |
| `2026-09-09_canonical_payment_lifecycle` | After checkout/refund foundations; creates canonical intents and disabled provider accounts | `b3007f580fc51afb03aa73d339e9945232adcc8e` |
| `2026-09-10_payment_attempt_intent_binding` | After canonical intents; supplies the nullable UUID/FK required by the existing full-schema contract | `709036630fcad32102b67a34996c4da73bc4d128` |
| `2026-09-06_user_onboarding_progress` | References canonical `party`; supplies the table/index/eight validated constraints required by the existing final schema check | `f24ec8cde83c2f50d569cd52ca164c2ca265dd7a` |

`git merge-base --is-ancestor <anchor> HEAD` succeeded for all four. `git show <anchor>:<path>`
matched each worktree SQL file byte-for-byte. Their SHA-256 values, in table order, are:

```text
699100a59dc180e12cc0c13d5053a6aab002c9ff891672411ac6ea3d67f771db
c9c77ac7f1f8d4fe2efd8ad72dd76c9609cf5c2b93d7704af83400d2acf8e506
31167db71edf0be5c3e391858c5be140ff2a9497b1701863b5e549e93d620016
2c84193c29966646de029c27b0ff1a29d231ebafa9db77e56d42b6d4c753ad57
```

The original manifest has 95 entries but only 94 unique IDs: `2026-09-01_contextual_reputation`
appears twice with identical path and introduction SHA. The candidate retains its first occurrence
and removes only the redundant second registration. All 94 unique prior migrations, their metadata/
checksum allowances and relative order remain; four registrations bring the candidate to 98 unique
entries. No historical ledger row is fabricated, deleted or rewritten. Event
operations migrations remain outside the production manifest and their API flag remains off.

## Formal and executable contracts

The existing [bounded models](../../formal/event-operations/README.md) were rerun successfully
**before changing the manifest** on 2026-09-14. Exact local command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Exit 0: positive TLC configurations, all 16 expected mutation counterexamples, one SAT Alloy
scenario and eight UNSAT assertions in their documented scopes. The sandbox initially blocked
TLC's local RMI socket; the approved rerun outside that restriction completed. No configuration,
scope, fairness assumption or assertion was weakened. These models do not prove the migration
engine or every legacy commerce behavior; concrete PostgreSQL tests remain independent gates.

`event schema prerequisites are registered once in dependency order` in
`scripts/__tests__/production-release.test.mjs` was executed before the manifest change and failed
on the missing storefront registration. The first three entries cleared the earlier blockers but
the strict rehearsal then failed with `Account-bound onboarding progress schema is missing or
incomplete`, before event SQL. The regression was extended, observed failing on the absent
onboarding entry, and that existing migration was registered too. The test checks unique
registration, canonical paths and six
dependency edges. The strict rehearsal also retries the authoritative batch and compares the full
ledger (including timestamps/checksums), then checks disabled/unverified provider accounts and
production payment flags. Event apply/rollback/reapply snapshots now include canonical financial
rows and revenue/provider settings, in addition to legacy event records.

After restoring the four omissions, the schema contract succeeded but the strict ledger-count
gate still rejected 98 rows for 99 manifest entries. A new global manifest uniqueness assertion
reproduced the duplicate ID (`98 !== 99`). This is corrected by consolidating the identical entry,
not by weakening the ledger cardinality check or inserting another ledger row.

## Verification checkpoint

- Pinned TLC/Alloy runner: passed as described above.
- `npm run test:event-operations-schema-runner`: 4/4 passed; no diagnostic mode is selected by CI.
- `./scripts/test-canonical-payment-lifecycle-migration.sh`: exit 0 on disposable PostgreSQL 16;
  apply twice, clean rollback/reapply, disabled providers, missing-intent rejection, exact amounts,
  immutable provider evidence and payout separation of duties. Both evidence-bearing rollbacks
  intentionally raise errors, are asserted as refusals, and preserve evidence. No provider request.
- `sh scripts/test-user-onboarding-progress-migration.sh`: exit 0 on disposable PostgreSQL 16;
  forward/repeat application, eight validated constraints, index, invalid intent/chronology and
  duplicate-party rejection, intended party-deletion cascade, history-preserving rollback/reapply.
  Direct execution was unavailable because this existing script is non-executable; `sh` was used
  without changing its mode. Docker socket access required the approved unsandboxed rerun.
- `./scripts/test-artist-merch-storefronts-migration.sh`: exit 1, reproduced using `bash -x`.
  At line 315, expiry returns `2`, while the test expects `1`. The existing function at SQL lines
  935–953 counts expired **checkouts**; the fixture has two expiring checkouts but one successful
  stock reservation. Payment evidence gating and the competing reservation checks were reached;
  later refund/rollback checks were **not** reached. Neither function nor assertion is changed here.
  This owning-domain test discrepancy remains a release blocker, not a waived failure.
- Release regression suite: 61/61 passed with the first three registrations; `quality:repo` also
  passed with all four registrations before the duplicate-ID assertion was added.
- Final `npm run quality:repo`: exit 0 after deduplication and the new uniqueness assertion;
  internship 8, loop 42, heuristic audit tests 4, release 61, CI contracts 21, visual-artifact tests 2
  and persona-program tests 3 all passed. These artifact/program checks are not browser screenshots
  or actual persona E2E executions.
- Full-schema rehearsal with the first three entries: exit 3 at the missing onboarding contract;
  no event SQL executed in that run. With four entries, exit 1 at the duplicate-derived ledger
  count mismatch; event SQL still did not run. A second run printed no missing or unexpected IDs,
  confirming set equality but a duplicate manifest entry.
- **Final strict `bash scripts/test-event-operations-schema-rehearsal.sh`: exit 0** with the
  deduplicated 98-entry manifest, without the diagnostic option. The full-schema contract passed
  before event SQL; authoritative batch retry preserved every ledger field. All event seed,
  apply-twice, real-command/retry/privacy, blocked-task/orphan/cycle, reverse rollback, immutable
  history, reapply and final schema/ledger assertions executed and passed. Provider accounts and
  checked production flags remained disabled; canonical financial/settings snapshots were retained.
  This is a synthetic full-schema fixture, not a copy of production data or a provider sandbox E2E.
  No claims are made about preservation of populated financial history beyond the owning payment
  suite: the broad fixture's payment/refund rows are empty, while provider/settings rows are populated.
- Both manifest regressions now pass (2/2), including global unique IDs. The staged repository
  heuristic `npm run verify:formal` passed with 0 critical/errors and 354 advisory warnings; this
  is separate from the completed TLC/Alloy execution. All 39 touched documentation links resolve;
  `git diff --check` and shell syntax validation passed. Read-back found no remaining containers
  belonging to the completed schema, payment, merch or onboarding runners.
- Parent's full `stack test tdf-hq --fast --no-run-tests --no-terminal`: still running. This command
  only compiles tests; it cannot establish a full runtime test pass.

## Security, migration and rollback effects

Future application of this manifest would create the existing merch/canonical-finance structures,
install existing merch-specific checkout/refund/dispute triggers, expand provider allowlist checks,
add/validate the nullable attempt-to-intent FK, and persist per-party onboarding progress without
backfilling historical accounts. These are meaningful release-time schema and
locking changes even though no SQL content changes here. Seeds use disabled defaults and
`ON CONFLICT DO NOTHING`; existing explicit settings are not overwritten. Thus a real environment's
pre-existing flags/accounts still require a separate preflight, rather than assuming all are off.

Before any future release, review the failed storefront test, existing commerce consumers, ledger
checksums, table sizes/constraint-validation locks, backup/restore plan and provider settings.
Unverified providers must stay disabled; legal/accounting and sandbox provider review remain
mandatory. Generated contract terms are not legal advice.

Before application, reverting this manifest/test increment is code-only rollback. After application,
removing entries does **not** undo the schema: preserve ledger/evidence and prefer a reviewed forward
repair. Existing binding/lifecycle rollback scripts refuse to erase financial evidence; storefront
rollback also refuses commercial evidence and has downstream reputation dependencies. Do not run
those blindly against a used/full schema or delete ledger rows to force reapplication. Onboarding's
existing rollback deliberately retains its additive table/history. The event
rollback in the isolated rehearsal is a separate, non-destructive history-preserving contract.

No merge, deployment, production SQL, credential change, account activation, live charge/refund/
payout, provider API call or destructive migration against user data was performed. Full web/mobile,
offline synchronization and the remaining event product phases are still incomplete.
