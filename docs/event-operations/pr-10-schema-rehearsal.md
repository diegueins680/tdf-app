# PR draft: Complete-schema event migration rehearsal

## Scope and dependency

Base: `fix/event-session-transaction-fence`, draft PR 348 at
`3cd978045aa406c55512a6673c2ba5f2983ebc7c`. Branch: `test/event-operations-schema-rehearsal`.
EO-008/EO-023–028/EO-055–056/EO-058: exercise existing event migrations against the repository's full
schema fixture and authoritative migration batch. No new production feature, migration, manifest
entry, API or permission. Existing bounded `EventLifecycle`, `TaskRaci`, `TaskCommit`, `ReceiptReplay`
and Alloy checks remain applicable; these concrete integration tests are not another formal proof.

## Isolation and executable contract

`npm run test:event-operations-schema-rehearsal` owns a disposable `pgvector/pgvector:pg17` container
(the same PostgreSQL major/image family as broad CI), with `--network none`, no published port and
no caller-supplied DSN, image, env file or server. SQL uses `docker exec`; cleanup targets only the
returned container ID. No backend, provider, discovery/notification worker or production entrypoint
starts. No production data is fetched.

It loads `production-schema-20260814.sql`, the catalog source fixture and the unaltered production
migration renderer. The strict default exits on failure. Hosted `postgres-complete-schema` invokes
ONLY that default, without `continue-on-error`. Changes to SQL, manifest, renderers or fixtures
trigger verification. The event assertions cover:

- Canonical catalog identity, organization-owner mapping, unresolved-owner evidence, disabled flags,
  repeat application and unchanged legacy columns/rows/production ledger.
- Real transition SQL, exact retry metadata, one transition/receipt/audit and private snapshot denial.
- Deferred blocked-completion/orphan/cycle rejection in existing logistics tables.
- Reverse-order rollback, exact receipt/transition/audit/RACI preservation, restored guards and
  no silent reactivation or business-version reset after reapply.
- The mandatory full-schema contract after the event-specific assertions.

## Observed blockers, not waived failures

The strict baseline failed with PostgreSQL exit 3 BEFORE event migrations:
`Merch reputation requires the canonical artist merch storefront migration`.
`scripts/production-migrations.json` includes `2026-09-08_merch_reputation` but omits the existing
`2026-09-07_artist_merch_storefronts`. The focused merchandising runner applies that prerequisite
first; reputation explicitly checks six required tables at lines 257–265. Remote `main` was read
back at unchanged audited `17a33eca11d585d84435af85340beece9b51d14e`; the omission predates this chain.

An explicit diagnostic option, never selected by npm/CI, can supply ONLY that existing prerequisite
inside a fresh disposable database:

```sh
bash scripts/test-event-operations-schema-rehearsal.sh --diagnostic-missing-merch-prerequisite
```

It first observes the unaltered baseline failure, verifies all six prerequisite tables are absent,
applies the existing prerequisite, and reruns the same manifest. This exposed another failure:
`Provider event/refund runtime columns are missing or invalid`, in the embedded schema contract
from `scripts/lib/production-release.mjs`. Diagnostic event assertions may proceed only if every
manifest entry was recorded. Final full-schema verification remains mandatory and its failure
still produces a nonzero exit. A supplemented fixture is NOT a passing authoritative rehearsal.
Neither the manifest nor the provider checks were changed.

Static inspection also identifies a missing payment dependency: that column-contract block expects
`commerce_payment_attempt.payment_intent_id` (`uuid`, nullable). The existing
`2026-09-10_payment_attempt_intent_binding.sql` adds it and references `commerce_payment_intent`,
but neither that migration nor `2026-09-09_canonical_payment_lifecycle.sql` is registered in this
manifest. This identifies an additional manifest/contract mismatch; it is not a claim that adding
those entries alone repairs every remaining schema check. No financial migration was supplied by
the diagnostic option.

## Verification checkpoint

- Strict full-schema reproduction: exit 3 at the missing prerequisite; not a pass.
- Initial supplemented diagnostic: exit 3 at the provider/refund schema contract; not a pass.
- `npm run test:event-operations-schema-runner`: 4/4 passed (argument rejection before Docker,
  isolated owned database, fatal default failure and strict hosted invocation).
- `bash -n scripts/test-event-operations-schema-rehearsal.sh`: passed.
- Workflow YAML and trigger/command checks passed; all 33 relative links in the touched index,
  plan, matrix and PR document resolve. Read-back confirmed no rehearsal containers remain.
- Post-staging `npm run verify:formal`: passed with 0 critical/errors and 354 advisory warnings;
  this is the repository heuristic audit, not a fresh TLC/Alloy run. No formal model changed.
- The first `quality:repo` run caught an old `setup-node` action major in the new job. It was
  corrected to the existing required `v7`, without changing the assertion; focused
  `npm run test:ci-pipeline` then passed all 21 tests. The subsequent complete
  `npm run quality:repo` rerun passed (exit 0), including the existing 60 release tests.
- Final strict rehearsal reproduced the missing-prerequisite failure again (exit 3), before
  event SQL. Neither the green repository checks nor the diagnostic mode waive that failure.
- The later supplemented diagnostic exited 1 at the complete-ledger check: the embedded contract
  failure also prevents recording the entire manifest. **No event seed, event migration or event
  rollback/reapply assertion in this new runner has executed.** Those SQL files are prepared tests,
  not verified results. Existing reduced-fixture passes do not close this full-schema gate.
- Parent's `stack test tdf-hq --fast --no-run-tests --no-terminal` remains in progress. The earlier
  focused 36-case suite is not a full backend build or full test result.

## Rollback and remaining gates

Revert this test/CI/documentation increment to remove the additional rehearsal. Existing production
code, tests, migrations and flags remain unchanged. No merge, manual deployment, SQL against production,
credentials, provider activation or real-money operation was performed.

Reconcile the missing dependency and provider schema contract through their owning reviewed
migration work, then rerun the strict default to completion. Do not invent applied ledger records,
relax the schema contract or select diagnostic mode in a release/CI gate. Full application, mobile,
offline and later event product phases remain pending; this is not production readiness.
