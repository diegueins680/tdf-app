# PR draft: Task final-state and concurrency invariants

## Scope and dependency

Depends on `feat/event-logistics-transaction-hardening` (`f500b62c4`). Extends the existing
logistics/task-policy/RACI tables rather than introducing another task domain. This is a
phase-4 safety correction, not the complete RACI API or workspace.

## Requirements, models and implementation

EO-023–028 / EO-055: completion/dependency consistency, non-orphan responsibility cardinality,
deterministic concurrent outcomes and all-or-nothing writes. See [the operation contract](task-commit-contract.md).

The old BEFORE-status trigger accepted a transaction that completed a task and then inserted a
pending dependency. `TaskCommit` models the prepare/commit boundary and catches that bug and RACI
write skew with two intentional negative-control configurations. Final-state validation and
serialization pass in the finite scope before implementation; existing TLC and Alloy checks pass.

`2026-09-14_event_task_commit.sql` replaces that early trigger with deferred constraint triggers
on the four existing task-related tables and a shared per-event write-fence row. The fence is
updated, not merely advisory-locked, so stale RR/SERIALIZABLE snapshots fail rather than silently
using stale responsibility counts. Checks also work with constraints set to immediate. Generic
policy weakening/removal and protected task deletion/movement fail closed.

## Schema, security, compatibility and rollback

- One additive write-fence table; no API/DTO/generated-client/Haskell/UI changes.
- Invalid existing opted-in tasks abort the migration; no silent backfill or downgrade.
- The migration is not in the production manifest. All production activation remains prohibited.
- Only opted-in tasks receive completion/RACI policies. All event-logistics writers participate
  in the fence, including prerequisite changes. Large event plans may contend; performance and
  lock-timeout tuning remain unverified and are required before production rollout.
- No new permission is granted. Database override insertion still requires trusted authority;
  its future API must enforce contextual authorization and immutable audit explicitly.
- Rollback twice/reapply tested. Rollback retains data and history but restores the old weaker
  trigger; quiesce writes and prefer a corrected forward migration. Roll back in reverse order.

## Executed local verification

- Full pinned `scripts/verify-event-operations-formal.sh`: PASS. New positive model: 121 generated,
  112 distinct states, depth 5. Both mutation configurations yield expected named counterexamples.
  Existing Alloy scenario SAT; all 8 assertion checks UNSAT within documented scopes.
- `sh scripts/test-event-task-commit-migration.sh --foundation-only`: expected exit 1,
  demonstrating the old SQL accepts the invalid completion/dependency transaction.
- `sh scripts/test-event-task-commit-migration.sh`: PASS on disposable PostgreSQL 16: atomic final
  state, immediate constraints, protected-policy bypasses, RACI races in all three isolation
  levels, status/dependency races in both orders, exact-version overrides, immutable override,
  apply twice, rollback twice, incompatible-data migration rejection and reapply.
- Foundation and lifecycle-API migration suites: PASS again on disposable PostgreSQL 16.
- Shell syntax, JSON, workflow YAML and whitespace checks: PASS.
- No new HTTP/E2E/mobile/browser/accessibility/payment tests or Haskell compilation in this PR.

The initial concurrency test used a fixed delay and, on a slow run, observed safe rejection
`23514` instead of the expected stale-snapshot `40001`. It was replaced with advisory-lock
barriers that observe both writers waiting before releasing the first; the final suite passed.
This changes the synchronization evidence, not the allowed production invariant.

## Remaining limitations

RACI contextual grants and validity windows, collaborator removal, task evidence/history APIs,
policy administration, templates, recurrence, readiness and workspace views remain incomplete.
The legacy HTTP handler does not yet map these new database rejections to the intended Spanish
conflict UI; do not enable the sidecar in production. No end-to-end completion claim is made.
