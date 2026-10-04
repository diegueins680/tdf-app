# PR 31 — private guarded completion of preparation tasks

## Scope and dependency

Branch `feat/event-task-completion-command`, dependent on draft #403 at
`6047377348e901c919d7691b849faab30980b952`. The worktree started clean; unrelated
worktrees were not edited. Parent #403's [persona job](https://github.com/diegueins680/tdf-app/actions/runs/35122462345/job/104883660614)
now reports **109 passed / 12 existing skips**, including both RACI themes and
the WebKit negative contrast control. This does not establish the earlier
contrast failure's cause. Its repository/formal/PostgreSQL gates passed;
UI/mobile/backend jobs were skipped for its test-only diff, not newly executed.
Catalog governance remains open and is not waived by this increment.

This advances phase 4 with **one private database command**, not a complete task
workflow or public endpoint. It reuses `event_logistics_activity`, dependency and
RACI rows, strict opt-in policy, scoped task authority, the event write fence,
aggregate revisions, immutable audit and the existing command receipt ledger.
No new table, task identity, role, provider, financial operation or production
activation. Existing routes and generated clients are unchanged.

## Contract and formal evidence

The [pre-implementation contract](task-completion-contract.md) specifies TC01–07:
exact input/target binding, current scoped authority, post-lock time checks,
task-scoped idempotency, supported lifecycle, completed dependencies, current A/R,
and atomic status/version/audit/receipt effects. It links each guard to the
bounded model and complementary existing Alloy relations.

Before feature SQL, the full pinned formal suite passed: **24 positive TLC
configurations, 60 named negative controls, 13 PlusCal integrity tests, 2 SAT
Alloy scenarios, 13 UNSAT assertions**. The new model explored **944,014 generated /
231,576 distinct states, depth 14**. No fairness/liveness or unbounded proof claim.
All seven new mutations triggered the intended invariant. The earlier incomplete
successor due to expression precedence was corrected and rerun, not accepted.
Exact tool paths, command, bounds and limitations are in the contract.

## Implementation, security and compatibility

`2026-09-16_event_task_completion.sql` defines `event_operation_complete_task` as
SECURITY INVOKER, fixed search path and no PUBLIC execution. It requires the
existing disabled-by-default event API flag. Only draft/planning preparation work
with planned/confirmed status and both task-policy guards is supported.

The command locks existing authorization and task-write fences, task revision,
activity and actor identity before evaluating current authorization and RACI.
Dependencies are read under the common event write fence. Missing/foreign/hidden
targets remain `not_found`; blocked prerequisites produce a generic error, not
their IDs or private data. Ambient event.manage, assignment and coproduction are
not new task completion authority. Owner/exact current task.manage are required.

Exact same-actor/content retries return the original receipt after current read
authorization, before status/version validation; read-only downgrades can retrieve
history, revoked readers cannot. New commands cannot complete an already-completed
task. Different task/operation namespaces may independently use the same UUID.
There is **no override path**, even if a trusted override row exists: prerequisites
must be completed when this command decides. Existing deferred guards stay active.

One accepted command sets completed status, advances legacy and aggregate versions
once, updates the existing timestamp, and appends one audit and one receipt. All
other activity fields, RACI, permissions, dependencies and financial data stay
unchanged. Database errors and outer transaction aborts roll everything back.
Receipt availability before SQL COMMIT is not a durability guarantee.

## Executed verification (2026-09-16)

```sh
sh scripts/test-event-task-completion-migration.sh
bash scripts/test-event-operations-schema-rehearsal.sh
node --test scripts/__tests__/event-task-completion-runner.test.mjs \
  scripts/__tests__/event-operations-schema-rehearsal.test.mjs \
  scripts/__tests__/event-raci-reassignment-runner.test.mjs \
  scripts/__tests__/ci-change-scope.test.mjs scripts/__tests__/ci-pipeline.test.mjs
```

- First complete PostgreSQL 16 run passed: exact receipts/history, scoped privacy,
  rejected inputs, same-key replay, 32 exhaustive five-guard decision cases,
  audit/receipt fault injection, integer/aggregate overflow, outer abort, down/up,
  12 observed two-writer races (retry/competing/legacy/dependency across READ
  COMMITTED, REPEATABLE READ and SERIALIZABLE), four expiry waits (metadata,
  activity, actor Party, timed RACI), both revocation orders and aborted-first
  retry recovery. Barriers assert exact blocking PIDs, not inferred timing.
- The authoritative complete-schema PostgreSQL 17 rehearsal passed: actual
  blocked prerequisite rejection, completion/exact replay, preservation of all
  other activity fields, constraint commit, outer rollback, full migration
  rollback/reapply and final repository schema verification. The production
  ledger and activation settings were unchanged. No diagnostic workaround.
- The expanded reduced-schema suite adds draft/confirmed work, future A,
  weak-policy rejection, expired-RACI historical replay, cross-operation key
  separation and explicit audit/receipt immutability checks. Its first rerun
  failed because the four-second grant window elapsed before the command could
  start, not because an expired command was accepted. Coordinator setup now
  precedes a 15-second validity window; the exact blocking PID, valid command
  start, actual expiry and post-expiry rejection remain mandatory. The final
  rerun **passed the complete suite**, including all 12 isolation races and four
  observed expiry waits; retained local logs: `tdf-task-completion.q8g05J`.
- All **36 combined runner/schema/RACI/CI regression tests passed**, including
  checks that expiry orchestration preserves its causal assertions.
- Shell syntax and workflow YAML parsing passed. The initial sandbox could not
  access Docker; actual DB tests used approved runtime access. Containers have
  no network or published ports and accept no caller database target. Only their
  owned disposable databases are removed on exit; diagnostic logs are retained.
- Before creating the runner/workflow, its tests gave the expected two failures
  (missing runner and CI job) and one passing formal-wiring check. Twelve owning
  runner/schema/RACI checks passed after implementation.

No Haskell/UI/mobile build or new browser run was performed for this database-only
increment. No API/OpenAPI/generated client changed; a future HTTP PR must compose
the existing session fence and validate the bound receipt within the transaction.
Hosted CI on this increment is not inferred from parent or local results.

## Rollback and limitations

The migration is intentionally absent from `scripts/production-migrations.json`.
Rollback removes only the private function. It never reopens completed tasks or
deletes versions, receipts or immutable audit. Reapply retains accepted keys.
Quiesce future callers before rollback; none is exposed in this increment.

Task completion HTTP/client/editor, acceptance criteria/evidence, approver workflow,
authorized overrides, reopening, live-event effects, notifications, collaborator
removal, durable offline recovery and native mobile remain future dependent work.
The bounded model is not a proof of PostgreSQL or all event operations. API/provider
activation, deployments, merges and real-money operations remain unauthorized.
