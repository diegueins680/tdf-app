# PR 13 — scoped canonical task/RACI database projection

Base: `test/merch-expiry-checkout-contract` (draft PR 352).
Branch: `feat/event-task-read-projection`. Dependency-ordered draft; do not merge or deploy.

## Scope and evidence-backed decision

The existing task identity is `event_logistics_activity`, not a new event/task system.
Existing `event_operation_task_policy`, RACI assignments and resource-scoped grants supply
the remaining relations. The audit found that `event_operation_actor_can_read` accepts any
event capability for the minimal lifecycle snapshot. Reusing it to disclose tasks would
widen event.read/finance grants into task access. The new task-specific predicate only
accepts current ownership, event-level task.read/manage or the exact event/task grant.

[TR-01–08](task-read-contract.md) and the new TLC/Alloy models were written and checked
successfully **before** the SQL implementation. The new read takes existing feature and
authorization shared locks, samples fresh time after waits, then projects task, policy and
RACI in one statement snapshot. Task writers use a different fence and can proceed;
the read must not combine snapshots. Canonical text matching avoids parsing malformed or
overflowing grant resource IDs. Wrong-event/missing/denied/disabled targets all return NULL.

The allowlist contains only task identity/state/version, optional policy, current RACI
party IDs/roles and an accountability-attention bit. It intentionally excludes notes,
contacts, titles, dates, dependencies, evidence and history. An expired/future assignment
is not shown as current, and missing required A/R becomes explicit attention. This does
not complete task visibility policy, membership enforcement or responsibility mutation.

## Schema, API and security effects

- `2026-09-14_event_task_read.sql` adds two SECURITY INVOKER functions only. No new tables,
  migrated domain rows, grants, version changes, receipts, outbox effects or provider calls.
- Apply after foundation, API authorization fence and task-commit migrations. The migration
  stays outside the production manifest; no feature flag is enabled by the migration.
- This is an internal database boundary, **not a new HTTP endpoint**. API schemas, generated
  clients, Haskell, web and mobile code are unchanged. The next adapter must authenticate
  through the existing `withCurrentAuthSession` transaction and never accept actor/time
  parameters from the browser. SQL access itself remains a trusted-server capability.
- Activity version is not an aggregate ETag; policy and RACI have independent writes.
  Offline/responsibility updates may not assume otherwise. No last-write-wins semantics.
- No override, booking, payment or production permission is added. No legal advice.

## Executed verification

From the repository root:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
sh scripts/test-event-task-read-migration.sh
bash scripts/test-event-operations-schema-rehearsal.sh
```

- Full pinned formal suite: PASS, exit 0. `TaskRead`: 3,861 generated, 1,788 distinct states,
  depth 10; four named mutation controls detected. Full suite now detects 20 intentional
  negative controls. Both Alloy scenarios SAT; all 11 assertions UNSAT within their scopes.
  See [bounds and assumptions](../../formal/event-operations/README.md). No unbounded proof.
- Focused PostgreSQL 16 suite initially passed permissions, malformed IDs, exact time
  bounds, projection allowlist, RACI attention, unchanged nine-table snapshots, apply twice,
  rollback twice/reapply, revocation at RC/RR/SERIALIZABLE, expiry during an observed lock
  wait and coherent pre/post snapshots during a task/RACI replacement.
- The extended focused suite then passed again, including future Accountable attention and
  blocked-reader races against narrowing a grant to another task, co-owner revocation and
  feature disable. Every race observes database lock barriers; no success is inferred from
  a fixed delay. The expiry test checks that the transaction began before expiry and releases
  its lock only after the database clock reaches the deadline.
- Complete-schema PostgreSQL 17 rehearsal: PASS, exit 0, including all 98 canonical manifest
  entries, retry ledger preservation, extended event chain apply twice, actual schema task
  projection, rollback/reapply, disabled reactivation and final global schema contract.
- The focused suite uses `postgres:16-alpine` with `--network none`, no host port or caller
  DSN, captures its own container ID and removes only that disposable database. Diagnostic
  logs remain in a task-specific temporary directory. SQL statements contain synthetic data.
- `npm run quality:repo`: PASS, exit 0 (repository generation consistency, heuristic formal
  audit, 42 loop, 4 audit, 61 release, 23 CI, 2 visual-artifact and 3 persona-program checks,
  plus 8 internship checks). Artifact/catalog tests are not browser or screenshot evidence.
- Runner isolation/CI wiring tests: 6/6 PASS. Shell syntax, workflow YAML parsing, whitespace
  validation and all 79 local documentation links: PASS.

The inherited `stack test tdf-hq --fast --no-run-tests --no-terminal` build is still running
at this checkpoint. Its test executable linked, but the server compilation is unfinished;
this is not a full build/test pass. No Haskell files were changed here. No new HTTP/browser,
mobile, accessibility, provider sandbox or production tests were executed for this SQL-only
increment. Hosted workflow configuration is not evidence that remote CI passed.

## Rollback and remaining work

Stop consumers of the new function, apply `2026-09-14_event_task_read_rollback.sql` before
rolling back any prerequisite. It drops only the two new functions, without CASCADE or
domain deletion. Apply/rollback/reapply preserve the focused domain snapshot. Unlike the
task-commit rollback, this rollback does not weaken write constraints. Function DDL may
wait on active queries; use the bounded lock timeout and retry during a quiescent window.

Next: typed, authenticated task/RACI API and web client tests, explicit field visibility,
temporary-member/removal rules, aggregate concurrency tokens, task commands/history,
workspace UI and the remaining dependency-ordered phases. Performance on large assignment
sets and broader cross-domain end-to-end journeys remain unverified. No production
activation, merge, deployment or real-money operation is authorized.
