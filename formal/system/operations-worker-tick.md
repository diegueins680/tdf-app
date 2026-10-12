# Operations worker maintenance tick (OPS-WORKER-001)

Reconstructed on 2026-10-12 from `tdf-hq/src/TDF/Operations/Worker.hs`
(`startOperationsWorker`, `workerLoop`, `operationsWorkerIterationWith`,
`operationsMaintenanceTick`), `operations_process_outbox_batch` in
`tdf-hq/sql/2026-10-05_operations_scoped_projection.sql` and
`operations_validate_entity_reference` with its constraint trigger
`operations_work_item_entity_reference` in
`tdf-hq/sql/2026-08-09_admin_operations_control_center.sql`. The requirement
text was read against the code, not the other way round.

## The tick

Every second the worker runs one iteration: one transaction that calls
`operations_process_outbox_batch(250, 'tdf-hq-operations-worker')`,
`operations_tick_sla(now())` and the 90-day archive update, and reports the
counters or the fixed `tick failed` event (`WorkerLoggingSpec` covers the
logging boundary: fixed text, no exception data, cancellation propagates, no
retry on a failed sink). The batch locks due outbox rows `FOR UPDATE SKIP
LOCKED` in aggregate order, projects each into a work item, and on any error
in a row's `EXCEPTION WHEN OTHERS` block records the attempt, the SQLSTATE
and a backoff (`2^attempts` seconds, at most an hour); the eighth attempt
dead-letters the row and writes an `operations_integration_failure` with the
SQLSTATE and a fixed summary, which is itself a domain event projected by the
next tick.

## What was wrong

`operations_work_item_entity_reference` is a `DEFERRABLE INITIALLY DEFERRED`
constraint trigger: it checks that a work item's entity exists at commit, not
at the insert. A domain event for an entity that no longer exists (a deleted
social event in the fixture; any deleted party, booking, event or invoice in
production) therefore passed the batch's per-row handler and raised at the
tick's COMMIT: the whole transaction rolled back, the outbox batch, the SLA
reminders and the archive with it, no attempt was recorded, the row stayed
due, and the next second repeated the identical failure. Every healthy outbox
row behind it stayed pending; the only trace was `tick failed` once a second.
Reproduced on the migrated fixture, whose pending rows reference deleted
events, and with a synthetic event for a missing party.

Fixed in the worker: the tick runs `SET CONSTRAINTS ALL IMMEDIATE` first, so
deferred constraint triggers fire at each statement inside the batch and the
poisoned row fails alone, with its attempt, backoff and dead letter; the
healthy rows, the SLA tick and the archive proceed and commit. The trigger and
the batch function are unchanged (no migration).

## Model

`formal/event-operations/OperationsWorkerTick.tla`: three outbox rows, one
poisoned, three attempts to the dead letter, six ticks; a tick either commits
(each due row processed, or failed with its attempt and backoff) or, with the
deferred check, aborts as a whole; a backoff may elapse between ticks.
Invariants: `PoisonIsolated` (after any tick every healthy row is processed),
`NoAbortedTick`, `AttemptsRecorded`, `DeadLetterAtBound`. Positive run: 61
states, 27 distinct, depth 9. Controls: `DeferredCheck` (the previous tick),
`NoAttempt`, `UnboundedRetry`, each failing on the named invariant. The model
has no clock, no aggregate ordering, no concurrent replicas (`SKIP LOCKED`),
no SLA or archive content; the batch size and the real eight-attempt bound are
not modelled.

## Evidence

- `scripts/__tests__/operations-worker-tick-postgres.mjs` (CI step "Verify
  the Operations worker tick isolates a poisoned outbox row"): the tick's
  three statements mirrored in one transaction on the migrated fixture with
  a healthy and a poisoned synthetic event; the previous tick (deferred
  check) fails at commit as a whole with no attempt recorded; the fixed tick
  projects the healthy row, fails the poisoned one with attempt 1, SQLSTATE
  23503 and a future `next_attempt_at`, and commits; backoff is honoured;
  the eighth attempt dead-letters the row with a private integration failure
  (SQLSTATE and fixed summary only), which the next tick projects; the dead
  letter is never retried.
- `tdf-hq/test/TDF/Commerce/WorkerLoggingSpec.hs`: logging and cancellation
  boundary (unchanged).
- `scripts/lib/operations_conformance.py`: scoped projection and a poisoned
  projection whose error is immediate (unchanged).

## Ambiguities recorded

- `SET CONSTRAINTS ALL IMMEDIATE` applies to every deferrable constraint in
  the tick's transaction, not only the entity-reference trigger; the tick's
  own writes are the projection, the SLA timers and the archive, none of
  which relies on deferred checking.
- A dead-lettered row for a deleted entity needs an operator decision
  (`OPS-FAILURE-REPLAY-001`); the worker does not resolve it.
- The fixture's pending outbox rows reference deleted events, so the local
  worker logs `tick failed` every second against it until this fix; they are
  parked and un-parked by the test, not repaired.
