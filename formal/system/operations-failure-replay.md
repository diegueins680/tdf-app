# Retry of an Operations projection failure

Requirement `OPS-FAILURE-REPLAY-001`, decision `AUTHORITY-065`.

## What was checked

The Operations worker (`operations_process_outbox_batch`) turns recorded events
into work items. An event whose projection fails eight times is dead-lettered
and an `operations_integration_failure` row is opened for it. Managers see that
row in the failure queue and can retry it
(`POST /operations/integration-failures/{id}/replay`, `replayFailureHandler` in
`tdf-hq/src/TDF/Operations/Server.hs`).

## What was wrong

The retry changed the failure's status to `retrying`, counted an attempt and
wrote an audit row. Nothing else read that status:

- the dead-lettered projection stayed dead-lettered, so it never ran again;
- the worker does not project an event while an earlier event of the same
  record is unprocessed, so every later event of that record stayed blocked;
- nothing moved a failure out of `retrying`, and a `retrying` failure cannot be
  retried, so the manager's one retry was also the last;
- nothing ever set a failure to `resolved`.

`OPS-AUX-WRITE-001` described the transition as "open or dead_letter ->
retrying once" and recorded the missing execution as an open ambiguity. The
existing tests checked that exactly one of several simultaneous retries was
admitted, which was true.

## What changed

Migration `2026-10-11_operations_failure_replay` moves the rule into the
database, so it holds whichever server version performs the retry:

| Trigger | When | Effect |
| --- | --- | --- |
| `operations_integration_failure_retry` | a projection failure is marked `retrying` | the record's dead-lettered projection returns to `pending` with its attempts reset; if nothing of the record is left unprocessed the failure becomes `resolved` instead |
| `operations_outbox_settle_failure` | a projection becomes `processed` | the record's open failure becomes `resolved` |
| `operations_integration_failure_merge` | the worker opens a failure for a record that already has an open one | the existing failure returns to `dead_letter` and is announced again, reopening its work item; no second row |
| `operations_integration_failure_resolved` | a failure becomes `resolved` | a terminal event closes the work item that announced the failure |

A unique partial index (`operations_integration_failure_open_uidx`) enforces one
open projection failure per record and serves the triggers' lookups. An error
while announcing or closing the work item is reduced to a warning so that
bookkeeping cannot fail a projection or a retry.

The handler (no new route, same request and response shape):

- refuses with 409 a failure that is not a dead-lettered projection
  (`provider <> 'internal_outbox'`), because the server has nothing to run for
  it; the worker is the only code that opens failures today;
- refuses with 503 when the retry trigger is absent or disabled (`pg_trigger`,
  not only its function), instead of
  relabelling the row;
- records in the audit the status the failure actually reached (`retrying` or
  `resolved`).

The migration also repairs rows written earlier: a failure left `retrying` while
its projection is still dead-lettered returns to `dead_letter`; of several open
failures for one record the oldest is kept; a failure whose projections have all
run is resolved. Applying it again finds nothing to repair. The rollback script
locks the outbox and failure tables and refuses to run while an internal retry
is active. Once retries settle, it drops the index, triggers and functions and
leaves rows as they are. Roll the server back first; the new server refuses
retries unless all four replay triggers are present and enabled.

A retried projection gets eight more attempts with the worker's usual backoff
(roughly five minutes in total) before it dead-letters again.

## Locks

The handler locks the failure row; the retry trigger then updates the record's
dead-lettered projection rows and reads the queue again in a new statement. The
worker claims only `pending` or `processing` projections and touches the failure
row afterwards, through the triggers, so it can wait briefly for a retry in
progress and a retry can wait for a worker batch to commit. This ordering is
argued from the code and was exercised by hand in the independent review (worker
succeeding and worker dead-lettering during an open retry); the automated cases
race handlers against each other only, because the harness runs with the
background worker disabled.

## For the release

- The migration takes a `ShareRowExclusiveLock` on `operations_outbox` and
  `operations_integration_failure` while it creates the triggers, with a 10 s
  lock timeout. Writes that record an Operations event wait for it.
- A retry only ends if the worker runs (`OPERATIONS_WORKER_ENABLED`).
- The schema verifier does not check these triggers; the handler does, at each
  retry.

## Evidence

`scripts/lib/operations_conformance.py`, run by
`scripts/test-booking-conformance.py` (CI step "Verify booking object scope and
concurrent resource calendar"): the compiled server over HTTP against a
disposable PostgreSQL database built from the production schema fixture and the
ordered manifest. The projection is dead-lettered and later run by the real
worker function; a synthetic trigger on `operations_work_item` stands in for the
cause of the failure and is dropped to "repair" it. The background worker is
disabled in this harness, so the cases call the worker function themselves.

| Case | Shows |
| --- | --- |
| failure without an executor is not admitted as retrying | 409, status and attempt count unchanged |
| worker dead-letters the projection and opens one failure | fixture is produced by the worker, not inserted |
| dead-lettered projection blocks later events of its record | the cost of a stranded failure |
| replay audit error rolls back state and queue | failure and projection both unchanged after a 500 |
| retry admits exactly one request | eight simultaneous retries, forced to overlap: one 202, seven 409 |
| admitted retry puts the projection back in the queue | projection `pending`, attempts 0 |
| audit retains actual statuses | `dead_letter` to `retrying` |
| retried projection that fails again returns the same failure to dead letter | one failure row, `dead_letter` |
| second dead letter is announced again | a second event for the failure's work item |
| failure can be retried again after a second dead letter | 202 |
| retried projection that succeeds resolves its failure | projection `processed`, failure `resolved`, work item created |
| later events of the record flow after the retry | the blocked event is processed |
| resolved failure cannot be retried | 409 |
| resolving a failure closes the work item that announced it | work item `resolved` |
| relabel by an earlier server still puts the projection back in the queue | a bare `UPDATE … status='retrying'` re-queues |
| migration returns a stranded retry to dead letter, keeps one failure per record and keeps a queued retry | the three repairs touch only stale rows; index recreated |
| retry with nothing left to run resolves the failure | 202 with `resolved` |
| migration resolves a failure whose projections have all run | third repair |
| each replay trigger is disabled or removed | 503, failure and queue unchanged |
| rollback while a replay is pending | refused; all triggers and pending retry retained |
| rollback after settlement, repeated | succeeds; migration can then be reapplied |

Local result on 2026-10-11: 636 checks passed, including these. Against the
server binary built before this change the run stops at the first case above
(the old handler answers 202 and relabels the failure; that control was run before the retry trigger was added); the later cases were not
run against the old binary.

`formal/event-operations/OperationsFailureReplay.tla` models one record: its head
projection, one later event, the failure, and a cause that may or may not be
repaired. TLC explores 24 distinct states with up to three retries.

| Configuration | Result |
| --- | --- |
| `OperationsFailureReplay` | no violation; `RetryEnds` holds under weak fairness of the worker |
| `OperationsFailureReplayRelabelOnly` (retry does not re-queue) | `RetryingIsQueued` violated |
| `OperationsFailureReplayUnsettled` (a run does not close the failure) | `RunIsSettled` violated |
| `OperationsFailureReplayUnmerged` (second dead letter opens a second failure) | `OneOpenFailure` violated |

This is bounded model checking of an abstraction, not a proof of the SQL. The
model does not include attempts before the eighth, backoff, authorization, or
several records.

## Left open

- A failure whose cause nobody repairs dead-letters again after every retry.
  That is the intended behaviour, but nothing limits how often a manager retries.
- Only dead-lettered projections can be retried. The worker is the only code
  that opens failure rows today; if provider failures are added, each needs an
  executor before the queue offers a retry for it.
- A failure with no branch cannot be retried (404), as before.
- The route's description in `tdf-hq/docs/openapi/api.yaml` still says the retry
  "records retry admission, not provider delivery". That text is copied into the
  generated web and Mobile types, so correcting it needs a Mobile change and a
  pin bump; it is not part of this change. Paths, status codes and the response
  schema are unchanged, except that 503 can now be returned.
- The response has no `resolvedAt`; a retry that resolves the failure is visible
  only as `status: resolved`.
- A failure of another provider that an earlier server relabelled `retrying`
  stays that way. The product creates none.
- Whether Operations is enabled in production and whether any failure rows exist
  there has not been observed. To count rows the migration would change:

  ```sql
  SELECT status, count(*) FROM operations_integration_failure GROUP BY status;
  ```
