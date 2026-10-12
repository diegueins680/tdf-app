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

The handler (no new route, same request and response):

- refuses with 409 a failure that is not a dead-lettered projection
  (`provider <> 'internal_outbox'`), because the server has nothing to run for
  it; the product creates no other kind today;
- in the same statement that records the retry, returns the record's
  dead-lettered projection to `pending` with its attempts reset;
- records `retrying` when a projection is now queued, and `resolved` when no
  projection of the record is left unprocessed.

Migration `2026-10-11_operations_failure_replay` adds three triggers:

| Trigger | When | Effect |
| --- | --- | --- |
| `operations_outbox_settle_failure` | a projection becomes `processed` | the record's open failure becomes `resolved` |
| `operations_integration_failure_merge` | the worker opens a failure for a record that already has an open one | the existing failure returns to `dead_letter`; no second row |
| `operations_integration_failure_resolved` | a failure becomes `resolved` | a terminal event closes the work item that announced the failure |

It also returns to `dead_letter` every failure left `retrying` while its
projection is still dead-lettered, so those can be retried. After the migration
that combination does not occur, so applying it again changes nothing. The
rollback script drops the triggers and leaves rows as they are.

A retried projection gets eight more attempts with the worker's usual backoff
(roughly five minutes in total) before it dead-letters again.

## Locks

The handler locks the failure row, then updates the projection row only if it is
dead-lettered. The worker claims only `pending` or `processing` projections and
touches the failure row afterwards, through the triggers. A projection row is
therefore never wanted by the handler while the worker holds it, and a second
retry that finds the failure `retrying` or `resolved` is refused without
touching the queue.

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
| admitted retry puts the projection back in the queue | projection `pending`, attempts reset |
| audit retains actual statuses | `dead_letter` to `retrying` |
| retried projection that fails again returns the same failure to dead letter | one failure row, `dead_letter` |
| failure can be retried again after a second dead letter | 202 |
| retried projection that succeeds resolves its failure | projection `processed`, failure `resolved`, work item created |
| later events of the record flow after the retry | the blocked event is processed |
| resolved failure cannot be retried | 409 |
| resolving a failure closes the work item that announced it | work item `resolved` |
| migration returns a stranded retry to dead letter and keeps a queued one | repair touches only stranded rows; also a third application of the migration |
| retry with nothing left to run resolves the failure | 202 with `resolved` |

Local result on 2026-10-11: 632 checks passed, including these. Against the
server binary built before this change the run stops at the first case above
(the old handler answers 202 and relabels the failure); the later cases were not
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
- Whether Operations is enabled in production and whether any failure rows exist
  there has not been observed. To count rows the migration would change:

  ```sql
  SELECT status, count(*) FROM operations_integration_failure GROUP BY status;
  ```
