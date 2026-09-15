# Task aggregate revision contract

TV/TR projections expose an activity version and optional policy version, neither of which
changes on independent RACI edits. Neither is a valid optimistic-write precondition for the
aggregate. Reuse canonical task rows and the existing event task-write fence; do not add
another task, assignment, identity, authorization or lock domain.

## Database-only increment

`event_operation_task_revision` is version metadata keyed by the canonical activity ID.
It tracks activity, policy, RACI, outgoing dependency and override storage mutations.
Legacy logistics contacts/route-verification/notifications are not RACI and are outside this
aggregate. No public read shape, API, client, UI or feature flag changes in this increment.
Canonical generated task IDs must not be reused after deletion. Restoring/resetting IDs,
tampering with metadata, disabling triggers and superuser bypass are outside this boundary.
Revision metadata is not immutable audit history; the existing audit domain remains canonical.

| ID | Guard/effect |
|---|---|
| RV-01 | Backfill existing tasks once at revision 1. New activities receive revision 1. Every subsequent tracked row mutation advances its task revision, even if the legacy activity version does not change. |
| RV-02 | A row update that changes a value and later changes it back cannot restore the old revision (no ABA). Unrelated tasks do not advance; multi-row transactions may advance more than once. |
| RV-03 | `event_operation_lock_task_revision(event, task, expected)` checks exact task/event identity and a positive expected revision, acquires the existing transactional event write fence, then checks the current revision. Retain the fence until the caller commits/rolls back. |
| RV-04 | A mismatch raises 40001 and rolls back the statement/command; no stale success. Concurrent commands from the same revision cannot both mutate and commit. RC checks fresh state after waits; stale RR/Serializable may abort. |
| RV-05 | Tracked changes and revision advancement commit/roll back together, including deferred task invariants. Counter overflow aborts, never wraps. Task deletion follows existing guards/FKs and removes only its version metadata. |
| RV-06 | No browser-supplied actor or new permission. These are SECURITY INVOKER internal SQL facilities, not an authenticated command API. Future handlers must use SessionFence, current authorization/feature checks, immutable audit, idempotency and final-state task guards in the same transaction. |
| RV-07 | Reapplication does not reset counters. Rollback removes the new command guard but deliberately retains passive tracking/counters to prevent old versions becoming valid again after reapply. No production manifest registration. |

Time alone does not advance a storage revision. Expiry, grant revocation, prerequisite
readiness and current membership must be checked again at command execution; a matching
revision is not authorization, an effective RACI certificate or permission to complete.
Do not expose the event-wide fence counter to a task-scoped reader: it includes other tasks.
The task-scoped version remains internal until typed API/privacy/consumer tests are delivered.

The guard's only fence write is internal synchronization; it does not advance a task revision
without a tracked mutation. Missing/wrong-event/missing-metadata targets raise fixed P0002;
invalid revision raises 22023. A future HTTP boundary must authorize before translating any
target/conflict errors. 40001 is not permission to refresh a stale expected version silently.
Retry only the entire authorized command or surface a user-visible conflict.

## Formal refinement and verification gate

`TaskRevision` models two commands that capture an activity/RACI snapshot, then acquire the
shared fence and compare its revision. One independent RACI writer can interleave before
the fence. Atomic Finish abstracts tracked SQL mutation plus commit. Negative controls omit
RACI revision advancement or check before acquiring the fence; both must violate
`NoStaleCommit`. Existing TaskCommit/TaskRaci and Alloy EventStructure cover complementary
final-state and relational constraints. This model does not prove SQL trigger/isolation
behavior, authorization, unbounded progress or clock-based invalidation.
Activity and RACI counters abstract the tracked storage fields; the new table is an
implementation of a task attribute, not a separate conceptual task/relationship domain.
The finite graph has activity 0–2, RACI 0–1 and revision 1–4; no fairness is assumed.

Run positive and named negative TLC checks before implementation. Real PostgreSQL checks
must cover backfill/reapply, every tracked relation, ABA, rollback/deferred failures, identity
binding, overflow, isolation levels and observed competing-command lock waits. Keep feature
and production activation off; API/UI/offline editing remain subsequent increments.
