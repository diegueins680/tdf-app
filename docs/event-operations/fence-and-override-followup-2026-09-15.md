# Task fence deletion and completion exception scope

The task write fence is transient serialization metadata. Its foreign key now
cascades when an otherwise deletable event is removed; reapplying the opt-in SQL
upgrades the previous restrictive foreign key. Task policies and immutable audit,
override, RACI and event history restrictions are retained. This does not grant
permission to delete events or remove protected tasks.

A completed task cannot acquire a newly inserted or retargeted incomplete
prerequisite through an earlier blocked-completion exception. The deferred check
examines the final surviving edge and its final prerequisite status. It retains
valid transactions that complete the prerequisite after inserting the edge, and
unchanged edge updates do not invalidate existing evidence. To use a completion
exception, establish the blocked dependency graph while the task is planned
before the separate, authorized completion transaction. This is a conservative
restriction until a future command can bind approval to an explicit graph revision.

## September 16: bind approval to the open task's graph

The foundation now records a database-generated dependency snapshot (ordered edge
IDs and prerequisite IDs) on each new immutable override. Capture acquires the
same event write fence as graph mutations. Completion requires both the exact
task version and the approved snapshot; an edge added while the task is still
open no longer inherits an earlier approval. Retained, unchanged edges preserve
their identity. A new approval after a graph change requires a new task version;
the previous approval remains immutable audit history.

Reapplying this opt-in migration adds a nullable column without inventing
historical approvals: old NULL snapshots never authorize blocked completion.
An installation with already-completed blocked tasks relying on such evidence
must resolve prerequisites or obtain an operator-reviewed fresh authorization
before migration validation can succeed. Do not rewrite approval history.
The non-destructive rollback retains snapshots and their capture trigger; no
production manifest or feature activation changes are part of this fix.

Validation: PostgreSQL 16.10 reproduced acceptance of the stale approval before
the fix and rejected it afterward. The complete foundation SQL runner passed
on an isolated native PostgreSQL fixture, including concurrency, rollback and
reapply. Only its Docker transport was replaced in a temporary local copy;
repository and CI Docker commands are unchanged. Local Docker was unresponsive.
Final-head CI remains required. Full backend: 2,547 examples passed; full UI:
1,992 tests passed with lint, types and production build, after one bounded
load-related retry without changing test deadlines.

The same guards are maintained in foundation PR #336 and successor PR #339.
The successor test retains both valid statement orders, immediate constraints,
RC/RR/Serializable RACI races and both status/dependency writer orders. New
regressions cover event deletion and stale override insert/retarget denial.
Foundation also rehearses an already-installed restrictive FK and deletion after
rollback. Production migration manifests, API contracts and feature activation
are unchanged. A binary rollback does not remove protected history.

Verified locally on 2026-09-15: both complete SQL runners exited successfully.
The foundation runner required two corrections to the newly added shell test
block before its complete successful rerun; those attempts are not passing tests.
The successor runner retained all existing assertions and passed without changes
to their expected outcomes. No live application database was accessed.
