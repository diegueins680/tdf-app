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
