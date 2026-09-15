# Task commit contract

This correction refines EO-023–028 and EO-055. It reuses existing logistics rows and the
opt-in task policy; it does not expose a new task API or grant new authority.

## Operations and transaction semantics

All activity, dependency, task-policy and RACI mutations in an event acquire a common
transactional write fence. Validation occurs against the final transaction state, before
commit. READ COMMITTED writers serialize; a stale REPEATABLE READ or SERIALIZABLE writer
must abort rather than validate an old snapshot. Deadlocks/serialization failures are retryable
only by replaying the whole authorized, version-checked command, never a subset of its writes.

| Operation | Guard at commit | Failure |
|---|---|---|
| Complete or edit completed opted-in task | Every dependency is completed, or an immutable blocked-completion override references the immediately preceding activity version | `23514`, entire transaction rolls back |
| Add dependency / reopen prerequisite | Must not leave any gated completed task blocked without that version-bound override | `23514` |
| Revoke or replace required RACI | Exactly one non-revoked Accountable and at least one non-revoked Responsible remain | `23514` |
| Opt in an existing or newly inserted completed activity | Same final-state checks, even when status was set before policy insertion | `23514` |
| Delete policy, weaken its flags, move or delete protected task | No generic edit bypass; dedicated audited workflow is not implemented | `23514` |
| Move an assignment between tasks | Revoke old assignment and create new assignment in one transaction; do not change its task identity | `23514` |

A completion override remains a trusted database input, not a public operation. Authorization,
evidence, version advancement and audit insertion for a future override API remain mandatory.
The old BEFORE-status validation is replaced by deferred final-state validation so a task and
its dependencies may be completed in either statement order within one transaction.

## Model refinement and limits

`TaskCommit.tla` separates preparation from commit for two competing transactions. It models
two Responsible people, one task, an abstract blocked-dependency bit, and five commands:
remove either person, complete, block, or complete then block. Commit validation plus a
transaction-duration fence preserves `NoBlockedCompletion` and `NoOrphanResponsibilities`.
The early-validation and no-serialization mutation configurations must produce the specified
counterexamples; they are intentional negative controls, not accepted production models.

`TaskRaci` and `EventStructure` retain the complementary DAG, unique Accountable, override,
and relational scoping specifications. The new model abstracts DB exceptions as rejection;
it does not establish SQL isolation behavior. Real concurrent PostgreSQL tests are required.

RACI validity windows, membership removal, contextual authorization, field privacy, policy
administration, accepted evidence, task history and user-visible conflict handling are not
implemented by this correction. In particular, non-revoked assignment cardinality is not a
claim that a time-limited assignment remains effective forever. Do not expose this sidecar
for production use until those workflows are implemented and tested.

## Rollout / rollback

Apply after the foundation, with the event-operations API disabled. The migration validates
existing opted-in tasks and refuses invalid data without repairing it silently. It is excluded
from the production migration manifest. Rollback removes only these additional guards, restores
the original BEFORE-completion guard, and retains the write-fence rows and all domain history.
Rollback deliberately restores the previous weaker safety boundary; keep task writes quiesced
until a corrected forward migration can be applied. Reapply revalidates retained records.
