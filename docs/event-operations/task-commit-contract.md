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
| Complete or edit completed opted-in task | Every dependency is completed, or an immutable blocked-completion override references the immediately preceding activity version and the current dependency snapshot | `23514`, entire transaction rolls back |
| Add dependency / reopen prerequisite | Must not leave any gated completed task blocked without that version- and dependency-bound override | `23514` |
| Revoke or replace required RACI | Exactly one currently effective Accountable and at least one currently effective Responsible remain | `23514` |
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

The current foundation additionally enforces RACI validity windows at commit and
requires attributed retirement of expired assignments. The read projection still reports
attention when time passes beyond an assignment's validity without a new write. This
integration retains those foundation guards, scoped command authorization, and audited
history. Formal bounds do not establish unbounded clock or provider behavior; production
activation remains separately gated.

## Rollout / rollback

Apply after the foundation, with the event-operations API disabled. The migration validates
existing opted-in tasks and refuses invalid data without repairing it silently. It is excluded
from the production migration manifest. Rollback retains the integrity triggers, write-fence
rows and all domain history. It does not restore weaker historical completion or deletion
behavior. Disable and roll back the later command/read adapters in reverse dependency order,
with task writes quiesced; reapply revalidates retained records. Migration tests prove stale
approval graphs, unsafe deletion and version regression are rejected after both apply and
rollback.
