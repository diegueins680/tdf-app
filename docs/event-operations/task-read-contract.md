# Scoped task read contract

This phase-4 increment refines EO-023–028, EO-045 and EO-055 using canonical
`event_logistics_activity`, `event_operation_task_policy`, RACI assignments and grants.
It adds a database projection, not a second task domain or a public endpoint.

## Operation and privacy boundary

`event_operation_read_task(event_id, activity_id, actor_party_id)` returns an
allowlisted JSON object or SQL NULL. Actor identity is a trusted server input, not a
client-selected identity. A future HTTP adapter MUST use the existing same-transaction
`withCurrentAuthSession` boundary and opaque 404 mapping; this SQL function alone does
not authenticate a session or provide row-level security for arbitrary database users.

| ID | Guard / postcondition |
|---|---|
| TR-01 | API feature enabled; canonical event state exists; activity belongs to that exact event. Otherwise NULL. |
| TR-02 | Effective primary/co-owner, event-level task.read/task.manage, or task.read/task.manage for exactly this task and event. Other scopes, coproduction alone, invitations, or RACI assignment alone grant no access. |
| TR-03 | Grants/ownership are non-revoked and `valid_from <= checked_at < valid_until` (unbounded end allowed). Resource IDs match canonical decimal BIGINT text, with no coercion from arbitrary grant text. |
| TR-04 | Shared feature and event authorization fences precede the decision; sample one fresh wall-clock instant after waits. All projection fields use that instant. Stale RR/SERIALIZABLE readers may fail with 40001, never fall back to stale authorization. |
| TR-05 | Task identity, status/version, policy and RACI come from one SQL statement snapshot. Task writers may proceed concurrently; do not mix separate header/assignment queries or claim the authorization fence locks task content. |
| TR-06 | Return eventId, activityId, status, activity version, policy flags/version if present, current RACI party IDs/roles, and accountabilityNeedsAttention. No notes, contact details, dependency IDs, documents, comments, invitation data, grant records, audit payload or arbitrary row serialization. |
| TR-07 | Current RACI uses the same half-open validity window. For a policy requiring accountability, attention is true unless exactly one current Accountable and at least one current Responsible exist. This reports time-window gaps; it does not silently repair assignments or certify collaborator membership. |
| TR-08 | Missing, wrong-event, unauthorized and disabled results are identically NULL. Reads create no grants, tasks, receipts, audit entries or fences and do not advance domain versions. Rollback removes only these new functions; apply/rollback/reapply preserve records. |

This initial internal projection deliberately omits titles and scheduling fields until their
field-level disclosure contract and typed consumers are delivered. Task policy is optional;
no policy means no assertion of RACI readiness. The activity version is NOT an ETag for the
whole projection: policy and RACI can change independently. No offline write may use it as
proof of unchanged responsibility. No dependency metadata means hidden prerequisites cannot
be discovered through this projection. Rich private/assigned-only task policies, pagination,
history, mutation APIs, temporary-member enforcement and UI remain separate work.

## Formal refinement and assumptions

`TaskRead.tla` models one read, eight permission classes, correct/wrong event binding,
one revocable grant, clock 0–3 (expiry 2), two task revisions and independently interleaved
task writers. Current authorization is fenced; projection records one snapshot. Negative
controls must expose scope widening, cross-event access, pre-wait authorization and mixed
task/RACI snapshots. `TaskReadStructure.als` checks concrete event/task/grantee/scope matching,
with a satisfiable least-privilege scenario and non-vacuous denied alternatives.

These are finite safety checks, not universal proofs or liveness claims. Existing
`SessionFence`, `SnapshotRead`, `TaskCommit`, `TaskRaci` and `EventStructure` remain applicable.
Trusted SQL writers cannot bypass the existing authorization/task triggers. Superuser access,
timing side channels, arbitrary SQL access, clock rollback and provider behavior are outside
scope. PostgreSQL tests must demonstrate current permission after a blocked read and snapshot
coherence; formal atomic steps alone are not evidence that actual SQL is atomic.

## Release constraints

Apply after foundation, API authorization fence and task-commit migrations. The new migration
must stay out of the production manifest. Existing feature remains disabled by default; this
increment does not enable it, expose an HTTP route or change generated clients. No real-money
or production operation is authorized. A function-only rollback preserves every domain row.
