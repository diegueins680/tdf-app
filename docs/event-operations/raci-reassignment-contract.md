# Scoped RACI reassignment command — specification before implementation

## Bounded increment and audit findings

The canonical activity, RACI, aggregate revision, task-write fence, event authorization
fence, command receipts and immutable audit already exist. Current task reads intentionally
omit assignment UUIDs and scheduling windows. A full replace-all command would therefore risk
silently discarding hidden future/expired assignments. This increment instead reassigns one
explicit `(partyId, role)` pair on the existing task; no other assignment is removed.

The existing receipt unique key is `(event_id, operation_code, command_id)`. Reusing an
event-wide operation code for task commands would couple otherwise-private tasks. Keep the
same receipt ledger and define its resource-scoped operation namespace as
`event.task.raci.reassign/<canonical activity ID>`; audit operation remains
`event.task.raci.reassign` with explicit task resource fields. A UUID can be independently
used on different tasks. A server-computed SHA-256 binds event/task/actor/expected revision,
role, old/new party, exact reason and correlation text; callers cannot supply their own hash.

Only the private SQL entry point is implemented in this phase. No EXECUTE for PUBLIC, no
HTTP route, no UI, no production-manifest entry, no provider/notification activation. The
existing event API flag is still checked and shared-locked. A future route must compose
`withCurrentAuthSession` in the same transaction, strict decimal revision transport and
non-cached authorized responses; passing an actor ID to SQL is not authentication.

## Operation, guards and effects

`event_operation_reassign_raci(event, task, actor, command UUID, expected BIGINT,
role, old party, new party, reason, correlation) -> JSONB`

Identities retain the existing task API's positive JavaScript-safe integer range; expected
revision is a positive signed BIGINT. Distinct old/new parties; role is one
of responsible/accountable/consulted/informed; reason is nonblank and at most 2,000 characters;
correlation is nonblank and at most 200. Nulls and noncanonical roles fail `invalid_request`.
No trimming/case normalization changes the accepted request identity.

1. Lock enabled feature and event authorization rows. Check exact current task read access
   before task/receipt work. Missing/foreign/unreadable task returns only `not_found`.
2. Serialize through the existing event task-write fence (a write, not an advisory lock),
   then lock the exact task revision row. Recheck current read authority after waiting.
3. Look up the task-scoped receipt before comparing expected revision. Exact actor/hash
   replay returns its immutable historical outcome with `replayed=true`, no RACI/revision/
   audit/receipt duplication. Current read-only access suffices for replay, not new mutation.
   Changed actor or content with current read access returns `idempotency_conflict`.
4. For a new command, require current owner/event or exact-task `task.manage`; `event.manage`,
   event.read, task.read, assignment and coproduction alone are not task mutation authority.
   Only event `draft`/`planning` and activity `planned`/`confirmed` are supported. Approved,
   published, live, completed and terminal event workflows require later effect/approval gates.
5. Require the locked aggregate revision to match. Find and lock only the source's current
   non-revoked assignment; it must be unbounded (`valid_until IS NULL`). Future, expired or
   time-bounded rows return `assignment_not_replaceable`; scheduling amendments are separate.
6. Lock the new canonical Party's identity and recheck current actor read/manage, source
   validity and new-party task read eligibility after blocking locks. Missing/ineligible
   recipients return `assignee_unavailable`; no existence or grant details are returned.
   This conservative eligibility is not a membership/availability/booking certification.
7. Reject duplicate target `(party,role)`. In one transaction revoke the old row with actor,
   reason and decision instant, insert a new row beginning at that instant, validate final
   storage dependencies/RACI and current accountability. Preserve every other row and all
   old assignment data. No grant, owner, lifecycle, task status or legacy version changes.
8. Append one immutable audit with before/after assignment IDs/parties/role and revisions,
   and one accepted receipt. Return only event/task/command IDs, role, old/new party IDs,
   exact textual result revision and `replayed=false`. Existing tracking advances twice
   (one revocation, one insert); it is not a command sequence number.

Expected failures use stable codes: `feature_disabled`, `invalid_request`, `not_found`,
`forbidden`, `idempotency_conflict`, `version_conflict`, `operation_not_ready`,
`assignment_not_replaceable`, `assignee_unavailable`, `assignment_conflict`,
`accountability_not_ready`. Only accepted operations reserve keys; rejected attempts do
not produce receipts/audits in this private primitive. The internal event-wide coordination
fence may advance on attempts/replays; it is not the task revision or a business side effect.
Future HTTP rejection observability
must remain sanitized and must not be presented as already implemented.

DB errors (including stale RR/SERIALIZABLE 40001, deadlock, overflow, unexpected constraint
failure, audit/receipt failure) abort the whole transaction. No catch-and-success partial
write. Caller must wait for COMMIT, not treat the function result as durable early success.
Default deferred task constraints are required for atomic Accountable replacement; a caller
forcing immediate checks may fail closed. The function never weakens or defers caller guards.

## Invariants and traceability

| Requirement | Invariant / operation | Formal artifacts | Implementation and required executable tests |
| --- | --- | --- | --- |
| RC01 current authority | `CurrentAuthority` | `RaciReassignment`, `TaskRead`, `SessionFence` | Exact scope predicate; post-wait expiry/revocation, wrong event/actor, read-only replay |
| RC02 no stale replacement | `NoStaleReassignment` | `RaciReassignment`, `TaskRevision` | Post-fence revision compare; RC/RR/Serializable competing commands and legacy writer |
| RC03 idempotency/privacy | `ExactRetry`, `TaskKeyIsolation` | `RaciReassignment`, `ReceiptReplay`, `CommandPrivacy` | Same key/content retry, changed content/actor, same key private sibling |
| RC04 no orphan or silent loss | `NoOrphanResponsibilities` | `RaciReassignment`, `TaskCommit`, `TaskRaci`, `EventStructure.als` | Atomic revoke/insert, unique A, required R, reject timed source, unchanged other assignments |
| RC05 audit/revision/receipt coupled | `AuditCoupled` | `RaciReassignment`; canonical append-only constraints | Inject audit failure and abort outer transaction; exact history and counters preserved |
| RC06 no privilege/lifecycle side effects | Explicit restricted operation | Existing scoped Alloy assertions and executable contract | No new grants, no ambient manage, disabled/unsupported states fail closed |

## Formal bounds and refinement assumptions

Two tasks, two commands, two keys, one replaceable obligation per task, revisions 1..3;
manage/read/no grant and clock 0..3 with expiry 2. The assignment boolean represents
**committed** obligation coverage: uncommitted revoke/insert steps are hidden by MVCC.
No fairness/liveness claim is made. Six negative configurations omit fresh authorization,
revision comparison, replay handling, task key namespacing, atomic replacement or audit.
They must each produce their named invariant violation, not merely a parser error.

Full multi-role cardinality, target binding, payload tampering, SQL rollback and exact money/
time are not proven by this abstraction. Existing bounded Alloy `EventStructure` and
`TaskReadStructure` assertions remain applicable without adding relational entities;
SQL adversarial tests must refine them. Identity/role tables and triggers remain trusted.
Direct legacy SQL that intentionally bypasses application authorization is outside this
private command boundary. Future collaborator revocation/reassignment workflows remain open.

Rollback removes only new functions, never receipts, audit or revoked assignment history.
Reapply cannot reset keys or counters. Deployment stays disabled pending HTTP session
composition, notification/consent review, operational limits and broader lifecycle support.

## Verification evidence

Before feature implementation on 2026-09-15, the pinned full suite completed with exit 0:
20 positive TLC configurations, 43 named negative controls, 13 PlusCal integrity tests,
2 SAT Alloy scenarios and 11 UNSAT assertions within their documented bounds. The new
model generated 426,052 states / 165,180 distinct, depth 14. All six mutations produced
their precise invariant violations with exit 12. Exact command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Post-implementation PostgreSQL 16 tests passed: 40 deterministic generated four-role histories,
nine writer races across RC/RR/Serializable, four observed expiry waits, both revocation orders,
aborted-first retry recovery, atomic audit/receipt failure rollback and exact down/up history.
Authoritative full-schema PostgreSQL 17 rehearsal also passed, including Accountable replacement
and retained evidence through rollback/reapply. Exact commands, supplementary checks and remaining
limitations are in [PR 23 evidence](pr-23-raci-reassignment-command.md). Finite model success alone
is not a database refinement proof or production authorization.
