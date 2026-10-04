# Planning-task completion — specification before implementation

Dependency: draft #403 at `6047377348e901c919d7691b849faab30980b952`.
Reuse canonical logistics activities/dependencies, opted-in task policy, current
RACI, event authorization/task write fences, aggregate revisions, audit and receipt
ledger. No second task, status, identity or financial system.

## Bounded operation

Private SQL `event_operation_complete_task(event, task, actor, command UUID,
expected BIGINT revision, reason, correlation) -> JSONB`. The existing API flag
must be enabled. PUBLIC cannot execute. This is not authentication: a later HTTP
route must compose the existing session transaction witness and validate its
receipt before COMMIT. No route, UI control, provider or production-manifest change.

This increment completes **pre-production work** only: event draft/planning,
task planned/confirmed, an existing policy with both accountability and dependency
gating enabled. Require exactly one current Accountable and at least one current
Responsible at a single post-lock decision instant. Every direct prerequisite
must be completed. Do not silently opt in a legacy task or invoke even a stored
blocked-completion override. Tasks needing richer acceptance/evidence, approvals,
live execution, cancellation or reopening need separate commands and policies.

| ID | Preconditions / effects / failures |
| --- | --- |
| TC01 | Positive JS-safe event/task/actor IDs, nonnull UUID, positive BIGINT revision, nonblank exact reason <= 2,000 characters and correlation <= 200; otherwise `invalid_request` |
| TC02 | Lock enabled flag and event authorization row; exact scoped read before task/receipt inspection. Missing/foreign/unreadable task gives only `not_found`. New writes require owner or contextual `task.manage`, not assignment or ambient `event.manage` |
| TC03 | Serialize using the existing event-wide WRITE fence, lock task revision AND activity tuple and the audit actor's Party key, then read current time and recheck authority. Legacy writes and stale RR/Serializable transactions cannot bypass the fence. No frozen pre-wait clock |
| TC04 | Existing ledger namespace `event.task.complete/<task>`; server SHA-256 binds all exact inputs. Exact same actor/content replay after current read authorization returns historical receipt before revision/status checks, even with only read access. Changed actor/content gives `idempotency_conflict`; other tasks have independent keys |
| TC05 | New command checks scope, draft/planning event, planned/confirmed task, strict opted-in policy, expected revision, current RACI and dependencies. Stable generic failures: `forbidden`, `operation_not_ready`, `version_conflict`, `accountability_not_ready`, `dependencies_not_ready`. No dependency IDs, titles or parties are exposed by errors |
| TC06 | Set task status to completed, increment its legacy version once and set its existing `updated_at` to the decision instant. Existing tracking increments aggregate revision once. One immutable audit records old/new status and both revisions; one accepted receipt binds the result. No grants, RACI, dependencies, policy, bookings, budgets, messages or payments are changed |
| TC07 | Deferred final-state guards remain active; validate immediately after the write too. Any audit/receipt/constraint/overflow failure or outer abort rolls back all business effects. Only COMMIT makes the response durable. Rejected attempts consume no key or audit; internal coordination-fence advancement is not business history |

Response: event/task/command IDs, `status: completed`, `activityVersion` as a
positive integer, `aggregateRevision` as an exact decimal string, `replayed`.
The decision instant is authorization/assignment validity time, not a guarantee
that a timed responsibility remains valid forever after completion. A later task
scope-removal/retention workflow remains separate. Protected committed tasks
cannot be left blocked by a later prerequisite edit without a valid trusted
override under existing deferred guards. This command itself never uses overrides.

## Formal and executable refinement

`TaskCompletion` checks two concurrent attempts, one task, same/different keys,
one external revision edit, prerequisite ready/not-ready, supported/unsupported
lifecycle, grant manage/read/none, and clock 0..3 (RACI expiry at 1, grant expiry at 2).
One event fence abstracts transaction serialization; commit is atomic. No liveness
or fairness promise is made. Seven guard mutations must violate their named
invariants: fresh authority, expected revision, dependency readiness, current RACI,
lifecycle support, exact replay and coupled audit.

Existing `TaskCommit`, `TaskRevision`, `ReceiptReplay`, `RaciReassignment` (task key
scoping), `SessionFence` and bounded Alloy `EventStructure` / `TaskReadStructure`
remain complementary. No new relational entity or permission is added. Finite
success is not a proof of SQL refinement, isolation, text/hash transport or privacy.
Implement only after the full pinned TLC/Alloy suite passes. PostgreSQL tests must
cover valid/invalid commands, same-key retries, hidden/foreign targets, expiring
authority/RACI after tuple waits, concurrent dependency/completion/legacy writes,
audit/receipt fault rollback, outer abort and reversible down/up with evidence kept.

Rollback drops only the private function. Never reverse completed tasks, reset
versions, erase audit/receipts or enable production. Reapply retains accepted keys.
The whole event-operations mission and the HTTP/UI task-completion boundary remain
incomplete after this database increment.

## Formal gate before feature implementation (2026-09-16)

The full pinned command below completed exit 0 before writing feature SQL:
24 positive TLC configurations, 60 named negative controls, 13 PlusCal integrity
tests, 2 SAT Alloy scenarios and 13 UNSAT assertions. `TaskCompletion.cfg` explored
944,014 generated / 231,576 distinct states, depth 14. Its seven negative controls
each produced exit 12 and the required invariant violation. An earlier run exited
75 because unparenthesized boolean assignments left a successor unspecified;
parenthesizing the RHS corrected the model, not any guard or invariant.

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```
