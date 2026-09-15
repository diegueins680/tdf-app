# Opt-in aggregate-revision task read

Status: specified and model-checked before implementation; scoped SQL/HTTP/web verification
recorded below and in [PR 22](pr-22-task-revisioned-read.md). Depends on [aggregate revisions](task-revision-contract.md),
the existing canonical task reader and current-session authorization fence.

## Contract and compatibility

`GET /event-operations/events/{eventId}/tasks/{activityId}/revisioned` returns
`{ "task": <existing task projection>, "aggregateRevision": "1" }`.
This is an opt-in representation of the same canonical task, not another task store.
The existing task route, strict DTO and clients remain unchanged. No editor, new write
authority, automatic retries, offline cache or native client activation is included.

| Requirement | Operation / invariant | Model | Executable boundary / required test |
| --- | --- | --- | --- |
| RR01: coherent storage revision | Capture revision and project while sharing metadata lock | `TaskRevisionRead.CoherentRevisionRead` | SQL reader; reader-first and writer-first PostgreSQL races |
| RR02: current scoped authorization | Recheck after every potentially blocking fence | `TaskRevisionRead.NoExpiredDisclosure`, existing `TaskRead`, `SessionFence` | Canonical reader reuse; expired wait, revoked token, wrong actor/target HTTP checks |
| RR03: lossless token | Positive canonical decimal text, at most signed BIGINT maximum | Executable numeric contract (not abstract TLC integers) | Strict Haskell/web decoders; boundaries, malformed values, round-trip properties |
| RR04: compatible private representation | Exact old projection nested in strict envelope; no-store | Existing `TaskReadStructure.als` relational assertions | OpenAPI, generated client, exact JSON/unknown-field tests |
| RR05: read has no business writes | No counter, task, fence creation, audit or receipt insertion | SQL contract / executable checks | Repeated reads and rollback/reapply assertions |

The revision is a string matching ASCII `[1-9][0-9]*` and in
`1..9223372036854775807`. JSON numbers, whitespace, signs, leading zeroes,
non-ASCII digits, fractions and exponents are invalid. Existing IDs and legacy
versions remain positive JavaScript-safe JSON integers. A revision may exceed that
range without loss. The token does not prove present authority, dependency readiness
or time-window validity: time passing can change RACI attention without a storage write.

The authenticated server supplies the actor. Disabled feature, unauthorized resource,
wrong event/task binding and absence use existing opaque denial. Invalid captures are
400; invalid/inactive credentials are 401; malformed database envelopes and database
failures use sanitized unavailability, never an empty success. Responses use
`Cache-Control: private, no-store`. No extra personal or relationship data is added.

## SQL linearization and environmental assumptions

Within the current-session transaction, take locks in order: enabled feature row,
event authorization row, exact task revision row (`FOR SHARE`). Early canonical scope
check avoids locking unrelated inaccessible task metadata. Missing metadata denies;
reads never repair or create it. After any metadata wait, call the existing VOLATILE
canonical task reader, which samples a fresh clock and rechecks authorization in its
single projection statement. Return NULL on denial; otherwise pair that projection
with the locked revision cast to text. Locks last until transaction end.

The prior migration advances metadata transactionally for every tracked mutation.
A writer that already holds metadata makes the reader wait; at READ COMMITTED the
next statement sees its committed task/revision. If the reader holds metadata first,
writers may stage row changes but cannot commit until their metadata update proceeds;
MVCC hides those uncommitted changes from the reader. REPEATABLE READ / SERIALIZABLE
may reject conflicting stale snapshots with 40001; never claim a mixed successful read.
No task-write fence is acquired after metadata. The legacy reader is not modified.

Assumptions: triggers remain enabled, no privileged counter tampering, no task ID reuse,
ordinary PostgreSQL MVCC/row-lock semantics, permission writers obey the existing event
fence, session writers obey the existing token fence, and transactions eventually end
or are cancelled by operational timeouts. Read locks can delay writes, so this endpoint
is not a high-frequency polling API. No new liveness or universal proof is claimed.

## Finite verification plan

`TaskRevisionRead.tla` has one reader, one writer, two committed generations, two
revisions, a revocable grant and clock 0..3 (expiry 2). The writer can stage data before
metadata locking. Begin/read/lock/project and writer/lock/commit interleave; SQL rollback
is covered by database tests, not this abstraction. No fairness assumption is used for
these safety checks. Identity/scoped relation and session checks reuse the existing
`TaskRead`, `TaskReadStructure.als` and `SessionFence` suites; this representation adds
no relational entity or authority. Removing metadata fencing must violate coherence;
using pre-wait authorization must violate freshness. Exact commands, observed bounds
and outcomes must be recorded before implementation proceeds.

Deployment is additive and disabled by the existing API flag. Apply only after the
revision migration. Rollback drops only the new read function, retaining old task reads
and monotonic counters. Remove/disable the new route before database rollback; otherwise
its requests correctly fail unavailable. Production migration activation is out of scope.

## Verification evidence

Before feature implementation on 2026-09-15, TLC completed `TaskRevisionRead.cfg`:
400 generated / 192 distinct states, depth 11, no errors. The two negative controls
returned exit 12 and the required named invariant violations. Exact command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

The full command then exited 0 before feature implementation: 19 positive TLC checks,
37 required named negative controls, 13 PlusCal integrity tests, 2 SAT Alloy scenarios and
11 UNSAT assertions in their documented scopes. This is not an unbounded proof or an
automatic proof of SQL refinement. PostgreSQL 16 observed-barrier tests, PostgreSQL 17
complete-schema apply/down/up, and 73 real authenticated HTTP/Hspec examples subsequently
passed; the latter includes three 100-case QuickCheck properties. The strict web API suite
passed 78 tests. Exact implementation commands and remaining gates are tracked in PR 22.
