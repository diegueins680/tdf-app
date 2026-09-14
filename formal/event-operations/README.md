# Event operations formal models

These models define the safety boundary for the incremental event-operations work. They do not
claim an unbounded mathematical proof. TLC exhaustively explores the finite configurations below;
Alloy searches the stated finite scopes. Executable database, API, property, concurrency, and
authorization tests remain required for the implementation.

## Toolchain used

- OpenJDK `21.0.12.1`, extracted from the Homebrew Sonoma bottle into a temporary directory because
  the host Command Line Tools were too old for a normal Homebrew installation.
- TLA+ tools/TLC `1.7.2`, SHA-1 `7f21faa2cdae3189e7d5fadb4488f0dfcc658407`, matching the upstream
  release checksum; SHA-256 `fa18543e44ed5974a85bd2c60c0dc16620ae117680ea8e693d2691999ed90b22`.
- Alloy `6.2.0`, SHA-256
  `6b8c1cb5bc93bedfc7c61435c4e1ab6e688a242dc702a394628d9a9801edb78d`.
- TLC was run with one worker outside the filesystem sandbox because TLC opens a local RMI socket.
  Every run used a distinct temporary metadata directory. TLC runs must remain sequential because
  version 1.7.2 copies standard modules through a shared temporary location.
- Alloy used the cross-platform `sat4j` solver.

## Exact local command

After setting the paths to a Java 17+ runtime and the two pinned JARs:

```bash
JAVA_BIN=/path/to/java \
TLA2TOOLS_JAR=/path/to/tla2tools-1.7.2.jar \
ALLOY_JAR=/path/to/alloy-6.2.0.jar \
bash scripts/verify-event-operations-formal.sh
```

The script validates JAR checksums, executes TLC sequentially, requires the Alloy scenario to be
satisfiable, and requires every Alloy assertion to have no counterexample.

## Bounds and results (2026-09-14)

| Model/configuration | Finite scope or assumptions | Result |
|---|---|---|
| `EventLifecycle.cfg` | 3 actors, 14 states, 2 command IDs | PASS; 3,613 generated/distinct states, depth 3 |
| `ReservationRace.cfg` | 2 overlapping engagements, 1 exclusive resource, unauthorized/no-reason override | PASS; 7 generated, 5 distinct states, depth 3 |
| `ReservationOverride.cfg` | Same race, owner-authorized non-empty override reason | PASS; 7 generated, 5 distinct states, depth 3 |
| `InvitationSafety.cfg` | 3 actors, 2 command IDs, expiry 2, horizon 3 | PASS; 1,107 generated, 912 distinct states, depth 7 |
| `TaskRaci.cfg` | 2 tasks, 3 collaborators, one documented override reason | PASS; 5,810 generated, 1,338 distinct states, depth 10 |
| `TaskCommit.cfg` | 2 competing transactions, 2 Responsible people, 1 task, 1 abstract dependency, 5 operations | PASS; 121 generated, 112 distinct states, depth 5 |
| `TaskCommitEarlyValidation.cfg` / `TaskCommitWriteSkew.cfg` | Negative controls: disable final validation / serialization respectively | Expected TLC exit 12 and named invariant violations; runner checks both |
| `ReceiptReplay.cfg` | 1 stored receipt/read attempt; 8 actor/event/hash bindings; 3 grants; clock 0–3, expiry 2 | PASS; 2,866 generated, 1,164 distinct states, depth 8 |
| `ReceiptReplayBypass.cfg`, `ReceiptReplayStaleClock.cfg`, `ReceiptReplayStaleSnapshot.cfg` | Negative controls: omit authorization, fresh clock, or scope-write serialization | Expected exit 12 with `NoUnauthorizedDisclosure`; all three detected |
| `SnapshotRead.cfg` | 1 read, 1 revocable grant, clock 0–3 with expiry 2, 2 representative secrets | PASS; 1,133 generated, 296 distinct states, depth 12 |
| `SnapshotReadEarlyAuth.cfg`, `SnapshotReadMixedClock.cfg`, `SnapshotReadRawLog.cfg` | Negative controls: pre-lock authorization, different projection clocks, raw error logging | Expected exit 12 with `NoUnauthorizedSnapshot`, `CoherentProjection`, `LogFieldsAllowlisted` respectively; all detected |
| `ContractPayment.cfg` | 2 contract versions, 2 required parties, 1 payout command | PASS; 31 generated, 16 distinct states, depth 8 |
| `OperationalLiveness.cfg` | horizon 3, hold expiry 2, 2 notification attempts; weak fairness for each worker action | PASS; 5,713 generated, 1,440 distinct states, depth 11; all 5 temporal properties checked |
| `EventStructure.als` scenario | 1 event, 5 parties, 2 tasks/bookings, 2 contract versions, 5-bit integers | SAT; a valid integrated instance exists |
| `EventStructure.als` assertions | Command-specific bounds up to 4 atoms per top-level signature and 4-bit integers | PASS; all 8 checks UNSAT (no counterexample in scope) |

The counts above came from completed commands. An earlier four-command lifecycle exploration was
stopped after 1,126,075 distinct states because the audit permutations made that scope inefficient;
it is not reported as a pass. The checked configuration was reduced to two command IDs while keeping
all lifecycle states, actors, transition targets, guards, and authority rules.

## Coverage

- `EventLifecycle.tla`: controlled lifecycle transitions, separation of approval/settlement duties,
  visibility coupling, idempotency keys, and append-only audit behavior.
- `ReservationRace.tla`: PlusCal translation of two concurrent confirmations for one exclusive
  resource, including the justified owner override path.
- `InvitationSafety.tla`: intended recipient, expiry, revocation, replay/idempotency, and permission
  attenuation during account conversion.
- `TaskRaci.tla`: dependency DAG, completion guards, audited emergency override, exactly one
  accountable party, non-empty responsible set, and collaborator-removal orphan prevention.
- `TaskCommit.tla`: separate prepare/commit steps, final transaction-state validation, and write
  serialization; negative controls detect blocked completion and concurrent responsibility loss.
- `ReceiptReplay.tla`: historical receipt reads require current access and exact actor/event/hash
  binding. Captured decision evidence avoids incorrectly treating a later revocation as retroactive.
  Reauthorization, current time and scope-write serialization each have an independent negative control.
- `ContractPayment.tla`: exact-version consent, material amendment reset, milestone gate,
  separation of payout approval, and deduplicated payout effect.
- `SnapshotRead.tla`: current authorization after locking, one authorization instant for a coherent
  projection, and allowlisted error-log fields. Its lock abstracts the PostgreSQL scope-write fence;
  feature-disable races, JSON decoding and exception cancellation require executable tests.
- `OperationalLiveness.tla`: eventual hold expiry, notification dead-lettering, offline sync/conflict,
  work terminal/attention state, and financial reconciliation/failure/alert under weak fairness.
- `EventStructure.als`: ownership/coproduction, time-bounded grants, visibility, RACI, dependency,
  invitation, contract-version, booking, override, and exact-money relations.

## Assumptions and limitations

- Time is a bounded integer abstraction. Application tests must cover IANA timezone conversion,
  daylight-saving changes, UTC persistence, recurrence, and event-local presentation.
- A reservation confirmation is one atomic transition. PostgreSQL exclusion constraints and
  serializable transaction tests must implement that atomicity.
- The PlusCal scope contains one exclusive resource and two overlapping requests. Capacity greater
  than one, buffers, multi-resource bookings, deadlocks between resources, and non-exclusive
  capacity require executable transaction tests and later enlarged models.
- RACI models actionable tasks only. Checklist items and advisory tasks may use a weaker policy in
  implementation, but must not bypass a task marked as requiring accountability.
- Fairness means an enabled worker is eventually scheduled and its dependencies eventually return.
  It does not assume external providers succeed; terminal failure, conflict, dead letter, and alert
  are valid observable liveness outcomes.
- Alloy integer arithmetic is bounded and does not establish financial arithmetic correctness.
  Money remains `(currency, minor_units)` and requires checked arithmetic/property tests.
- Audit append-only behavior at this layer assumes the database role cannot update/delete audit
  rows. Database privileges, hash chaining, retention, and backup recovery need independent tests.

## Counterexample log

1. The initial Alloy task-locality fact used equality between `dependsOn.event` and a task's event.
   It accidentally required every task to have a dependency, making every finite acyclic task graph
   impossible. It was changed to subset membership: dependencies, if present, must be event-local.
2. TLC reported terminal command exhaustion as deadlock. The models intentionally allow finite
   command sets to terminate, so configurations set `CHECK_DEADLOCK FALSE`; safety properties still
   examine the complete reachable graph and liveness has its own fair temporal specification.
3. TLC rejected invitation transitions that did not explicitly preserve `now`. The missing
   `UNCHANGED now` clauses were added before the passing run.
4. The first SQL foundation checked completion before the legacy handler replaced dependencies.
   A PostgreSQL regression reproduced a committed completed task with a pending dependency.
   `TaskCommitEarlyValidation.cfg` detects that unsafe abstraction; `TaskCommit.cfg` validates the
   complete proposed state before commit. The corrective migration defers checks until final state.
5. Independent snapshots can each approve removing a different Responsible party, leaving none.
   `TaskCommitWriteSkew.cfg` detects that unsafe abstraction. The passing model serializes writers;
   PostgreSQL tests use a real per-event write fence and deterministic concurrency barriers under
   READ COMMITTED, REPEATABLE READ and SERIALIZABLE. This does not prove arbitrary SQL isolation.
6. The lifecycle SQL returned an exact stored receipt before checking current access. A disposable
   PostgreSQL test reproduced an accepted response after revocation. `ReceiptReplayBypass` finds
   the same disclosure. Two further negative configurations expose stale transaction-start time
   and stale permission snapshots. The positive model rechecks current access with fresh time,
   serialized against grant changes. Its scope is receipt disclosure, not all HTTP authorization;
   duplicate-effect and durable-audit claims require the accompanying SQL tests.
7. The prior GET used transaction-start time and separately fetched state, capabilities and allowed
   transitions. `SnapshotRead` negative controls expose early authorization and mixed-clock reads;
   a PostgreSQL control reproduces stale-clock disclosure after expiry. The corrected read locks
   first and samples one instant. A third negative control detects raw exception-log fields.
   This model verifies field allowlisting, not all application logging or arbitrary secret content;
   Haskell property tests and real database races complement the finite abstraction.
