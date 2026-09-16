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

The script validates JAR checksums, requires exact PlusCal regeneration and its real-translator
regression tests, executes TLC sequentially, requires the Alloy scenarios to be satisfiable,
and requires every Alloy assertion to have no counterexample. Node 22+ is required for the
integrity checker/tests; CI selects Node 22 explicitly.

### PlusCal regeneration integrity

`ReservationRace.tla` is generated with pinned `pcal.trans -nocfg -unixEOL -lineWidth 120`.
The pre-TLC gate regenerates only a temporary copy and compares every byte, including checksum
markers and whitespace; it never auto-repairs the checked-in model. It also scans additional
PlusCal files while requiring ReservationRace to remain present. See the
[integrity contract and regeneration commands](../../docs/event-operations/pluscal-integrity-contract.md).

On 2026-09-15 the earlier warning was traced to two stripped trailing spaces in default-width
generated `UNCHANGED` lists. Explicit width 120 emits those lists without wrapping and matches
the committed file exactly, with no algorithm or expression changes. The checker is a
reproducibility boundary, not proof of translator correctness or SQL refinement.

## Bounds and results (2026-09-14–16)

| Model/configuration | Finite scope or assumptions | Result |
|---|---|---|
| `TaskCompletionClient.cfg` | One invocation, two caller/receipt identities, shape boolean, at most two dispatches; no fairness or rollback assumption | PASS before client code; 44 generated, 32 distinct states, depth 4 |
| `TaskCompletionClientCapture/Shape/Binding/Retry.cfg` | Remove request capture, receipt shape, receipt binding or no-retry guard | All four expected exit 12: OriginalRequestSent / ValidatedReceipt / ValidatedReceipt / SingleDispatch |
| `TaskCompletion.cfg` | Two attempts, one task, two keys, one intervening edit, prerequisite/lifecycle booleans, three grants, clock 0–3, distinct RACI/grant expiries; no fairness | PASS; 944,014 generated, 231,576 distinct states, depth 14 |
| `TaskCompletionAuthority/Version/Dependencies/Raci/Lifecycle/Replay/Audit.cfg` | Remove fresh authority, revision, dependency, current RACI, lifecycle, replay or audit guard | All seven expected exit 12 with their named invariant failures |
| `RaciWebEditor.cfg` | Three context generations, two revisions, eligible/ineligible context, one reviewed body/key, two attempts, valid/invalid receipt; no fairness | PASS; 154 generated, 120 distinct states, depth 9 |
| `RaciWebEditorConsent/Context/Flight/Retry/Receipt.cfg` | Remove confirmation, context, single-flight, exact-retry or receipt-validation guard | Expected exit 12 with ExplicitConfirmation / CurrentEditor / OneFlight / SameRetry / ValidatedSuccess; all five detected |
| `RaciEditorContext.cfg` | One reader/writer, manage/read/no grant, matching/foreign target, eligible/ineligible candidate, two revisions, clock 0–2; no fairness | PASS; 3,724 generated, 1,584 distinct states, depth 9 |
| `RaciEditorContextEarly/Candidate/Mixed.cfg` | Use pre-wait authority, expose ineligible candidate or omit metadata fence | Expected exit 12 with `PrivateOptions`, `EligibleOptions`, `CoherentContext`; all three detected |
| `CommandBoundary.cfg` | One command, four shape/target validity combinations, validation and commit/abort phases; no fairness | PASS; 14 generated/distinct states, depth 4 |
| `CommandBoundaryEarly/Unbound.cfg` | Commit before decoding or omit request/receipt binding | Expected exit 12 with `ValidatedCommit`; both detected |
| `RaciReassignment.cfg` | Two tasks/commands/keys, one committed obligation per task, revisions 1–3, clock 0–3, manage/read/no access; no fairness | PASS; 426,052 generated, 165,180 distinct states, depth 14 |
| `RaciReassignmentEarly/Version/Replay/Scope/Split/Audit.cfg` | Omit current authority, expected revision, replay, task-scoped key, atomic swap or audit | Expected exit 12 with the six named command invariants; all detected |
| `TaskRevisionRead.cfg` | One reader/writer, staged uncommitted write, two revisions, clock 0–3, expiry 2; no fairness | PASS; 400 generated, 192 distinct states, depth 11 |
| `TaskRevisionReadMixed/Early.cfg` | Omit metadata fence or use pre-wait authority | Expected exit 12 with `CoherentRevisionRead` / `NoExpiredDisclosure`; both detected |
| `TaskRevision.cfg` | Two captured commands, one independent RACI write, activity 0–2, RACI 0–1, revision 1–4; no fairness | PASS; 110 generated, 93 distinct states, depth 8 |
| `TaskRevisionRaci/Early.cfg` | Omit RACI revision advancement or compare before the write fence | Expected exit 12 with `NoStaleCommit`; both detected |
| `TaskView.cfg` | 3 generations, 2 read slots, 2 targets, 2 accounts plus logged out; no network fairness | PASS; 2,617 generated, 898 distinct states, depth 7 |
| `TaskViewLate/Retained/Invalid.cfg` | Omit current-receipt guard, clear-on-context-change or valid-DTO requirement | Expected exit 12 with `CurrentView` / `ValidatedView`; all three detected |
| PlusCal regeneration / integrity tests | Pinned translator, width 120, byte-exact temporary-copy comparison; real translator mutations | PASS; exact match and 13 tests, no skips; source/checksum/whitespace drift rejected |
| `FanHubOnboarding.cfg` | 3 context generations, 2 pending slots, valid/invalid eligibility, explicit/implicit exit, terminal/nonterminal receipts | PASS; 1,249 generated, 215 distinct states, depth 12 |
| `FanHubOnboardingConsent/Context/Terminal/Flight.cfg` | Negative controls: missing consent, context, terminal or same-context single-flight guard | Expected exit 12 with `ConsentOnly`, `CurrentContext`, `TerminalOnly`, `SingleFlight`; all four detected |
| `ArtistFollowConsent.cfg` | 2 Parties plus logged out, 2 artists, 3 context generations, known/unknown read and 1 command | PASS; 791 generated, 341 distinct states, depth 7 |
| `ArtistFollowConsentClick/Unknown/Stale.cfg` | Negative controls: omit click, known-state or current-context guard | Expected exit 12 with `NoUnconfirmedMutation` or `CurrentTargetReceipt`; all three detected |
| `WebOnboardingRecovery.cfg` | 2 request slots, 2 Parties plus logged-out state, 3 session generations; arbitrary response order | PASS; 883 generated, 179 distinct states, depth 7 |
| `WebOnboardingRecoveryStale/Receipt/Overlap.cfg` | Negative controls: omit generation guard, server receipt guard or in-flight coalescing | Expected exit 12 with `CurrentSessionOnly`, `AuthoritativeOnly` or `SingleFlight`; all three detected |
| `EventLifecycle.cfg` | 3 actors, 14 states, 2 command IDs | PASS; 3,613 generated/distinct states, depth 3 |
| `ReservationRace.cfg` | 2 overlapping engagements, 1 exclusive resource, unauthorized/no-reason override | PASS; 7 generated, 5 distinct states, depth 3 |
| `ReservationOverride.cfg` | Same race, owner-authorized non-empty override reason | PASS; 7 generated, 5 distinct states, depth 3 |
| `InvitationSafety.cfg` | 3 actors, 2 command IDs, expiry 2, horizon 3 | PASS; 1,107 generated, 912 distinct states, depth 7 |
| `TaskRaci.cfg` | 2 tasks, 3 collaborators, one documented override reason | PASS; 5,810 generated, 1,338 distinct states, depth 10 |
| `TaskCommit.cfg` | 2 competing transactions, 2 Responsible people, 1 task, 1 abstract dependency, 5 operations | PASS; 121 generated, 112 distinct states, depth 5 |
| `TaskCommitEarlyValidation.cfg` / `TaskCommitWriteSkew.cfg` | Negative controls: disable final validation / serialization respectively | Expected TLC exit 12 and named invariant violations; runner checks both |
| `TaskRead.cfg` | 1 read, 8 permission classes, correct/wrong event, clock 0–3, expiry 2, task revisions 1–2 | PASS; 3,861 generated, 1,788 distinct states, depth 10 |
| `TaskReadScope/Event/Early/Mixed.cfg` | Negative controls for scope widening, cross-event target, pre-wait authorization and mixed task/RACI projection | Expected exit 12 with `NoUnauthorizedTask` or `CoherentTaskProjection`; all four detected |
| `ReceiptReplay.cfg` | 1 stored receipt/read attempt; 8 actor/event/hash bindings; 3 grants; clock 0–3, expiry 2 | PASS; 2,866 generated, 1,164 distinct states, depth 8 |
| `ReceiptReplayBypass.cfg`, `ReceiptReplayStaleClock.cfg`, `ReceiptReplayStaleSnapshot.cfg` | Negative controls: omit authorization, fresh clock, or scope-write serialization | Expected exit 12 with `NoUnauthorizedDisclosure`; all three detected |
| `SnapshotRead.cfg` | 1 read, 1 revocable grant, clock 0–3 with expiry 2, 2 representative secrets | PASS; 1,133 generated, 296 distinct states, depth 12 |
| `SnapshotReadEarlyAuth.cfg`, `SnapshotReadMixedClock.cfg`, `SnapshotReadRawLog.cfg` | Negative controls: pre-lock authorization, different projection clocks, raw error logging | Expected exit 12 with `NoUnauthorizedSnapshot`, `CoherentProjection`, `LogFieldsAllowlisted` respectively; all detected |
| `CommandPrivacy.cfg` | Paired absent/existing observations; 1 command, 4 receipt classes, 2 versions, 3 grants with pre-decision changes | PASS; 81 generated, 49 distinct states, depth 4 |
| `CommandPrivacyExistenceLeak.cfg`, `CommandPrivacyReceiptLeak.cfg` | Negative controls: distinct hidden-event error / receipt response before visibility | Expected exit 12 with `OpaqueTarget`; both detected |
| `SessionFence.cfg` | 1 authentication/transaction; 32 token records; 2 actors; witness present/absent; read/new/replay | PASS; 12,685 generated, 1,153 distinct states, depth 5 |
| `SessionFenceStale/Unlocked/Party/Credential/Purpose/Witness.cfg` | Six negative configurations remove recheck, lock, actor binding, credential binding, purpose or witness requirement | Expected exit 12 with `CurrentBoundSession`; all detected |
| `ContractPayment.cfg` | 2 contract versions, 2 required parties, 1 payout command | PASS; 31 generated, 16 distinct states, depth 8 |
| `OperationalLiveness.cfg` | horizon 3, hold expiry 2, 2 notification attempts; weak fairness for each worker action | PASS; 5,713 generated, 1,440 distinct states, depth 11; all 5 temporal properties checked |
| `EventStructure.als` scenario | 1 event, 5 parties, 2 tasks/bookings, 2 contract versions, 5-bit integers | SAT; a valid integrated instance exists |
| `EventStructure.als` assertions | Command-specific bounds up to 4 atoms per top-level signature and 4-bit integers | PASS; all 8 checks UNSAT (no counterexample in scope) |
| `TaskReadStructure.als` scenario | Exactly 2 parties, 2 events, 2 tasks and 1 grant | SAT; exact-task access coexists with denied sibling and other-party access |
| `TaskReadStructure.als` assertions | Up to 4 atoms per top-level signature; owners abstracted as equivalent effective grants | PASS; all 5 checks UNSAT, including manager-gated and exact-task recipient options (no counterexample in scope) |

The PR 25 rerun completed all 22 positive TLC configurations, 48 named negative controls,
13 PlusCal integrity tests, 2 SAT Alloy scenarios and 13 UNSAT assertions. Exact commands and
executable refinement evidence appear in the [context contract](../../docs/event-operations/raci-editor-context-contract.md)
and [PR 25 report](../../docs/event-operations/pr-25-raci-editor-context.md).

The counts above came from completed commands. An earlier four-command lifecycle exploration was
stopped after 1,126,075 distinct states because the audit permutations made that scope inefficient;
it is not reported as a pass. The checked configuration was reduced to two command IDs while keeping
all lifecycle states, actors, transition targets, guards, and authority rules.

## Coverage

- `TaskCompletion.tla`: completion-time scoped authority, dependency/RACI readiness,
  supported lifecycle, exact replay and coupled audit under a serialized task write.
  See [the private completion contract](../../docs/event-operations/task-completion-contract.md).
  On 2026-09-16 the full suite passed 24 positive TLC configurations, 60 negative
  controls, 13 PlusCal tests, 2 SAT scenarios and 13 UNSAT assertions before feature
  SQL. SQL refinement, receipt hashing, multi-task namespaces and real concurrency
  additionally require the linked executable evidence; no unbounded proof claim.

- `RaciWebEditor.tla`: explicit reviewed intent, current-context dispatch/receipt, one in-flight
  operation, same-command retry and validated success. Existing scoped Alloy relations apply
  unchanged. Page replacement, source filtering, error classification and accessibility require
  executable contracts; see [the editor contract](../../docs/event-operations/raci-web-editor-contract.md).
  The PR 26 full rerun passed 23 positive configurations, 53 negative controls, 13 PlusCal tests,
  2 SAT scenarios and 13 UNSAT assertions before editor feature code was written.
- `RaciEditorContext.tla`: current manager-only options, candidate eligibility and metadata
  coherence. The SQL context is advisory: pagination, intervals, source eligibility and
  lifecycle readiness additionally require executable tests; no editing or liveness claim.
- `CommandBoundary.tla`: application validation before committing a SQL command and exact
  receipt binding, complementary to `SessionFence` and `RaciReassignment`; see the
  [HTTP contract](../../docs/event-operations/raci-api-contract.md). Does not prove SQL refinement.

- `RaciReassignment.tla`: private planning-stage assignment replacement, retry before
  expected-version comparison, task-scoped keys and atomic obligation/audit commit.
  The [command contract](../../docs/event-operations/raci-reassignment-contract.md) defines
  SQL refinement, bounds, unsupported timed assignments and future HTTP prerequisites.

- `TaskRevisionRead.tla`: coherent opt-in storage revision and canonical task projection
  under a shared metadata fence, with authorization checked after waiting. See the
  [revisioned-read contract](../../docs/event-operations/task-revisioned-read-contract.md)
  for MVCC refinement assumptions, read-lock costs and exact-string transport limits.

- `TaskView.tla`: current-generation task rendering and validated receipt consumption.
  Session/target/reload changes hide the old receipt; old responses cannot become visible.
  The [task view contract](../../docs/event-operations/task-view-contract.md) maps the abstraction
  to optional bearer/signal transport, local state, route isolation and rendered tests.
  No cross-tab cookie identity, remote push revocation or network liveness proof is claimed.

- `FanHubOnboarding.tla`: explicit optional exit, current-context eligibility/receipts,
  terminal-response validation and per-context single-flight. Stale reads cannot reopen a
  terminal acknowledgement. The [FanHub contract](../../docs/event-operations/fanhub-onboarding-contract.md)
  maps the finite abstraction to runtime decoding and rendered tests. No network liveness,
  cross-tab identity or backend authorization proof is claimed.

- `ArtistFollowConsent.tla`: client-side explicit consent, successful read before toggling,
  frozen command context and suppression of stale UI receipts. It does not verify server
  authorization or URL parsing; the [artist contract](../../docs/event-operations/artist-follow-continuity-contract.md)
  supplies executable URL, component and browser refinements. No network liveness is assumed.
- `WebOnboardingRecovery.tla`: client-only reconciliation receipt consumption and reconnect
  coalescing; session invalidation abstracts cleanup/logout/credential rotation. It does not
  model server authorization, database evidence or cookie transport identity. Two request slots
  bound retries; no eventual-network-response or lossless analytics claim is made. The
  [web integration contract](../../docs/event-operations/web-onboarding-integration-contract.md)
  maps these transitions to provider tests and documents remaining limitations.
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
- `TaskRead.tla` / `TaskReadStructure.als`: exact task/event scope matching and coherent internal
  task/RACI projection. The grant matcher does not infer permission from event.read, finance,
  coproduction or assignment. SQL identity inputs are trusted; HTTP authentication remains the
  existing separate session fence, not a capability of this projection.
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
- `CommandPrivacy.tla`: paired absent/unreadable status/body equality for fresh commands and all
  receipt classes, while retaining read-only mutation denial. The atomic observation assumes the
  existing current-authorization fence; it does not prove session revocation or constant-time access.
- `SessionFence.tla`: request-local token/party/credential binding, recheck after token locking and
  lock retention through the event decision. It models current validity, not permanent revocation
  epochs: explicit reactivation of the same credential reauthorizes it. Global role/catalog changes
  and hash collision resistance are outside this bounded token model.
- `EventStructure.als`: ownership/coproduction, time-bounded grants, visibility, RACI, dependency,
  invitation, contract-version, booking, override, and exact-money relations.

## Assumptions and limitations

- PR 33 ran only the new completion-client model and its four controls locally;
  unchanged server/Alloy model evidence is inherited from the exact checked PR 32
  parent. The full runner now configures 25 positive TLC checks and 64 negative
  controls; that updated full suite has not been rerun locally in PR 33. See the
  [client checkpoint and exact commands](../../docs/event-operations/task-completion-client-contract.md).
  The model does not establish client authorization, text validation, HTTP rollback,
  arbitrary JavaScript interleavings or durable offline recovery.
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
8. POST distinguished nonexistent targets from unreadable events. A new PostgreSQL regression
   failed before the SQL correction with `target existence leaked ... {"error":"forbidden"}`.
   `CommandPrivacyExistenceLeak` reproduces the distinct-error observation, and
   `CommandPrivacyReceiptLeak` detects premature receipt/conflict selection. The positive model
   returns the same opaque envelope while retaining internal denial history and visible read-only
   rejection. Timing, generic server Date headers, database faults and privileged audit access are
   excluded; the SQL and wire-level HTTP comparisons test the concrete envelope.
9. An already-authenticated party-only context still read a snapshot after its token was revoked;
   the new regression produced 18 passing existing HTTP examples and one failing in-flight example
   before implementation. `SessionFenceStale` exposes the missing recheck; the other five negative
   configurations distinguish lock retention, both actor bindings, credential, purpose and witness
   requirements. The corrected model checks current validity through the decision. Real HTTP
   barriers and observed database blocking refine its atomic lock abstraction. Same-credential
   reactivation is explicit reauthorization, not a permanent-revocation guarantee.
10. The first `RaciEditorContext` negative-control run failed because an unparenthesized latch
    RHS allowed an incompletely assigned successor. The runner rejected this tool/model error;
    it was not accepted as a business counterexample. Parenthesizing all three latch RHS
    expressions corrected the specification without weakening invariants. The complete rerun
    passed; Early/Candidate/Mixed mutations then produced the required named invariant failures.
11. Initial `RaciWebEditor` runs rejected ambiguous `=<<` tokenization and a mixed integer/string
    visible-generation sentinel. Spaces before tuple literals and a tuple-wrapped visible
    generation fixed the specification. Those parser/evaluation errors were not accepted as
    invariant counterexamples. A separate sandbox RMI denial required an approved unsandboxed
    rerun. The complete corrected suite passed; no safety assertion was weakened or omitted.
