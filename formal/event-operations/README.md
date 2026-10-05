# Event operations formal models

These models define the safety boundary for the incremental event-operations work. They do not
claim an unbounded mathematical proof. TLC exhaustively explores the finite configurations below;
Alloy searches the stated finite scopes. Executable database, API, property, concurrency, and
authorization tests remain required for the implementation.

The shared gate also runs the [chat mutation boundary](../system/messaging.md),
with its separately stated sequential scope and two mandatory negative controls.

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

## Bounds and results (rechecked 2026-09-15)

| Model/configuration | Finite scope or assumptions | Result |
|---|---|---|
| `EventLifecycle.cfg` | 5 actors (distinct event/finance approvers and records manager), 14 states, 2 command IDs; draft start | PASS; 9,941 generated/distinct states, depth 3 |
| `EventLifecycleBoundaries.cfg` | Same scope; starts separately at each of the 14 lifecycle states | PASS; 139,174 generated/distinct states, depth 3 |
| `EventLifecycleUnsafeFinance.cfg` | Mutant allowing the event approver to settle finances | Expected `AcceptedAuditIsAuthorized` violation at settlement, depth 2 |
| `EventLifecycleUnsafeArchive.cfg` | Mutant permitting the owner to archive without records-manager authority | Expected `AcceptedAuditIsAuthorized` violation at archival, depth 2 |
| `EventLifecycleUnsafeAudit.cfg` | Mutant rewriting an earlier audit actor without shortening the sequence | Expected `AuditAppendOnly` action-property violation, depth 3 |
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
The original draft-only two-command run could not reach settlement. The additional
boundary configuration starts at every lifecycle state, so settlement authority is
exercised without claiming a complete draft-to-archive trace. `RequiredAuthority`
checks accepted records independently of the mutable admission guard. Append-only
means every old record remains an unchanged prefix, not merely nondecreasing length.
The runner requires the exact named failures from all three negative controls.

## Coverage

- `FanHubOnboarding.tla` (rechecked 2026-09-17): explicit close, terminal receipt,
  current session generation and one pending completion per generation. Three
  generations and two request slots: 1,249 generated / 215 distinct states, depth 12.
  Safety and conditional `RequestsResolve` liveness pass. Four unsafe guard configs
  must violate their named invariants; the unfair config must violate liveness.
  `WF_vars(ReturnSlot(slot))` assumes every dispatched request eventually returns
  success or failure. Indefinitely hung transport is deliberately not certified.
  [Implementation conformance and evidence](../../docs/ux-ui-audit/2026-09-17/fanhub-onboarding.md)
  map these transitions to real component and browser tests; this does not prove
  server persistence, authorization, unlimited sessions or cross-tab revocation.

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
  The same obligation applies to lifecycle transitions: their database adapter validates
  event, command, target state and exact next version inside the session transaction.
  The lifecycle HTTP regression corrupts real SQL results after writes and checks rollback
  of state, transition, audit and receipt rows while retaining the existing error envelope.
  The feature-disable boundary is checked separately against PostgreSQL: an enabled flag
  row is share-locked before the event row until transaction completion. Both command-first
  and disable-first orders are observed at READ COMMITTED, REPEATABLE READ and SERIALIZABLE;
  the pre-fix implementation is a failing control. These executions do not establish a
  universal refinement proof or authorize enabling the feature in production.

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

## Public Live Session credential validation

`AccessCodeValidation.tla` checks one request, two code edits (including an ABA return),
successful/invalid account responses and timeout. CurrentCredential and VerifiedAccount
map to the generation guard and positive safe integer partyId check in
LiveSessionPublicPage. Native disabled fieldset prevents input before verification;
component tests cover same-origin API, superficial 200 responses, stale completion and
retained input. TLC 1.7.2 checks eventual termination under weak fairness of a response
or timeout; the browser implementation uses AbortController and a 30-second timeout.
This assumes fetch honors abort and the event loop is scheduled. It does not establish
ongoing token validity, backend authorization, submission persistence, or network
availability. The server must authorize each submission independently. Negative controls
remove each guard and must violate the corresponding named invariant.
## Artist activation context (UX-013 / PR #406)

`ArtistActivation.tla` checks one activation and session refresh with up to two context
changes, including navigation away and back (ABA). A context generation represents
both the session object and React Router location key. TLC 1.7.2 checks CurrentContext,
PersistedAuthority, and eventual termination assuming both requests eventually return
(success or failure), via weak fairness. It does not promise network termination in the
implementation or certify backend authorization. `isCurrent()` fences both await
boundaries; the authoritative response must retain the party and Artist role. Two
component regressions defer each boundary, navigate to a claim and back, and require
no login or redirect; both fail before the generation repair. Disabling the generation
fence is an executable negative control for CurrentContext. Existing account-change,
storage-denial and single-flight component cases remain required.

Execution2026-09-18:15 distinct ArtistActivation states, CurrentContext/PersistedAuthority
and Terminates pass; unsafe configuration violates CurrentContext. Initial model
syntax mistakenly made the context predicate a transition guard instead of the
assigned boolean; TLC's liveness counterexample exposed the disabled stale-response
transition. Parenthesizing the assigned expression restored the intended discard
transition. This model-authoring error is distinct from the component regressions.

## Live Session submission authority — 2026-09-18

`LiveIntakeAuthority` connects UX-260917-020 to explicit credential transport and
receipt fencing in `LiveSessionIntakeForm` / `submitLiveSessionIntake`. TLC1.7.2
exhaustively checked96 generated /42 distinct states (depth5): two accounts,
one verified code, one submission, up to two code edits, arbitrary ambient cookie
switches, and successful or failed persistence. `ExplicitAuthority` requires that
writes use the verified code account; `CurrentReceipt` excludes stale/ABA success;
`PersistedReceipt` permits success only after persistence. Weak fairness of the
backend response action establishes that a pending request eventually settles.
This assumes a response eventually arrives; it does not prove network availability,
transactional atomicity of the intake handler, duplicate-submission prevention,
or permissions of unrelated CRM handlers.

Three executable negative controls independently remove explicit credentials,
receipt generation fencing, or persistence confirmation. Each produced its named
invariant counterexample. Component regressions cover the corresponding UI/API
mechanisms, and an actual isolated HTTP/PostgreSQL browser test checks code-account
persistence while a different cookie account remains signed in. Optional nested
null handling is covered by multipart parser contract tests, not this state model.

## Paused onboarding experiment contract — EXP-01

ExperimentAuthority.tla bounds two account identities and three requests (two share
one account). Assignment/exposure is one transaction under the progress-row lock;
completion/expiry can occur before acquisition or after commit, never through the
held row. Enabled and paused configurations check AccountAndEligibilityAuthority,
ExposureAtMostOnce, PausedDoesNotWrite and ExposureHasAssignment. RequestsSettle
assumes weak fairness of lock acquisition/commit, finite requests and eventual DB
availability. The enabled configuration explores876distinct/1920generated states,
depth12. Three negative controls remove locked eligibility, idempotent exposure or
account binding and must produce the named invariant counterexample.

TLC1.7.2 and Alloy6.2.0 are checksum-pinned by the existing runner. The complete
runner passed before integration of main4b0bc6ed7. The model assumes a valid server
authentication decision at request admission; it does not certify revocation during
an already admitted transaction, credential storage, arbitrary handlers, mobile UI,
statistical validity, production performance or unbounded executions.

Implementation: requireSessionUser supplies Party authority; withExperimentProgress
locks the stored progress row before reading eligibility and the clock. The existing
unique account/experiment/version key and exposedAt compare-and-set preserve stable
assignment and at-most-once exposure. Paused requests bypass locking/writes. The
historical8c2960874handlers and3f1e6f3both-arm regression were ported narrowly; their
existing migration and recorded introduction ancestry are unchanged.

Conformance: scripts/__tests__/experiment-http-runtime.mjs runs two isolated backend
processes against PostgreSQL. It verifies paused/no-write state, returning-device
accounts without signup markers, completed/expired accounts, 16 concurrent assignment
requests and16exposures, account isolation, revoked tokens and no granted roles.
A second SQL connection holds completion/expiry changes while the actual handler is
observed waiting for that row lock; afterward neither exposure nor expired assignment
is accepted. SQLite tests separately cover both variants and paused configuration.
The experiment remains disabled; deployment tooling rejects activation and verifies
the effective flag. No experiment launch or conversion claim is authorized here.
## Checkout readiness — UX-260917-026 / PR #429

`CheckoutReadiness.tla` models two request slots and generations 0..2, an open/closed
buyer dialog, arbitrary SDK readiness, cancellation and reopening. TLC1.7.2 checks
389 generated /221 distinct states (depth9): `ReadyBeforeReservation`,
`CurrentReservation`, `SingleCurrentFlight`, and `Settles`. Weak fairness for each
`Resolve` assumes the SDK promise eventually settles, successfully or unsuccessfully.
An indefinitely stalled SDK/network is not certified. Three negative configurations
independently remove readiness, current-generation fencing, or single-flight admission;
each must violate its named safety invariant.

The implementation checks `loadCheckoutStripe()` before `createPaymentIntent`, fences
both await boundaries using `buyerAttempt`, and rejects duplicate submissions through
`buyerPending`. Layout cleanup, close, event/tier/session changes invalidate the generation.
Session changes also clear pending success timers; late payment callbacks are fenced.
Component regressions exercise null SDK plus retry/input retention, unmount, cancellation
and duplicate submissions. The model does not prove payment settlement, backend
idempotency, inventory transactions, session authorization or provider availability.
Existing event/payment models and backend gates retain their separate scope.

### Cancellation during reservation (review PRRT_kwDOQPdUrM6joQEq)

`CheckoutCancellation.tla` splits SDK readiness from the consequential reservation
request. With `GuardReservation=TRUE`, TLC1.7.2 explores 13 generated /8 distinct
states (depth6), checks `NoAbandonedReservation`, `PendingRetainsDialog` and
`ReservationSettles` under weak fairness of the server response. Cancel is permitted
before that request. The component sets a synchronous `reservationPending` ref before
sending and guards every dialog-close path; `reserving` disables the visible button.
The response reaches the payment form; failure restores cancellation and input.

The reservation negative configuration removes that guard: submit, SDK ready, cancel, successful
server response is the expected abandoned-reservation counterexample. A component
regression failed against60d754aab and passes after the guard, exercising button,
Escape, backdrop and duplicate submission while the API promise is pending.
This focused model assumes the component remains mounted and the account/context
remains fixed after dispatch. It does not prove recovery after tab/browser shutdown,
forced navigation, account switching, ambiguous transport failure, server expiry or
payment compensation. Existing generation fencing still prevents another context
from receiving old results; server reservation recovery remains a separate concern.

Review PRRT_kwDOQPdUrM6jofR3 extends the same close guard across elements.submit and
stripe.confirmPayment. A synchronous child ref also prevents duplicate payment
submission before React commits processing state; a generation-fenced callback
updates the parent paymentPending guard. Success reaches confirmation and onSuccess
once; a rejected payment restores dismissal without reporting success.
CheckoutCancellation now includes payment/paying/confirmed states, NoLostPayment,
PaymentSettles and a second negative configuration that specifically permits closing
during payment confirmation. Removing GuardPayment yields the lost-success trace.
The additional component regression reproduced duplicate confirmation before the
fix and verifies Escape/backdrop suppression and one successful order callback;
another verifies rejection recovery. The fixed-context and eventual-response limits
above still apply; no real card, provider settlement or background recovery is proved.

## Concurrent recent navigation — UX-260917-031

NavigationVisit.tla models two accounts and three requests (two for one account),
plus one concurrent settings update. Atomic upsert linearizes insertion/increment;
NoFailedVisits, CountsMatchAccepted and SettingsPreserved require no duplicate-key
failure, exact per-account counts, and unchanged independent preferences. AllSettle
assumes weak fairness of each finite DB operation and eventual DB availability.
The unsafe configuration separates lookup/insertion and must violate NoFailedVisits.
This model assumes valid account authority supplied by authentication; it does not
prove credential validation, exactly-once network retry or timestamp ordering.
Each accepted HTTP visit intentionally increments; requests have no idempotency key.

Conformance is scripts/__tests__/navigation-http-runtime.mjs: real isolated PostgreSQL
and the actual compiled handler. A table write lock permits both legacy reads before
insertion, then is released only after two DB waiters are observed. The old binary
returns500; the corrected executable must pass16 first visits, mixed settings/visits,
separate accounts, denied access and revoked tokens. Test hosts/databases are restricted
to local/CI isolated databases. Tool versions remain TLC1.7.2/Alloy6.2.0 as pinned above.
Execution results and limits are recorded in the canonical audit checkpoint.

Executed2026-09-18: NavigationVisit33generated/16distinct states, depth5, all
listed invariants and conditional liveness pass. Legacy negative reaches the named
NoFailedVisits counterexample (two reads, insertion, duplicate insertion). The full
formal runner passes, including all existing negative controls and Alloy checks.
The pinned TLA+ release artifact is1.7.2; its runtime reports TLC2 engine2.17.

### Provider identity release recovery

`ProviderRollback.tla` models two machines, additive migration, canary/fleet
deployment, an arbitrary concurrent provider binding, verification failure and
per-machine recovery. Four configurations enumerate legacy, compatible and
mixed prior binaries plus a compatible fallback from a legacy fleet. `PriorSafe`
is the verified candidate chosen for recovery; `InitialSafe` describes the original
fleet independently. A compatible fallback is a precondition of recovery when a
prior binary is unsafe. `NoUnsafeRestoration` and `ModernNeverDowngrades` prohibit
restoring legacy email authority. `StoppedFleetSafe` also requires every replica
to be compatible after successful recovery, including untouched legacy replicas.
`BindingPreserved` forbids clearing established bindings. `RecoveryDecisionSettles`
assumes weak fairness and successful completion of recovery commands; it does
not guarantee cloud availability. No fairness of deployment is assumed. The
model allows the temporary mixed fleet during rollout/recovery; it does not
prove that public traffic cannot reach legacy replicas during that interval.

The unsafe configuration reproduces unconditional legacy rollback and must
violate `NoUnsafeRestoration`. The partial configuration reproduces recovering
only the canary and must violate `StoppedFleetSafe`. `withCompatibleRollback`
is the implementation boundary before the actual deploy command;
`recoverReleaseMachines` is the actual outer recovery loop and includes every
unsafe replica once any deployment was attempted. Unit conformance enumerates
both canary choices and all four prior combinations. An injected recovery
failure verifies subsequent replicas are still attempted and failure is recorded;
no successful fleet recovery is claimed in that case. Pre-deployment failures
perform no machine recovery. The executable model abstracts verified immutable
artifacts and trusted commit ancestry. It does not prove identity-token validation,
the cloud provider, or whole-system availability. Exact executions are in the
canonical UX audit record.

## Optional private-token cache recovery (2026-09-18)

`OptionalTokenRecovery` checks32initial scenarios,96generated/distinct states (depth3).
No invented/missing token can dispatch a request; tracking fragments take precedence.
Weakly fair resolve/dispatch ensure recovery terminates when storage returns or throws.
Unsafe storage (84states) violates liveness; cache-first mutation (41states) violates
fragment precedence. This is client token-presence conformance, not server permission
or payment-settlement verification. [Evidence and limits](../../docs/ux-ui-audit/2026-09-17/storage-boundaries.md).
### Marketplace catalog selection consistency

`MarketplaceCatalogRead.tla` models one listing, approved/unapproved immutable terms,
selection, concurrent sale deactivation, the terms read and response rendering. Both
approved and unapproved configurations explore13generated/11distinct states, depth5;
SelectedRentalKeepsApprovedTerms, UnapprovedTermsNotUsed and NoUnselectedListing hold.
RequestFinishes assumes weak fairness for selection/read/render; provider/database
outages and changing term approval are not modeled. Terminal quiescence is expected.
The unsafe active-listing join produces the named price-fallback counterexample in
11states: select→deactivate→read terms→render. The full pinned TLC/Alloy suite passes.

Implementation: bind the IDs already selected into the batch's parameterized IN
query, with an empty-list guard, retaining approved/active term filters. Actual HTTP
regression interleaves a second PostgreSQL connection after selected listings have
reached the handler and before the terms read. It reproduces10001 instead of2001
minor units on the old code, then preserves2001/terms on the corrected handler; the
next request excludes the deactivated row. Query count stays5 with80generated
fixtures. This does not certify checkout/fulfillment transactions or claim snapshot
isolation for every returned asset field.
### Calendar connection and OAuth return

`CalendarConnection.tla` bounds one returned code, two automatic dispatch attempts,
three session occurrences (including A→B→A), and success/failure responses. TLC
explores 27 generated / 21 distinct states, depth 5. `AtMostOneAutomaticExchange`,
`OnlyPersistedConnection`, and `CurrentSessionReceipt` pass. `RequestSettles` assumes
weak fairness of the combined successful/failed response; a permanently unavailable
network is outside that liveness assumption. Terminal quiescent states are expected,
so deadlock checking is disabled explicitly; safety and temporal properties remain
enabled. Three unsafe configurations separately reproduce replay (9 states), a
storage-derived connection claim (3 states), and a stale session receipt (11 states;
see the executable output for the exact exploration).

Mapping: CalendarSyncPage consumes/removes the URL code before queued dispatch;
a synchronous busy guard also prevents duplicate clicks. Session occurrences remount
the form and partition query caches, with mounted/current-session checks immediately
before response effects. Only the selected calendar's API configuration establishes
its saved connection. React tests cover replay/error/retry, StrictMode, logout before
render, A→B→A, cache races, and persisted timestamps. The model abstracts provider
exchange/persistence, selected-calendar identities and query-library scheduling; those
require HTTP and component/browser conformance tests. It does not prove Google OAuth
consent, token revocation, server authorization or arbitrary calendar handlers.
## Directory favorite authority (2026-09-18)

`DirectoryFavoriteAuthority` bounds one read/save request, three session occurrences,
lagging render and unmount. TLC checks356 generated/208distinct states (depth9),
current-session dispatch/receipt and authoritative persistence. Weak fairness assumes
dispatch and eventual success/failure response; terminal quiescence is permitted.
Unsafe dispatch/receipt variants must violate their named invariants (29/63states).
[Implementation, counterexamples and limits](../../docs/ux-ui-audit/2026-09-17/directory-entry.md).

## Marketplace optional storage boundary (2026-09-18)

`MarketplaceStorage.tla` connects UX-260917-007 to the page's optional
read/write/remove helpers and the unchanged required checkout idempotency key.
One visit selects working/denied storage; at most two checkout attempts are modeled.
`NoStorageExceptionEscapes` and `NoDispatchWithoutDurableKey` hold; `BrowsingAvailable`
assumes weak fairness of opening the page, not eventual storage availability. TLC
1.7.2 distribution / TLC2 2.17 explores eight generated/distinct states, depth four.
Unsafe optional-cache and required-key configurations produce their named invariant
counterexamples (four and six generated states). Terminal quiescence is allowed.
The full pinned TLC/Alloy6.2.0 suite passes on Java21.0.12.1.

The first draft's unparenthesized Boolean assignment was a model defect and failed
the positive invariant. Parenthesizing that assignment repaired the specification;
that failure is not claimed as a product counterexample. Product negative controls
are four original Marketplace component failures. The 23 final component/API tests
also reject checkout twice without any POST when getter/getItem/setItem fail.
Browser conformance checks use the production bundle and actual isolated PostgreSQL
catalog: search, empty results, URL state and reload despite denied operations.
This small model does not prove server payment execution, successful persistence,
all crash/reload schedules, account isolation or storage availability transitions;
existing payment models and HTTP contracts remain separate evidence.
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
