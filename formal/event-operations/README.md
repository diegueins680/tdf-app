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

## Bounds and results (rechecked 2026-09-15)

| Model/configuration | Finite scope or assumptions | Result |
|---|---|---|
| `EventLifecycle.cfg` | 5 actors (distinct event/finance approvers and records manager), 14 states, 2 command IDs; draft start | PASS; 9,941 generated/distinct states, depth 3 |
| `EventLifecycleBoundaries.cfg` | Same scope; starts separately at each of the 14 lifecycle states | PASS; 139,174 generated/distinct states, depth 3 |
| `EventLifecycleUnsafeFinance.cfg` | Mutant allowing the event approver to settle finances | Expected `AcceptedAuditIsAuthorized` violation at settlement, depth 2 |
| `EventLifecycleUnsafeArchive.cfg` | Mutant permitting the owner to archive without records-manager authority | Expected `AcceptedAuditIsAuthorized` violation at archival, depth 2 |
| `EventLifecycleUnsafeAudit.cfg` | Mutant rewriting an earlier audit actor without shortening the sequence | Expected `AuditAppendOnly` action-property violation, depth 3 |
| `ReservationRace.cfg` | 2 overlapping engagements, 1 exclusive resource, unauthorized/no-reason override | PASS; 7 generated, 5 distinct states, depth 3 |
| `ReservationOverride.cfg` | Same race, owner-authorized non-empty override reason | PASS; 7 generated, 5 distinct states, depth 3 |
| `InvitationSafety.cfg` | 3 actors, 2 command IDs, expiry 2, horizon 3 | PASS; 1,107 generated, 912 distinct states, depth 7 |
| `TaskRaci.cfg` | 2 tasks, 3 collaborators, one documented override reason | PASS; 5,810 generated, 1,338 distinct states, depth 10 |
| `ContractPayment.cfg` | 2 contract versions, 2 required parties, 1 payout command | PASS; 31 generated, 16 distinct states, depth 8 |
| `OperationalLiveness.cfg` | horizon 3, hold expiry 2, 2 notification attempts; weak fairness for each worker action | PASS; 5,713 generated, 1,440 distinct states, depth 11; all 5 temporal properties checked |
| `EventStructure.als` scenario | 1 event, 5 parties, 2 tasks/bookings, 2 contract versions, 5-bit integers | SAT; a valid integrated instance exists |
| `EventStructure.als` assertions | Command-specific bounds up to 4 atoms per top-level signature and 4-bit integers | PASS; all 8 checks UNSAT (no counterexample in scope) |

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

- `EventLifecycle.tla`: controlled lifecycle transitions, separation of approval/settlement duties,
  visibility coupling, idempotency keys, and append-only audit behavior.
- `ReservationRace.tla`: PlusCal translation of two concurrent confirmations for one exclusive
  resource, including the justified owner override path.
- `InvitationSafety.tla`: intended recipient, expiry, revocation, replay/idempotency, and permission
  attenuation during account conversion.
- `TaskRaci.tla`: dependency DAG, completion guards, audited emergency override, exactly one
  accountable party, non-empty responsible set, and collaborator-removal orphan prevention.
- `ContractPayment.tla`: exact-version consent, material amendment reset, milestone gate,
  separation of payout approval, and deduplicated payout effect.
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
