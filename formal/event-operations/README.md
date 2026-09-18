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

## Directory favorite authority (2026-09-18)

`DirectoryFavoriteAuthority` bounds one read/save request, three session occurrences,
lagging render and unmount. TLC checks356 generated/208distinct states (depth9),
current-session dispatch/receipt and authoritative persistence. Weak fairness assumes
dispatch and eventual success/failure response; terminal quiescence is permitted.
Unsafe dispatch/receipt variants must violate their named invariants (29/63states).
[Implementation, counterexamples and limits](../../docs/ux-ui-audit/2026-09-17/directory-entry.md).
