# Event operations assumptions and environmental constraints

## Domain assumptions

1. `PartyId` remains the identity/organization interoperability key. Email is contact data, not an
   authorization principal.
2. Existing `social_event` identifiers and public routes remain stable. New tables reference them
   and compatibility projections translate old lifecycle codes during rollout.
3. An event always has at least one owner. Coproducers and collaborators receive explicit scopes;
   they do not inherit owner authority.
4. UTC instants are authoritative for scheduling. Each event/session retains an IANA timezone for
   input and display. `America/Guayaquil` is the initial default, not a hard-coded system timezone.
5. Half-open intervals `[start, end)` define overlap. Buffers are materialized into the occupied
   interval before a reservation reaches `confirmed`.
6. Actionable tasks require at least one Responsible party and exactly one Accountable party.
   Removing a party must atomically reassign obligations or fail.
7. A material contract change always produces a new immutable version and clears confirmation
   eligibility until all required parties accept that version.
8. Money uses ISO currency plus integer minor units (or an explicit exact-decimal exception for
   currencies that require it). Binary floating point is forbidden for authoritative amounts.
9. Provider redirects are advisory navigation. Only a signature-verified provider event or an
   authenticated server-to-server verification can advance payment evidence.
10. Legal language, tax policy, insurance requirements, worker classification, and accounting
    policy require qualified review before production activation.

## Fairness and liveness assumptions

- The database, queue, and clock eventually become available long enough for a worker lease to run.
- A continuously enabled worker is eventually scheduled (weak fairness).
- External dependencies may fail forever. Liveness therefore permits a visible terminal failure,
  dead letter, reconciliation case, conflict, or needs-attention state instead of assuming success.
- Operators monitor dead-letter/reconciliation alerts and retention jobs; the model does not prove
  human response time.
- Clients eventually reconnect or explicitly discard their offline queue. Safety-critical conflicts
  are never silently resolved by last-write-wins.

## Environmental constraints found on 2026-09-14

- Repository baseline: local `origin/main` at `17a33eca11d585d84435af85340beece9b51d14e`.
- The primary worktree contained unrelated uncommitted work, so this phase uses the isolated local
  branch `feat/event-operations-formal-foundation` in
  `/Users/diegosaa/GitHub/tdf-app-event-operations`.
- GitHub network resolution failed and the configured `gh` token was invalid. No push, PR, remote
  check, review, or merge was performed.
- The host had no working Java runtime. A normal `brew install openjdk@21` failed because Apple
  Command Line Tools were too old after downloading the bottle. Homebrew auto-updated and upgraded
  `cmake` to 4.4.3 before the dependency failure. Verification used the extracted bottle in
  `/private/tmp`; no Xcode setting or Command Line Tools installation was modified.
- TLC needs a local RMI socket and therefore required an approved run outside the sandbox. Models
  were run sequentially after parallel processes collided in TLC's shared standard-module temp path.
- Docker was installed, but access to the local daemon socket was denied inside the sandbox. No
  container or production service was changed.
- PostgreSQL client 16.10, Stack 3.7.1, Node 24.8.0, npm 11.6.0, Playwright 1.59.1, Git, and the
  repository's Haskell/JS test runners were present. Alloy and TLC were downloaded as pinned official
  temporary JARs.

## Formal limits

Finite TLC/Alloy results establish absence of a counterexample only inside the documented bounds.
They do not prove correctness for arbitrary parties, events, tasks, resources, messages, currencies,
or time. The implementation must preserve the modeled atomic transitions and reinforce them with
database constraints, typed contracts, property tests, concurrency tests, and authorization tests.
