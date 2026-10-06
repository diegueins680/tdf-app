# ADR 0116: Bounded hybrid formal verification for event operations

Status: accepted

Date: 2026-09-14

## Context

Event operations combine authorization changes, concurrent booking, expiring tokens/holds,
dependency graphs, exact-version consent, retries, and financial gates. Unit examples alone do not
explore enough interleavings. The repository's prior formal gate is a useful deterministic JS/model
audit but did not contain TLA+/PlusCal or Alloy artifacts.

## Decision

- Use TLA+ state machines for lifecycle, invitation, task/RACI, contract/payment, and liveness.
- Use a PlusCal process model for simultaneous confirmation of overlapping exclusive reservations.
- Use Alloy for ownership/coproduction, grants/visibility, RACI/dependencies, invitation conversion,
  contract acceptance, and booking/override relations.
- Pin tool versions/checksums and record exact finite scopes, fairness assumptions, results,
  counterexamples, changes, and limitations.
- Require a satisfiable Alloy scenario as well as assertions with no bounded counterexample.
- Translate every critical modeled transition into database constraints/transactions, typed API
  guards, property/model tests, authorization tests, and concurrency tests.

## Consequences

Passing results mean no counterexample was found in the recorded finite state space; they are not a
universal mathematical proof. Bounds should grow or split when implementation introduces capacity,
multi-resource locking, new lifecycle extensions, or authorization forms. CI executes these models
sequentially because the chosen TLC release shares a temporary standard-module path.

## Alternatives considered

### Call the existing heuristic audit formal verification

Rejected as the sole method. It detects useful code smells but does not enumerate concurrent state
transitions or relational counterexamples.

### Model everything in one specification

Rejected because audit-history permutations and unrelated product state create state explosion and
make counterexamples harder to interpret. Composed executable contracts cover the abstraction seams.
