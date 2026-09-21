# Disabled legacy escrow writes

Authority: explicit user decision, 2026-09-20: “Disable the two unverified financial writes.”
This resolves SYS-C06 for the current delivery, consistently with accepted
[ADR 0101](../../docs/adr/0101-verified-payment-events.md). No current exception was approved.
SYS-D07 adds these two real financial writers and safe release recovery to the finite audit scope.

| ID | Current guarantee | Actual implementation |
|---|---|---|
| SYS-ESCROW-001 | POST `/service-marketplace/bookings` rejects decoded authenticated requests with 503 and no financial or booking mutations | `TDF.API.ServiceMarketplaceAPI` → `protectedServer` → `serviceMarketplaceServer` → `createServiceMarketplaceBooking` |
| SYS-ESCROW-002 | POST `/service-marketplace/bookings/:bookingId/escrow/release` rejects decoded authenticated requests with 503 and no payout or other mutations | Same route composition → `releaseServiceMarketplaceEscrow` |
| SYS-ESCROW-003 | Recovery of a release containing this disablement cannot restore either legacy writer | `scripts/production-release.mjs` preflight, snapshot compatibility and recovery guard |

The response is unavailable until a separately approved verified escrow implementation exists.
There is no admin bypass, provider-mode exception or configuration switch. Existing nominal rows
remain historical records, not verified funds. This change makes no assertion that they represent
real money. Ad/slot operations and completion remain outside these two disabled writes; completion
is not payout authorization. No schema migration or historical-row rewrite is required.

## Executable contract and correspondence

Let `S` be the complete persistent state before the handler, `u` any authenticated principal,
`r` any decoded request, and `E` any environment. For each disabled handler `h`:

```
run(h(u,r), E, S) = (Left HTTP503(message_h), S, [])
```

`[]` is the sequence of handler-generated IO effects. The permitted transition is a stuttering
step on every persistent entity, including Payment, ServiceEscrow, Booking, ServiceOrder and slots.
All pre-existing states are admitted, including inconsistent legacy rows and absent IDs; the
guarantee does not rely on their validity. No progress-to-payment property is required: these
operations are explicitly disabled. Termination of the local handler follows its constant error
expression; network response delivery is an environment assumption, not a liveness guarantee.

The executable source contract is `scripts/lib/legacy-escrow-contract.mjs`. It recognizes exactly
the two constant `throwError err503` definitions, failing on any body change. The manual reduction
argument uses the ReaderT/Servant Handler error semantics: neither function evaluates its two
arguments, reads Env, lifts IO, nor calls persistence. This is a narrow source-correspondence check
and manually inspected effect argument, **not a machine-checked proof of Haskell or the whole API**.
The compiler, imports, monad instances, Servant routing/authentication, runtime and deployment are
trusted; the checker is intentionally not a general Haskell parser.

Hspec extracts the actual handlers from `serviceMarketplaceServer` and executes them with a poison
Env. It covers five role combinations, boundary Int64 identifiers, absent records and repeated
release requests (55 executions). Accessing Env fails the test. Source-contract negative controls
detect injected environment access, IO, successful return and an admin override in both bodies.
The tests establish those executions, not unrestricted implementation equivalence. Authentication,
JSON decoding and capture parsing may reject earlier with their existing status codes; HTTP clients
are not promised 503 before they reach these handlers. Ordinary request logging is outside the
handler-generated IO boundary.

Composition: both mutations are removed, so interleavings of these handlers add no financial
effects or state changes. This does not prevent independent writers or operators from changing
the database. Future escrow implementation, existing-data reconciliation, provider correctness,
global accounting invariants and all other payment routes remain separate open obligations.

## Recovery correspondence

Commit `33083d469727da73e73ea4a2c2be5aaec130b137` removes the two writers. For a target
containing that commit, the actual production guard requires its recovery candidate to contain
it too, in addition to existing identity protections and identical migration checksums. Preflight
checks the supplied forward-recovery artifact and every prior machine image; recovery rechecks the
candidate before mutation. Missing history is a tool failure, never compatibility. The concrete
counterexample `d517d6ed12a0f9ed3929df124507c7f4b025d16d` has identical migrations and identity
protections but still permits nominal escrow; it is now rejected for the new target. Tests exercise
real Git ancestry and all four combinations of the two protection predicates. Git commit ancestry
is provenance, not a proof against a later deliberate reintroduction; source gates and independent
review must continue to hold on target/recovery revisions. Use the current guarded release tool;
this policy does not control independent manual Fly operations.

## Reproduce

```
node --test scripts/__tests__/legacy-escrow-contract.test.mjs scripts/__tests__/provider-rollback.test.mjs
cd tdf-hq
stack test --test-arguments='--match "disabled legacy service marketplace financial writes"'
```

The normal backend Hspec suite and formal specification CI run these checks. A relevant source
change invalidates the correspondence argument and requires renewed review, checks and an exact
image build. Required review, CI, compatible recovery, staging and production evidence remain
delivery gates; committing this contract is not a deployment claim.
