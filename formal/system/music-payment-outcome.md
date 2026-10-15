# Music payment outcome projection — PAY-MUSIC-OUTCOME-001

The normative requirement and its complete mappings are in
[`requirements.json`](requirements.json) under `PAY-MUSIC-OUTCOME-001`.
`AUTHORITY-MUSIC-OUTCOME-20261015` reconciles the separate payment-attempt,
money and fulfillment lifecycles. This contract does not authorize activation.

## Reproduced contradiction

On main `2781ddc2a3c9fd661684742550918d653afdc0cf`, a synthetic PostgreSQL17
fixture ran the installed `music_sync_verified_checkout` body after checking it
byte-for-byte against main's historical migration. A failed attempt cancelled
the purchase. Subsequent bound verified payment left checkout `paid`, purchase
`cancelled` and one active entitlement. Authoring fixtures bypassed publication
triggers; outcome transitions used the real installed triggers. No provider or
production write was involved.

## Compatibility boundary

The additive migration preserves historical migration bytes and refuses existing
active purchase entitlements whose purchase is not paid. Those records require
explicit reconciliation; the migration neither deletes evidence nor reopens orders.
Its checkout, purchase and entitlement table locks protect installation from
concurrent ordinary writes. Production rollout still requires the canonical writer
fence and qualified recovery.

A failed attempt leaves fulfillment awaiting payment. It does **not** authorize
a new payment attempt: the music handler calls
`music_checkout_payment_attempt_allowed` in the same database transaction as
attempt creation/replay. That function locks the canonical checkout and denies
failed and closed checkouts, and any persisted failed attempt or failure code even
if a late binding/pending response reopens checkout or attempt status. The same
locked check gates lookup/replay of an existing provider binding, including
previously created payment widgets. Denial returns HTTP409 before a new provider
create/capture call. Read-only Datafast status lookup can precede the second check.
This preserves the earlier retry quarantine while correcting fulfillment state.

An already admitted, authoritative payment can still settle an eligible order.
For an already closed **purchase** linked to a checkout that accepts verified
money, the projection preserves payment evidence, leaves fulfillment closed and
grants no entitlement. It creates an existing commerce reconciliation exception
and an idempotent audit event. The case's internal reference is the checkout ID;
the strict-administrator report links that ID to the unique matching attempt and
provider binding. The known reason `music_paid_closed_order` is allowlisted by the
report, OpenAPI and generated web/mobile clients. The release schema verifier
requires the exact admission-function signature before application admission.
This is not recovery of a cancelled or expired **checkout**:
the shared payment recorder excludes those states and that recovery remains open.

Application rollback must retain this compatibility migration and its admission
function. Older application code bypasses the new admission helper: rollback
requires commerce to remain disabled, not merely retention of this SQL migration.
Restoring the historical trigger reopens the defect. No SQL down migration
is supported, no flag is enabled and no automatic refund or reopening is introduced.

## Executable implementation evidence

`scripts/test-music-payment-outcomes.sh` runs the SQL assertions in an explicit
Read Committed transaction on an admitted, fully migrated disposable database and
rolls back its fixture. The test covers failed and successful outcomes, retry
denial, replay, closed orders, refund, dispute and missing payment evidence.

`scripts/lib/music_payment_conformance.py`, called by the existing full
HTTP/PostgreSQL fixture, checks installed SQL source, the intended projection and
six transactional SQL mutations. Each mutation must fail a named assertion:
failure cancels fulfillment; closed fulfillment receives a grant; the exception
is omitted; its internal reference uses an order ID; failed retry is admitted; or a late binding clears the failure quarantine.
Every control checks that rollback restores the installed functions. Unrelated
SQL/transport errors cannot satisfy a control.

Seven concurrent schedules witness an actual PostgreSQL lock wait before release:
failure before success; success before late failure; cancellation or expiry before
verified projection; failure before retry admission; and failure before late binding or pending
responses. Each retry case must reject admission after its witnessed wait. Fixtures synthesize provider evidence; these
checks do not exercise provider calls, the complete payment adapter, financial
ledger/receipt posting or every isolation mode. Administrative HTTP checks verify
case linkage and anonymous/unprivileged denial. Before any behavioral checks,
the controlled fixture rejects an existing `/app/COMMIT` runtime override,
inherits no commit environment override, checks the running executable's
`/version` against the candidate Git SHA and records the executable hash. This
is reported-identity evidence under a trusted, unchanged fixture environment,
not reproducible-build attestation, source authentication or whole-binary
refinement; privileged host mutation is outside this check. A stored PayPal order first
returns successfully without a provider call, then returns409 after failure even
when checkout/attempt status reopens. Only the disposable test database temporarily
enables sandbox commerce; it restores the prior flags in a finally block.

## Bounded model and exclusions

`MusicPaymentOutcome.tla` has one purchase, one checkout, zero to two grants,
one review marker, an authoritative-capture marker and an admission-result
snapshot, a durable failure fence and the fence value observed at admission. It starts with normal awaiting fulfillment or a legacy cancelled
purchase and failed checkout. `awaiting` abstracts pending/awaiting fulfillment
and eligible checkout processing; legacy terminal purchases are represented by
`cancelled`. Refund and dispute after settlement remain explicit terminal states.
SQL statements and their triggers are atomic committed actions. Environment
outcomes may occur in either order; crashes and failed transactions are excluded.

The intended configuration checks type safety, failed-attempt fulfillment,
active-entitlement ownership by paid fulfillment, closed-money review, one grant,
authoritative evidence failed-retry denial and quarantine after late pending responses. Six configurations mutate the
corresponding guards and must violate named invariants. Duplicate-grant mutation
abstracts removal of both status-change admission and entitlement upsert/uniqueness.
It does not claim one implementation guard alone is necessary.

There is no fairness assumption or liveness claim. The model trusts verified
provider evidence, canonical checkout/purchase linkage, atomicity and committed
row serialization. It does not prove Haskell/SQL refinement, cross-provider
admission, ambiguous HTTP outcomes, idempotency-key scoping, arbitrary privileged
SQL, account/session authorization, storage, provider retention, complete music
OpenAPI/client coverage or recovery of closed checkouts. Existing music routes
are not yet represented in the current OpenAPI; this repair adds no route or DTO. Those exclusions remain requirements, not evidence of conformance.

## Research decisions

- [PostgreSQL17 trigger semantics](https://www.postgresql.org/docs/17/trigger-definition.html):
  adopted transactional outcome projection so a statement/trigger error rolls back
  its effects together. This applies to local persistence, not remote charges.
- [PostgreSQL17 explicit locking](https://www.postgresql.org/docs/17/explicit-locking.html):
  adopted the existing checkout/purchase lock boundary and witnessed wait tests;
  no production isolation setting is inferred from the server default.
- [PayPal idempotency](https://developer.paypal.com/api/rest/reference/idempotency/):
  a timeout can follow a completed capture, and retained request IDs constrain
  safe retry. Adopted the distinction between failed observation and cancelled
  fulfillment. Rejected treating this SQL repair as permission to retry or enable
  music payments; shared runtime admission and durable uncertain-outcome recovery
  still need implementation and verification.

## Open release obligations

Music commerce stays disabled. Current handlers bypass the shared provider runtime;
ambiguous responses and closed-checkout recovery lack complete durable reconciliation.
The checkout expiry, global attempt keys, partial-refund entitlement and dispute
restoration obligations in [music-purchase-entitlement.md](music-purchase-entitlement.md)
remain open. Exact final-candidate full gates, independent review, merge and qualified
deployment evidence are required separately from the working-source checks above.
