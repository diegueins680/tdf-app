# Current redemption limits for new legacy ticket promo claims

Parent requirement: `EVT-TICKET-001`.

`EVT-LEGACY-PROMO-LIMIT-001`: a new legacy checkout increments a promo's
redemption count only if the current row is active and either has no maximum
or its pre-claim count is strictly below its current maximum. The increment
and resulting order remain in the existing hold transaction. A refused claim
creates no order or tickets and leaves no stock or redemption increment.

Authority is `validatePromoCodeRedemptionLimit`, its absent/below/equal-limit
tests, normal organizer `updatePromoCode`, and canonical checkout's current
locked promo validation in `TDF.Server.EventTicketCheckout`. `Nothing`/SQL NULL
means unlimited. Zero is accepted by the current complete schema and ordinary
organizer handler and blocks new redemptions; no new limit policy is introduced.

The old legacy claim compared the current database counter to a cached maximum
read before waiting for Event/Tier. Six actual-handler schedules include two
counterexamples: the organizer changes an initially limited or unlimited promo
to maximum0 while the buyer waits on an unchanged Event. The buyer still
returns200 with a paid zero-value order, three issued tickets, sold3 and one
redemption. The paid tier100 and10000bps promo are created through ordinary
organizer handlers; no test-side limit write or provider/email call is involved.
Four compatible nonexhausted/unlimited/raised/unchanged edits already pass.

The correction uses one atomic UPDATE whose predicate compares the current
`current_redemptions` and `max_redemptions` columns. It retains the current
active predicate, NULL semantics, increment, existing zero-row stock
compensation/409 and enclosing order transaction. It adds no separate unlocked
cap read or lock-order edge and does not alter canonical checkout or schema.
An additional actual Promo-row FIFO schedule queues the editor first and the
claim second; this covers an UPDATE that itself waits for a changed row.

This property is at claim time. The organizer may lower a cap below historical
redemptions after an earlier valid order. Existing paid orders and idempotent
replay keep their stored financial snapshot and do not claim again. A separate
compatibility test retains that history/replay and rejects a distinct new buy.
No global historical-count<=current-cap invariant or retroactive revocation
is asserted. Stock updatedAt may change during existing compensation; the
no-effects claim concerns durable stock/order/ticket/redemption effects.

Implementation scenarios are TESTED against PostgreSQL READ COMMITTED when
their source-bound receipts pass. The BOUNDED_VERIFIED design enumerates one
new claimant, one cap editor, counts0/1, caps NULL/0/1/2, twenty initially
admissible input/target combinations and all three quote-before-claim
interleavings (sixty histories). Correct behavior reaches46 accepted and14
refused histories and seven valid historical over-cap states. Controls use a
cached cap or retain effects on a refused claim and must expose their respective
`CurrentLimitAtClaim` / `RefusalHasNoEffects` counterexamples. The atomic-claim
abstraction is not SQL/Haskell refinement or a whole-provider workflow proof.

Assumptions and limits: valid initial counters, participating row updates and
PostgreSQL transaction semantics. Integer overflow aborts the transaction;
availability at machine bounds, alternate isolation, old binaries and direct
out-of-protocol writes are separate. Promo financial fields, date windows,
event/tier restrictions and policy snapshot governance remain separate checks
and races; this correction claims only the current redemption maximum.
Provider initialization/recovery, historical reconciliation, HTTP authentication
and required current-main/protected integration remain separate obligations.
