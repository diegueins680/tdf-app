# Current inventory bounds for legacy ticket purchases and tier edits

Parent requirement: `EVT-TICKET-001`.

- `EVT-TIER-EDIT-BOUNDS-001`: a tier edit commits its requested total only when
  that total is at least the current sold count. Its response entity is read in
  the same transaction as the conditional update.
- `EVT-LEGACY-RESERVE-BOUNDS-001`: participating direct and legacy checkout
  reservations use the locked current event and tier inventory authority and
  commit the stock reservation with the corresponding order/tickets.

Authority is the existing updateTier rejection "quantity cannot be below sold
quantity", the established `TDF.Ticketing.Inventory.reserveTicketInventory`
contract and the ticket-admission capacity specification. No new allocation,
pricing, owner override or financial policy is inferred.

The previous tier edit validated a separately read sold count before an
unconditional UPDATE. Both legacy reservation copies used a tier total read
before their transaction. Four actual-handler counterexamples cover direct
purchase and the checkout endpoint, each in both orderings against a tier
reduction: an initially valid total20/sold0 ends at total1/sold3. All purchases
are free and call no provider. The barrier locks an unchanged tier row; both
business mutations are real endpoint handlers, not test-side stock updates.

Both sides need repair. An atomic edit predicate alone still permits a waiter
with a stale total to oversell after the edit. A fresh reservation alone does
not prevent an unconditional stale edit from lowering total below the purchase.
The edit now conditionally updates all its fields against the current sold
count and event binding, returning the updated entity while its row lock is held.
The existing early invalid-total rejection remains400; a lost conditional update
returns409 without partially editing the tier.

The two reservation copies now call the existing inventory authority inside
order creation. It locks Event then Tier, reads the current capacity/total,
checks current active and sale-window values using the supplied handler-start
`now`, uses Integer arithmetic for the event capacity comparison, and updates
stock before the caller inserts its order/tickets in that same transaction.
Existing positive wire quantities are1–100 for both DTO decoders, matching this
helper's bound. This is not a new public quantity limit. Sale-window ends remain
inclusive; no database-time admission deadline is claimed.

Stripe checkout keeps its Event lock and terms-policy SHARE check before the
helper. Its zero-total response, promo claim/compensation, idempotent checkout
reuse/conflict handling and provider rollback stay in their existing paths.
No provider or email IO occurs inside the inventory transaction. Empty capacity
for an unsold tier (total0/sold0), equal-sold edits, capacity increases, absent
event capacity and the exact event-capacity boundary remain supported.

The preservation claim assumes initially valid nonnegative tier stock and the
participating reservation/edit writers. Other writers, old binaries, manual
stock changes and existing negative inventory are separate obligations. Changing
an event's capacity below already issued stock does not retroactively revoke
orders; future participating reservations still check its current capacity.
Workflow/privacy, original payment, price/currency/manager snapshots and terms
lifecycle beyond the retained existing check are not proven by inventory reuse.

Actual handlers are TESTED against the full227-migration PostgreSQL schema
with its default READ COMMITTED isolation. Serialization failures or other
isolation defaults are not promoted to availability guarantees by these tests.
The bounded design model enumerates48 initial sold/total/requested/quantity/
fixed-event-capacity combinations and both interleavings, including a tier read
held through reservation. It checks sold never exceeds tier total. Mutants use
stale sold, stale total or omit the tier lock and must yield counterexamples.
This BOUNDED_VERIFIED abstraction is not a SQL/Haskell refinement, whole-handler
proof or proof about concurrent capacity edits (capacity is fixed per model input).
Existing inventory component tests and full regression suites remain required.
