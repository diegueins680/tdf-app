# Legacy Stripe refund inventory release

Requirement: `EVT-TICKET-001`. Invariant:
`EVT-LEGACY-REFUND-INVENTORY-001`.

The canonical completion in `TDF.Ticketing.Refund.completeTicketRefund` releases
inventory only from the order's event and tier, with a conditional decrement
that refuses insufficient sold stock. The legacy Stripe completion must respect
the same inventory safety boundary. The complete 227-migration test schema has
no nonnegative CHECK on `event_ticket_tier.quantity_sold`; a plain decrement can
persist negative stock without any SQL error.

Acceptance criteria:

- Require a strictly positive captured order quantity before negating it.
- Decrement only the captured tier belonging to the order's event, and only when
  its current `quantity_sold` is at least that quantity. This predicate and the
  decrement must be one database UPDATE, not a separate read and later write.
- Require exactly one updated tier. Request approval, order/ticket retirement
  and this inventory update must commit or roll back in the same transaction.
- On refusal, preserve the previously committed processing fence and observed
  provider ID. Return the existing generic reconciliation response, keep the
  completion timestamp absent, and refuse another provider POST on approval retry.
- Successful completion releases exactly the positive quantity once. Competing
  distinct requests sharing a tier cannot both consume the same sold capacity.

This narrow property assumes the existing request/provider evidence boundary and
the captured order snapshot. If stock starts nonnegative, these decrements keep
it nonnegative. A pre-existing negative counter is refused and preserved for
investigation; this change does not repair historical counters or constrain other
writers. It adds no lock before provider IO and no new event lock or schema.
The existing tier UPDATE already acquires a row lock; its predicate now decides
whether the release is allowed under contention.

Verification uses routed Haskell handlers with synthetic provider callbacks on
the full production migration manifest. Cases cover insufficient and negative
stock, zero/negative order quantities, a foreign-event tier, the signed storage
upper boundary, and two distinct refunds sharing both sufficient and insufficient
stock. An independent finite model explores two actors, sold counters -1 through
6, quantities -1 through 3, and both event bindings. Unsafe controls replace the
atomic update with a stale precheck, omit event binding, or omit positive quantity.
Its assurance is BOUNDED_VERIFIED only; implementation evidence is TESTED.

Partial-refund allocation remains unresolved for this legacy Stripe path.
`TICKETING_IMPLEMENTATION_STATUS.md` documents amount-level partial support,
while `formal/ticket-admission/README.md` explicitly binds whole-ticket allocation
to the canonical PayPal path and preserves other providers' existing behavior.
Those sources do not establish which Stripe tickets an arbitrary partial amount
should retire. This correction preserves that behavior and does not invent an
allocation, disable partial refunds, or claim correct partial-ticket fulfillment.
Admission during uncertain refunds, current snapshot/actor fencing, global event
capacity, recovery and real authenticated provider operation remain separate.
