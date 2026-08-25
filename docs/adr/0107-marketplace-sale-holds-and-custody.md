# ADR-0107: Equipment-sale holds and custody remain separate from payment

Status: Accepted — 2026-08-15

## Context

The legacy marketplace reused listing and order status as payment, stock, and delivery state. That
made a checkout start capable of removing an asset from sale and allowed staff payment edits to
stand in for provider evidence. Rental listings also shared the same cart despite lacking dates,
deposit terms, availability, handoff, return, and damage controls.

## Decision

Keep the legacy marketplace order as the sale-domain record and link it one-to-one to the canonical
checkout aggregate. Checkout creation locks the cart, listings, and assets; snapshots each unique
asset at quantity one; and creates a 15-minute exclusive `marketplace_asset_sale` hold. One
provider-neutral client idempotency key identifies that checkout even if the buyer changes payment
rail.

Payment and physical fulfillment use distinct state machines. Only canonical verified-payment
evidence may project `paid` onto the marketplace order. Payment consumes the hold and makes the
sale `ready_to_fulfill`; it does not transfer custody. A database-enforced pickup or delivery
transition marks the asset `Sold` only at `delivered`. Returns restore TDF custody without silently
reactivating the public listing. Operator transitions record actor, reason, notes, and time as
append-only evidence.

Rental listings use the separate dated runtime adopted in ADR-0108. Production sales and rentals
have independent capability gates, while provider rails and emergency kill switches remain
separate.

## Alternatives

- Mark the asset sold when checkout starts: rejected because abandoned or failed payment would
  destroy availability.
- Mark the asset sold when payment succeeds: rejected because payment is not physical custody or
  delivery evidence.
- Reuse sale checkout for rentals: rejected because duration pricing, overlapping availability,
  deposits, contracts, returns, late fees, and damage evidence require a separate aggregate.
- Create a new order whenever the customer changes provider: rejected because it creates competing
  holds and ambiguous customer obligations.

## Consequences

Sales can be rolled out independently with honest payment and custody status. Operations must own
pickup/shipping transitions, exceptions, and returns. Carrier APIs, customer-initiated returns,
marketplace refunds, and rental deposit settlement adapters remain separately gated.
