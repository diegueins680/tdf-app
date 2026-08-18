# ADR 0112: Ticket payment evidence and expiring seat holds

Status: accepted

Date: 2026-08-18

## Context

The social-events ticket domain already models tiers, capacity, promotions, transfers, refunds,
waitlists, QR tickets, and a four-percent platform-fee concept. Its buyer journey is authenticated
and Stripe-oriented. More importantly, the legacy direct-order route allowed an event manager to
create a priced order already marked `paid`, while the Stripe success webhook matched only the
PaymentIntent identifier before issuing tickets. Neither manager identity nor a browser/provider
identifier alone proves that the immutable order amount, currency, event, and merchant order were
paid.

## Decision

Payment and ticket fulfillment remain separate aggregates.

- A priced ticket can reach `paid` only through signature-verified or server-to-server provider
  evidence bound to the immutable order, event, amount, currency, merchant, environment, and
  provider resource. Generic status administration cannot advance it to `paid`.
- Direct issuance is restricted to authoritative zero-priced tiers. A future complimentary-ticket
  workflow must record its own entitlement and operator evidence; it must not relabel a priced
  ticket as paid.
- The fulfillment baseline is `seat_held -> issued -> checked_in`, with separate transfer,
  cancellation, refund, and expiry branches. `seat_held -> issued` requires verified payment or an
  explicit no-payment entitlement, but payment verification does not itself imply check-in or
  event attendance.
- The canonical public flow will link the existing event ticket order to the shared checkout
  aggregate. It will snapshot the tier price, promotion, buyer and organizer fee allocations, tax,
  currency, quantity, policy version, and refund/transfer terms. It will use an atomic, expiring
  seat hold rather than treating `quantity_sold` as permanent at checkout creation.
- Datafast and PayPal use the shared provider-attempt/binding/event/receipt/ledger primitives.
  Stripe remains an optional supported-entity rail and is not an Ecuador-critical dependency.
- Guest tracking uses a random capability whose digest is stored; lookup responses are
  enumeration-resistant and customer-safe. Tickets and QR material are returned only after
  verified issuance.
- Organizer proceeds are immutable payable-ledger entries. Settlement starts as dual-controlled,
  staff-verified manual settlement; no balance is described as paid out without actual evidence.

The initial hardening change is immediate: priced direct issuance and generic manual `paid`
transitions fail closed, and a signed Stripe event must also carry and match amount, currency,
order, event, and succeeded status before ticket issuance.

## Alternatives considered

### Keep manager-created paid orders as a box-office shortcut

Rejected. Authentication proves who performed an action, not that money moved. Cash and POS require
the canonical manual-evidence and independent-review path.

### Trust the Stripe PaymentIntent identifier alone

Rejected. A valid identifier can still be associated with the wrong amount, currency, event, or
metadata. Every immutable binding must match before fulfillment.

### Replace event ticket orders with the generic checkout table

Rejected. Event inventory, transfers, waitlists, check-in, organizer accounting, and refunds remain
domain fulfillment. The shared checkout links to the domain order instead of erasing it.

## Consequences

- Existing zero-priced ticket issuance remains available.
- Legacy priced box-office issuance now returns a recoverable conflict until a verified checkout or
  reviewed manual-payment workflow exists.
- Older mobile Stripe intents created without `metadata.tdf_context` cannot issue tickets; the
  customer PaymentIntent builder now writes the same nested immutable context as web checkout.
- Public ticket checkout and production provider rails remain feature-gated until their migration,
  provider sandbox evidence, organizer accounting review, and operational ownership are complete.
