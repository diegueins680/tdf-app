# ADR-0109: Service bookings use approved quotes, expiring holds, and separate fulfillment

Status: Accepted — 2026-08-17

## Context

The public studio and DJ routes created honest tentative bookings, but they had no authoritative
quote, checkout, deposit schedule, or expiring hold. Availability was checked before insert, so two
concurrent public or staff requests could reserve the same room. Client estimates could not safely
become a charge, and payment confirmation had no formal relationship to the operational booking.

## Decision

Keep `booking` and `service_order` as the scheduling and service-domain records, then link them to
one canonical checkout through `service_booking_checkout_runtime`. An approved, active,
versioned commerce policy snapshots the offering rate, billing unit, tax basis points, deposit
basis points, duration limits, hold duration, currency, and accepted terms. The server computes the
full price and deposit using integer minor units. Draft policies preserve current catalog values but
cannot quote or create checkout until an operator independently approves and activates them.

Every booking resource, including legacy public and authenticated staff bookings, is projected into
one PostgreSQL exclusion-backed calendar. Active ranges for the same resource cannot overlap.
Canonical checkout creation inserts the booking, service order, immutable deposit checkout, runtime
snapshot, lookup capability, and resource allocations in one transaction. A conflict rolls the
whole transaction back. Unpaid holds expire; paid allocations remain reserved.

Payment and service fulfillment are independent state machines. Verified deposit evidence may move
`on_hold` to `confirmed` and the domain payment state to `deposit_paid`; it does not start or
complete a session. Scheduling, work start, balance due, completion, cancellation, rescheduling,
no-show, overtime review, and disputes remain explicit domain transitions. Direct `paid` updates are
rejected unless the canonical checkout has matching succeeded provider evidence and immutable
provider bindings, or approved manual-payment evidence.

Guest tracking deterministically derives a high-entropy lookup token from the caller's random
idempotency key and stores only its hash, so a lost create response can be replayed without losing
the tracking capability. Unknown booking IDs and wrong tokens return the same not-found response.
The production domain flag starts disabled. Provider
selection remains unavailable until a real Datafast or PayPal action is bound to this checkout and
the matching environment capability is enabled.

## Alternatives

- Trust the browser estimate: rejected because duration, tax, deposit, and policy must come from an
  approved server snapshot.
- Mark a booking confirmed when the provider redirects the browser: rejected because redirects are
  not payment evidence.
- Keep read-then-insert availability checks: rejected because they race across public and staff
  writers.
- Replace bookings with a generic commerce-order table: rejected because room scheduling,
  engineers, no-shows, overtime, and rescheduling remain booking-domain concepts.
- Activate migrated rates automatically: rejected because preserved catalog data is evidence for
  review, not authorization to begin charging deposits.

## Consequences

Operations must approve one policy version before canonical public checkout can be enabled for an
offering. Historical overlapping future bookings must be reviewed before the exclusion constraint
can be applied. Datafast/PayPal production actions, balance collection, automated refunds,
notifications, package credits, and production deployment remain separately gated and cannot be
inferred from this booking runtime.
