# ADR-0108: Marketplace rentals use dated holds, separate custody, and due-state deposits

Status: Accepted — 2026-08-16

## Context

Rental listings were public discovery records priced like sales. They had no start/end dates,
atomic availability, approved terms, identity-at-handoff fields, condition reports, or truthful
deposit lifecycle. Reusing the sale runtime would allow payment to imply custody and would make a
refundable deposit indistinguishable from revenue.

## Decision

Keep the marketplace order and canonical provider-neutral checkout, but link rental orders to a
dedicated runtime. A rental cart contains exactly one physical asset and both inclusive dates.
PostgreSQL locks the cart/listing/asset, calculates duration and daily/weekly pricing from an active
approved terms record, snapshots the charge and separately disclosed deposit, creates a 15-minute
hold, and rejects overlapping active date ranges with an exclusion constraint.

Payment, custody, and deposit settlement are independent state machines. Verified payment moves a
rental only from `on_hold` to `confirmed`; it never hands off the asset. `checked_out` requires an
outbound condition report and changes custody to `Booked`. Return requires an inbound report and
restores TDF custody before inspection. Damage creates a deduction proposal, and
`deposit_refund_due` records either a full- or partial-refund due state. It does not claim that
money moved. A non-zero-deposit rental cannot close without terminal verified settlement evidence.

Rental rates, limits, late fees, cancellation window, timezone, terms text, and activation are
operations-managed. Migrated public rental listings retain their published daily price, use a
six-daily-rate weekly price, start with a zero deposit, and receive `marketplace-rental-v1` terms.
Every commercial edit requires a new terms version and is copied to append-only history. Existing
orders retain the accepted version and immutable monetary snapshot.

The customer document number is validated in memory and discarded. Only document type and the
last four characters are retained for handoff coordination; neither the raw identifier nor a
dictionary-attackable unsalted hash is stored.

## Alternatives

- Reuse sale fulfillment: rejected because date availability, custody return, inspection, late
  state, and refundable deposits are not sale concepts.
- Mark the asset unavailable for the whole catalog after checkout: rejected because rentals block
  a date interval, not all future availability.
- Treat a deduction proposal as a refund or capture: rejected because an accounting intention is
  not provider or bank evidence.
- Store a SHA-256 government-ID fingerprint: rejected because Ecuadorian identifiers have low
  entropy and a database reader could enumerate likely values.
- Enable migrated listings with invented deposits: rejected. The preserved zero deposit is an
  explicit editable initial policy, not fabricated money movement.

## Consequences

Marketplace sales and approved rentals can be enabled independently of provider rails, with
emergency kill switches retained. Operations must version commercial edits and record condition
evidence. Datafast/PayPal production charging, non-zero-deposit refund execution, carrier
integrations, automated late-fee charging, and production deployment remain separate gates and
must not be inferred from this runtime.
