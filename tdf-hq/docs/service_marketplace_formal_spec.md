# Service Marketplace Formal Specification (Escrow + Booking)

**Historical design; financial transitions superseded.** This document originally described
nominal bookkeeping, not provider-verified escrow. A Payment row alone does not establish
funds held or paid out (accepted [ADR 0101](../../docs/adr/0101-verified-payment-events.md)).
The user's explicit delivery decision on 2026-09-20 disables T3 and T5 below.
Their current contract is [SYS-ESCROW-001/002](../../formal/system/legacy-escrow.md):
POST booking and POST escrow release return HTTP 503 after ordinary authentication and
request decoding, before database access. No admin exception or runtime flag enables them.
Existing data is retained; completion, ad and slot operations are outside this disablement.

The remaining text records the old design for provenance. It is prose, not a mechanized
formal specification, and does not authorize nominal financial writes.

## State Variables

- `ServiceAd(active, providerPartyId, feeCents, slotMinutes, serviceCatalogId)`
- `ServiceAdSlot(status ∈ {open, booked}, adId, startsAt, endsAt)`
- `ServiceCatalog(active, kind, currency)`
- `Booking(status ∈ BookingStatus, partyId, engineerPartyId, serviceOrderId)`
- `ServiceOrder(status ∈ {escrow_held, performed, paid_out}, customerId, artistId, serviceKind, priceQuotedCents)`
- `ServiceEscrow(status ∈ {held, released, refunded}, bookingId, serviceOrderId, amountCents, heldPaymentId, releasedPaymentId)`

## Invariants

1. **Paid-before-booked**
   - For every `ServiceEscrow` with status `held|released|refunded`, `heldPaymentId` exists.
2. **Escrow amount consistency**
   - `ServiceEscrow.amountCents = ServiceAd.feeCents` at booking creation.
3. **Provider/customer consistency**
   - `Booking.engineerPartyId = ServiceEscrow.providerPartyId`
   - `Booking.partyId = ServiceEscrow.patronPartyId`
4. **Catalog-kind consistency**
   - `ServiceOrder.serviceKind = ServiceCatalog.kind` for the catalog referenced by the originating ad/order.
5. **Slot exclusivity**
   - `ServiceAdSlot.status = booked` implies at most one escrow references a booking in that slot.
6. **Release safety**
   - `ServiceEscrow.status = released` implies `Booking.status = Completed`.

## Transition System

### T1: PostAd
Preconditions:
- `feeCents > 0`
- `serviceCatalogId` exists and references an active `ServiceCatalog`
- `headline != ""`, `roleTag != ""`

Postconditions:
- A new active `ServiceAd` is created.

### T2: PublishSlot
Preconditions:
- Caller is `ServiceAd.providerPartyId`
- `endsAt > startsAt`

Postconditions:
- New `ServiceAdSlot(status=open)` is created.

### T3: BookAndHoldEscrow — DISABLED (historical behavior below)
Preconditions:
- `ServiceAd.active = true`
- `ServiceAdSlot.status = open`
- `ServiceAd.serviceCatalogId` references an active `ServiceCatalog`
- Booker is not provider

Postconditions:
- Create `ServiceOrder(status=escrow_held)`
- `ServiceOrder.serviceKind := ServiceCatalog.kind`
- Create `Booking(status=Confirmed)`
- Create patron `Payment(concept=escrow_hold)`
- Create `ServiceEscrow(status=held)`
- Update slot to `booked`

### T4: MarkServiceCompleted
Preconditions:
- Caller is provider (or admin)

Postconditions:
- `Booking.status := Completed`
- `ServiceOrder.status := performed`

### T5: ReleaseEscrow — DISABLED (historical behavior below)
Preconditions:
- Caller is patron (or admin)
- `Booking.status = Completed`
- Allowed transition: `held -> released`

Postconditions:
- Create provider `Payment(concept=escrow_release)`
- `ServiceEscrow.status := released`
- `ServiceOrder.status := paid_out`

## Mechanized Guard

`escrowTransitionAllowed` is a legacy helper, no longer used by the disabled release handler.
Its historical relation (not current authorization) is:

- `held -> released`
- `held -> refunded`
- self-loop (`x -> x`)

All other transitions are rejected.
