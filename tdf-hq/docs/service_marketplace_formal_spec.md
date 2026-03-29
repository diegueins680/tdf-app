# Service Marketplace Formal Specification (Escrow + Booking)

This module models the service marketplace as a finite-state system with explicit invariants.

## State Variables

- `ServiceAd(active, providerPartyId, feeCents, slotMinutes, serviceCatalogId)`
- `ServiceAdSlot(status ∈ {open, booked}, adId, startsAt, endsAt)`
- `Booking(status ∈ BookingStatus, partyId, engineerPartyId, serviceOrderId)`
- `ServiceOrder(status ∈ {escrow_held, performed, paid_out}, customerId, artistId, priceQuotedCents)`
- `ServiceEscrow(status ∈ {held, released, refunded}, bookingId, serviceOrderId, amountCents, heldPaymentId, releasedPaymentId)`

## Invariants

1. **Paid-before-booked**
   - For every `ServiceEscrow` with status `held|released|refunded`, `heldPaymentId` exists.
2. **Escrow amount consistency**
   - `ServiceEscrow.amountCents = ServiceAd.feeCents` at booking creation.
3. **Provider/customer consistency**
   - `Booking.engineerPartyId = ServiceEscrow.providerPartyId`
   - `Booking.partyId = ServiceEscrow.patronPartyId`
4. **Slot exclusivity**
   - `ServiceAdSlot.status = booked` implies at most one escrow references a booking in that slot.
5. **Release safety**
   - `ServiceEscrow.status = released` implies `Booking.status = Completed`.

## Transition System

### T1: PostAd
Preconditions:
- `feeCents > 0`
- `serviceCatalogId` exists
- `headline != ""`, `roleTag != ""`

Postconditions:
- A new active `ServiceAd` is created.

### T2: PublishSlot
Preconditions:
- Caller is `ServiceAd.providerPartyId`
- `endsAt > startsAt`

Postconditions:
- New `ServiceAdSlot(status=open)` is created.

### T3: BookAndHoldEscrow
Preconditions:
- `ServiceAd.active = true`
- `ServiceAdSlot.status = open`
- Booker is not provider

Postconditions:
- Create `ServiceOrder(status=escrow_held)`
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

### T5: ReleaseEscrow
Preconditions:
- Caller is patron (or admin)
- `Booking.status = Completed`
- Allowed transition: `held -> released`

Postconditions:
- Create provider `Payment(concept=escrow_release)`
- `ServiceEscrow.status := released`
- `ServiceOrder.status := paid_out`

## Mechanized Guard

`escrowTransitionAllowed` implements the authorized transition relation:

- `held -> released`
- `held -> refunded`
- self-loop (`x -> x`)

All other transitions are rejected.
