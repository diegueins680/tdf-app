# `/bookings` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading the booking sections of `tdf-hq/src/TDF/Server.hs`, `Commerce/CheckoutStore.hs`, `Commerce/PaymentRuntimeStore.hs` and the service-booking SQL (`2026-08-16_service_booking_checkout_runtime.sql`, `2026-08-17_service_booking_manual_payments.sql`, `2026-10-05_booking_calendar_projection.sql`) on main `f615c862`. New requirements written from it have status `implemented but unspecified`.

**Mounting and guards**
- `POST /bookings`, `PUT /bookings/{id}` and the manual-payment review are behind `AuthProtect "bearer-token"`; the staff routes need `ModuleScheduling` plus studio roles or the stored customer/engineer identity (AUTH-BOOK-001) and run under `runBookingTransaction` (current-session fence, SQL states 23P01/23514/40001/40P01 → 409). The review needs `ModuleInvoicing` only.
- `POST /bookings/public` and `/bookings/public/checkout` are public-intake under an `Idempotency-Key`; the five `/bookings/public/orders/*` routes are capability-header under `X-Order-Lookup-Token` (SHA-256 compared in constant time, 404 otherwise).
- `requireServiceBookingPaymentContext` expires stale holds (409), refuses statuses outside the checkout lifecycle (409) and answers 503 for environment, domain or provider mismatches. Payment attempts use deterministic keys per booking, provider and operation and replay existing bindings.
- The database owns the lifecycle: GiST exclusion on allocations, `service_booking_require_verified_payment`, `service_booking_sync_checkout` (confirms the booking when the checkout is paid), the manual-evidence transition trigger and the legacy allocation projection.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| bookings create, update | AUTH-BOOK-001 (with BOOK-CALENDAR-001) | reviewed |
| public, public/checkout | BOOK-PUBLIC-INTAKE-001 | new (critical) |
| datafast checkout, paypal create and capture, manual-payment, evidence | BOOK-PUBLIC-PAYMENT-001 | new (critical) |
| manual-payment review | BOOK-MANUAL-REVIEW-001 | new (critical) |

`PAY-CAPTURE-RECON-001` now names service-booking deposits: the PayPal capture and Datafast status paths already used `recordVerifiedPaymentOrReconcile`.

## Defects fixed on 2026-11-07

1. **Public booking transactions turned lifecycle and serialization failures into 500.** Only the calendar exclusion (23P01) was mapped to 409; the projection trigger (23514) and serialization or deadlock aborts (40001, 40P01), which the staff path maps to 409, surfaced as a generic 500 from the two public creation routes. `publicBookingSqlConflict` now maps the same set. Tested in `ServerSpec`.
2. **Bank transfer could be selected on a paid deposit.** The Datafast and PayPal routes refuse a paid checkout (409) but the manual-payment selection did not, so a paid booking could still obtain an awaiting-evidence row. `validatePublicManualPaymentSelection` now refuses it like the online rails. Tested in `ServerSpec`.

## Open behaviours for the owner to decide

**Calendar and money**
- The legacy `POST /bookings/public` lets an anonymous caller hold a resource as `reserved` until the slot ends with no payment and no expiry.
- The booking lookup token is derived from the client-chosen idempotency key with no server secret (same pattern as courses; Domo, tickets use HMAC).
- Whether two concurrent first payment creations can bind two provider resources depends on a binding uniqueness constraint that was not identified.

**Authorization**
- The manual-payment review is gated by the invoicing module alone and runs without the current-session fence of the staff booking routes; the marketplace sibling behaves the same.

**Data**
- Every public booking creates a new party; email never reuses an existing one.
- A trigger rejection on the manual-evidence table surfaces as 500.
