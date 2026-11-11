# `/public` mutating routes: current behaviour (for owner review)

Derived on 2026-11-07 by reading `tdf-hq/src/TDF/ServerExtra.hs`, `Server/CourseCheckout.hs`, `Server/DomoQuoteCheckout.hs`, `Server/EventTicketCheckout.hs`, `Server/TicketPaymentReceipt.hs` and the `Routes/*` modules on main `f615c862`. New requirements written from it have status `implemented but unspecified`. (`formal/system/public-routes.json` is the AUTH-PUBLIC-001 register; this document is the behavioural review.)

**Guards**
- None of the 18 routes is behind `AuthProtect`; all are registered in `public-routes.json` as public-intake, capability-path (QR token) or capability-header (`X-Order-Lookup-Token`).
- Lookup tokens: Domo and tickets derive them with HMAC under `COMMERCE_LOOKUP_TOKEN_SECRET` (503 when unset); courses derive `sha256("course-order-lookup:" + Idempotency-Key)` with no secret.
- Online rails share one pattern (context load with hold expiry, provider enablement, one active rail, deterministic attempt keys, binding replay, capture validation with reconciliation exceptions, `recordVerifiedPaymentOrReconcile`); none sends email or WhatsApp directly.

## Route → requirement

| Routes | Requirement | Basis |
|---|---|---|
| assets/qr checkout, checkin, upload | INV-QR-PUBLIC-001 | new |
| courses registrations, datafast, paypal create and capture | COURSE-PUBLIC-CHECKOUT-001 | new (critical) |
| domo quotes, accept, datafast, paypal create and capture | DOMO-PUBLIC-QUOTE-001 | new (critical) |
| events ticket-orders, bank-transfer, evidence, datafast, paypal create and capture | EVT-PUBLIC-TICKET-CHECKOUT-001 | new (critical) |

Existing requirements cover parts of these routes: LEGAL-CONSENT-001 and COURSE-ENROLL-001 (registration), PAY-DOMO-QUOTE-001 and PAY-DOMO-CANCEL-001 (quotes), EVT-TICKET-001, EVT-TICKET-MANUAL-001 and NOTIF-TICKET-001 (tickets), PAY-DATAFAST-001 and PAY-CAPTURE-RECON-001 (rails). None specified the routes themselves.

## Open behaviours for the owner to decide

**Capabilities and secrets**
- The course lookup token is derivable from the client-chosen idempotency key (no server secret). Switching to the HMAC scheme used by Domo and tickets invalidates the stored hashes of in-flight registrations, so it is left for the owner.
- A Domo replay returns the lookup token again to whoever holds the key.

**Abuse limits**
- Domo (6/hour) and ticket (10/hour per event) rate limits are keyed on the buyer-supplied email, so rotating emails can hold dates or exhaust seat holds without limit. Course registration has no per-email limit.
- QR routes have no rate limit; uploads are unbounded anonymous writes to the public media root, not bound to a checkout.

**Inventory QR**
- The open-checkout check and the insert are not locked, so two concurrent checkouts can both pass.
- Anyone with the printed token can close someone else's loan.

**Workflow**
- Course PayPal capture does not finalize enrollment; where enrollment finalizes was not identified.
- Ticket receipt files are written inside the transaction, so a later failure leaves an orphan file.
- Whether ticket order creation checks `acceptedTermsVersion` as LEGAL-CONSENT-001 states was not confirmed.
