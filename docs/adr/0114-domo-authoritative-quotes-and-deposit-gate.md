# ADR 0114: Domo uses approved authoritative quotes, expiring date holds, and verified deposits

Status: accepted

Date: 2026-08-18

## Context

The Domo public page previously calculated a historical formula in the browser and later became an
honest manual lead form. Neither version could prove availability, retain the requested date, bind a
customer to an immutable rate card and terms version, or collect a verified deposit. The historical
formula is commercial evidence worth preserving, but it has not been approved by finance,
operations, tax, or legal owners.

Quote acceptance, payment, venue reservation, and event completion are different facts. A provider
redirect or an approved PayPal popup cannot prove any of them except that the browser navigated.

## Decision

- Keep the historical public formula as an inactive `commerce_product_version`. Public checkout
  requires exactly one active Domo version, an independently approved
  `commerce_rate_card_review`, and both environment-scoped quote/checkout gates. Migration never
  performs that review or activation.
- Calculate line items, integer-minor-unit subtotal, tax, total, deposit, and balance only on the
  server. Snapshot the exact product version, pricing-rule SHA-256, policy, terms, requested venue
  window, customer input, and line breakdown into a `commerce_quote`, Domo runtime, and linked
  canonical checkout.
- Begin the canonical checkout in `holding`. Creating a quote atomically places one expiring
  exclusion-constrained venue hold but does not accept terms or open a payment rail. Customer
  acceptance records the exact terms version and advances independently to `deposit_due` plus
  `awaiting_payment`.
- Protect customer reads and provider actions with a server-keyed HMAC lookup capability. Store
  only its SHA-256 digest, keep it out of URLs, rate-limit creation by a server-keyed pseudonymous
  email identity, and return enumeration-resistant lookup failures.
- Use the shared Datafast and PayPal attempts and immutable provider bindings. Environment,
  merchant, internal Domo ID, checkout ID, provider order/resource path, amount, and currency must
  match server-to-server evidence. The canonical checkout order reference and provider merchant
  reference are the same `domo-quote:<quote-id>` value, so the shared verifier cannot accept a
  parallel or ambiguously prefixed identity. A browser return stays unpaid until verification.
- On verified deposit, advance only to `deposit_paid` and `date_reserved`, consume the generic hold,
  post the canonical receipt/ledger path, and write one append-only state event. Event work,
  remaining balance, change orders, cancellation, refunds, completion, and final fulfillment remain
  separate domain operations.
- If payment is first verified after hold expiry, preserve the real provider payment/capture ID as
  a reconciliation exception without reopening the terminal checkout or reclaiming the date.
  Economic, timezone, policy, and identity snapshots are immutable; corrections require a new
  quote/change order or compensating financial records.
- The public venue page reads server limits and event-type keys when authoritative checkout is
  available. When the gates are closed it preserves the prior manual lead flow and says explicitly
  that no availability, hold, order payment, or reservation was created.

## Alternatives considered

### Keep browser quote arithmetic and validate only at payment

Rejected. It exposes business rates as client authority, permits stale totals, and cannot bind an
availability hold to the exact rate and policy used at capture.

### Convert every Domo lead directly into a payable order

Rejected. Custom events still need an approved rate card or staff-authored change order. A lead is
not a price, contract acceptance, date reservation, or payment.

### Reserve the date on provider return

Rejected. Browser-controlled return parameters are not payment evidence and may be replayed or
tampered with.

### Activate the preserved historical rate during migration

Rejected. Copying a client formula is not commercial, tax, legal, or operational approval. The
review and activation must be explicit and attributable to authorized people.

## Consequences

- Production Domo quote and checkout flags remain off. Activation requires an approved current rate
  card, versioned terms and cancellation/refund policy, merchant sandbox evidence, callback and
  reconciliation ownership, availability-calendar ownership, and separate production authorization.
- The secure customer route can show a server quote, terms acceptance, Datafast/PayPal deposit
  actions, and independent quote/payment/venue states. It cannot claim a completed event or paid
  balance from a deposit.
- The first slice does not implement staff change orders, balance milestones, cancellations,
  refunds, calendar operator UI, customer email verification/recovery, or the required admin
  comparison/approval screen. The database review gate is authoritative until those operator
  surfaces are implemented.
