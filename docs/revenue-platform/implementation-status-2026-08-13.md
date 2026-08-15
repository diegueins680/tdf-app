# Revenue platform implementation status

Date: 2026-08-14

Branch: `feat/unified-revenue-platform-20260813`

Baseline: `692e8d75d9c6fa00390e33a931d052f0f3ce2a38`

This is an evidence record, not a production-readiness declaration. A database object, generated
package, queued attempt, local fixture, or mocked response is not evidence of a payment, delivery,
DSP acknowledgement, live release, royalty receipt, settlement, or payout.

## Implemented and locally verified

- The mixing/mastering browser no longer creates a demo order after an API error. Datafast, PayPal,
  and bank transfer have distinct initiation paths and honest `order_created`,
  `awaiting_payment`, `processing`, and `paid` presentation.
- Mixing/mastering package price and song limits are enforced by the server. Creation has an
  idempotency key and returns a cryptographically random customer lookup token; public lookup
  requires that token and exposes a customer-safe view.
- Datafast service confirmation binds the internal order, stored checkout resource, amount,
  currency, entity, and merchant transaction reference before accepting the provider status.
- PayPal service checkout uses Orders v2 create/capture, binds `custom_id`/`invoice_id`, amount and
  currency and configured payee merchant ID, and supplies a request id for retry safety. Browser
  return alone does not mark payment successful.
- Every newly created mixing/mastering order now creates, in the same database transaction, one
  canonical checkout and immutable line-item snapshot. Datafast/PayPal initiation and capture use
  canonical attempts and immutable provider bindings. A server-verified payment atomically posts a
  balanced service-revenue ledger transaction, one payment receipt and the separate domain payment
  state. A second succeeded attempt is rejected.
- Provider amount, currency, order, resource, environment or merchant mismatches remain unpaid and
  create a deduplicated reconciliation exception. Manual bank transfer/cash/POS selection creates a
  review record and remains `awaiting_manual_confirmation`. Stripe returns an explicit
  capability-unavailable response instead of `501` or success.
- PayPal capture webhooks are remotely signature-verified over the exact raw event, timestamp-
  bounded, encrypted at rest, deduplicated and processed through a bounded retry/dead-letter inbox.
  A completed capture must still match the immutable order, capture, amount, currency, environment
  and merchant binding. Unknown events do not mutate commerce state; externally observed refund or
  reversal events open reconciliation exceptions instead of inventing an internal refund.
- A five-second bounded worker claims due or stale provider events atomically, decrypts only rows
  already marked signature-verified, verifies the stored SHA-256 and immutable event metadata, and
  reuses the same idempotent PayPal processor. Strict administrators have a bilingual redacted
  `/admin/commerce/provider-events` view. A dead letter can move back to `retry` only through the
  database requeue function, which appends an immutable actor/reason action; replay never resets the
  attempt counter and never marks an order paid by itself.
- Strict administrators can request a full or partial PayPal refund from the server-calculated
  remaining captured balance using an active database-managed bilingual reason code. A different
  authenticated administrator must approve it. Provider success creates immutable allocation
  evidence, a balanced compensating ledger transaction and one credit note. Refund and webhook
  production flags remain off.
- Strict administrators can run read-only provider reconciliation for a paid mixing/mastering order.
  Mismatches create deduplicated exceptions; reconciliation never edits payment or fulfillment state.
- Service-storefront administration now requires strict Admin access. Its generic order updater can
  advance only the fulfillment lifecycle; it cannot manufacture paid, refund, dispute or chargeback
  states.
- Marketplace equipment sales now link the legacy domain order to one canonical checkout with
  immutable per-asset line snapshots and a 15-minute atomic hold. One cart-level idempotency key is
  reused when the customer switches among Datafast, PayPal and manual payment; retries cannot create
  a second internal order or stock hold. Rental listings fail closed because they still lack dates,
  deposits and custody terms.
- Marketplace Datafast and PayPal use the shared attempt, immutable provider binding, verified
  payment, receipt, ledger and reconciliation primitives. Verification includes environment,
  merchant, internal order, amount, currency, provider resource and resource path. Guest capture,
  confirmation and tracking require a hashed lookup capability. Stripe's legacy marketplace create
  endpoint now returns an explicit capability-unavailable response instead of creating an unheld
  order.
- Marketplace payment and physical fulfillment are separate state machines. Verified payment moves
  the order only to `ready_to_fulfill`; validated pickup/shipping/delivery/return transitions append
  immutable operator evidence. The asset becomes `Sold` only on delivery and a returned asset is not
  silently relisted. Fully refunded or disputed checkouts cannot start outbound fulfillment.
- The public Records page no longer installs a demo/admin bearer token. It uses the existing public
  booking path.
- Provider-neutral persistence exists for immutable checkout snapshots, attempts and provider
  bindings, event inbox, refunds and disputes, manual evidence, receipts, double-entry ledger,
  expiring reservations, audit events, reconciliation exceptions, feature flags, and guest lookup
  tokens. Database constraints and the pure state machine enforce the critical lifecycle rules.
- DDEX upload is private/local-staging only, size-limited, strict UTF-8 and XML checked, blocks
  entity/DOCTYPE/XInclude content, records SHA-256, deduplicates, and verifies the checksum again on
  raw download. Preview and catalog read-through are implemented.
- DDEX validation detects ERN 4.3.2 and runs structural/business checks. It always reports the
  missing licensed recipient-profile validation, so a structural check cannot be misrepresented as
  recipient-valid.
- Unimplemented DDEX import/export/delivery operations are closed behind explicit `503` capability
  gates; no reachable DDEX handler returns a misleading `501`.
- The distribution accounting schema separates release versions, rights, accepted 100% splits,
  partner profiles, immutable packages, delivery attempts and evidence, usage reports, statements,
  corrections, beneficiary profiles, and payouts. Production mock/sandbox evidence is rejected and
  payout approval requires separation of duties.
- The historical Domo browser formula is preserved as an inactive, reviewable server rate version.
  The public page no longer presents its local arithmetic as an authoritative quote or deposit.
- Fourteen bilingual distribution-product benchmark rows cover single/EP/album, catalog management,
  a non-renewing monthly domain product, and add-ons. They are inactive; an independent market and
  margin review is required before activation.
- `/comercio` (with `/commerce` redirect) and `/distribucion` (with `/distribution` redirect) provide
  accessible bilingual discovery. They disclose whether a path is checkout, request-only, private
  pilot, or unavailable and do not display invented distribution prices.
- OpenAPI, generated web types, and the mobile generated client are synchronized for the service
  storefront and marketplace public checkout, webhook, tracking, fulfillment, refund,
  reconciliation, provider-event operations, and administration contracts.

## Explicitly feature-disabled

- Production Datafast and PayPal execution until merchant credentials, webhook/callback semantics,
  sandbox evidence, reconciliation ownership, and production authorization are verified.
- Production mixing/mastering checkout through its independent `commerce.mixing_mastering` database
  kill switch, in addition to the provider-specific switches.
- Production marketplace sales through `commerce.marketplace_sales`; production remains off until
  the additive migration, anonymized backfill report, provider sandbox verification, shipping/pickup
  ownership, reconciliation alerts and a separately authorized low-value rollout pass. Marketplace
  rentals remain independently disabled through `commerce.marketplace_rentals`.
- Production asynchronous provider-event processing through
  `checkout.provider_event_worker`; sandbox is enabled for local/staging rehearsal while production
  remains off pending credentialed retry evidence and named alert ownership.
- Datafast recurrence, tokenization, authorization/capture, installments, and refunds unless the
  specific merchant contract confirms them.
- Datafast webhook and refund runtime; the public merchant documentation reviewed does not establish
  an authenticated callback or refund contract for TDF's merchant capability.
- PayPal subscriptions and Payouts; they are different products from PayPal Checkout.
- Domo checkout until the historical and proposed rate card receives independent approval.
- DDEX production export/delivery/takedown, DSR ingestion, statements, automatic payouts, and every
  recipient-specific operation until the required licence, DPID/partner contract/profile/transport
  evidence, credentials, and authorization exist.
- Automatic recurring charges and automatic third-party/artist/organizer payouts.
- Public self-service distribution. The public page describes the private-pilot gate truthfully.

## Not implemented in this branch

The following requested domains remain future phases and must not be represented as complete:

- Wiring the canonical checkout aggregate into real rentals, room/resource bookings, courses, Domo
  accepted quotes, public tickets, tips, memberships, provider services, and verified donations.
- Marketplace carrier integrations and customer return/refund initiation; rental dates and deposit,
  custody and damage workflows; booking deposits/balances; atomic course seats; and guest ticket
  issuance through Datafast/PayPal.
- Mixing/mastering private object-store multipart upload, malware scanning, engineer workflow,
  deliverable version history, revision billing, notifications, and non-PayPal refund adapters.
- Public event detail/storefront, distribution onboarding/release wizard, staff QC consoles,
  customer catalog/submission tracking, statement UI, and reconciliation-exception assignment dashboard.
- DDEX import-plan generation/resolution/commit, ERN rendering/download, partner transport,
  acknowledgement ingestion, correction/takedown execution, DSR parsing, royalty allocation jobs,
  statement generation, or settlements. Their schemas and gates do not equal runtime completion.
- A real staging deployment, Datafast/PayPal sandbox transaction, contracted-recipient test delivery,
  recipient acknowledgement, live-store check, usage report, royalty statement, or payout.

## Compatibility and data migration

All forward migrations are additive. Legacy service and marketplace orders and provider references are preserved;
the read-only `service_storefront_checkout_backfill_report` classifies them as `linked`,
`safe_unpaid_candidate`, or `requires_reconciliation` without creating a checkout or inferring
payment. Its rollback is available before runtime links exist and refuses to remove the linkage once
any service order references a canonical checkout. Other rollbacks likewise refuse to remove schemas
after material checkout, product approval, distribution, royalty, or payout evidence exists. No
existing record was backfilled or rewritten in this branch; production dry-run counts still require
a read-only staging/production snapshot.

The read-only `marketplace_sale_checkout_backfill_report` classifies legacy marketplace rows as
`linked`, `requires_payment_reconciliation`, `eligible_unpaid_manual_review`, or
`historical_terminal_manual_review`. It never infers payment. The marketplace runtime rollback works
before live links exist and refuses after any canonical sale linkage exists.

## Release conclusion

This branch is suitable for a draft review and an isolated migration/application staging exercise.
It is not production-ready and does not satisfy the full multi-phase definition of done. Two
low-risk domains—mixing/mastering and equipment sales—are now wired into the canonical
checkout/receipt/ledger model. The next safe external step is credentialed Datafast and PayPal
sandbox checkout/capture/webhook/reconciliation evidence for both domains. The next internally
implementable domain slice is payable studio/DJ bookings or a date-aware rental aggregate; neither
should reuse the sale lifecycle. Datafast refund or callback work remains blocked on a verified
merchant contract.
