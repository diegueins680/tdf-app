# Revenue platform implementation status

Date: 2026-08-18

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
  a second internal order or stock hold.
- Marketplace rentals now use a dedicated dated runtime instead of the sale lifecycle. The server
  calculates inclusive duration and daily/weekly pricing from an approved versioned terms record,
  snapshots the rental charge separately from the refundable deposit, and enforces non-overlapping
  asset holds in PostgreSQL. Verified payment confirms the reservation but never implies handoff.
  Outbound/inbound condition reports, custody, inspection, damage review, deduction proposals and
  deposit-refund-due states are separately audited. Raw government identifiers are validated and
  discarded; only document type and last four characters are retained.
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
- Marketplace bank-transfer checkout now completes the customer and finance workflow for both sales
  and rentals. The scoped guest tracker accepts a transfer reference without claiming payment;
  online rails cannot start while evidence is submitted or under review; and only an authenticated
  Invoicing reviewer different from the submitter may approve or reject it. Approval requires the
  exact checkout, attempt, environment, amount, currency, merchant reference and an unexpired asset
  hold, then posts canonical payment/receipt/ledger evidence atomically. Payment still does not imply
  sale fulfillment, rental handoff, custody, inspection, damage resolution or deposit settlement.
- Public studio and DJ availability now resolves the selected offering, duration, default resources,
  and approved policy on the server. Canonical checkout creates an immutable full-price/deposit/
  balance snapshot, a service order, booking, guest lookup capability, 15-minute hold, and deposit-
  only checkout in one transaction. The browser presents `order created` and `deposit pending`; it
  never presents paid or confirmed from checkout creation.
- All booking resource writers now share an exclusion-backed UTC calendar. Concurrent legacy,
  authenticated staff, and canonical booking inserts cannot overlap an active room/resource range.
  Verified deposit evidence confirms only the held booking; scheduling, work, balance due, no-show,
  overtime, completion, cancellation, rescheduling, and dispute remain separate formal states.
- Existing service-offering rate/tax/currency values are copied only into inactive draft policies.
  They require explicit approval and activation before an authoritative public quote or checkout is
  possible, so this migration does not silently authorize a new deposit policy.
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
- Public course registration now creates an atomic 15-minute seat hold and one canonical checkout
  from an approved active immutable policy. Datafast and PayPal actions use the shared attempt,
  binding, verification, receipt, ledger, and secure guest-lookup contracts. A registration becomes
  enrolled only after verified payment; checkout creation and browser return remain unpaid.
- Priced event tickets can no longer be issued through the manager direct-order shortcut or generic
  status update. The signed Stripe webhook now also requires matching succeeded status, amount,
  currency, ticket order, event, and stored PaymentIntent evidence before issuing QR tickets.
- Public event ticket checkout now links the existing domain order to one canonical checkout and
  snapshots an approved policy, tier, promotion, quantity, price, buyer/organizer fee split, tax,
  currency and terms. Event capacity, tier inventory and promotion claims are reserved atomically,
  rate-limited by a server-keyed pseudonymous buyer identity, and released exactly once on expiry.
  Later tier-price edits cannot mutate or strand the accepted snapshot. Datafast and PayPal actions
  use immutable provider bindings and secure guest lookup capabilities; late payment after hold
  expiry opens reconciliation instead of issuing a ticket.
- Ticket issuance is separately serialized and can happen only after verified payment. Duplicate
  provider callbacks cannot create a second `issued` audit, promotion redemption, ticket batch or
  confirmation. The ledger credits only TDF's snapshotted fees as platform revenue and records
  organizer proceeds as a payable liability; it does not claim that settlement occurred.
- The bilingual public routes `/eventos/:eventId/entradas` and
  `/eventos/:eventId/orden/:orderId` show only server-calculated totals and truthful independent
  payment/fulfillment states. A provider return is not success and ticket codes remain absent until
  the server returns `paid` plus `issued`.
- Fourteen bilingual distribution-product benchmark rows cover single/EP/album, catalog management,
  a non-renewing monthly domain product, and add-ons. They are inactive; an independent market and
  margin review is required before activation.
- `/comercio` (with `/commerce` redirect) and `/distribucion` (with `/distribution` redirect) provide
  accessible bilingual discovery. They disclose whether a path is checkout, request-only, private
  pilot, or unavailable and do not display invented distribution prices.
- OpenAPI, generated web types, and the mobile generated client are synchronized for the service
  storefront, marketplace, courses and public event-ticket checkout, webhook, tracking,
  fulfillment, refund, reconciliation, provider-event operations, booking deposit provider actions,
  and administration contracts.

## Explicitly feature-disabled

- Production Datafast and PayPal execution until merchant credentials, webhook/callback semantics,
  sandbox evidence, reconciliation ownership, and production authorization are verified.
- Production mixing/mastering checkout through its independent `commerce.mixing_mastering` database
  kill switch, in addition to the provider-specific switches.
- Canonical public studio/DJ checkout through `commerce.service_bookings`. Its production row starts
  disabled; each offering also requires one approved active policy. Datafast create/status and
  PayPal create/capture actions now use canonical immutable bindings, but remain unavailable in
  production until their rail flags, merchant configuration, and sandbox evidence are verified.
- Canonical public event tickets through `commerce.event_tickets`. Production starts disabled and
  every event also requires an explicitly approved active policy. The migrated two-percent buyer /
  two-percent organizer allocation is an inactive draft only. Datafast/PayPal production execution
  remains independently disabled pending sandbox verification and named reconciliation ownership.
  Organizer settlement remains disabled through `commerce.event_ticket_settlements`; a payable
  ledger entry is not a payout.
- The marketplace sale and rental domain rows, `commerce.marketplace_sales` and
  `commerce.marketplace_rentals`, are enabled by the additive rollout migration. This exposes the
  truthful domain workflows while Datafast/PayPal production execution remains independently off.
  A production deployment, provider charging, and a low-value verification window still require
  separate authorization, sandbox evidence, operational ownership, alerts, and rollback rehearsal.
- Public sale/rental tracking now supports idempotent cancellation, return, extension, and dispute
  requests through the scoped lookup capability. A request is evidence, not an automatic state
  change. Staff review can atomically open supported cancellation/return/dispute domain states;
  rental extensions remain quote-only and direct approval is rejected until atomic availability,
  price, terms, and change-order checkout exist. Manual non-provider deposit settlement is enabled
  behind `commerce.marketplace_manual_deposit_settlement`, requires exact server-derived amounts and
  an independent reviewer, and posts a balanced liability settlement without fabricating a
  Datafast/PayPal refund.
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

- Wiring the canonical checkout aggregate into Domo accepted quotes, tips, memberships, provider
  services, and verified donations.
- Marketplace carrier integrations, approved-return shipping, and sale/provider refund execution;
  payable rental extensions, automated late-fee charging, and non-zero-deposit provider refund
  execution; booking balance collection, refunds, rescheduling/no-show/overtime operator APIs and
  notifications; public event refunds, waitlist promotion, transfer acceptance, and organizer
  settlement execution; public-ticket mailbox verification and verified-email order recovery.
- Mixing/mastering private object-store multipart upload, malware scanning, engineer workflow,
  deliverable version history, revision billing, notifications, and non-PayPal refund adapters.
- A public event index, distribution onboarding/release wizard, staff QC consoles, customer
  catalog/submission tracking, statement UI, and reconciliation-exception assignment dashboard.
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

The read-only `marketplace_sale_checkout_backfill_report` classifies legacy marketplace sale rows as
`linked`, `requires_payment_reconciliation`, `eligible_unpaid_manual_review`, or
`historical_terminal_manual_review`. It never infers payment. The marketplace runtime rollback works
before live links exist and refuses after any canonical sale linkage exists. Existing public rental
listings are linked to an approved `marketplace-rental-v1` terms record without changing their
published daily rate: weekly is six daily rates, deposit is explicitly zero, minimum/maximum are
one/30 days, and the cancellation window is 24 hours. The migration records the system approval in
append-only terms history; it does not classify any historical rental as paid or handed off.
Customer-request and deposit-settlement migrations preserve those links, add no historical payment
classification, and expose `marketplace_rental_deposit_ledger_backfill_report`. A historical paid
rental without an explicit deposit-liability payment entry is `requires_reclassification`; the
migration never edits its posted ledger. Rollback succeeds only before customer or settlement
evidence exists and otherwise refuses data loss.

The service-booking migration backfills only resource-time allocations for existing bookings and
copies current offering commerce values into inactive draft policies. It does not create checkout,
infer payment, approve a deposit, or activate public checkout. Any overlapping active/future legacy
booking fails the migration for explicit operator review. The rollback works before canonical
booking links exist and refuses after material runtime data exists.

The public-ticket migration creates inactive per-event policy drafts from existing paid tiers but
does not approve fees, activate checkout, create orders, reserve seats, or classify historical
payments. Runtime links are created only by the new public checkout. Hold expiry releases tier and
promotion reservations exactly once. Rollback succeeds before an approved policy or material
checkout/payment/fulfillment record exists and otherwise refuses evidence loss. The local migration
rehearsal used a disposable PostgreSQL 16 database because the Docker daemon stalled; no production
or staging data was read or modified.

## Release conclusion

This branch is suitable for a draft review and an isolated migration/application staging exercise.
It is not production-ready and does not satisfy the full multi-phase definition of done. Three
low-risk domains—mixing/mastering, equipment sales, and dated equipment rentals—are wired into the
canonical checkout/receipt/ledger model; studio/DJ bookings have canonical deposit checkout; and
public event tickets have atomic seat holds, guest Datafast/PayPal actions, truthful tracking and
organizer-payable accounting behind independent gates. The next safe external step is credentialed
Datafast and PayPal sandbox checkout/capture/webhook/reconciliation evidence for these domains.
Booking/ticket production policy activation, organizer settlement, Datafast refund/callback work,
balance collection, and non-zero rental-deposit provider settlement remain blocked on verified
merchant capabilities and operational approval.
