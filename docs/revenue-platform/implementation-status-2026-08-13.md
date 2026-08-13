# Revenue platform implementation status

Date: 2026-08-13

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
  currency, and supplies a request id for retry safety. Browser return alone does not mark payment
  successful.
- Manual bank transfer remains `awaiting_manual_confirmation`. Stripe returns an explicit
  capability-unavailable response instead of `501` or success.
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
  storefront security changes.

## Explicitly feature-disabled

- Production Datafast and PayPal execution until merchant credentials, webhook/callback semantics,
  sandbox evidence, reconciliation ownership, and production authorization are verified.
- Datafast recurrence, tokenization, authorization/capture, installments, and refunds unless the
  specific merchant contract confirms them.
- PayPal subscriptions and Payouts; they are different products from PayPal Checkout.
- Domo checkout until the historical and proposed rate card receives independent approval.
- DDEX production export/delivery/takedown, DSR ingestion, statements, automatic payouts, and every
  recipient-specific operation until the required licence, DPID/partner contract/profile/transport
  evidence, credentials, and authorization exist.
- Automatic recurring charges and automatic third-party/artist/organizer payouts.
- Public self-service distribution. The public page describes the private-pilot gate truthfully.

## Not implemented in this branch

The following requested domains remain future phases and must not be represented as complete:

- Wiring the canonical checkout aggregate into marketplace sales, real rentals, room/resource
  bookings, courses, Domo accepted quotes, public tickets, tips, memberships, provider services, and
  verified donations.
- Physical shipping/pickup/returns, rental dates/deposit/custody/damage, booking deposits/balances,
  atomic course seats, and guest ticket issuance through Datafast/PayPal.
- Mixing/mastering private object-store multipart upload, malware scanning, engineer workflow,
  deliverable version history, revision billing, notifications, and refunds.
- Public event detail/storefront, distribution onboarding/release wizard, staff QC consoles,
  customer catalog/submission tracking, statement UI, and admin reconciliation dashboards.
- DDEX import-plan generation/resolution/commit, ERN rendering/download, partner transport,
  acknowledgement ingestion, correction/takedown execution, DSR parsing, royalty allocation jobs,
  statement generation, or settlements. Their schemas and gates do not equal runtime completion.
- A real staging deployment, Datafast/PayPal sandbox transaction, contracted-recipient test delivery,
  recipient acknowledgement, live-store check, usage report, royalty statement, or payout.

## Compatibility and data migration

All migrations are additive. Legacy orders and provider references are preserved. The checkout
foundation provides `legacy_unreconciled` linkage rather than inferring payment from ambiguous
history. Rollbacks refuse to remove schemas after material checkout, product approval,
distribution, royalty, or payout evidence exists. No existing record was backfilled or rewritten in
this branch; production dry-run counts still require a read-only staging/production snapshot.

## Release conclusion

This branch is suitable for a draft review and an isolated migration/application staging exercise.
It is not production-ready and does not satisfy the full multi-phase definition of done. The next
safe implementation slice is to wire one low-risk domain into the canonical checkout/event/ledger
model, complete signed provider webhooks and refunds, then prove reconciliation in provider
sandboxes before broadening storefronts.
