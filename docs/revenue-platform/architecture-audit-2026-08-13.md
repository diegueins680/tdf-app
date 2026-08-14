# Revenue platform architecture audit

Status: accepted implementation baseline
Audited repository: `diegueins680/tdf-app`
Audited commit: `692e8d75d9c6fa00390e33a931d052f0f3ce2a38`
Audit date: 2026-08-13

This document is a source-backed inventory and decision record, not a production-readiness
claim. Source code, migrations, tests, provider responses, and recipient acknowledgements remain
authoritative.

## Executive finding

TDF currently has six separate money paths: marketplace Datafast/PayPal/Stripe, service-storefront
Datafast/PayPal stubs and partial handlers, Stripe ticketing, Stripe course checkout, Stripe Connect
tips, and staff-entered payments/nominal service "escrow". They do not share an immutable checkout,
provider-event inbox, refund model, ledger, reconciliation, guest lookup capability, or fulfillment
contract. Public bookings and Domo are leads, courses are paid out-of-band unless Stripe is used,
donations have no verification, and ticket buying is authenticated. The DDEX API exposes nine
reachable `501` handlers and has no delivery, acknowledgement, usage-report, royalty, statement, or
payout domain.

Phase 0 must remove false success and browser credentials before any rollout. Later phases will add
one checkout aggregate that **links to** each domain order; domain fulfillment remains separate.
Production provider, delivery, takedown, refund, and payout operations remain disabled until their
specific capability and authorization gates pass.

## Evidence inspected

- Servant API composition and handlers under `tdf-hq/src/TDF/API*`, `TDF/Server*`, course routes,
  ticket handlers, and DDEX modules.
- Persistent models plus every SQL migration through `2026-08-09`.
- Public/protected React routes, commercial pages, API clients, and the generated client artifacts.
- Mobile submodule `87ceff2d0a8b203c32d25a6f2818e9f38a9d2684` and its Stripe ticket/course surfaces.
- `.github/workflows`, production-migration tooling, feature registry, deployment configuration, and
  current GitHub check runs.
- Canonical OpenAPI `tdf-hq/docs/openapi/api.yaml`: 78 paths, with the commercial APIs above mostly
  absent despite existing source routes.

The complete endpoint/surface mapping, including auth, pricing, tax, idempotency, verification,
payment state, fulfillment, refunds, reconciliation, and public access, is in
[`endpoint-inventory.csv`](endpoint-inventory.csv).

## Implemented delta after the audited baseline

The first domain-linked runtime slice now covers new mixing/mastering orders. Order creation and its
immutable canonical checkout snapshot are one transaction. Datafast/PayPal operations persist
idempotent attempts and immutable resource bindings; only server verification with matching
environment, merchant, internal order, provider resource, amount and currency can post payment.
Posting creates one balanced ledger transaction and one receipt while fulfillment remains separate.
Manual selection remains under review. Provider mismatches create reconciliation exceptions, strict
Admin is required for service administration, and the generic admin update cannot set financial
states.

Historical service orders are not silently linked. The migration exposes a read-only classification
of safe unpaid candidates versus records requiring evidence-preserving reconciliation; it performs
no backfill. Production Datafast/PayPal remain disabled, and signed event ingestion, refunds and
sandbox proof remain later work.

## Validated known findings

1. `MixingMasteringPage.tsx` sends Datafast, PayPal, and bank transfer through the same
   `createOrder` call; on failure it fabricates `TDF-<timestamp>`, reports success, and links to the
   marketplace tracker. It also silently falls back to client package prices.
2. Service orders snapshot the selected package price but accept any `songCount` (including unsafe
   package/quantity combinations). Their public lookup uses an eight-hex order number and returns
   buyer PII without a lookup token. Stripe and revision handlers return `501`.
3. Service Datafast confirmation checks a caller-supplied resource path remotely but does not bind
   it to the stored checkout, amount, currency, merchant entity, or merchant transaction reference.
   Service PayPal capture checks the stored PayPal order ID but not captured amount/currency and has
   no request id or verified webhook.
4. Marketplace snapshots line prices and has the strongest existing Datafast path, including
   checkout/resource-path and amount/currency checks. It still creates duplicate orders across
   Datafast/PayPal retries, lacks stock/rental holds and fulfillment, does not verify PayPal capture
   amounts, and has no shared event/refund/reconciliation ledger.
5. `/reservar` and `/dj-booth` create tentative bookings. The UI probes a nonexistent
   `/bookings/public/availability` path and can continue with availability `unknown`; no service
   order, hold, invoice, deposit, or balance exists.
6. Domo prices, tax, add-ons, and 40% suggestion are browser constants. The submitted record is only
   a public booking note; there is no rate-card version, quote, availability hold, acceptance, or
   payment.
7. Courses persist database prices and Stripe identifiers, but the buyer path is Stripe-only or
   staff follow-up. Capacity is reported but not atomically held. Course metadata serializes a
   `Double` display price although the database authority is cents.
8. Ticket tiers, capacity, promos, refunds, transfers, waitlists, and QR records exist behind the
   authenticated `/social-events` API. Checkout is Stripe-only; the public router has no event
   storefront. Finance entries are not a settlement ledger.
9. Artist tips use a hard-coded 10% and Stripe Connect destination charges. There is no React tip
   CTA, provider-neutral checkout, artist payable ledger, or manual settlement gate. Fan clubs have
   no paid tier/entitlement lifecycle.
10. Service marketplace booking inserts `Payment` rows and calls the result `escrow_held` without a
    verified charge or lawful funds-holding adapter; release inserts another nominal payment.
11. `/donar` renders a client-configured Cardano address and an external QR image. No donation API,
    transaction verification, attribution, receipt, or reconciliation exists.
12. `RecordsPublicPage.tsx` installs `VITE_PUBLIC_BOOKING_TOKEN`, falling back to
    `VITE_API_DEMO_TOKEN`, as a transient browser bearer. The safe `/bookings/public` flow already
    exists and must replace it.
13. DDEX upload, raw download, preview, import-plan create/resolve/commit, export render/download,
    and catalog read-through return `501`. Existing validation only queues a pending database row.
    There is no private storage implementation, delivery attempt, acknowledgement, live evidence,
    DSR ingestion, royalty ledger, statement, or payout.

## Duplication and centralization boundary

Centralize only protocol-neutral commerce and provider concerns:

- checkout session and immutable line snapshot;
- provider attempt/resource/event/refund/dispute records and idempotency;
- holds and expiry coordination;
- manual-payment evidence/approval;
- receipts/invoice references, double-entry ledger, and reconciliation;
- secure customer lookup capability and audit events;
- provider adapter contracts for create, verify/capture, refund, webhook, and capability discovery.

Keep domain ownership in marketplace, rentals, bookings, Domo quotes, courses, tickets, tips,
memberships, service-provider jobs, donations, and distribution. A verified payment publishes one
idempotent domain event; each domain owns fulfillment, inventory/capacity consumption, cancellation,
and entitlement behavior.

## Price, tax, and data authority

- Money is signed integer minor units plus uppercase ISO 4217 currency. No binary floating point is
  authoritative.
- Product/rate-card/quote versions are immutable once referenced. Checkout lines copy description,
  quantity, unit price, discount, tax basis points, fee allocation, and total.
- Taxes and fees are server-calculated from an approved configuration version. Ecuadorian electronic
  invoicing remains a separate adapter; a payment receipt is not an SRI tax invoice.
- Client prices may render estimates only. A mismatch returns a fresh server quote; it never silently
  changes the charge.

The enforceable transition tables and invariants are in [`formal-model.yaml`](formal-model.yaml).

## Secret and capability boundary (names only)

Existing server payment variables are `DATAFAST_ENTITY_ID`, `DATAFAST_BEARER_TOKEN`,
`DATAFAST_BASE_URL`, `DATAFAST_TEST_MODE`, `DATAFAST_MID`, `DATAFAST_TID`, `DATAFAST_PSERV`,
`DATAFAST_USER_DATA2`, `DATAFAST_VERSIONDF`, `DATAFAST_ENV`, `COMMERCE_CHECKOUT_ENV`,
`PAYPAL_CLIENT_ID`, `PAYPAL_CLIENT_SECRET`, `PAYPAL_ENV`, `PAYPAL_MERCHANT_ID`,
`STRIPE_SECRET_KEY`, `STRIPE_PUBLISHABLE_KEY`, and `STRIPE_WEBHOOK_SECRET`.
Frontend/mobile public configuration includes `VITE_API_BASE`, `EXPO_PUBLIC_API_BASE`, Stripe
publishable/merchant identifiers, and `VITE_CARDANO_ADDRESS`. Browser bearer configuration was
removed in Phase 0 and must not be reintroduced.

Missing explicit boundaries to add are PayPal webhook ID, Datafast notification
verification/capability configuration, private object-storage bucket/region/endpoint
and credential references, malware-scanner configuration, signed-link keys, per-partner credential
references, DDEX mode, and payout enable/approval gates. Secret values belong in the deployment
secret manager only. Provider payloads, access tokens, raw card data, protected asset URLs, KYC/tax
documents, and payout account details must be redacted from logs and docs.

Datafast documents separate test and production credentials, requires server-side status lookup,
and requires Datafast certification before production. Tokenization/recurrence requires separate
commercial and acquiring-bank approval; it is not inferred from the existing checkout contract:
<https://developers.datafast.com.ec/index.aspx> and
<https://developers.datafast.com.ec/msdk.aspx>.

PayPal Orders v2 supports create/capture, while `PayPal-Request-Id` supplies POST idempotency. Refunds,
webhook verification, subscriptions, and Payouts are distinct capabilities:
<https://developer.paypal.com/docs/api/orders/sdk/v2/>,
<https://developer.paypal.com/reference/guidelines/idempotency/>, and
<https://developer.paypal.com/api/rest/webhooks/event-names/>.

## Existing production data and compatibility

Do not rewrite historical rows in place. The backfill links existing records by domain type and key:

- `marketplace_order` and item/provider reference columns;
- `service_storefront_order`, packages, status changes, and revisions;
- `booking`, `service_order`, `invoice`, `receipt`, `payment`, `payment_split`, package ledger, and
  nominal `service_escrow` rows;
- course registrations, receipt evidence, Stripe PaymentIntent/subscription IDs;
- ticket orders/tickets, promo redemptions, refund requests, transfers, waitlists, Stripe intents and
  webhook events, and finance entries;
- artist tips and Stripe account references;
- catalog/DDEX documents, validation/import/export records, identifiers, assets, deals, and partners.

Backfill is insert-only into link/snapshot tables. It emits counts by domain/status/currency/provider,
duplicate or conflicting references, missing totals/items, and ambiguous payment classifications.
Ambiguous rows remain `legacy_unreconciled`; no `paid`, captured, refunded, delivered, acknowledged,
live, or settled status is inferred. Existing public routes remain redirects or compatibility
adapters; unsafe false-success and public bearer behavior are removed immediately.

## Standards and distribution profile

The partner registry must pin a recipient-specific profile and assets/checksums rather than a global
version. The current DDEX standards list names ERN 4.3.2 as the baseline schema, ERN release profiles
2.3.1, DSR architecture 1.4, Basic Audio 1.4, Record Type Definitions 1.5.1, and Financial Reporting
to Record Companies 1.2: <https://kb.ddex.net/reference-material/standards-specifications/>.

Evaluation/development is allowed under the evaluation licence, but commercial exchange requires a
DDEX Implementation Licence and DPID. A DPID identifies each sender/recipient; partner-specific
implementation is recommended:
<https://kb.ddex.net/general-implementation-guidance/licensing-the-standards/>,
<https://kb.ddex.net/general-implementation-guidance/licensing-the-standards/ddex-party-identifier-%28dpid%29/>,
and <https://kb.ddex.net/general-implementation-guidance/planning-an-implementation/>.

Initial implemented profile: conventional audio single/EP/album, ERN 4.3.2 internal generation and
validation, with recipient profile/version configurable. ERN delivery/update/takedown and
acknowledgement remain production-disabled until a named contracted partner supplies its profile,
transport, test credentials, and acknowledgement semantics. DSR ingestion targets the contracted
partner's applicable profile; RIN and MEAD are adapter placeholders only.

## Architecture decisions

The accepted ADRs are:

- [ADR-0100](../adr/0100-domain-linked-checkout.md): domain-linked provider-neutral checkout.
- [ADR-0101](../adr/0101-verified-payment-events.md): verified provider events and split lifecycles.
- [ADR-0102](../adr/0102-versioned-money-and-holds.md): versioned minor-unit pricing and atomic holds.
- [ADR-0103](../adr/0103-guest-order-capabilities.md): scoped guest lookup capabilities.
- [ADR-0104](../adr/0104-immutable-financial-ledger.md): immutable double-entry finance and manual payout gate.
- [ADR-0105](../adr/0105-private-versioned-assets.md): private, checksummed, immutable asset versions.
- [ADR-0106](../adr/0106-partner-profiled-ddex.md): partner-profiled DDEX with evidence-based status.

## Feature flags and phased rollout

Every flag is independently killable. The canonical runtime flags are rows in
`revenue_feature_flag`, keyed by environment; production rows default to disabled. Environment
variables select immutable sandbox/production configuration but do not override a disabled
production capability:

- implemented keys: `checkout.datafast`, `checkout.paypal`, `commerce.mixing_mastering`; planned provider/domain keys
  must use the same registry rather than introducing an untracked environment-only bypass;
- domains: `COMMERCE_MIXING_ENABLED`, `COMMERCE_EQUIPMENT_SALES_ENABLED`,
  `COMMERCE_RENTALS_ENABLED`, `COMMERCE_BOOKINGS_ENABLED`, `COMMERCE_DOMO_ENABLED`,
  `COMMERCE_COURSES_ENABLED`, `COMMERCE_TICKETS_ENABLED`, `COMMERCE_TIPS_ENABLED`,
  `COMMERCE_MEMBERSHIPS_ENABLED`, `COMMERCE_PROVIDER_SERVICES_ENABLED`,
  `COMMERCE_DISTRIBUTION_ENABLED`;
- operations: `PROVIDER_EVENTS_ENABLED`, `REFUNDS_ENABLED`, `RECONCILIATION_ENABLED`,
  `PRIVATE_ASSETS_ENABLED`, `SRI_INVOICING_ENABLED`;
- distribution: `DDEX_IMPORT_ENABLED`, `DDEX_EXPORT_ENABLED`, `DDEX_DELIVERY_ENABLED`,
  `DDEX_TAKEDOWN_ENABLED`, `ROYALTY_INGEST_ENABLED`, `STATEMENTS_ENABLED`,
  `AUTOMATIC_PAYOUTS_ENABLED`, plus a recipient-specific flag.

Phase gates follow the requested sequence: truthfulness/security; checkout core; sandbox adapters;
marketplace and mixing; bookings/courses/Domo; one TDF-owned ticket pilot; TDF catalog DDEX sandbox;
contracted partner acknowledgement; invited artists/labels; public distribution; reports/statements;
and finally payout-dependent tips/memberships/provider settlement. A phase is not production-ready
until credentials/contracts, migrations, webhooks, reconciliation, alerts/runbooks, ownership, tests,
and rollback are verified.

## CI and operational baseline

At the audited SHA, repository build checks are green, scheduled token/enrichment checks are green,
and Cloudflare Pages is failing. The PR workflow performs path-scoped repository/UI/mobile/backend,
OpenAPI generated-client, and PostgreSQL migration checks. It does not currently prove provider
sandbox contracts, commercial E2E flows, reconciliation, DDEX delivery/acknowledgement, DSR,
royalties, statements, or payouts. Those lanes must be added as opt-in secret-backed checks with
non-secret fixtures and production-event rejection tests.

## External blockers and required approvals

- Datafast merchant capability confirmation, test/production credentials, notification semantics,
  refund/installment/tokenization capabilities, certification, and separate production-charge and
  refund authorization.
- PayPal sandbox/live credentials, webhook ID, merchant refund capability; separate subscription or
  Payouts product approval and production capture/refund/payout authorization.
- A private object-store account, retention policy, malware scanner, encryption/backup ownership,
  and signed-link key rotation.
- DDEX Implementation Licence/DPID or a contracted partner acting on TDF's behalf; ISRC/UPC/EAN
  authority or partner allocation; named recipient profile, transport credentials, test acceptance,
  acknowledgement/live/report semantics, and separate real delivery/takedown authorization.
- Ecuador/launch-market legal review for merchant-of-record, consumer/tax/invoicing, rental/deposit,
  event settlement, third-party proceeds, donations, distribution/rights, privacy/retention,
  sanctions/KYC/tax, minors, infringement/counter-notice, and payout operations.
- Verified bank/manual settlement controls. Automatic payouts remain disabled until production
  authorization, KYC/tax/bank validation, reconciliation, dual approval, and rollback are proven.
