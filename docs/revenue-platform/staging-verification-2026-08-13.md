# Staging verification record

Date: 2026-08-17

Environment tested: local isolated worktree and disposable PostgreSQL 16 and 17 containers.

Staging deployment: **not performed**. Provider sandbox transaction: **not performed**. DDEX partner
sandbox delivery: **not performed**. No credentials or real customer/royalty data were used.

## Local evidence

The final command transcript is summarized here after the branch-wide verification run:

| Area | Command | Result |
|---|---|---|
| Service phase 0 migration | `./scripts/test-service-storefront-phase0-migration.sh` | Pass: forward, constraints, rollback, reapply |
| Checkout core migration | `./scripts/test-unified-checkout-migration.sh` | Pass: dry-run, rollback, binding, inbox, hold, ledger |
| Service checkout runtime migration | `./scripts/test-service-storefront-checkout-runtime-migration.sh` | Pass: rerun, pre-use rollback, legacy classification, one checkout link, one succeeded attempt, reconciliation deduplication, manual evidence and receipt constraints; rollback correctly refused after a live link |
| Provider-event/refund runtime migration | `./scripts/test-checkout-event-refund-runtime-migration.sh` | Pass: rerun, clean rollback, production-off gates, unsigned-event rejection, immutable encrypted inbox evidence, active configured reason enforcement, two-person approval, allocation/credit-note constraints; rollback correctly refused after live evidence |
| Provider-event operations migration | `./scripts/test-provider-event-operations-migration.sh` | Pass: rerun, clean rollback, formal status transitions, control-safe audited dead-letter requeue, event-scoped transition authorization, duplicate prevention, sandbox-on/production-off worker gates, immutable action evidence; rollback correctly refused after replay evidence |
| Marketplace sale checkout runtime | `./scripts/test-marketplace-sale-checkout-runtime-migration.sh` | Pass on PostgreSQL 17: rerun, clean rollback/reapply, one active hold per unique asset, direct paid transition rejected, verified payment separated from fulfillment, fully refunded outbound fulfillment rejected, pickup/delivery custody, return without relisting, immutable history; rollback correctly refused after a live link |
| Marketplace rental checkout runtime | `./scripts/test-marketplace-rental-checkout-runtime-migration.sh` | Pass on PostgreSQL 17: rerun, approved legacy-rate migration with append-only history, both marketplace domain gates enabled, same-version commercial mutation rejected, inclusive date exclusion, direct paid transition rejected, verified payment separated from custody, handoff/return condition reports, truthful deposit-deduction/refund-due states, non-zero deposit closure gate; rollback correctly refused after a live link |
| Service booking checkout runtime | `./scripts/test-service-booking-checkout-runtime-migration.sh` | Pass on PostgreSQL 17: rerun, inactive draft policy preservation, production-off domain gate, atomic resource hold, direct paid rejection, verified deposit confirmation separated from fulfillment, failed-provider-attempt expiry and resource release, reviewed manual evidence, independent approver, exact amount/currency binding, rollback refusal after reviewed evidence, follow-up rollback/reapply, and base rollback refusal after a canonical link |
| Distribution accounting migration | `./scripts/test-distribution-accounting-migration.sh` | Pass: rollback, lifecycle, splits, package/evidence, royalty, separation of duties, payout gates |
| Versioned revenue products | `./scripts/test-versioned-revenue-products-migration.sh` | Pass: inactive legacy Domo rate, approval/immutability, production flag |
| Distribution pricing seeds | `./scripts/test-distribution-product-seeds-migration.sh` | Pass: 14 inactive bilingual seeds, activation/mutation/rollback gates |
| Backend focused tests | Stack-built `tdf-hq-test --match …` | Pass: 50 examples, including 800 property cases, zero failures |
| Provider-event backend invariants | Stack-built `tdf-hq-test --match 'service storefront commercial invariants'` | Pass: 19 examples, including 300 property cases, zero failures; immutable event metadata/provider/environment tampering, invalid replay reasons, and oversized PayPal request IDs are rejected |
| Marketplace rental backend invariants | Stack-built `tdf-hq-test --match 'marketplace rental'` | Pass: 5 examples, including 100 property cases, zero failures; inclusive dates, weekly pricing, separate deposits, overflow rejection, terminal-state closure and no skipped payment/handoff/inspection states |
| Service booking backend invariants | Stack-built `tdf-hq-test --match 'service booking pricing and fulfillment invariants'` | Pass: 4 examples, including 100 property cases, zero failures; server minor-unit totals, duration/policy limits, overflow rejection, and no skipped deposit/fulfillment states |
| Backend test/build | `stack test --fast` with the default optimized Stack profile | Pass: 2,342 examples, zero failures; executable and all 162 test modules compiled and linked |
| Web regression/accessibility | Jest: five changed suites | Pass: 16 tests, zero failures |
| Provider-event operator UI/access | Jest: provider-event page and access-control suites | Pass: 14 tests, zero failures; raw payload and merchant binding are absent from the UI contract |
| Marketplace web regressions | Jest: marketplace admin, API, Datafast return and storefront suites | Pass: 71 tests, zero failures; canonical payment is read-only in operations, fulfillment uses its dedicated transition API, one checkout key survives provider switching, lookup secrets stay in headers/session storage, and missing lookup/provider failure never clear the cart |
| Marketplace rental web regressions | Jest: marketplace storefront and rental-operations suites | Pass: 67 tests, zero failures; unapproved rentals fail closed, approved rentals require inclusive dates, and custody transfer requires an outbound condition report without sending a deposit deduction |
| Public booking web regressions | Jest: public booking, order-tracking and booking API contract suites | Pass: 27 tests, zero failures; lookup secrets stay in headers/session storage, Datafast browser return remains processing until server verification, PayPal approval calls server capture, manual evidence remains pending review, and only a server-paid checkout is described as verified |
| Web type safety | `npm run typecheck:ui` | Pass |
| Web production build | `npm run build --workspace=tdf-hq-ui` | Pass: 12,384 modules; bundle/secret gate 5 preloads and 403,507 gzip bytes |
| Release/CI contracts | `npm run test:production-release`; `npm run test:ci-pipeline` | Pass: 37 + 12 tests |
| Registered production batch | Restore schema-only fixture plus three synthetic published Records rows; read-only preflight; render/apply twice; schema verification before and after the rerun on PostgreSQL 17 | Pass: all 49/49 migrations were recorded, the exact second run skipped all 49 entries idempotently, marketplace sales/rentals and reviewed manual methods remained enabled, Datafast/PayPal and service bookings remained disabled, active booking policies and provider bindings remained zero |
| OpenAPI/generated clients | `npm run generate:api` for web and mobile | Pass: canonical service-storefront, marketplace sale/rental, booking checkout, Datafast/PayPal actions, manual-evidence submission, protected finance projection, and independent review contracts generated for both clients; mobile submodule commit `cecc281` |
| Mobile type safety | `npm run typecheck:mobile` | Pass |
| Formal-method audit | `npm run verify:formal` | Pass: 0 critical, 0 errors; repository warnings remain advisory |
| Catalog authority audit | `npm run audit:catalog-lists` | Pass: configured refund reasons are database-managed; provider/environment discriminants have reviewed technical-constant decisions |

The focused backend groups were: service storefront (5), checkout state machine (6), distribution
state machine (5), DDEX intake truthfulness (3), ERN parser (20), and business rules (11). A first
web run performed concurrently with GHC compilation had one 5-second timeout; the exact five-suite
command was rerun without contention and all 16 tests passed. No assertion failed in the timed-out
run.

Follow-up verification for the service checkout runtime passed 10 focused examples, including 200
property cases, with zero failures. `stack test --no-run-tests --fast` compiled both the executable
and test suite after the runtime integration. This is local database/application evidence only: no
Datafast or PayPal sandbox request was made and no provider success was asserted.

After registering the runtime migrations with their immutable feature commits, the exact production
batch applied in a fresh PostgreSQL 17 container. The schema-only snapshot was paired with the
repository's three synthetic, published Records source rows, then the read-only preflight passed.
All 48 manifest entries were recorded, the release schema verifier passed, and an unchanged second
run skipped all 48 entries idempotently. The final aggregate evidence was 48 distinct ledger rows,
marketplace sales/rentals enabled, service bookings disabled, zero active booking policies, zero
provider bindings and a verified failed-provider-attempt expiry definition. This fixture exercise
did not classify a payment, create a marketplace order, or alter any external environment.

The 2026-08-14 PayPal event/refund follow-up passed 19 focused examples, including 400 property
cases, with zero failures. The executable and all 155 test modules compiled under the repository's
default optimized Stack configuration. No PayPal or Datafast network request was made; the provider
sandbox evidence requirement remains open.

The provider-event operations follow-up passed 18 focused service-commerce examples, including 300
property cases, and linked the executable plus all 159 test modules. The dedicated migration
rehearsal additionally proved that a requeue authorization cannot leak to another event in the same
transaction. The registered 24-entry production batch was then applied twice and schema-verified in
a disposable PostgreSQL 17 container. These are local fixtures only: no provider callback, sandbox
transaction, staging deployment, refund, or production state was exercised.

The 2026-08-15 marketplace-sale follow-up compiled the backend executable and all 160 test modules,
passed 22 focused backend examples including 400 property cases, passed the dedicated PostgreSQL 17
migration rehearsal, and passed 71 focused web tests. The
OpenAPI document now includes public sale checkout and secure tracking plus authenticated
fulfillment operations; generated web/mobile artifacts were refreshed. This remains local fixture
evidence only. No Datafast or PayPal network request, customer charge, provider refund, carrier
handoff, staging deployment, or production state was exercised.

The 2026-08-16 marketplace-rental follow-up compiled and linked the backend executable and all 161
test modules, passed five focused backend examples including 100 property cases, passed the
dedicated PostgreSQL 17 rehearsal, and passed 67 focused web tests. The migration preserves each
public rental's existing daily rate, records the approved initial terms in append-only history, and
enables the sale/rental domain flags while leaving production provider execution independently
disabled. No provider request, customer charge, deposit refund, physical handoff, staging
deployment, or production migration was made.

The 2026-08-17 service-booking follow-up compiled the backend, passed four focused formal examples
including 100 property cases, passed the PostgreSQL 17 runtime rehearsal, and passed 26 focused web
tests plus web/mobile type checking and the production web build. Datafast create/status and PayPal
create/capture actions now require the secure guest lookup capability, bind provider resources to
the immutable canonical deposit, and only transition payment after server verification. The public
tracker keeps payment and fulfillment separate; a browser return or PayPal approval cannot display
success. A verified Datafast payment first observed after hold expiry creates a reconciliation
exception without confirming or reallocating the booking. The registered follow-up then completed
the full 48-entry production batch in a disposable PostgreSQL 17.10 database, passed the schema
verifier before and after an exact idempotent rerun, and left the service-booking production flag
disabled with zero active policy and provider rows. It created no approved rate, provider resource,
payment, notification, staging deployment, or production transaction. Provider sandbox evidence
and production enablement remain explicit external gates.

The reviewed-manual-payment follow-up passed the complete 2,342-example backend suite, the dedicated
PostgreSQL 17 migration rehearsal, 27 focused public-booking web tests, web/mobile type checking and
the production web build. Bank transfer, cash and POS now create canonical manual-verification
attempts; customer references are evidence only, and approval requires a separate authenticated
reviewer plus exact checkout, attempt, amount, currency, environment and active-hold bindings.
Approval posts the receipt and ledger in the same transaction as the verified payment transition;
expired holds or an already-paid competing rail create an operator reconciliation exception instead
of confirming the booking. The full 49-entry production manifest then passed the read-only preflight,
initial apply, schema verifier, exact idempotent rerun and final verifier in a disposable PostgreSQL
17 database. Aggregate evidence showed 49 distinct migration rows, marketplace sales/rentals and
manual methods enabled, Datafast/PayPal and service bookings disabled, zero active booking policies
and zero provider bindings. The disposable database was removed afterward. No manual evidence was
approved, no payment was classified, and no staging or provider network request occurred.

## Required staging exercise

1. Restore an anonymized database snapshot and run all migration preflight/apply/verification steps;
   record row counts, conflicts and the `legacy_unreconciled` report. Do not infer payment status.
2. Install provider sandbox secrets through the secret manager; verify redaction and kill switches.
3. Exercise duplicate create/callback/capture, signature failure, reordered/delayed events, declined,
   abandoned, outage and full/partial refund paths. Reconcile provider/internal/ledger totals to zero
   unexplained variance.
4. Exercise private asset upload/download authorization, checksum mismatch, malware quarantine,
   expiry, retention and restore.
5. With a named partner profile, generate and validate one TDF-owned test package, send only to its
   sandbox under separate authorization, preserve transport/acknowledgement evidence, and confirm
   that sandbox evidence cannot transition production rows.
6. Ingest a licensed/non-sensitive report fixture, reconcile totals, create a draft statement and a
   correction. Do not issue or pay it.
7. Run accessibility/responsive/browser E2E, bundle secret scan, log-redaction assertions, alerts and
   every operator tabletop in `operator-runbooks.md`.

No screenshots are attached because the changed pages have not been deployed to a review/staging
URL. Local component and accessibility tests are evidence of rendering behavior, not staging proof.
