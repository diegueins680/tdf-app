# Staging verification record

Date: 2026-08-18

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
| Marketplace customer operations migration | `./scripts/test-marketplace-operations-migration.sh` | Pass on isolated local PostgreSQL 16 and CI PostgreSQL 17: rerun/clean rollback, customer-request transition guards, quote-only extensions, dispute/payment separation, independent deposit review, balanced liability settlement, credit note, no fabricated provider refund, and evidence-preserving rollback. CI run: `32093892269`. |
| Service booking checkout runtime | `./scripts/test-service-booking-checkout-runtime-migration.sh` | Pass on PostgreSQL 17: rerun, inactive draft policy preservation, production-off domain gate, atomic resource hold, direct paid rejection, verified deposit confirmation separated from fulfillment, failed-provider-attempt expiry and resource release, reviewed manual evidence, independent approver, exact amount/currency binding, rollback refusal after reviewed evidence, follow-up rollback/reapply, and base rollback refusal after a canonical link |
| Public event ticket checkout runtime | `TDF_TICKET_PGURL=postgresql:///tdf_ticket_checkout_test_20260818_6 ./scripts/test-public-ticket-checkout-runtime-migration.sh` | Pass after current-main synchronization on disposable local PostgreSQL 16: rerun, clean rollback/reapply, inactive policy preservation, immutable checkout snapshot, later tier-price change without snapshot drift, paid-without-evidence and unpaid-issuance rejection, explicit issuance, unique issued audit under duplicate callback, atomic keyed-buyer rate counter, exact-once tier/promotion release, and evidence-aware rollback refusal. The local fixture database was dropped immediately after this run. |
| Domo quote/deposit checkout runtime | `TDF_DOMO_PGURL=postgresql:///tdf_domo_quote_test_20260818 ./scripts/test-domo-quote-checkout-runtime-migration.sh` | Pass on a disposable local PostgreSQL 16 database: clean rollback/reapply, inactive historical-rate preservation, immutable economic/timezone snapshot, exact approved-rate and `domo-quote:<quote-id>` provider binding, overlapping venue-window rejection, accepted-terms gate, paid-without-evidence rejection, exact-once verified deposit/date-reserved transition, consumed generic hold, and evidence-aware rollback refusal. The final disposable database was dropped immediately after the run. |
| Distribution accounting migration | `./scripts/test-distribution-accounting-migration.sh` | Pass: rollback, lifecycle, splits, package/evidence, royalty, separation of duties, payout gates |
| Versioned revenue products | `./scripts/test-versioned-revenue-products-migration.sh` | Pass: inactive legacy Domo rate, approval/immutability, production flag |
| Distribution pricing seeds | `./scripts/test-distribution-product-seeds-migration.sh` | Pass: 14 inactive bilingual seeds, activation/mutation/rollback gates |
| Backend focused tests | Stack-built `tdf-hq-test --match …` | Pass: 50 examples, including 800 property cases, zero failures |
| Provider-event backend invariants | Stack-built `tdf-hq-test --match 'service storefront commercial invariants'` | Pass: 19 examples, including 300 property cases, zero failures; immutable event metadata/provider/environment tampering, invalid replay reasons, and oversized PayPal request IDs are rejected |
| Marketplace rental backend invariants | Stack-built `tdf-hq-test --match 'marketplace rental'` | Pass: 5 examples, including 100 property cases, zero failures; inclusive dates, weekly pricing, separate deposits, overflow rejection, terminal-state closure and no skipped payment/handoff/inspection states |
| Marketplace customer-operation backend build | CI `bash scripts/quality-backend.sh` on GHC 9.10.3 | Pass: executable linked across 153 modules, test component linked across 171 modules, and 2,367 examples completed with zero failures. The earlier local clean retry was stopped before disk exhaustion from an unrelated worktree; CI run `32093892269` is the authoritative clean result. |
| Service booking backend invariants | Stack-built `tdf-hq-test --match 'service booking pricing and fulfillment invariants'` | Pass: 4 examples, including 100 property cases, zero failures; server minor-unit totals, duration/policy limits, overflow rejection, and no skipped deposit/fulfillment states |
| Backend test/build | `stack test --fast` with the default optimized Stack profile | Pass after current-main synchronization: 2,360 examples, zero failures; executable and all 169 test compilation units compiled and linked |
| Current public-ticket backend suite | `stack test --fast`; focused `--match=pricing` | Pass after current-main synchronization: 2,396 examples / 0 failures full suite and 27 / 0 focused; includes HMAC capability derivation, server pricing, fee allocation, tamper/overflow rejection, no issuance from browser/provider return, and terminal fulfillment properties |
| Current Domo backend suite | `stack test --fast`; focused `--match Domo` | Pass after the final current-main synchronization: 2,415 examples / 0 failures full suite and 5 / 0 focused, including 200 Domo property cases; approved server-rate arithmetic, input/rate limit rejection, overflow protection, deposit/fulfillment separation and terminal quote states |
| Web regression/accessibility | Jest: five changed suites | Pass: 16 tests, zero failures |
| Provider-event operator UI/access | Jest: provider-event page and access-control suites | Pass: 14 tests, zero failures; raw payload and merchant binding are absent from the UI contract |
| Marketplace web regressions | Jest: marketplace admin, API, Datafast return and storefront suites | Pass: 71 tests, zero failures; canonical payment is read-only in operations, fulfillment uses its dedicated transition API, one checkout key survives provider switching, lookup secrets stay in headers/session storage, and missing lookup/provider failure never clear the cart |
| Marketplace rental web regressions | Jest: marketplace storefront and rental-operations suites | Pass: 67 tests, zero failures; unapproved rentals fail closed, approved rentals require inclusive dates, and custody transfer requires an outbound condition report without sending a deposit deduction |
| Marketplace reviewed manual payment | Hspec validation plus Jest API/tracker/storefront suites | Pass: approve/reject input validation, scoped lookup header, idempotent evidence submission, truthful unpaid/submitted/review copy, protected finance projection, independent review controls, competing-rail exclusion, provider feature flag and expired-hold rejection |
| Marketplace customer-operation UI/API | Jest: marketplace API, public tracker and admin operations suites | Pass: 3 suites, 67 tests; lookup capabilities remain header-only, request creation is idempotent, persisted requests remain review states rather than success, extension approval remains quote-gated, reviewers are independent, and manual deposit routes never claim provider execution |
| Public booking web regressions | Jest: public booking, order-tracking and booking API contract suites | Pass: 27 tests, zero failures; lookup secrets stay in headers/session storage, Datafast browser return remains processing until server verification, PayPal approval calls server capture, manual evidence remains pending review, and only a server-paid checkout is described as verified |
| Public ticket web/API regressions | `npm test -- --runTestsByPath src/api/eventTickets.test.ts src/__tests__/PublicEventTicketsPage.test.tsx` | Pass: 2 suites / 6 tests; idempotency and lookup headers are exact, invalid identifiers fail locally, provider return remains processing, API failure cannot show success, and ticket codes appear only for server-returned paid plus issued state |
| Domo web/API regressions | `npm test --workspace=tdf-hq-ui -- --runInBand src/pages/DomoVenuePage.test.tsx src/__tests__/DomoQuoteCheckoutPage.test.tsx src/api/domoQuotes.test.ts` | Pass: 3 suites / 8 tests; no browser-authoritative price, failed quote creation invents no hold/payment, lookup capabilities stay in headers/local storage, browser return remains unpaid/date-held until server verification, failed verification fabricates no reservation, and a verified deposit remains separate from event completion/balance |
| Full web regression | `npm test --workspace=tdf-hq-ui` | Pass after current-main synchronization: 155 suites, 1,622 tests, zero failures |
| Current Domo full web regression | `npm test --workspace=tdf-hq-ui -- --silent` | Pass: 159 suites / 1,645 tests, zero failures. One new full-suite run first exposed only a five-second timeout in the Domo failure-path test; the unchanged assertion passed in isolation, its explicit ceiling was raised to 15 seconds for the media-heavy page, and the full suite then passed. |
| Web type safety | `npm run typecheck:ui` | Pass |
| Web production build | `npm run build --workspace=tdf-hq-ui` | Pass after current-main synchronization: 12,390 modules; bundle/secret gate 5 preloads and 407,515 gzip bytes |
| Current public-ticket web production build | `npm run build` in `tdf-hq-ui` | Pass after current-main synchronization: 12,392 modules; ticket page lazy chunk 15.75 kB / 5.99 kB gzip; initial bundle gate 5 preloads and 408,030 gzip bytes |
| Current Domo web production build | `npm run build --workspace=tdf-hq-ui` | Pass: 12,395 modules; Domo quote page lazy chunk 12.48 kB / 4.70 kB gzip; initial bundle gate 5 preloads and 408,449 gzip bytes |
| Current Domo bundle/token review | Production build secret gate plus targeted source/dist search for private-key, OpenAI-style, AWS-style, demo/admin bearer, and embedded-token patterns | Pass: no embedded credential value found. Remaining bearer construction is runtime OAuth/session behavior or admin documentation, and the Domo lookup capability is stored device-side and sent only in its scoped header. |
| Release/CI contracts | `npm run test:production-release`; `npm run test:ci-pipeline` | Pass: 43 + 15 tests |
| Prior registered production batch | Restore schema-only fixture plus three synthetic published Records rows; read-only preflight; render/apply twice; schema verification before and after the rerun on PostgreSQL 17 | Pass before current-main synchronization: all 49/49 then-registered migrations were recorded, the exact second run skipped all 49 entries idempotently, marketplace sales/rentals and reviewed manual methods remained enabled, Datafast/PayPal and service bookings remained disabled, active booking policies and provider bindings remained zero |
| Current merged migration manifest | JSON uniqueness/immutability contracts, dedicated migration rehearsals, and production release tests | Pass: 60 unique manifest entries including current-main's optional event-end, DDEX compatibility, automatic-migration, and profile-image host-compatibility work plus the public-ticket and Domo quote runtimes. The earlier aggregate snapshot evidence is revision-specific; a new aggregate 60-entry anonymized-snapshot apply remains required in staging. |
| OpenAPI/generated clients | `npm run generate:api` for web and mobile | Pass: canonical service-storefront, marketplace sale/rental, customer request/review, manual deposit settlement, booking/course/public-ticket checkout, Datafast/PayPal actions, manual-evidence submission, protected finance projection, independent review, and music-directory contracts generated for both clients; merged mobile contract committed at `5f67f61` |
| Current Domo OpenAPI/generated clients | `npm run generate:api`; web build/typecheck; mobile typecheck | Pass: public capability, quote creation/tracking/acceptance, Datafast create/status, and PayPal create/capture contracts generated identically for web and mobile; mobile native screens are not implemented in this slice |
| Mobile type safety | `npm run typecheck:mobile` | Pass |
| Mobile regression | `npm run test:mobile -- --runInBand` | Pass after current-main synchronization: 51 suites, 264 tests, zero failures |
| Feature-registry audit | `npm run audit:features` | Pass: 137 features, 156 web routes and 44 mobile routes; the secure Domo quote route is classified and its native web-only exception is explicit |
| Formal-method audit | `npm run verify:formal` | Pass: 8,550 findings scanned, 0 critical, 0 errors; 299 repository warnings remain advisory |
| Catalog authority audit | `npm run audit:catalog-lists` | Pass across 1,137 scanned files and 864 unique reviewed decisions, with no stale or unreviewed entries; Domo payment availability remains governed provider data while closed quote/fulfillment/actor state-machine discriminants remain technical protocol constants |

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

The marketplace reviewed-manual-payment follow-up closes the remaining public bank-transfer dead
end for equipment sales and rentals. A buyer can submit a reference only through the scoped guest
lookup capability; the public tracker continues to say unpaid while evidence awaits review. The
protected marketplace operations screen exposes the canonical finance projection and permits
approve/reject only through the Invoicing boundary and an independent reviewer. Approval is refused
after the asset hold expires or when another payment already won, and those conflicts remain
reconciliation work rather than fabricated payment. After that earlier synchronization, local
verification passed 2,360 backend examples, 155 web suites/1,622 tests, 51 mobile suites/264 tests,
the three marketplace/manual database rehearsals, the event-research and music-directory migration
rehearsals, production UI build, feature-registry audit, release/CI contracts, formal audit and
catalog-authority audit. The current merged manifest contains 60 unique immutable entries; the
earlier 49-entry aggregate apply evidence remains valid for that exact revision, while an aggregate
60-entry snapshot apply is explicitly still required in staging. No external provider call, manual
payment approval, inventory handoff, deposit refund, staging deployment or production mutation
occurred.

The marketplace customer-operations follow-up passed its isolated PostgreSQL 16 rehearsal and 67
focused web tests across the public tracker, API boundary and protected operations page. Buyers can
now submit scoped cancellation, return, rental-extension and dispute requests without changing
payment or fulfillment state themselves. Staff review is constrained by the formal domain state
machine; an extension can only move to `needs_quote` until atomic availability, a versioned quote
and a payable change order exist. Rental security-deposit settlement is manual and dual-controlled:
the submitter records immutable bank-transfer, cash, POS or forfeiture evidence, a different staff
member verifies it, and the database posts a balanced liability release plus a manual credit note.
It does not create a provider refund or say that Datafast or PayPal moved funds. Historical paid
deposits without a canonical liability entry are reported for reclassification rather than silently
backfilled. No customer request was reviewed, deposit was settled, provider was called, or external
state was changed during this fixture exercise.

The 2026-08-18 public-ticket follow-up adds a bilingual guest storefront and secure tracker for an
existing public event detail. The server requires a secret-manager-backed lookup capability,
approved immutable event policy, atomic event/tier/promotion hold, shared Datafast/PayPal binding,
and verified canonical payment before separately issuing tickets. Organizer proceeds remain a
payable liability and settlement is disabled. The synchronized full backend suite passed 2,396
examples, the targeted web/API suites passed six tests, both OpenAPI clients regenerated, all 53
mobile suites/275 tests and mobile type checking passed, the production web build passed, and the
feature audit covered 136 features/155 web routes. The
dedicated PostgreSQL 16 rehearsal used only synthetic data and its disposable database was removed.
No policy was activated, provider was contacted, payment captured, ticket issued, organizer settled,
staging deployment performed, or production state changed.

The guest buyer email is normalized and snapshotted, but this slice does not independently verify
mailbox ownership and has no verified-email recovery path. Production public-ticket activation
therefore remains blocked even after provider sandbox verification until those controls are tested.

The 2026-08-18 Domo follow-up preserves the historical browser formula as an inactive draft and
adds a canonical quote/deposit runtime without approving that formula. Quote creation snapshots the
exact reviewed active rate card, lines, venue timezone, policy, terms and customer/date input while atomically
holding the venue window. Acceptance opens only the deposit checkout; verified Datafast/PayPal
evidence may reserve the date but cannot complete the event or pay the balance. The local migration
rehearsal, complete 2,415-example backend suite, complete 159-suite/1,645-test web regression,
eight focused web/API tests, mobile typecheck and
production web build passed. No rate card was approved, flag enabled, provider contacted, deposit
captured, date reserved, customer notified, staging deployment performed, or production data
changed. The admin comparison/approval screen, verified-email recovery, change orders, balances,
cancellations/refunds and a credentialed sandbox exercise remain explicit blockers.

## Required staging exercise

1. Restore an anonymized database snapshot and run all migration preflight/apply/verification steps;
   record row counts, conflicts and the `legacy_unreconciled` report. Do not infer payment status.
2. Install provider sandbox secrets through the secret manager; verify redaction and kill switches.
3. Exercise duplicate create/callback/capture, signature failure, reordered/delayed events, declined,
   abandoned, outage and full/partial refund paths. Reconcile provider/internal/ledger totals to zero
   unexplained variance.
   For the owned-event ticket pilot also prove event/tier contention, promotion exhaustion, hold
   expiry, late verified payment, duplicate issuance suppression, customer-safe tracking, QR
   issuance only after payment, and organizer-payable totals. Install and rotation-plan
   `COMMERCE_LOOKUP_TOKEN_SECRET`; keep `commerce.event_ticket_settlements` disabled.
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
