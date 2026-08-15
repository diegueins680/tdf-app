# Staging verification record

Date: 2026-08-15

Environment tested: local isolated worktree and disposable PostgreSQL 16 containers.

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
| Distribution accounting migration | `./scripts/test-distribution-accounting-migration.sh` | Pass: rollback, lifecycle, splits, package/evidence, royalty, separation of duties, payout gates |
| Versioned revenue products | `./scripts/test-versioned-revenue-products-migration.sh` | Pass: inactive legacy Domo rate, approval/immutability, production flag |
| Distribution pricing seeds | `./scripts/test-distribution-product-seeds-migration.sh` | Pass: 14 inactive bilingual seeds, activation/mutation/rollback gates |
| Backend focused tests | Stack-built `tdf-hq-test --match …` | Pass: 50 examples, including 800 property cases, zero failures |
| Provider-event backend invariants | Stack-built `tdf-hq-test --match 'service storefront commercial invariants'` | Pass: 18 examples, including 300 property cases, zero failures; immutable event metadata/provider/environment tampering and invalid replay reasons are rejected |
| Backend build | `stack test --fast --no-run-tests` with the default optimized Stack profile | Pass: executable and all 159 test modules compiled and linked |
| Web regression/accessibility | Jest: five changed suites | Pass: 16 tests, zero failures |
| Provider-event operator UI/access | Jest: provider-event page and access-control suites | Pass: 14 tests, zero failures; raw payload and merchant binding are absent from the UI contract |
| Marketplace web regressions | Jest: marketplace API, Datafast return and storefront suites | Pass: 14 tests, zero failures; one checkout key survives provider switching, lookup secrets stay in headers/session storage, missing lookup and provider failure never clear the cart, rental sale checkout is disabled |
| Web type safety | `npm run typecheck:ui` | Pass |
| Web production build | `npm run build --workspace=tdf-hq-ui` | Pass: 12,383 modules; bundle/secret gate 5 preloads and 403,425 gzip bytes |
| Release/CI contracts | `npm run test:production-release`; `npm run test:ci-pipeline` | Pass: 25 + 12 tests |
| Registered production batch | Render preflight; apply twice; raw idempotency reruns; schema verification against PostgreSQL 17 | Pass: 24/24 migrations, second run idempotent, provider-event action table present, worker flags `sandbox=true` and `production=false` |
| OpenAPI/generated clients | `npm run generate:api` for web and mobile | Pass: canonical service-storefront and marketplace-sale contracts generated for both clients |
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
batch applied twice in fresh PostgreSQL 17 containers. All 24 manifest entries were recorded, the
release schema verifier passed, checkout/refund production flags remained disabled, and the legacy
classification view contained no invented rows in the pristine fixture.

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
passed the dedicated PostgreSQL 17 migration rehearsal, and passed 14 focused web tests. The
OpenAPI document now includes public sale checkout and secure tracking plus authenticated
fulfillment operations; generated web/mobile artifacts were refreshed. This remains local fixture
evidence only. No Datafast or PayPal network request, customer charge, provider refund, carrier
handoff, staging deployment, or production state was exercised.

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
