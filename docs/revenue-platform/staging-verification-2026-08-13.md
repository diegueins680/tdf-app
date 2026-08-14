# Staging verification record

Date: 2026-08-14

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
| Distribution accounting migration | `./scripts/test-distribution-accounting-migration.sh` | Pass: rollback, lifecycle, splits, package/evidence, royalty, separation of duties, payout gates |
| Versioned revenue products | `./scripts/test-versioned-revenue-products-migration.sh` | Pass: inactive legacy Domo rate, approval/immutability, production flag |
| Distribution pricing seeds | `./scripts/test-distribution-product-seeds-migration.sh` | Pass: 14 inactive bilingual seeds, activation/mutation/rollback gates |
| Backend focused tests | Stack-built `tdf-hq-test --match …` | Pass: 50 examples, including 800 property cases, zero failures |
| Backend build | `stack test --no-run-tests` with the default optimized Stack profile | Pass: executable and all 155 test modules compiled and linked |
| Web regression/accessibility | Jest: five changed suites | Pass: 16 tests, zero failures |
| Web type safety | `npm run typecheck:ui` | Pass |
| Web production build | `npm run build` | Pass: 12,382 modules; bundle/secret gate 5 preloads and 399,972 gzip bytes |
| Release/CI contracts | `npm run test:production-release`; `npm run test:ci-pipeline` | Pass: 25 + 12 tests |
| Registered production batch | Render preflight; apply twice; schema verification against PostgreSQL 17 | Pass: 23/23 migrations, second run idempotent, service checkout/event/refund runtime contracts verified |
| OpenAPI/generated clients | `npm run generate:api` | Pass: canonical service-storefront contract generated for web and mobile |
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
batch applied twice in fresh PostgreSQL 17 containers. All 23 manifest entries were recorded, the
release schema verifier passed, checkout/refund production flags remained disabled, and the legacy
classification view contained no invented rows in the pristine fixture.

The 2026-08-14 PayPal event/refund follow-up passed 19 focused examples, including 400 property
cases, with zero failures. The executable and all 155 test modules compiled under the repository's
default optimized Stack configuration. No PayPal or Datafast network request was made; the provider
sandbox evidence requirement remains open.

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
