# Known-ID held-refund recovery: implementation and verification

Date: 2026-09-16 UTC. Bounded continuation, not completion of the payment platform.
Dependency: [root draft #401](https://github.com/diegueins680/tdf-app/pull/401).
Decision: [ADR 0130](../adr/0130-known-id-refund-query-recovery.md).

## Access and integration checkpoint

Worktree: `/Users/diegosaa/GitHub/tdf-app-payment-checkout`; original dirty worktree
untouched. Root branch: `codex/payment-held-refund-query-20260916`, from
`66fa88e90c80866e0090761addfabd9f1e817cc2`. Latest fetched default `main`,
`7eafd3f479817f267ba0de6592c3e16ea920f4ae`, is already an ancestor.
Workflow doctor: 16 OK, two missing daily-memory warnings, zero errors.

Accessible payment PR/branch metadata was inspected before implementation. #331
advanced to `19af56e441e51c2ca9a378b967f07872d4d2c993`: reviewed the recurring
completion capability gate, exact-environment operator evidence filter, tests and
documentation. They do not implement this refund lookup and are not integrated
here. Preserve that work in the eventual stack-integration PR. Mobile #83 remains
on an older base than #82; do not replace #82's generated recovery/evidence types.

Parent #401 checks observed during this work: build, catalog audit, repo quality,
persona web E2E, API contracts and production-migration validation succeeded;
backend quality was still running. Other change-filtered checks were skipped.
These are parent results, not evidence for this branch.

GitHub reads/fetch, local branches and commits, official documentation access and
local build/test tools are available. Remote delivery is recorded below only when
verified. No staging deployment, provider HTTP request or real transaction was run.

Fresh read-only staging observation: **2026-09-16T17:12:20.897Z**, using the inspector
from staging branch `0081b6b03bd58716090c57d936f31a364e944522`. Imported and invoked
`inspectStaging()` and `inspectKoyeb({token: process.env.KOYEB_API_TOKEN})` using
`node --input-type=module -e`; printed only their fixed redacted metadata projection,
without replacing the staging worktree's existing artifact. Both fixed app health
endpoints returned HTTP 200/status OK (API DB OK; web does not report DB OK).
All six status/config/secret-name reads reported hosting authentication unavailable;
deployed source SHA remains unknown and provider qualification false. Alternative
hosting token was not configured. The wrapper exited 0 because importing the
inspector does not run its CLI failure-exit logic; the explicit qualification
result is **blocked**, not a successful staging payment test. Health alone does not
authorize payment tests. Missing access aliases remain `FLY_STAGING_API_TOKEN` and
`FLY_STAGING_WEB_TOKEN`, or an authorized Fly login for the existing staging apps.
No production credentials were borrowed and no replacement hosting was created.

## Implementation and discovered defects

- PayPal GET-refund adapter uses the stable adapter request/transport contract,
  fixed environment origins and original immutable IDs. Amount parsing covers the
  whole positive `Int64` range without floating-point arithmetic.
- Recovery core validates approval, account, capability evidence and flags; commits
  audit admission before the lookup; shares durable query quota; rechecks authority
  before exact completion through the existing atomic `RefundStore` path.
- Strict-admin GET readiness and POST lookup are documented in canonical OpenAPI.
  Web and mobile clients were generated from it, not hand-edited. This increment
  adds a web admin panel, not a native mobile administrator screen.
- The panel provides Spanish/English copy, exact money, explicit query action,
  double-submit protection, response binding checks, stale-result clearing and
  accessible status/error messages. No provider payload/credential is displayed.
- Initial DB verification found a real implementation mistake: the new binding
  query referred to nonexistent `commerce_provider_binding.checkout_id`. The
  schema instead relates binding -> payment attempt -> checkout. Corrected the
  query while retaining the original attempt and checkout restrictions.

## Source-backed scope and limits

The [official PayPal refund lookup](https://developer.paypal.com/api/payments/v2/refunds-get)
was read on 2026-09-16 (high confidence for published API). It documents OAuth,
known-ID lookup and optional consented merchant assertion. Tests use the published
10.99 USD response structure with sanitized IDs/data; no published example token
was copied into code or used. The documentation is not proof of TDF's merchant
approval, Ecuadorian onboarding, USD withdrawal route or sandbox entitlement.
This narrow review does not refresh or replace the full
[market research](ecuador-payment-platform-audit-2026-09-11.md).

Only existing PayPal mixing/mastering refunds with known IDs and independent
approval are eligible. Unknown-ID timeouts, other domains/providers, legacy failed
records and canonical drift remain held for review. No new provider refund is
created. Provider fee/tax/seller allocations and SRI issuance are not fabricated.

## Verification evidence

Environment: local macOS; Node 24.8.0, PostgreSQL 16.10; Stack `lts-24.42`, GHC
9.10.3. DB tests use only synthetic fixtures in Unix-socket-only disposable
`tdf_provider_retry_test` databases. Query callbacks are injected; none contacted
PayPal. No sandbox, staging, browser/device manual verification or screenshot is
claimed. Full results and exact source hashes are recorded before delivery.

Source checkpoints:

- Baseline: `66fa88e90c80866e0090761addfabd9f1e817cc2`.
- First adapter/core implementation: `b783a9ade`.
- Redacted test diagnostics: `81ed3fd74`.
- Corrected binding, bounded transactions and admin boundary tests: `eb25affda`.
- Web/OpenAPI/source freeze: `7729b00fc`.
- Explicit authentication/error contract and tests: `12439478e`.
- Mobile generated contract: `b9222df77d0a187cca4359278704362ec17fb3af`
  (initial generation `e401f21`; parent `33720ef45b0565005c4b54b0e0c106cc93831613`, #82).

Development checks (not final qualification): baseline refund-safety tests passed
5 examples with two 100-case properties. First new adapter compile failed because
the test attempted to print a deliberately non-Show credential-bearing request;
tests now compare a Boolean and the recovery view's Show instance is redacted.
The fixed adapter passed 31 examples and 100 full-range generated money cases.
Initial real DB suite at `81ed3fd74`: 246 examples, 11 failures, seed 1473352766,
23.2198s; log `/private/tmp/tdf-held-refund-db-20260916.log`. The nonexistent-column
bug above caused the failures; this was not an intentionally failing test run.
A development rerun without escalation could not access the local Unix socket
(operation not permitted): 46 examples, one setup failure, 14 pending; no provider
was involved. It was retried with approval. Interim runs while sources changed
are development diagnostics, not final source qualification.

The strict catalog gate initially reported two unreviewed fingerprints and one
stale fingerprint. Individually reviewed the changed ServiceStorefront HTTP method
registry and the two-constructor internal `RefundQueryOutcome` parser result as
technical constants, replacing the stale method-registry entry. Persisted payment
states/permissions remain authoritative. No gate was disabled or policy weakened;
both generated inventory formats must pass strict verification before delivery.

Commands below ran from the root unless marked `tdf-hq`. Saved-log pipelines used
`set -o pipefail; COMMAND 2>&1 | tee LOG | tail -N`; the tail bounds console output,
not the saved evidence. UTC timestamps are completed-log modification times.

| Command | Completed UTC | Result / saved log |
| --- | --- | --- |
| `stack test --fast --rerun-tests --test-arguments='--match=refund-safety'`, baseline, `tdf-hq` | 16:29:43Z | 5 examples, 0 failures, two 100-case properties; exit 0; `/private/tmp/tdf-held-refund-baseline-20260916.log` |
| `stack test --fast --rerun-tests --test-arguments='--match=held-refund'`, `81ed3fd74`, `tdf-hq`, no DB configured | 16:45:26Z | 31 examples, 0 failures, 100 full-range money cases; exit 0; `/private/tmp/tdf-held-refund-unit-fixed-20260916.log` |
| `npm run test --workspace=tdf-hq-ui -- --runInBand HeldRefundRecoveryPanel CommerceProviderEventsPage serviceStorefront.test` | 17:01:13Z | 26 tests, 3 suites passed, 53.719s; exit 0; source later committed as `7729b00fc`; `/private/tmp/tdf-held-refund-web-final-20260916.log` |
| `npm run typecheck:ui` | 16:54:27Z | Passed, exit 0; in-progress source, before explicit error-contract generation; `/private/tmp/tdf-held-refund-web-typecheck-20260916.log` |
| `npm run quality:repo`, `7729b00fc` | 17:05:45Z | Passed, exit 0; before the two additional contract tests; `/private/tmp/tdf-held-refund-repo-quality-20260916.log` |
| `node --test scripts/__tests__/payment-query-report-contract.test.mjs`, source committed as `12439478e` | 17:12:51Z | 8 tests passed, 5.399s, exit 0; `/private/tmp/tdf-held-refund-contract-20260916.log` |

Full backend/real-DB, mobile, full web, final type checks and catalog report results
are pending. An ongoing full mobile run has already reported a five-second timeout
in the unchanged `TicketCheckout` guest-auth navigation test; it is not treated as
passing or hidden by increasing the timeout. Final outcomes must be appended.

Remote draft delivery is pending verification. No review is requested yet.

## Configuration and sandbox runbook

1. Keep `PAYPAL_REFUND_RECONCILIATION_ENABLED=false` (new `.env.example` default).
   Absence is also disabled. Do not change the charge or refund execution flags.
2. An authorized operator may register the new flag disabled, per environment:

   ```sql
   INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason)
   VALUES ('checkout.paypal.refund_reconciliation',false,'sandbox',
           'Awaiting merchant refund lookup qualification')
   ON CONFLICT(flag_key,environment) DO NOTHING;
   ```

   This is an operator instruction, not an executed staging/production write. No
   migration is required. Do not overwrite an existing operator policy.
3. Use approved environment secret management for `PAYPAL_CLIENT_ID`,
   `PAYPAL_CLIENT_SECRET`, `PAYPAL_MERCHANT_ID`; explicitly set `PAYPAL_ENV=sandbox`.
   Only check presence/validation status; never paste values into code or logs.
4. Confirm the same merchant's USD settlement/contract and API refund-query
   permission. Record dated evidence on its `commerce_provider_account` and
   `commerce_provider_capability` rows. Required wallet capabilities:
   `server_verification`, `full_refund`, `partial_refund`; all must be verified for
   that environment. A documented claim or production evidence does not qualify a
   sandbox row, nor does sandbox evidence qualify production.
5. With authorized hosting, deploy the reviewed stack only to the existing
   staging environment, identify its exact source commit and run migration checks.
   Separately authorize/qualify synthetic merchant sandbox refunds. Only after
   qualification enable the sandbox DB flag and process switch. Production
   activation remains a separate human decision outside this task's authority.
6. In the commerce provider-events admin page, enter an internal refund UUID,
   inspect local readiness, confirm Sandbox and exact USD amount, then choose
   Check original refund. GET readiness has no provider-side action. POST lookup
   can commit local completion, but never creates another refund.
7. Verify the original refund/capture IDs, exact amount/currency, single ledger
   refund/credit-note record, canonical intent/checkout totals and audit outcome.
   A second command must not create a second financial record. Test nonterminal,
   malformed, error, duplicate/concurrent and revoked-authority cases separately.
8. Retain only sanitized evidence and query correlation; never raw bearer tokens,
   buyer details or provider response payloads. Record environment, commit, UTC
   time and real sandbox outcome separately from mocked/local evidence.

## Operations, rollback and unresolved items

For a timeout, 429, 404, rejected payload or audit/database error, keep the original
reservation. Respect the shared query cooldown (429 includes Retry-After 10).
Review immutable `refund_query` admission/outcome audits. An admission without an
outcome may indicate a crash; query the same known ID later after reconciliation.
Never approve/reissue a new refund to resolve an ambiguous result.

If completion evidence conflicts with a dispute or accounting totals, stop manual
financial changes and escalate to accounting with original IDs and sanitized
correlation. Do not mark historical failed refunds succeeded or decrement totals
by hand. App credit-note records do not replace SRI-authorized documents.

Rollback: disable this process switch and exact-environment DB flag, drain
in-flight requests, preserve all refunds/ledger/history/audits and deploy the last
reviewed compatible binary. Do not delete used query queues or financial data.
Legacy drift requires an explicit reviewed backfill, not a rollback workaround.

Still blocked/unverified: merchant account/secret qualification and webhook
registration, full sandbox and staging refund tests, unknown-ID discovery,
legacy-failed resolution, other-domain/provider refund adapters, seller liability
and fee/tax allocations, automatic settlement matching, and qualified Ecuadorian
legal/accounting/PCI review before activation. Existing payment links, recurring
mandates, marketplace custody and all-provider coverage are not declared done by
this increment. See the inherited [operator runbooks](operator-runbooks.md).
