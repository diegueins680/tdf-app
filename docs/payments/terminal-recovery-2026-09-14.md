# Exact payment response recovery — 2026-09-14

This continuation depends on draft [#340](https://github.com/diegueins680/tdf-app/pull/340),
parent `b8c0e38cdd365e7f977d5cf15f93c164ebfc2149`. It repairs recovery of an already contacted
PlaceToPay/PayPhone operation; it does not activate a provider or complete the payment platform.

## Access and baseline

The isolated `tdf-app-payment-checkout` worktree started clean. `git fetch origin` confirmed
default `main` remained `17a33eca11d585d84435af85340beece9b51d14e`, already an ancestor of this
dependent stack. Existing payment PRs #331, #332, #334 and #340 were retained. The new event
replay PR #341 and snapshot-privacy PR #342 were inspected; their changes concern event
authorization, snapshots and formal models, not provider session execution. #342 also changes
the shared backend test registry/build manifest; this recovery patch does not change those
files. No pending payment implementation was copied over. The original dirty worktree and
its mobile submodule were not changed.

GitHub read access, local branch creation and commits were exercised. On #340, backend job
[104163606851](https://github.com/diegueins680/tdf-app/actions/runs/34900032832/job/104163606851)
was still building/testing at 22:31 UTC, then passed at 22:47:06 UTC. Overall quality passed
at 22:47:11 UTC. Those checks validate the parent, not this continuation's SHA.
One GitHub API request failed to connect in the sandbox; its approved retry succeeded.

No provider credentials or account activation were verified. The user subsequently authorized
finding or setting up staging. Both existing Fly endpoints passed real HTTPS health checks:
`tdf-hq-studio-audit-staging.fly.dev` returned database/status OK and
`tdf-studio-audit-staging-web.fly.dev` returned status OK. No payment flow or deployment was
performed. Local Fly has no usable login. GitHub's stored Fly token was exercised inside a
fixed-target, redacted inspection workflow and was denied access to both staging apps; this
does not prove the token is expired. The existing Koyeb token was also exercised through a
read-only app-list request and returned HTTP 401. The sanitized staging inspection evidence
is in [run 34907233146](https://github.com/diegueins680/tdf-app/actions/runs/34907233146),
generated at 23:07:20.546 UTC. Public provider documentation was accessible. Existing
contract, sandbox, legal/accounting and PCI dependencies remain in the parent audit/runbooks.

## Implemented behavior and security boundary

The create handler previously rejected a paid or expired checkout, or unavailable provider,
before looking for its original operation. A browser that lost the first response had no
attempt ID and therefore could not use the otherwise durable GET endpoint.

1. Validate request headers, checkout UUID, provider and method.
2. Authenticate the checkout lookup token and find the exact provider/idempotency operation.
   Check its attempt/intent/checkout bindings, environment, original merchant account, amount,
   currency, automatic capture mode, selected method and request fingerprint. Contact fields
   are normalized exactly as for creation. Preserve the original reference and fingerprint
   byte format, including older checkout-derived references.
3. For a previously contacted operation, decrypt and return its durable state without a
   provider call, claim, account-readiness check or financial mutation. Checkout expiry and
   terminal status do not revoke this read capability; replacing the lookup-token hash does.
4. For no operation, or a merely `prepared` operation, retain every current new-contact gate:
   payable/unexpired checkout, exact capabilities, ready account, provider secrets and routing.
   A prepared operation on an expired checkout is not silently started or cancelled.

`COMMERCE_EVENT_ENCRYPTION_KEY` remains necessary; recovery does not bypass encryption or
invent a redirect. Requests with changed immutable fields fail closed. A stored create
outcome describes that original operation: it is not the current refundable balance,
settlement state, ticket fulfillment or service delivery status.

The web component now restores pending/known attempts independently of current method
offerings, presents an explicit original-payment recovery action, freezes original PayPhone
contact fields, and never generates a replacement key during recovery. A missing key or
failed status lookup does not prove no charge; the safety lock remains. Late results from a
different checkout scope are ignored. Paid/terminal ticket, course, booking, mixing/mastering
and Domo order pages keep recovery mounted while withholding new-payment methods.
The existing mobile paid-ticket web handoff uses this same surface. Mobile generated contract
comments were regenerated with the web client and pushed in mobile draft
[#79](https://github.com/diegueins680/TDF-mobile/pull/79), commit
`9c86c459081b32b47579b9c95c8514029225ad75`, dependent on mobile #78. No native runtime change
or device test is claimed in this patch.

## Compatibility and rollback

No database migration, backfill or historical reference rewrite is introduced. The existing
POST/GET routes and JSON shapes are unchanged; OpenAPI descriptions and generated web/mobile
type comments were updated with `npm run generate:api:ui` and
`npm --prefix tdf-mobile run generate:api`. `cmp` confirmed the two generated files agree.

Deploy the backend before the dependent UI. Roll back this UI first if necessary. Reverting
the backend restores the terminal-replay limitation, so retain the original operation and
use its authorized GET/operator reconciliation rather than creating a replacement payment.
Do not clear ambiguous browser markers as a rollback procedure. The parent
[writer-cutover restrictions](retry-reconciliation-safety-2026-09-14.md#compatibility-and-rollback)
still apply. No deployment or rollback was performed.

## Verification record

Local environment: macOS; Stack/GHC 9.10.3; disposable PostgreSQL 16; Jest/jsdom with mocked
API responses. All database merchant approvals, operations, contacts and outcomes are
synthetic. Servant handlers are invoked against the actual PostgreSQL store; this is not a
credentialed provider sandbox or an end-to-end HTTP/provider test.

| Command | Source and timestamp evidence (UTC) | Result |
|---|---|---|
| `cd tdf-hq && stack test --fast --test-arguments='--match=provider'` | Baseline parent `b8c0e38cd`; started 22:19:53 | 85 examples, 0 failures. |
| Same focused command after recovery implementation | Started 22:25:13; completed observed by 22:36:45; backend source `1e1309347` | 85 examples, 0 failures; executable and tests compiled. |
| `npm --prefix tdf-hq-ui test -- --runTestsByPath src/api/providerPaymentSessions.test.ts src/utils/providerPaymentResume.test.ts src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | Intermediate working tree; started 22:26:58 | 4 suites / 23 tests passed, 69.483 seconds. |
| Same command with `src/pages/CourseProductionLandingPage.test.tsx` appended | UI source `4f65fb9cc`; started 22:35:35 | 5 suites / 28 tests passed, 47.872 seconds. |
| `npm --prefix tdf-hq-ui run typecheck` | Intermediate working tree; completion observed by 22:36 UTC | Exit 0. |
| `npm run generate:api:ui` | OpenAPI description-only change | Exit 0; two generated comment lines changed. |
| `sh scripts/test-provider-retry-runtime.sh` | First invocation at 22:36:45 | Docker access denied, exit 126; no database test ran. Approved isolated reruns are recorded below. |
| Same PostgreSQL command, approved | Backend source before `b83299e27`; result observed 22:44 UTC | 27 examples, 17 failures: the new query incorrectly referenced `intent.environment`, which is inherited through checkout rather than stored on the intent. Existing nine retry tests and missing-key test passed. |
| Same PostgreSQL command after schema correction | Backend source `b83299e271518c0a4df7a6cac2f3861adb881e63`; complete observed by 23:01:43 | 27 examples, 0 failures; 19.8779 seconds. All existing assertions retained. |
| `npm --prefix tdf-hq-ui run build` | Started 22:37:42 | Failed with TS7030 for the new recovery effect's incomplete cleanup return paths. Commit `af759ee65` makes those returns explicit. |
| `npm run audit:catalog-lists`; `npm run test:catalog-list-audit` | This continuation's source tree; complete observed 22:44 UTC | Both exit 0; catalog audit unit test passed. |
| `npm --prefix tdf-hq-ui run lint` | Final UI implementation; complete observed by 23:10 UTC | Exit 0, full web lint with existing zero-warning gate. |
| `npm --prefix tdf-hq-ui run build` after cleanup fix | UI source `af759ee65`; complete observed by 23:10 UTC | TypeScript/Vite build completed; Vite reported 3m23s and the bundle guard reported 5 preloads / 378238 gzip bytes. Aggregated terminal output truncated the process-status header; the build's successful final output was retained. |
| `node tdf-hq-ui/scripts/check-initial-bundle.mjs` | At 23:09:59 UTC | Exit 0; 5 preloads / 378238 gzip bytes. |
| `cd tdf-hq && stack test --fast` | Started 23:02:13 UTC; complete observed by 23:10 UTC | Exit 0. Verbose SQL output truncated the example count; the explicit rerun below preserves that summary. |
| `cd tdf-hq && stack test --fast --rerun-tests 2>&1 \| tail -n 45` | `b2a7710e33be4aad0eec0df3f2b6005f360c31ba`; started 23:14:51 UTC; last test log 23:15:57 UTC | Stack explicitly reported the test suite passed: 2556 examples, 0 failures, 37.9766 seconds. The pipeline status alone is not used as evidence of Stack success. |
| Five-suite focused web command above, repeated on final source | `b2a7710e33be4aad0eec0df3f2b6005f360c31ba`; started 23:14:51 UTC | Exit 0; 5 suites / 28 tests passed, 57.928 seconds. |
| `npm run generate:api:ui`; `npm --prefix tdf-mobile run generate:api`; `cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts` | Final OpenAPI source in `b2a7710e33be4aad0eec0df3f2b6005f360c31ba` | Generators and comparison passed; mobile source committed separately as noted above. |
| `docker ps -a --filter name=tdf-provider-retry --format '{{.Names}} {{.Status}}'` | Observed after the final PostgreSQL run, by 23:18 UTC | Exit 0 with no rows: disposable retry-test containers had been removed. |

Parent #340 backend-quality and overall quality checks subsequently passed at 22:47:06 and
22:47:11 respectively. They validate that parent SHA, not this continuation's changes.

The full backend, PostgreSQL, focused web, lint, catalog audit and web build checks above are
complete. Full web Jest, native-device tests, provider sandbox transactions and staging
payment/deployment tests were not executed in this continuation. Exact start/end timestamps
were not emitted by every invocation; observed completion bounds are labeled as such, not
fabricated provider timestamps. Existing compiler warnings were not suppressed.

## Source evidence and remaining work

Access date **2026-09-14**, high confidence for the published endpoint semantics only:
[PlaceToPay session API](https://docs.placetopay.dev/en/checkout/api/reference/session/)
separates session creation from authenticated session lookup;
[PayPhone API Sale](https://docs.payphone.app/api-sale) provides individual transaction
references and transaction queries. TDF's read-only replay is a local architecture decision,
not a claim of provider-side idempotency or successful merchant onboarding.

If the original browser key/token is missing or its existing tab recovery record has expired,
do not create a replacement. Recover through the original order's private capability or
authorized operator reconciliation. Browser records remain tab-scoped with their existing
24-hour retention; cross-device private-capability recovery is not added here. Prepared but
uncontacted operations that can no longer pass new-contact gates need an explicit reviewed
operator resolution; this patch does not invent automatic financial evidence for them.

Outstanding platform work includes mixed-tax line allocation; Datafast/PayPal transport
consolidation; recurring mandates and saved-method removal; payment links; operational
refund/void/capture interfaces; settlement/dispute ingestion; SRI invoice/credit-note workflows;
and compliant seller onboarding/payouts. Provider-managed marketplace capabilities remain
contract-blocked. Raw notification-field minimization is also a remaining hardening item:
the current encrypted inbox retains accepted notification payloads, not only allowlisted
fields. Encryption alone is not proof that arbitrary callback fields contain no prohibited
data. No claim of legal, accounting, regulatory or PCI certification is made.
