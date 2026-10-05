# Payment retry and reconciliation safety — 2026-09-14

Dependent implementation after draft #334 (`3b81b687753520c6b56a6128d6c110846f1a8995`).
This is a code/security repair, not provider certification or production activation.
The default branch remained `17a33eca11d585d84435af85340beece9b51d14e` when continuation began.
The original dirty worktree was not changed. Work uses the isolated payment-checkout worktree.

## Audit findings and implemented controls

| Finding | Repair and verification boundary |
|---|---|
| The canonical intent key included checkout/provider but not the payment request. A second create key could reuse the same active intent and insert another attempt. A genuine retry could also inherit a terminal failed intent. | Lock the checkout before replay/creation; replay only an exact existing attempt; derive new intent keys from an unambiguous JSON tuple containing checkout, environment, provider, operation and idempotency key. The existing active-intent database constraint rejects a second unresolved payment. PostgreSQL concurrency tests exercise the actual store functions. |
| PlaceToPay/PayPhone references were derived from the checkout, making separate attempts indistinguishable to the provider. | Derive new references from the canonical attempt UUID. Replays first load an existing operation's immutable reference, preserving pre-upgrade references/fingerprints. No historical backfill is performed. |
| Legacy PayPal captures need a separate attempt on the original intent. | A capture can continue a matching active, externally bound create/authorize intent. A changed capture key is rejected once a capture attempt exists; the original key remains replayable. This does not implement multi-capture. |
| PlaceToPay reconciliation checked request totals and the existence of an approved status, but not actual approved transaction money or reversal evidence. | Require exactly one complete approval, positive internal transaction ID, matching merchant reference, exact minor-unit USD original/converted totals, and `refunded=false`. Reject unsupported partial, recurring, subscription, dispersion and check-in/autopay request types. Reject missing/duplicate approvals. Contradictory session/transaction statuses cannot produce no-charge certainty. |
| PlaceToPay cancellation parsing accepted outer `OK` without the returned session. | Require the nested session to match its immutable binding and contain no-charge evidence. Cancel remains an unexposed adapter contract, not a new customer/admin cancellation API. |
| Datafast mixing/mastering status confirmation created a `capture` attempt despite performing only a GET of the original DB sale. | Success and failure branches now reuse the original create attempt, as ticket/course status verification already does. No delayed-capture capability is added. |
| Adding non-negative Int64 components could overflow and accidentally match a small positive total. | Sum in unbounded integer arithmetic before comparison. Regression uses two maximum Int64 components and a tax component. |

All mutations still require the existing environment, contract, credential and method-capability gates. No flags, credentials or merchant accounts were activated.

## Official specification evidence

Access date for every row: **2026-09-14**. Confidence: **high** for the published contract; merchant-specific behavior remains unverified.

| Primary source | Relevant verified fact and local interpretation |
|---|---|
| [PayPhone API Sale](https://docs.payphone.app/api-sale) | `clientTransactionId` identifies an individual transaction; monetary fields use integer cents. A fresh canonical attempt now receives a distinct reference. |
| [PlaceToPay session API](https://docs.placetopay.dev/en/checkout/api/reference/session/) | Session responses separately contain requested payment details and transaction entries with `internalReference`, `reference`, converted amounts and `refunded`. Cancellation returns a nested session. TDF validates these fields instead of inferring payment from the requested total. |
| [PlaceToPay session flows](https://docs.placetopay.dev/en/checkout/create-session/) | Partial sessions can finish partially approved or partially expired; check-in approval reserves funds instead of capturing them. TDF's one-time executor rejects these unsupported flows. The documented duplicate-charge protection is for token collection; TDF does not assume it makes WebCheckout creation automatically retryable. |
| [Datafast native integration](https://developers.datafast.com.ec/index.aspx) | Purchases use `paymentType=DB`; the subsequent checkout payment lookup is GET. TDF's confirmation reuses the debit-sale attempt, without pretending this is preauthorization capture. Provider-required integration certification remains outstanding. |

Fixtures are sanitized, minimal reconstructions of those contracts. No published example credentials or card details were copied into requests or executed.

## Compatibility and rollback

No schema migration, production data operation or API/OpenAPI change is included. Web/mobile clients retain the same provider-session contract and backend-authoritative outcome states.

Existing bound attempts retain their IDs, intent keys, provider references, operation fingerprints and ledger history. Exact replays may read terminal attempts without creating new money movement. Unbound historical attempts fail closed for explicit reconciliation; they are not silently attached to a fresh intent.

Do not mix older and newer create writers during rollout: the old intent-key algorithm can bypass the new per-request isolation if both versions serve traffic. Disable new payment creation during the application cutover; retain reconciliation processing; validate no duplicate active intents/attempts and compare provider references before re-enabling the approved environment. This is a runbook only; no rollout was performed.

Prefer roll-forward for a defect. An application rollback requires new creates to remain disabled, all writer replicas to be quiesced, and outstanding v2 attempts to be reconciled first. Do not remove or rewrite new intent keys or external references to make an older build accept them. No database rollback is necessary for this patch.

## Test matrix and evidence

Environment: isolated local macOS worktree; Stack 3.7.1/GHC 9.10.3; PostgreSQL 16 Docker for runtime tests. All merchant/account/approval records inside the disposable database are synthetic fixtures. They are not sandbox activation evidence.

| Command | Evidence class | Result |
|---|---|---|
| `cd tdf-hq && stack test --fast --test-arguments='--match=provider'` before edits | Compiled baseline unit/property/mocked boundaries at parent `3b81b6877` | 77 examples passed. |
| Same command after adapter repairs | Compiled unit/property/mocked boundaries in the working tree | 85 examples passed; no provider contact. |
| `sh scripts/test-provider-retry-runtime.sh` | Actual store functions against disposable PostgreSQL, including concurrent connections | 9 examples passed, 0 failures, observed complete at 2026-09-14 21:31 UTC on source `f83705647`; test runtime reports `rts_thr`. Container removed. |
| `npm run test:catalog-list-audit` | Local audit discovery test | 1 test passed. |
| `npm run audit:catalog-lists` | Exhaustive repository catalog audit | Exit 0; report timestamp `2026-09-14T21:25:15.934Z`; 1,413 source files, 1,122 candidates, zero unreviewed/stale entries. |
| `npm --prefix tdf-hq-ui test -- --runTestsByPath src/api/providerPaymentSessions.test.ts src/utils/providerPaymentResume.test.ts src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx` | Mocked browser/API recovery regressions | 4 suites, 16 tests passed; 43.51 seconds. |
| `cd tdf-hq && stack test --fast` | Full compiled backend regression suite at `f837056476a8171e2e25a84c3456006aa6362892` | 2,556 examples passed, 0 failures, 25.0532 seconds; final test SQL timestamp `2026-09-14T21:32:13.993059Z`. The separately gated PostgreSQL suite adds 9 executed examples. |
| Hosted CI | GitHub branch checks | Not yet run for this branch. |

The first local database-test invocation was refused by filesystem sandbox access to Docker (exit 126); no test ran in that invocation. Its approved rerun stalled because the non-threaded test binary blocked all Haskell execution behind one database lock; it was interrupted (exit 130) and its disposable container was removed. The next bounded run failed 9/9: the cached binary still reported `rts_v`, and fixtures reused the unique checkout lookup hash. Commit `f83705647` fixes fixture uniqueness and requires `+RTS -N2`; recompilation/relinking produced `rts_thr`, and the complete database suite then passed. These failures were not provider failures. Existing compiler warnings were not hidden or downgraded.

Source implementation commit: `791086c3acfc0bd7f97939c5b99cc8d00432bb15`; fixture/runner repair: `f83705647` (2026-09-14 21:28:51 UTC). Earlier local invocations did not emit exact start/end timestamps; their tool transcripts and results are retained, and no fabricated precision is supplied here. The audit report provides its own exact generation timestamp; the database completion time above is the observed minute, not a claimed provider timestamp. Hosted checks will provide independently timestamped evidence at the pushed SHA.

The PostgreSQL suite checks distinct-key concurrency, same-key concurrency, ambiguous fallback across all selected online rails, verified-decline retry, old-reference compatibility, exactly-one remote-operation claim, immutable-field/cross-checkout tampering, unbound-history refusal, Datafast status replay and PayPal capture continuation. The wrapper validates the actual dedicated database name and emptiness before any schema write, creates no provider client, and removes its own disposable container on exit. CI runs it against a separate empty PostgreSQL service database.

## Remaining boundaries

This patch does not complete the overall payment-platform objective. Outstanding implementations include provider-neutral Datafast/PayPal transport consolidation, expired/paid-checkout pre-response recovery, immutable mixed-tax line allocation, recurring mandates and saved-method deletion, payment-link issuance, selected-provider refund/cancel/preauthorization executors and their administrative controls, settlement/dispute ingestion, seller payouts and invoice/credit-note integration. Marketplace money movement remains blocked without approved provider-managed connected-account/split/payout capability; no TDF custody substitute is implemented.

No credentialed provider sandbox, staging, live transaction, deployment, settlement or payout was exercised. Contracts, merchant/site configuration, sandbox accounts, notification registration and legal/accounting/PCI review remain required as listed in the [operator runbooks](operator-runbooks.md) and [provider execution record](provider-execution-verification-2026-09-14.md). Mocked responses never establish those capabilities.
