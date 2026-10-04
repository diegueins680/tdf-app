# Closed-checkout approval evidence — 2026-09-15

Continuation after draft [#378](https://github.com/diegueins680/tdf-app/pull/378),
not completion or activation of the payment platform. Architecture and invariants:
[ADR 0124](../adr/0124-closed-checkout-approval-evidence.md).

## Access and dependencies

Started from the clean isolated `tdf-app-payment-checkout` worktree at
`c8ef56f404dc12f5311635214549221a7090ea70`. The original dirty worktree was not
changed. Fetched root refs and inspected open root/mobile PR metadata, open issues,
payment heads and #378 CI. Main advanced to
`6f67081a73e66e422f331cb31c455a78977a400e`: #370's read-only messaging-token
checks/documentation/catalog decisions are unrelated to this payment repair.
Their changes, #369 artwork and the parent's pending catalog/test additions must
be preserved when reconciling the stack with main. This dependent branch does
not claim the pending stack has been merged into latest main.

Payment heads #331 `25416b43e`, #332 `536964de6`, #334 `70823741b` and the
subsequent payment stack remained as reviewed in the parent handoff. No duplicate
late-approval implementation was found in the refreshed payment heads. Mobile
#81 remains `d84fc3196202f2ac2468f78816b3c98fb4a22144`, clean and pinned by the
unchanged root gitlink. Mobile #80's social-contract head `c1832c5` remains a
separate generated-contract merge dependency. Open issues #128/#130 concern
onboarding/RSVP, not this repair. No merge or production mutation was performed.

Created `codex/payment-closed-checkout-evidence-20260915` after #378. At the
16:35 UTC refresh, #378's reported checks were successful or intentionally
skipped, except backend-quality still running. That is parent CI evidence, not
evidence for this branch.

`npm run ai:doctor` initially reported GitHub authentication unavailable inside
the restricted network sandbox. Its approved unrestricted rerun passed with
16 OK, two missing-daily-memory warnings and zero errors; GitHub authenticated.
No token values were displayed. Branch creation and the local commits below
were completed; remote delivery is recorded in the draft PR and final handoff.

| Commit | Scope |
|---|---|
| `3032a4d4e1c0efd6468c999436838e4ffa3693ab` | Initial late-approval regression tests. |
| `3d6044762d2c6468268a0577d60c63ebe9b28e64` | Correct the test snapshot's ambiguous SQL row alias. |
| `2168852ea106b9b0e33f71a869455fb45bac4761` | Durable review evidence, held recovery projection and expanded DB regressions. |
| `88e75b536d5dc0401c6e722e23f6beaf626dff38` | API and web hold regressions; final frozen tested source. Later changes are documentation only. |

## Staging qualification

From the #344 staging worktree, `node scripts/inspect-payment-staging.mjs`
returned exit 1 at **2026-09-15T16:13:02.536Z**. An approved unrestricted rerun
returned the same result at **2026-09-15T16:14:06.284Z**:

- `tdf-hq-studio-audit-staging.fly.dev`: HTTP 200; health and database OK.
- `tdf-studio-audit-staging-web.fly.dev`: HTTP 200; health OK. Its lack of an API
  database-health field is not evidence of a database failure.
- All six hosting status/config/secret-name checks:
  `hosting_authentication_unavailable`.
- `sourceCommit=null`, `providerQualified=false`; no configured alternative host.

No secret values were read/printed, no hosting resource was created, and no
staging/provider transaction or deployment ran. Restore approved
`FLY_STAGING_API_TOKEN` / `FLY_STAGING_WEB_TOKEN` access or authorized local Fly
login, then verify exact deployed SHA, sandbox account aliases, merchant contracts,
required secret names and webhook registration. Healthy public endpoints alone do
not qualify a payment test environment. The user authorized staging setup, but
the available hosting authentication still cannot perform it.

## Research checked for this increment

These are current primary API/transaction references, not merchant onboarding,
pricing or sandbox qualification. They do not refresh every row of the parent
market matrix or establish a TDF contract.

| Official source | Access date | Finding and confidence |
|---|---|---|
| [PlaceToPay session API](https://docs.placetopay.dev/en/checkout/api/reference/session/) | 2026-09-15 | Authenticated session query supplies session/transaction details. High confidence in the documented integration mechanism; TDF sandbox unverified. |
| [PayPhone API Sale](https://docs.payphone.app/api-sale) | 2026-09-15 | Official sale/status integration remains the adapter reference. High confidence in documentation access; no live/sandbox request or account validation. |
| [PostgreSQL 16 explicit locking](https://www.postgresql.org/docs/16/explicit-locking.html) | 2026-09-15 | Row locks and savepoint rollback have distinct transaction-lifetime implications. High confidence; local concurrency/rollback tests below exercise this implementation. |

An attempted Hackage UUIDv5 documentation fetch returned a cache miss; it is not
claimed as a successfully verified source. The already installed UUID package's
API compiled through the repository's Stack toolchain; DB replay tests exercise
the deterministic identity in use. No dependency or provider specification changed.

## Implementation and threat-model update

Previously an exact approved query on a closed checkout returned a domain
rejection. Its capture application rolled back; the scheduled worker added only
a generic exception without actual money. The new shared query lane preserves a
deduplicated `verified_payment_on_closed_checkout` exception and correlated audit
instead, with no capture, receipt, order, attempt, intent or operation mutation.
The scheduled worker avoids a redundant generic exception for that same approval.

After this evidence exists, matched reordered results remain held. Changed
amount/currency/resource/certainty still fail before the review path. Existing
review money/identity must agree exactly, and assignment/resolution notes are not
rewritten by replay. Setting the exception to resolved/ignored does not authorize
release. Exact create replay and authenticated payment-session GET use the
existing `ambiguous`/no-fallback contract and return no redirect, while retaining
the original operation/resource IDs. Web recovery therefore cannot report paid,
navigate back into checkout or unlock another method from this held response.

| Threat | Control exercised | Remaining limit |
|---|---|---|
| Forged/altered money on a closed order | Existing authenticated query/parser, immutable binding and exact typed-result checks run first. | Merchant sandbox and hostile-network qualification remain unexecuted. |
| Duplicate or reordered callbacks | Operation lock plus deterministic exception primary key; one audit; subsequent matching outcomes stay held. | Historical generic exceptions are not backfilled or deleted. |
| Fulfillment/inventory corruption | No capture/checkout transition, receipt or inventory reacquisition. | Operator remediation/refund and accounting treatment are not automated. |
| Recovery/IDOR and duplicate payment | Existing lookup-token authorization; held response removes redirect and fallback permission. | Never manually reopen an order or create a replacement payment to bypass review. |
| Partial persistence or evidence loss | Review and audit share the caller transaction/savepoint; audit failure rolls both back. | No claim that direct privileged database edits/deletion are safe. |

The shared Datafast/PayPal capture guard is preserved and regression-tested, but
this new late-observation workflow applies only to PlaceToPay/PayPhone's shared
query lane. No new method, refund, payout, mandate, UI action or mobile native
screen is implemented. OpenAPI and generated clients are unchanged and still
byte-identical between web/mobile; no contract regeneration was necessary.

## Executed verification

Environment: local macOS, Node 24.8.0, Stack lts-24.42/GHC 9.10.3, PostgreSQL
16.10; Unix-socket-only disposable databases named `tdf_provider_retry_test`.
All provider responses, merchant accounts, encryption material and money are
synthetic. Database tests are real PostgreSQL tests with mocked provider evidence,
**not real provider sandbox tests**. Times below are UTC log-final-write times;
exit results were separately observed through the execution tool.

| Time / source | Command or run | Outcome |
|---|---|---|
| 16:09:48 / parent `c8ef56f40` | `stack test --fast --rerun-tests --test-arguments='--match=provider'` in `tdf-hq` | Exit 0; 123 examples, 0 failures, 15.8565 s. Baseline. |
| Before 16:18 / `3032a4d4e` | First restricted invocation of the DB harness | Exit 1; local socket connection unavailable; no schema/test execution. Approved unrestricted retry followed. |
| 16:18:27 / `3032a4d4e` | Full DB harness, cluster `Czbpb7` | Exit 1; 178 examples, 8 failures: four real late-approval rejections, four test-only ambiguous SQL row-alias errors. The existing 170 passed. Final populated-rollback checks were not reached. |
| 16:24:53 / `3d6044762` | `stack --stack-yaml tdf-hq/stack.yaml test --fast --rerun-tests --test-arguments='--match=closed +RTS -N2 -RTS'`, same test DB | Exit 1; 18 examples, 8 failures, 1.1160 s. All eight new tests now fail for the intended missing review behavior; ten matching existing tests passed. |
| 16:37:05 / implementation plus final test edits | Full DB harness, cluster `COD0nN` | Exit 0 including rollback checks; 198 examples, 0 failures, 28.3642 s. Build began at `2168852ea` and tests advanced to `88e75b536` while building; repeated below with frozen source. |
| 16:34:20 / `88e75b536` | Web command below | Exit 0; two suites, 14 tests, 0 failures, 25.731 s. |
| 16:36:46 / `88e75b536` | `npm --prefix tdf-hq-ui run typecheck` | Exit 0. No full web production build or native/device test claimed. |
| 16:39:37 / frozen `88e75b536` | `stack test --fast --rerun-tests` in `tdf-hq` | Exit 0; 2,596 examples, 0 failures, 43.5771 s. Optional PostgreSQL cases run separately. |
| 16:40:48 / frozen `88e75b536` | Full DB harness, fresh cluster `pdrKwB` | Exit 0 including repeat migration, empty rollback/operator-seed retention, reapply and populated-rollback refusal; 198 examples, 0 failures, 37.9311 s. |
| 16:40:14 / frozen `88e75b536` | `npm run quality:repo` | Exit 0; nine Node test groups, 157 tests, 0 failures; repository/formal/generated-drift gates passed. Initial run also passed. |

Exact final commands, from repo root unless noted:

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-late-payment-final-pg.pdrKwB' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-late-payment-final-db-20260915.log | tail -n 65

npm --prefix tdf-hq-ui test -- --runTestsByPath src/components/payments/HostedProviderCheckout.test.tsx src/pages/ProviderPaymentReturnPage.test.tsx
npm --prefix tdf-hq-ui run typecheck
npm run quality:repo
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /private/tmp/tdf-late-payment-catalog-final-20260915.json
cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts
git diff --check
```

The earlier full DB command used the identical environment variable/harness with
socket directory `tdf-late-payment-red-pg.Czbpb7` or
`tdf-late-payment-verified-pg.COD0nN`. Logs are retained locally as
`/private/tmp/tdf-late-payment-{baseline,red,red-focused,verified-db,final-db,web,web-typecheck,full-backend,repo-quality-final}-20260915.log`.
The first catalog pass at 16:33:14 and final frozen-source pass at
**2026-09-15T16:41:38.108Z** each reported 1,422 scanned files, 1,128 candidates
and exit 0: zero unreviewed candidates and zero stale decisions. No catalog
decisions or quality gates were changed. Generated web/mobile comparison and
`git diff --check` passed; both new documents' relative links resolved.

All three exact temporary clusters were stopped with `pg_ctl ... stop -m fast`.
At **16:44:17 UTC**, separate `pg_ctl -D <exact data directory> status` checks
returned `no server running` (expected exit 3) for `Czbpb7`, `COD0nN`, and
`pdrKwB`. The private data directories and logs were retained; no data was deleted.
Web typecheck and full-backend exits were observed by 16:40:50 UTC; the final DB
harness exit, including its post-test rollback checks, was observed at 16:41:22.

## Closed-approval operator procedure

1. Keep the original order, payment and all alternatives on hold. Do not recreate
   the payment, clear browser state to retry, reopen inventory, set paid, or issue
   a receipt from this exception.
2. In an authorized environment, inspect the existing strict-admin reconciliation
   aggregates and query dead letters. They are operational summaries, not booked
   cash/settlements. Individual exception navigation and automated release are
   missing in this increment. The subsequent
   [read-only evidence view](reconciliation-evidence-2026-09-15.md) adds individual
   navigation, but still no automated release. An authorized DB operator can
   select the exact review below;
   this sample was not run against staging or production.
3. Independently verify the original provider resource in the qualified merchant
   environment. Compare the observed amount/currency and checkout binding with
   provider transaction and eventual settlement evidence. Do not export raw
   callback/query payloads, redirect URLs, card details or credentials.
4. Have authorized finance/fulfillment staff decide the customer remedy and the
   accounting treatment. A provider approval is not proof of settlement. Suspense
   accounting, a later fulfillment decision, refunds, tax invoices/credit notes
   and any seller implications need their own approved, traceable workflow.
5. Assign the incident and retain its evidence. A label/note update cannot release
   this hold; no reviewed release command exists yet. Never delete or rewrite the
   review to make a callback succeed. Escalate rather than inventing a ledger entry,
   refund, seller transfer or new charge.

```sql
-- Authorized read only; supply the exact internal checkout UUID via psql.
SELECT id, provider, environment, internal_reference AS checkout_id,
       provider_reference AS provider_resource, expected_amount_minor,
       actual_amount_minor, currency, status, detected_at
FROM commerce_reconciliation_exception
WHERE internal_reference = :'checkout_id'
  AND exception_type = 'verified_payment_on_closed_checkout';
```

## Rollout, rollback and remaining work

Merge/review after #378 and its documented dependency chain; no automatic merge
is requested. No migration/backfill or mobile gitlink change. Drain older query
and callback consumers before rollout; they neither recognize this review hold
nor hide old recovery redirects. A binary rollback requires disabling those
consumers and retaining affected-order holds and all exception/audit data.

Staging authentication, provider accounts/contracts/secrets/webhook qualification,
real sandboxes, production-schema rehearsal, Datafast/PayPal late-observation
handling, full callback-to-product fulfillment coverage, review-detail/release
tools, actual suspense/refund/settlement accounting, remaining recurring and
marketplace flows, mobile lineage reconciliation and qualified Ecuadorian
legal/accounting/PCI review remain incomplete. None is replaced with a mock claim.
No production deployment, live transaction, refund or payout occurred.
