# Reconciliation evidence view — 2026-09-15

Continuation after draft [#380](https://github.com/diegueins680/tdf-app/pull/380),
not completion or activation of the payment platform. Decision and threat model:
[ADR 0125](../adr/0125-read-only-reconciliation-evidence.md).

## Access, baseline and dependency scope

Started from clean root `9d0d84dbb4e5afafbfda7f0074421246926fd98d` in the
isolated `tdf-app-payment-checkout` worktree. The original dirty worktree was not
modified. Fetched root refs and inspected open root/mobile PR metadata, payment
heads, open issues and #380 checks before implementing this increment. At the
16:56–16:59 UTC refresh, root main remained
`6f67081a73e66e422f331cb31c455a78977a400e`; the reviewed payment stack was
unchanged. Unrelated #381's event-read revision and #376's Instagram changes
must not be overwritten. Open issues #128/#130 remain onboarding/RSVP work.

This branch depends on the unmerged payment stack rather than claiming it is
already merged into main. Preserve main's #369/#370 changes and all pending
catalog/test additions when reconciling branches. Mobile starts from draft
[#81](https://github.com/diegueins680/TDF-mobile/pull/81),
`d84fc3196202f2ac2468f78816b3c98fb4a22144`; #80's separate social-contract head
`c1832c5eb150299c12b36ba156ff38c981a6b046` remains a merge dependency.

`npm run ai:doctor` initially reported authentication unavailable in the
restricted environment. Approved unrestricted retry authenticated successfully:
15 OK, three warnings (two absent daily-memory files and the ongoing dirty
worktree), zero errors. No token values were displayed. No credentials, merchant
account or provider sandbox are inferred from GitHub authentication.

Local baseline, completed and observed by **16:59:58 UTC**, parent source above:

| Command | Outcome |
|---|---|
| `stack test --fast --rerun-tests --test-arguments='--match=provider'` in `tdf-hq` | Exit 0; 123 examples, zero failures, 17.7927 s. |
| `npm --prefix tdf-hq-ui test -- --runTestsByPath src/components/payments/ProviderQueryRecoveryPanel.test.tsx src/api/commerceOperations.test.ts` | Exit 0; two suites, nine tests, zero failures, 36.516 s. |

Baseline logs: `/private/tmp/tdf-reconciliation-view-baseline-{backend,web}-20260915.log`.

## Implemented scope

- Strict-admin, bounded read-only report with schema readiness, exact money
  strings, safe classifications, unique internal links and filter echoes.
- Web admin filter/pagination/refresh panel with stale-data hiding, Spanish and
  English labels and no financial action controls.
- Additive OpenAPI endpoint/DTOs and regenerated web/mobile contracts. No native
  mobile admin screen or device-flow change.
- Database concurrency, redaction, money/legacy-reference, authorization,
  schema-drift and lock-timeout regressions; frontend/API/contract regressions.

No new provider, migration, backfill, feature activation, financial mutation or
secret. The new reader does not query a provider, reserve a worker lease, consume
a query budget, release a held payment or mutate an audit/ledger record. A stored
provider classification, including Stripe, is not a recommendation or eligibility
claim. Cash/crypto remain unimplemented.

The current primary technical sources and per-source access/confidence findings
are in ADR 0125. This increment does not refresh merchant pricing, onboarding,
contracts or the parent market matrix, and makes no new viability claim.

## Operator procedure

1. Open the existing payment operations admin page with strict Admin access.
   The new evidence panel defaults to **sandbox/open** independently of the
   page's legacy aggregates. Confirm the panel's selected environment. Selecting
   production is a read, not authority for a payment or deployment.
2. Choose a workflow status or all statuses; optionally submit an exact internal
   checkout UUID. Empty input removes that filter. A validation/error/schema
   warning is not evidence that no exception exists. Refresh instead of relying
   on previous rows. Live pages can shift; this is not an accounting export.
3. Record the exception UUID and uniquely verified internal checkout/attempt
   links when present. Unknown links or amounts are not no-charge evidence.
   Expected/observed money is not captured cash, revenue, settlement or seller
   credit. Other currencies are explicitly shown in minor units without an
   assumed decimal scale.
4. For a closed-checkout approval, retain the payment and all alternatives on
   hold. Independently verify the original transaction in the qualified provider
   environment and compare settlement evidence through the authorized finance
   workflow. Do not place credentials, payloads, card data or redirect tokens in
   issue comments or exported reports.
5. Follow [the closed-approval procedure](closed-checkout-approval-2026-09-15.md)
   for customer remedy/accounting escalation. Resolved/ignored is a review label,
   not authorization to fulfill, refund, reopen inventory or release a hold.
   There is no release command here. Never delete/rewrite evidence to bypass it.

For API consumers, use the bearer-authenticated GET with query parameters
`environment=sandbox&status=open&limit=25&offset=0` and optional `checkoutId`.
Never supply credentials or customer lookup tokens in the URL. Consume the
generated report schema; retain amount strings exactly, and respect `no-store`.
Check `crrSchemaReady` and all echoed filters before displaying a result.

## Rollout and remaining blockers

No schema change or historical-data reinterpretation. Deploy the backend reader
before the web view; older backends produce an unavailable report. Application
rollback can remove this additive reader/view without data rollback, but must
retain #380's held-payment behavior. No migration or production data operation
is authorized or performed by this handoff.

The last completed staging qualification is the parent's **16:14:06.284 UTC**
approved unrestricted inspector run: both public health endpoints were healthy,
but all six hosting status/config/secret-name checks reported
`hosting_authentication_unavailable`; `sourceCommit=null`,
`providerQualified=false`. This is dated prior evidence, not a fresh deployment
or sandbox result. Restore approved `FLY_STAGING_API_TOKEN` /
`FLY_STAGING_WEB_TOKEN` access or authorized local Fly login, then qualify deployed
SHA, sandbox account aliases/contracts/secret names and registered webhooks.

Real provider sandboxes, staging transactions/deployment, production-volume
performance, production-schema rehearsal, native/device testing, approved hold
release/refund/remediation accounting, complete settlement and seller-payout
workflows, recurring/saved methods, Datafast/PayPal late-observation workflows,
remaining product-fulfillment coverage and qualified Ecuadorian legal/accounting/
PCI review remain incomplete. No mock is substituted for those results.

## Executed verification

Environment: local macOS, Node 24.8.0, PostgreSQL 16.10, Stack
lts-24.42/GHC 9.10.3. Every provider response/account and amount in these tests
is synthetic; database tests use real local PostgreSQL, **not a real provider
sandbox**. UTC times below are log final-write times unless marked observed.

| Time / source | Run | Outcome |
|---|---|---|
| Before 17:17 / `59f7ae8b7` | Restricted DB harness against cluster `BNoUD8` | Exit 1; local socket connection unavailable. No schema/test execution; approved retry followed. |
| 17:25:55 / `59f7ae8b7` | Approved DB harness, `BNoUD8` | Exit 1 after schema/migration checks, before runtime tests: new test omitted the `errHeaders` import. Fixed in `af97d3192`; no runtime pass claimed for this run. |
| 17:27:38 / `4c32e1304` | Repeat harness against `BNoUD8` | Exit 1: correctly refused nonempty database; no existing data changed. Fresh cluster created instead. |
| 17:38:10 / backend `af97d3192`, root `1130acb99` advancing only catalog documentation to `174847ab4` | Complete harness, fresh `PrUiit` cluster | Exit 0; 206 examples, zero failures, 20.2555 s. Thirteen migrations, repeat apply, empty rollback/operator-seed retention, reapply and populated-history rollback refusal completed. Exit observed 17:39:47 UTC. |
| 17:18:45 / web tree committed unchanged in `4c32e1304` | Four targeted web suites, command below | Exit 0; 40 tests, zero failures, 51.375 s. Includes 27 new panel examples. |
| 17:17:41 / same web source | `npm --prefix tdf-hq-ui run typecheck` | Exit 0; observed complete with the sequential mobile typecheck by 17:23:10 UTC. |
| 17:20:54 / generated mobile tree committed as `33720ef` | `npm --prefix tdf-mobile run typecheck` | Exit 0. |
| 17:20:50 / same mobile tree | Three targeted payment suites, command below | Exit 0; 13 tests, zero failures, 20.783 s. |
| By 17:23:10 observed / web source `4c32e1304` | Changed-file ESLint, command below | Exit 0; zero warnings allowed. |
| 17:27:53 / web source `4c32e1304` | `npm --prefix tdf-hq-ui run build` | Exit 0; Vite built in 1m 40s; initial budget passed with five preloads / 314065 gzip bytes. Existing large-chunk warning remains; no limit was weakened. |
| 17:22:08 / contract-test tree committed in `4c32e1304` | `npm run quality:repo` | Exit 0; nine Node groups, 160 tests, zero failures. Formal scan: zero errors/critical, 377 warnings; not a formal certification. |
| 17:34:06.799 / `174847ab4` | Final catalog audit | Exit 0; 1424 files, 1131 candidates, zero unreviewed and zero stale decisions. |
| By 17:34:54 observed / `174847ab4` | `node --test scripts/__tests__/payment-query-report-contract.test.mjs` | Exit 0; six tests, including three new reconciliation contract/projection checks. |
| 17:40:49 / frozen `174847ab4` | `stack test --fast --rerun-tests` in `tdf-hq` | Exit 0; 2599 examples, zero failures, 27.9454 s. Optional PostgreSQL examples ran in the separate harness above. |
| 17:41:20 / frozen `174847ab4` | `npm run quality:repo` | Exit 0; final repeat of all nine Node groups / 160 tests and repository/formal/generated-drift gates. |

Exact commands from the root unless a working directory is stated:

```sh
set -o pipefail
TDF_PROVIDER_RETRY_DATABASE_URL='postgresql://postgres@/tdf_provider_retry_test?host=/private/tmp/tdf-reconciliation-final-pg.PrUiit' sh scripts/test-provider-retry-runtime.sh 2>&1 | tee /private/tmp/tdf-reconciliation-view-db-final-20260915.log | tail -n 65
npm --prefix tdf-hq-ui test -- --runTestsByPath src/components/payments/ReconciliationEvidencePanel.test.tsx src/components/payments/ProviderQueryRecoveryPanel.test.tsx src/api/commerceOperations.test.ts src/pages/CommerceProviderEventsPage.test.tsx
npm --prefix tdf-mobile test -- --runTestsByPath __tests__/providerNeutralCheckout.test.ts __tests__/ticketCheckoutIdempotency.test.ts __tests__/tickets.test.ts
# In tdf-hq-ui:
../node_modules/.bin/eslint src/components/payments/ReconciliationEvidencePanel.tsx src/components/payments/ReconciliationEvidencePanel.test.tsx src/api/commerceOperations.ts src/api/commerceOperations.test.ts src/pages/CommerceProviderEventsPage.tsx src/pages/CommerceProviderEventsPage.test.tsx --max-warnings=0
# Back in the root:
node scripts/catalog-list-audit.mjs --decisions docs/catalog-persistence/catalog-list-decisions.json --fail-on-unreviewed --output /private/tmp/tdf-reconciliation-view-catalog-verified-20260915.json
cmp tdf-hq-ui/src/api/generated/types.ts tdf-mobile/src/api/generated/types.ts
git diff --check
```

Earlier harness runs used the same command with socket path
`/private/tmp/tdf-reconciliation-report-pg.BNoUD8` and log suffix `db` or
`db-verified`. The first catalog audit reported two new OpenAPI allowlists; after
tracking the new panel, the next scan also found its JSX filter values. Three
explicit, narrowly reviewed decision entries now document these redaction/filter
boundaries. No prior decision, scanner or fail-on-unreviewed gate was removed or
weakened. This is additional scope-specific review, not reuse of the user's
earlier approval for the staging inspector's different safety lists.

`npm run generate:api` completed at 17:06:59 UTC with openapi-typescript 7.10.1.
Web/mobile generated files compare byte-identically. No manual generated edits.
Logs are retained locally as `/private/tmp/tdf-reconciliation-view-*-20260915.log`;
catalog reports use the corresponding `.json` suffix.

Mobile startup command:

```sh
EXPO_NO_DOTENV=1 EXPO_OFFLINE=1 CI=1 npm --prefix tdf-mobile run start -- --localhost --port 8089
curl --fail --silent --show-error --max-time 10 http://localhost:8089/status
```

The first command used mutually exclusive `--offline --localhost` and failed
exit 1. The corrected restricted attempt failed exit 7 while the port probe
exhausted its range. Approved unrestricted startup succeeded; the approved local
status probe returned `packager-status:running` (restricted probe first failed).
Dotenv loading and remote Expo services were disabled. No bundle/device checkout
or manual native QA is claimed. The exact observed Metro PID 88498 was stopped
with TERM; its session exited 0 and no listener remained on port 8089 by
17:39:47 UTC. No screenshot was produced.

Both exact PostgreSQL clusters were stopped with `pg_ctl -D <exact data directory>
stop -m fast`; separate status checks confirmed no server running. Their data
directories and logs were retained. No material data was deleted. No staging or
production deployment, live transaction, real-provider refund or payout occurred.

## Commits and delivery

| Commit | Scope |
|---|---|
| `59f7ae8b7280e1c41180308d3bb3a66df5f52eff` | Backend reader, DTO/API contract and new backend tests. |
| `af97d31927e1ebbf9eaf4ab29049216d29dcbd40` | Correct the test-only missing error-header import. Final backend source. |
| `4c32e1304b7b7827e42f8e0d28648473f1bf1590` | Web reader/view, generated types, contract/frontend tests and redaction-list decisions. |
| `1130acb99c2b8e750924ff8fb81f2752cbe08383` | Root gitlink pins mobile contract `33720ef45b0565005c4b54b0e0c106cc93831613`. |
| `174847ab441e0cca0724259f989294fdd0ae4095` | Specific JSX filter catalog review. Subsequent root changes are documentation only. |

Mobile draft [#82](https://github.com/diegueins680/TDF-mobile/pull/82) was opened
and independently verified: draft, exact head `33720ef45b0565005c4b54b0e0c106cc93831613`,
base `codex/payment-query-observability-contract-20260915` (#81), exactly one
generated file. The remote branch SHA was also verified with `git ls-remote`.
No formal reviewer request, merge or device-verification claim was made.

Root branch `codex/payment-reconciliation-evidence-view-20260915` depends on
#380. Required order: the documented root payment stack through #380, then this
reader/UI increment; mobile #81 then #82 before consuming its pinned gitlink.
The sibling staging-qualification draft #344 does not activate this reader or a
provider. Preserve #80's mobile social-contract additions during eventual merge.

Root draft [#385](https://github.com/diegueins680/tdf-app/pull/385) was created
and verified at **19:04:26 UTC** with base #380, 19 changed files and initial
head `f2898c7418050013b4e304bc53b539e906aa0d0a`; `git ls-remote` independently
matched that SHA. The first restricted push failed DNS resolution; the approved
retry succeeded. This delivery note is a subsequent documentation-only commit.
At that observation, hosted build, catalog, repository, web/mobile/backend,
persona/API and preview checks were still running/pending; production-migrations
was successful and migration-tests skipped. None of the pending checks is claimed
as passed. Root and mobile worktrees were clean before this note. No merge or
production deployment was performed.
