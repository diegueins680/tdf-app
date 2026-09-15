# Payment staging discovery and access qualification — 2026-09-14

## Outcome

The user authorized discovering or setting up staging. An existing isolated Fly API/web pair
was found in tracked configuration and verified over HTTPS. **It is healthy, not yet qualified
for payment testing.** No new hosting resource, Fly/Koyeb deployment, migration, secret update, provider
transaction or production change was performed. Authentication/authorization currently blocks
inspection and setup through both configured hosting providers.

| Surface | Existing target | Verification |
|---|---|---|
| API | https://tdf-hq-studio-audit-staging.fly.dev | `/health`: HTTP 200, status OK, database OK. |
| Web | https://tdf-studio-audit-staging-web.fly.dev | `/health`: HTTP 200, status OK. Web health does not attest database or checkout behavior. |

Tracked files are `fly.studio-audit-staging.toml` and `fly.studio-audit-staging-web.toml`.
They specify staging/sandbox payment environments, restricted CORS, no seeding/reset/automatic
migration, and a staging-specific volume. **These are intended settings, not proof of current
effective runtime values**: remote metadata access was denied. Existing September 8 schema
evidence belongs to that historical release and does not validate the new payment migrations.

## Executed access checks

Times below are UTC on 2026-09-14 unless explicitly dated otherwise. Credentials were used only through the existing GitHub
Actions secret injection mechanism. No credential value was displayed, exported or committed.

| Check | Source/environment/time | Actual result |
|---|---|---|
| Local `flyctl apps list --json` | Local macOS session | No access token/login available. No app metadata obtained. |
| Public API and web health requests | Approved local HTTPS requests, then GitHub Ubuntu runner | Both returned health OK; runner timestamps below. No payment request sent. |
| Fixed-app Fly status/config/secret-name inspection | [Run 34906363951](https://github.com/diegueins680/tdf-app/actions/runs/34906363951), `f900daa769f63579b489fd4e3b96f284402dcf28`; report 22:54:45.993 | All six metadata reads failed; public health succeeded. This version retained only generic failure classes. |
| Same reads with safe error classification | [Run 34906870039](https://github.com/diegueins680/tdf-app/actions/runs/34906870039), `3d136d34004e2bd214a1283a7026e9a94ff55aae`; report 23:01:39.552 | Both app status queries: inaccessible. Both config and secret-name queries: hosting authorization denied. No values/digests/raw errors retained. |
| Same reads plus Koyeb GET-only app listing | [Run 34907233146](https://github.com/diegueins680/tdf-app/actions/runs/34907233146), `2aee92f1bb809d8d0a569a0e535bb10012b7c45a`; report 23:07:20.546 | Fly results unchanged, both health endpoints HTTP 200. Koyeb: configured token, HTTP **401**, inaccessible. Inspection job exit 1. Five synthetic guard tests passed before inspection. |
| `gh secret list --repo diegueins680/tdf-app --json name --jq '.[].name'` | Authenticated GitHub metadata; repeated after staging token isolation change | Existing `FLY_API_TOKEN` and `KOYEB_API_TOKEN` names present. New staging-specific names below absent. No provider secret values or Fly secret-name list obtained. |
| `node --test scripts/__tests__/payment-staging-inspection.test.mjs` | Local Node; code committed in `1bd8d9b79`; run completed before 23:17:44 | **6 tests passed**, 0 failures, 908.822474 ms. Synthetic tests, not hosting or provider qualification. |
| Same six-test command after merging recovery locally | `f9f60df22b35b607ca95f87f75139b041ffdf2c8`; completion observed by 2026-09-15 00:07:45 | 6 tests passed, 0 failures; 4526.516514 ms. |
| `ruby -e 'require "yaml"; YAML.load_file(ARGV[0], aliases: true); puts "Workflow YAML parsed"' .github/workflows/ci.yml` | Local system Ruby 2.6; completion observed by 2026-09-15 00:07:45 | Exit 1: this Ruby version does not accept the `aliases` keyword. No workflow/schema defect inferred. |
| `ruby -e 'require "yaml"; YAML.parse_file(ARGV[0]); puts "Workflow YAML parsed"' .github/workflows/ci.yml` | Same source; at 2026-09-15 00:07:45 | Exit 0. YAML syntax parsed; not a complete Actions-schema validation. |

The final remote report is downloadable from
[artifact 10372957375](https://github.com/diegueins680/tdf-app/actions/runs/34907233146/artifacts/10372957375)
while its seven-day retention lasts. Its fixed findings are preserved in the table above.
An unsuccessful metadata read does not establish token expiry, app deletion, merchant state,
or absence of configured payment secrets. The Koyeb 401 does not establish account ownership
or whether new-resource creation would be allowed after reauthentication.

## Implemented inspection tooling

`scripts/inspect-payment-staging.mjs` permits only the two exact Fly staging app names.
It runs status, config show and secret-name listing; no deploy/SSH/exec/secrets-write command.
It projects allowlisted status/config and secret-presence booleans in memory, drops secret
digests and unknown values, and never serializes raw CLI errors. Koyeb discovery is a single
GET to `https://app.koyeb.com/v1/apps?limit=100`; only TDF/staging names and operational IDs
can survive projection. Redirects are rejected. No production app fields are reported.

The existing CI workflow gains a manual `inspect_payment_staging` boolean, default false.
Its job has read-only repository permissions and uploads only the sanitized access report.
Normal CI jobs/gates remain unchanged. No privileged job runs on a PR event. Fly tooling is
version-pinned with an immutable action commit; provider qualification is always reported as
false because metadata is insufficient evidence of a successful sandbox transaction.

After diagnosing the existing tokens, the workflow was changed to **remove the shared Fly
token entirely** and use separate per-app credentials. Each CLI child receives only its
target app's token; a missing staging token never falls back to `FLY_API_TOKEN`. That final
credential-isolation change passed local guard tests; it was not presented as a successful
remote login. The two new secret names are not configured yet.

## Shortest path to payment staging

1. An authorized Fly account owner signs in and grants short-lived app-scoped credentials
   through GitHub's existing secret store, without sharing values in chat or source:
   - `FLY_STAGING_API_TOKEN`: only `tdf-hq-studio-audit-staging`.
   - `FLY_STAGING_WEB_TOKEN`: only `tdf-studio-audit-staging-web`.
   Do not overwrite the shared `FLY_API_TOKEN`. Existing app resources can be managed with
   app-scoped tokens; creating a replacement app requires separate organization authority.
   An authenticated local Fly session with access to these targets is also a discovery path.
2. Rerun the read-only qualification from this dependent branch:

   ```sh
   gh workflow run ci.yml --repo diegueins680/tdf-app \
     --ref codex/payment-staging-qualification-20260914 \
     -f inspect_payment_staging=true
   ```

   Verify both exact app identities, effective configuration (including secret overrides),
   source/image versions, dedicated staging database/volume identity, and secret-name presence.
   A healthy URL or present secret does not pass provider qualification.
3. Preserve the existing staging data. Back up and qualify the database before applying the
   dependent payment migrations; run reconciliation and preflight against the exact target.
   The tracked release command uses **precheck only**. Do not toggle automatic migrations,
   seed/reset, replace the volume, copy production data, or deploy the payment branch blindly.
   Preserve an image/config rollback point; retain #340's old/new writer cutover restrictions.
4. Configure approved **sandbox** merchant accounts through the application's existing
   secret manager. Relevant names include `COMMERCE_EVENT_ENCRYPTION_KEY`,
   `DATAFAST_ENTITY_ID`, `DATAFAST_BEARER_TOKEN`, `PAYPAL_CLIENT_ID`, `PAYPAL_CLIENT_SECRET`,
   `PAYPAL_WEBHOOK_ID`, `PLACETOPAY_LOGIN`, `PLACETOPAY_SECRET_KEY`, `PAYPHONE_TOKEN`,
   `PAYPHONE_STORE_ID` and the provider-specific callback/method settings in
   [operator runbooks](operator-runbooks.md). These are required configuration names,
   **not claims they currently exist or are valid**. No sandbox account alias was verified.
5. Register exact staging callback/return URLs and verified merchant bindings. Keep routes
   disabled until real sandbox evidence exists for each selected method/capability. Deploy
   reviewed backend before web; test guest/authenticated checkout, interrupts, 3DS where
   available, callbacks/reconciliation, refunds and other enabled flows with controlled
   sandbox accounts. Record commit, environment, timestamps and sanitized external references.
   Never label the existing synthetic PostgreSQL tests as these sandbox tests.

If these Fly apps cannot be used, provisioning a new isolated application/database remains
blocked on usable hosting organization authority. The configured Koyeb credential currently
returns 401, so no alternate resource was created. No new paid account/subscription was opened.
After access is repaired, reuse the existing staging pair where suitable instead of creating
unnecessary infrastructure. Database, merchant, legal/accounting and PCI reviews remain gates.

## Dependencies and rollback

This branch depends on payment recovery [#343](https://github.com/diegueins680/tdf-app/pull/343),
which depends on #340 → #334 → #332 → #331 (review/merge in reverse arrow order). The recovery
branch includes the mobile contract pointer for mobile #79, dependent on mobile #78.
No PR was merged. No hosting or database rollback is necessary because no runtime mutation
of the Fly/Koyeb staging pair was performed. GitHub PR creation triggered the repository's
existing automatic Vercel/Cloudflare preview checks; those reported success on recovery #343.
They are not evidence of a payment-qualified staging backend or a live provider transaction.
Reverting this tooling removes the manual inspection only; it must not remove
or rotate existing production secrets. Future staging-only credentials should be revoked by
their owner after the qualification window, according to the approved secret lifecycle.

## Official sources

Access date **2026-09-14**, high confidence for documented mechanisms, not account access:

- [Fly access tokens](https://fly.io/docs/security/tokens/) and
  [app-scoped deploy-token command](https://fly.io/docs/flyctl/tokens-create-deploy/):
  app scope and explicit short expiry support the separate credential design.
- [Fly secrets](https://fly.io/docs/apps/secrets/): secret listing exposes metadata, not
  plaintext values; deploy access can run code that reads secrets, so no broad runtime
  inspection was attempted.
- [Fly app configuration](https://fly.io/docs/reference/configuration/): config show defaults
  to JSON and secret values can override plaintext environment settings.
- [GitHub manual workflow dispatch](https://docs.github.com/en/actions/how-tos/manage-workflow-runs/manually-run-a-workflow):
  existing default-branch workflow may be dispatched on a selected branch.
- [Koyeb API reference](https://www.koyeb.com/docs/reference/api) and
  [apps reference](https://www.koyeb.com/docs/reference/apps): authenticated app discovery
  mechanism. No inference of viable provisioning is drawn from the failed request.

## Follow-up: catalog gate repair (2026-09-15 UTC)

PR #344's [catalog check](https://github.com/diegueins680/tdf-app/actions/runs/34912159570)
failed with two unreviewed candidates and zero stale decisions. The candidates were the exact
`STAGING_APPS` target restriction (`d6ad7e3315a241e6d802`) and the redacted hosting-status
output vocabulary (`f0efc071b84fe0a2cb13`). Following explicit user approval, commit
`79f8366fd9cae469494bd7bbecebc2e3c788477b` adds narrow `security-system-registry` decisions with their risks and rationale.
No candidate was hidden, no prior decision removed, and no audit or CI gate was weakened.

Local preparation exposed two distinct setup issues, not additional hosted CI defects:

- First `npm run audit:catalog-lists` could not load `typescript`; no audit ran.
  `npm ci --ignore-scripts` then completed, installing 1565 packages from the unchanged lockfile.
  This was preparation for source inspection, not a claim of native build readiness.
- The next audit (report `2026-09-15T00:28:48.544Z`) found the same two candidates and 97
  stale decisions because this new worktree's mobile submodule was uninitialized.
  `git submodule update --init tdf-mobile` restored the committed
  `9c86c459081b32b47579b9c95c8514029225ad75` checkout. The decisions were not deleted to hide
  those missing files; no mobile source or submodule pointer was changed.

After the two review entries were added, the six synthetic staging guard tests passed
(2378.550307 ms), and `npm run test:catalog-list-audit` passed its one test (8800.920204 ms).
Both results were observed by `2026-09-15T00:36:35Z`. The restored full-source audit rerun
passed (exit 0), with report `2026-09-15T00:44:25.160Z`: 1415 files, 1124 candidates, zero
unreviewed candidates and zero stale decisions. The exact-source hosted
[catalog job 104207848422](https://github.com/diegueins680/tdf-app/actions/runs/34914104034/job/104207848422)
also passed, running from `00:39:54Z` to `00:41:43Z` on 2026-09-15. Other CI, including the
backend, was still running at 00:44:35. These passing checks do not establish staging login
or a successful provider sandbox transaction. A secret-name-only GitHub recheck at 00:44:35
still returned no `FLY_STAGING_*` entries.

An approved final local `node scripts/inspect-payment-staging.mjs` run generated
[this unchanged sanitized report](staging-access-report-2026-09-15T004740Z.json) at
`2026-09-15T00:47:40.611Z` (exit 1). Both health endpoints again returned HTTP 200/OK, with
API database OK. All six Fly metadata reads failed with `hosting_authentication_unavailable`;
no local Koyeb credential was configured. Source was the same inspector code in `79f8366fd`;
the report's `sourceCommit` is null because the local process had no `GITHUB_SHA` injected.
It is not a statement about the deployed image SHA. Existing hosting and sandbox access
blockers remain; no deployment or payment transaction followed this inspection.
