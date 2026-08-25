# Deployment, staging, activation, and rollback guide

## Approval sequence

No step below implies approval. The required order is:

1. Revalidate the already-resolved Stewart Moreira identity as exactly one active party/email with the `Intern` role; keep the exact identifiers runtime-only.
2. Review code, inventory, cases, Spanish instructions, authorization, notification recipients, and data controls.
3. Obtain explicit authorization for branch creation, then separately for staging, commit, push, and PR actions as requested by the repository workflow. Branch, commit, push, PR creation, isolated staging deployment, and inactive in-app draft creation were authorized on 2026-08-23 and 2026-08-24. Production deployment, issue creation, activation, assignment, and real notification remain unapproved.
4. Deploy to an isolated staging tenant/database with test transports.
5. Run migration, backend/web/mobile checks, E2E as both synthetic intern and administrator, accessibility, provider-contract, and rollback rehearsal.
6. Use the preparation script in `preview` mode. With approval, use `create` only to create an inactive draft.
7. Present the in-app draft preview to Diego.
8. Activate and notify only after Diego gives a later, explicit approval.

The regenerated mobile OpenAPI client and feature registry were committed to the matching `tdf-mobile` feature branch, and the parent repository records that submodule revision. A staging or production release must preserve that pairing and must not publish an orphaned submodule reference.

The production `fly.toml` routes internal feedback uploads to `/data/assets/feedback/internal`, beneath the existing persistent `tdf_assets` mount. Production release validation fails closed when `TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT` is absent, relative, normalized outside its declared path, or not beneath a persistent mount. This configuration is release readiness only; it does not authorize or perform a production deployment.

CI regenerates and diff-checks the audit artifacts in repository quality, rehearses the new migration and rollback in migration quality, exercises the mocked Chromium journey through the existing Playwright job, and checks OpenAPI client drift through the existing contract job. The disposable backend API lifecycle script remains a required pre-deployment command because the current backend CI container topology does not expose the local database lifecycle expected by that script.

## Dedicated staging topology

Use the production architecture versions but a dedicated database/tenant, private evidence storage prefix, isolated provider configuration, and synthetic identities. Never seed a shared or production database. `TDF.Seed` already refuses hosted/production marker combinations; retain those controls.

The reviewed Fly configurations are `fly.studio-audit-staging.toml` for the API and `fly.studio-audit-staging-web.toml` for the web client. They name only isolated staging apps. The API uses an app-scoped one-gigabyte data volume, disables production research workers, keeps payment providers in sandbox mode, stores evidence outside the served asset directory, and accepts browser requests only from the staging web origin. The web image compiles `VITE_API_BASE` to that staging API and serves the SPA with a dedicated health check. The web machine may stop when idle. The single API machine remains running because every cold start deliberately verifies the complete reviewed migration bundle; an auto-stopped API produced multi-minute startup latency that is unsuitable for supervised testing. The database is a separate minimum-size staging cluster and must never be forked from production.

The deterministic data contract is `test/internships/studio-audit/staging-fixtures.json`. The existing idempotent base seed supplies service catalog, rooms/resources, availability, parties, inventory, and sample sessions. The reserved persona seed supplies synthetic roles. Scenario setup uses the `AUDIT-2026` identifiers. Cleanup is destroying and recreating the dedicated database/tenant and clearing its private storage, test inbox/outbox, browser state, and provider mocks; broad row deletion in shared staging is prohibited.

### Observed isolated staging deployment

The authorized deployment uses only the following isolated applications:

- API: `https://tdf-hq-studio-audit-staging.fly.dev`
- Web: `https://tdf-studio-audit-staging-web.fly.dev`
- Database: `tdf-hq-studio-audit-staging-db`, with a dedicated application database and least-privilege application user

The database was initialized from the reviewed migration manifest and deterministic synthetic seeds. It contains no production clone or provider credentials. The application secret inventory contains only the dedicated `DATABASE_URL`; email, WhatsApp, calendar, social, Datafast, and PayPal credentials are absent. All 65 reviewed migrations are recorded, including the base audit migration, completion-exception control, and historical-failure completion gate. Spanish is the default locale and `de`, `en`, `es`, `fr`, and `pt` remain enabled.

The web staging image was built with `npm ci --legacy-peer-deps` because the repository lockfile's root peer resolution otherwise selects React 19 while this workspace supports React 18. It also copies the canonical backend feature registry required by web type checking. These are image-build reproducibility fixes; the deployed bundle still resolves its API base to the isolated staging API.

The observed API release is Fly release 6 at source commit `aa86367560b98399115a1aa75b6dddd2def22547`, image digest `sha256:687b487665fa2ec838f74915a2fa06e3abf315cfcc45987e672bb4ede5c0b74b`. Its health response is `{"db":"ok","status":"ok"}` and `/version` reports that exact commit. The observed web release is Fly release 1, image digest `sha256:7c1400dbc4fad5d6bba0b5658e5aea9edae98a5779b3552ad51aaf63de453632`, with HTTP 200 from its health endpoint. CORS accepts the exact staging web origin and rejects an unrelated origin.

The original 256 MB Postgres machine exhibited internal monitor/proxy timeouts under PostgreSQL 18. The API was stopped, the encrypted database volume was preserved, and only the isolated database VM was resized to 512 MB. All three database checks and the API health check then passed. Keep 512 MB as the reviewed minimum for this staging topology.

The authorized staging draft was created idempotently after the synthetic Manager resolved exactly one active synthetic Intern. It contains one draft project, one draft principal task, one draft plan, and 174 unique cases. The task has no assigned party or due date; only the proposed synthetic assignee is stored. The plan retains 14 days from future activation, 20–30 expected hours, and a 50% midpoint. The proposed and unrelated synthetic Intern accounts cannot see the inactive task or plan. Both the notification outbox and the assignment-notification count remained zero. The real production Stewart identity is not stored in staging or source control.

Required safe configuration:

```text
APP_ENV=staging
RESET_DB=false
SEED_DB=false
TDF_ENABLE_SYNTHETIC_PERSONAS=1
TDF_SYNTHETIC_PERSONA_FILE=../test/personas/personas.json
PAYPAL_ENV=sandbox
COMMERCE_CHECKOUT_ENV=sandbox
DATAFAST_ENV=sandbox
DATAFAST_BASE_URL=https://test.oppwa.com
SMTP_* unset or directed to an isolated sink
WhatsApp/calendar/social credentials unset or fake
TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT=/data/audit-evidence
```

Secrets and the runtime-only persona password are installed through the staging secret manager and never committed. Because application seeding intentionally refuses hosted runtimes, initialize the empty staging database through an authenticated private Fly proxy while running the already-tested backend locally with `APP_ENV=test`, `RESET_DB=false`, `SEED_DB=true`, and the deterministic persona file. Stop that local process immediately after health succeeds, verify the expected synthetic rows, close the proxy, and deploy with the committed `RESET_DB=false` and `SEED_DB=false` values. Never bypass or disable the hosted-runtime seed guard.

`APP_ENV`, not the report's user-selected environment label, controls whether outbox rows are marked for a test transport. The API refuses non-staging audit-plan creation. Evidence storage must be outside the public web root and downloadable only through the authorized endpoint.

## Migration

Apply `tdf-hq/sql/2026-08-21_studio_internship_audit.sql` through the repository migration mechanism only after a backup and migration preflight. It is rerunnable. Confirm the new tables, constraints, indexes, triggers, and explicit draft columns. Existing public `feedback` rows are not rewritten.

The migration entries added by this integration use the immutable pre-integration main commit `dac84b099b18b51032fb94f58273120f5375eb85` as their release-ancestry anchor. That commit remains reachable whether GitHub represents the reviewed changes as a merge or a synthetic squash; the selected release commit's manifest and migration checksums still determine which SQL is rendered and applied. Release preflight must prove that anchor is an ancestor of the selected deployment SHA.

## Draft creation

Preview locally:

```sh
node scripts/prepare-studio-audit-draft.mjs preview
```

Creation is designed to fail closed unless the target is local/test/staging, an authorized runtime token exists, Stewart's exact runtime-only party ID and email are both supplied, the lookup resolves exactly once to an active `Intern`, and the explicit confirmation value is present. It idempotently creates only a hidden draft project, draft principal task, draft plan, and its cases. It does not activate, assign, or notify.

## Verification before activation

- Public feedback submission and legacy readability.
- Intern/report privacy and direct-API authorization.
- Admin triage, clarification, duplicate, retest, close/reopen, audit, search, and export.
- Attachment upload/download validation and private object storage.
- Calculated progress, preserved retest history, daily/final summary, and every completion gate.
- Test in Spanish and English where supported; desktop, responsive web, and mobile fallback/native routes.
- Test notification sink/outbox contains only synthetic recipients.
- Verify no production hostname, customer, schedule, payment, inventory, or provider credential appears.

The repository implementation creates reporter and immediate authorized-team in-app notifications plus durable immediate/digest outbox rows. The inspected baseline has no worker that dispatches the new outbox, so grouped lower-severity digests are not delivered beyond their durable queue. Before any real activation, operations must either connect the approved notification worker or explicitly approve in-app-only delivery, test the digest schedule, recipient allowlist, retries, idempotency, and dead-letter handling, and record that decision. Staging must leave every external outbox row undispatched.

## Activation

Activation is a separate administrator endpoint and UI confirmation. It assigns the previously proposed party, sets the project/task start and due date to activation plus fourteen days, changes the plan to active, and only then queues the permitted notification. Do not invoke it during development or draft review.

## Rollback

1. Disable activation UI/routes at deployment/config level and stop new internal audit writes.
2. Export internal report/test data if retention is required.
3. Apply `tdf-hq/sql/2026-08-24_studio_audit_historical_failure_gate_rollback.sql` in staging first.
4. Apply `tdf-hq/sql/2026-08-24_studio_audit_completion_exception_rollback.sql` only after its guard confirms no completion depends on an exception marker.
5. Apply `tdf-hq/sql/2026-08-21_studio_internship_audit_rollback.sql`.
6. Verify legacy `feedback`, internships, catalogs, audit logs, and public feedback remain readable.
7. Roll back application code and generated clients.

The rollback drops only the new normalized audit/report structures and draft columns. It intentionally preserves the existing `feedback` table and all legacy rows. Any already-created GitHub issue is external state and requires a separate authorized disposition.
