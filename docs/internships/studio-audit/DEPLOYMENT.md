# Deployment, staging, activation, and rollback guide

## Approval sequence

No step below implies approval. The required order is:

1. Revalidate the already-resolved Stewart Moreira identity as exactly one active party/email with the `Intern` role; keep the exact identifiers runtime-only.
2. Review code, inventory, cases, Spanish instructions, authorization, notification recipients, and data controls.
3. Obtain explicit authorization for branch creation, then separately for staging, commit, push, and PR actions as requested by the repository workflow. Branch, commit, push, and PR creation were authorized on 2026-08-23. The isolated staging configuration is committed for review, but deployment, issue creation, in-app draft creation, activation, assignment, and real notification remain unapproved.
4. Deploy to an isolated staging tenant/database with test transports.
5. Run migration, backend/web/mobile checks, E2E as both synthetic intern and administrator, accessibility, provider-contract, and rollback rehearsal.
6. Use the preparation script in `preview` mode. With approval, use `create` only to create an inactive draft.
7. Present the in-app draft preview to Diego.
8. Activate and notify only after Diego gives a later, explicit approval.

The regenerated mobile OpenAPI client and feature registry were committed to the matching `tdf-mobile` feature branch, and the parent repository records that submodule revision. A staging or production release must preserve that pairing and must not publish an orphaned submodule reference.

CI regenerates and diff-checks the audit artifacts in repository quality, rehearses the new migration and rollback in migration quality, exercises the mocked Chromium journey through the existing Playwright job, and checks OpenAPI client drift through the existing contract job. The disposable backend API lifecycle script remains a required pre-deployment command because the current backend CI container topology does not expose the local database lifecycle expected by that script.

## Dedicated staging topology

Use the production architecture versions but a dedicated database/tenant, private evidence storage prefix, isolated provider configuration, and synthetic identities. Never seed a shared or production database. `TDF.Seed` already refuses hosted/production marker combinations; retain those controls.

The reviewed Fly application configuration is `fly.studio-audit-staging.toml`. It names only the isolated staging app, uses an app-scoped one-gigabyte data volume, disables production research workers, keeps payment providers in sandbox mode, stores evidence outside the served asset directory, and allows the machine to stop when idle. The database is a separate minimum-size staging cluster and must never be forked from production.

The deterministic data contract is `test/internships/studio-audit/staging-fixtures.json`. The existing idempotent base seed supplies service catalog, rooms/resources, availability, parties, inventory, and sample sessions. The reserved persona seed supplies synthetic roles. Scenario setup uses the `AUDIT-2026` identifiers. Cleanup is destroying and recreating the dedicated database/tenant and clearing its private storage, test inbox/outbox, browser state, and provider mocks; broad row deletion in shared staging is prohibited.

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

The migration is registered in `scripts/production-migrations.json` with the approved feature commit `d68b794e531501589e18c24dbb60aa00ada0a0f7` as `introducedBy`. The registry entry was added only after that commit existed. Release preflight must still prove that commit is an ancestor of the selected deployment SHA.

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
3. Apply `tdf-hq/sql/2026-08-21_studio_internship_audit_rollback.sql` in staging first.
4. Verify legacy `feedback`, internships, catalogs, audit logs, and public feedback remain readable.
5. Roll back application code and generated clients.

The rollback drops only the new normalized audit/report structures and draft columns. It intentionally preserves the existing `feedback` table and all legacy rows. Any already-created GitHub issue is external state and requires a separate authorized disposition.
