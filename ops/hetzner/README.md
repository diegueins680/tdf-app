# TDF portable deployment and recovery

The live TDF web API moved to Hetzner on 2026-09-28; see the
[cutover evidence and outstanding validation](validation-2026-09-28.md). Trader and its shared Fly
database remain running. Older mobile builds require a new release.
Google interactive login passed on 2026-10-05. Cutover validation remains
incomplete until the authenticated-upload client repair is deployed and passes
without browser instrumentation; see the validation record.

The rehearsal configuration deploys a quarantined copy and does not switch
production writers or traffic. Never promote that rehearsal database: take a
fresh consistent export after quiescing every production writer. Production is
already accepting writes on Hetzner; never restore service from the stale Fly
copy without a new freeze and reverse migration.

The old `release:backend`, `release:backend:plan` and
`release:backend:preflight` npm commands are retired. Direct invocation of
`scripts/production-release.mjs` also exits before contacting any provider or
database. Its imported source/artifact validation helpers remain in use by
`npm run release:backend:prepare -- FULL_RELEASE_SHA FULL_RECOVERY_SHA NEW_PRIVATE_DIRECTORY`.
That preparation command does not deploy; follow the guarded procedure below
and preserve the live Hetzner database for application recovery.

## Host selection and cost

On 2026-09-28 the authenticated Hetzner project reported CX23/CX33/CAX11/CAX21
unavailable. CPX12 was available in Nuremberg: one shared x86 CPU, 2 GB RAM,
40 GB local storage, USD 13.49/month. Daily server backups add 20% and IPv4
adds USD 0.60, giving USD 16.788/month before usage beyond included traffic.
No yearly commitment is required. These are account-specific prices; check
the pricing API and capacity again before provisioning.

The observed Fly production baseline is approximately USD 23.56/month:
two shared-CPU 256 MB APIs in ORD/LAX, one 2 GB database in GRU, 5 GB database
storage, and 11 GB of API asset volumes. Staging adds at least USD 8.45.
The source audit also found active Trader databases on the same Fly database
server (about 3 GB combined). Its compute/storage cost is shared and cannot be
removed by migrating TDF alone. Leave that server and Trader connections running
unless a separate Trader migration is explicitly authorized and verified; do not
count shared database retirement as realized TDF savings. These estimates exclude
additional traffic, snapshots, taxes, and other account charges. Savings occur only after the corresponding Fly resources
are retired; stopping a VM does not remove all storage charges. Outstanding
Fly invoices are unaffected by migration.

A single host loses the existing API's regional redundancy. Server backups
do not provide database failover or replace a tested logical backup. An
initial read workload demonstrated memory headroom, not peak-load capacity
or equivalent availability. A temporary 256 MB ORD forwarder for clients
using `tdf-hq.fly.dev` would add approximately USD 1.94/month.

Pricing references: [Hetzner billing](https://docs.hetzner.com/cloud/billing/faq/),
[Hetzner API](https://docs.hetzner.cloud/reference/cloud),
[Fly pricing](https://docs.fly.io/about/pricing/).

## Prepare and isolate

1. Provision a dedicated server with `cloud-init.yaml`, daily backups,
   deletion/rebuild protection, a dedicated SSH key, and a firewall allowing
   SSH only from the operator's address. Never reuse unrelated servers or
   keys. Keep PostgreSQL private. Apply firewall updates sequentially and
   re-read the resulting rules.
2. Prepare artifacts from the reviewed full commit and its reviewed recovery
   ancestor:

   ```sh
   node scripts/prepare-portable-restore.mjs FULL_RELEASE_SHA FULL_RECOVERY_SHA /private/tmp/new-restore-bundle
   ```

   The command reuses the existing release source, migration ancestry,
   checksum, image metadata, identity, and disabled financial-write checks.
   It requires identical migration manifests for release and recovery. It
   emits SQL and an immutable-image report into a new private directory;
   it neither contacts a database nor authorizes production cutover.
3. Export a custom-format `pg_dump` over authenticated SSH and preserve both
   API asset volumes. Keep dumps, archives, role metadata, and credentials in
   mode-0700 directories outside the repository. Hash every transferred file.
   Detect conflicting asset paths by content hash before combining them.
   Preserve the source archives. Seed the generated `feature-registry.json`
   from the exact release image instead of either stale regional copy.
4. Production uses PostgreSQL **17**, with `vector` **0.8.1**, `btree_gist`,
   `pg_trgm`, `pgcrypto`, and `unaccent`. Do not restore into PostgreSQL 16
   or an image lacking those extensions. Use a compatible patched PG17 image
   and pin its digest; verify extension versions after restore.
5. In `/opt/tdf/restore`, install `compose.restore.yaml` as `compose.yaml` and
   `Caddyfile.restore` as `Caddyfile`. Set `POSTGRES_IMAGE`, `TDF_IMAGE`, and
   `CADDY_IMAGE` to verified immutable digests in `.env`. Store a newly
   generated database password in mode-0600 `postgres_password`. Do not copy
   production provider, SMTP, OAuth, payment, or encryption secrets.
6. Start only `db`, confirm it is empty, and restore with
   `pg_restore --exit-on-error --no-owner --no-privileges`. Run the prepared
   `preflight.sql`, then `migrations.sql`, then `verify.sql`, each with
   `psql -X -v ON_ERROR_STOP=1`. Record the ledger and extension versions.
7. Create a dedicated rehearsal login with `SELECT` on public tables and
   sequences, plus `UPDATE (usage_count)` on `content_reaction_type` and
   `creator_badge_type`. Startup refreshes those two counters, so a wholly
   read-only transaction setting prevents boot. Other worker writes remain
   denied; their permission-error logs are expected in this rehearsal.
   Populate `api.restore.env` with this login, the private `db` hostname,
   and regional settings read from the restored deployment-reference tables.
   Make the asset directory writable by container UID 1000.
8. Start the `smoke` profile. Confirm API and database attach only to the
   `internal: true` quarantine network. Prove external egress fails from the
   API's network namespace. Probe health, exact version, canonical-origin
   CORS, and public feed responses from the host over SSH.
9. Add an unused API hostname only after backing up DNS and comparing every
   existing record. Preserve the website and mail records. Allow TCP 80/443
   and start the `edge` profile. During rehearsal the edge serves only
   `/health` and `/version`; all other routes return 503. Verify trusted TLS
   without disabling certificate validation.
10. Rehearse recovery to the compatible immutable image, verify its health
    and version, then restore and verify the target image. Preserve the
    reports outside the secret bundle.

## Original cutover gates and outstanding validation

The operator explicitly chose a web-first cutover on 2026-09-28 and accepted
downtime for older installed mobile clients until updated. Preserve this
decision: no temporary Fly forwarding service is required. The following is the
original cutover checklist, retained for recovery and evidence. Production now
accepts writes on Hetzner; these historical instructions do not authorize moving
traffic back to Fly. Google login and authenticated upload validation remain
outstanding, as stated above. Subsequent releases must preserve the current
database and use reviewed immutable images, canonical migration checks, backups,
the release lease and a compatible recovery image.

- Preserve the existing release lease and security-emergency readiness
  checks. Capture old machine configurations, runtime gates, immutable
  images, database roles, ownership, grants, and all asset volumes.
- Fly's overdue-invoice block rejected release creation and a supported
  staging Machine update on 2026-09-28. The rejected update left staging
  unchanged. Fence the old backend from further writes; installed clients
  using the old hostname must not keep writing to a divergent database.
- Prepare a separate production database and assets directory, private
  credentials, application grants, HTTPS configuration, and logical backup
  schedule. Retain tested recovery images. Do not give the production app
  the rehearsal role or expose the database publicly.
- Quiesce all old writers and scheduled jobs, acquire the release lease,
  take the final consistent export and asset copy, and verify restore counts,
  financial/audit invariants, schema checksums, and security readiness. Keep
  event discovery and automatic publication disabled through the cutover.
- Apply `migrate-owned-urls.sql` with `psql -X -v ON_ERROR_STOP=1` to the
  final restored database before starting any application. Repeat it to prove
  idempotence; confirm no active asset, event-image (including its directory
  view projection), or radio URL retains the Fly prefix. Preserve historical
  cutover metadata. Verify every referenced local asset exists in the final
  merged assets directory.
- Run only the isolated `canary` profile against the final database first.
  Create the restricted reader role and fresh `api.canary.env` exactly as in
  rehearsal step 7: SELECT plus the two startup-counter column grants, no
  production provider credentials. Its only network is the internal database
  network. Prove outbound access and business writes fail, then probe health,
  exact version, public reads, asset availability, and canonical CORS from
  the host. Background loops may log denied writes but cannot execute them.
  Stop the canary and revoke its login after validation. Only then start the
  `serve` profile; this explicitly enables production workers and writes on
  the new database. Switch callbacks and frontend while the old database stays
  fenced, so there is only one writable database.
  Preserve payment webhook IDs/signature secrets and verify callbacks without
  creating a real charge. Verify Google authentication and uploads before
  reopening writes. Prepare native builds using the new hostname separately.
- Cloudflare Pages has two independent backend settings: browser
  `VITE_API_BASE` and event-preview function `PUBLIC_API_BASE`. Before deploying
  this hostname change, pin both production settings to the current Fly base.
  At cutover change both to `https://api.tdfrecords.net`, preserving all other
  bindings, and rebuild production. Verify an actual public event page as
  well as browser API calls; the preview function runs before the SPA.
- Once new writes are accepted, recovery must preserve them. Roll back the
  application to the compatible image on the new database; never switch
  traffic back to a stale pre-cutover Fly database. A provider reversal needs
  another write freeze and fresh reverse migration.
- After successful verification and the retention window, retire the
  superseded Fly compute and volumes deliberately. Retain the forwarding
  endpoint only if provider access is later restored and forwarding is wanted.

`compose.production.yaml` uses a separate external volume named
`tdf_production_postgres_data`; create it only for the final production restore.
Install it as `/opt/tdf/production/compose.yaml` with a root-only raw `api.env`,
new database credentials, the verified image `.env`, and merged final assets.
Copy `Caddyfile.production` as `Caddyfile`. The production edge reuses the
validated certificate volumes. Stop the rehearsal edge before starting the
production `serve` profile to avoid port conflicts. The database has no
published ports; the production API has outbound access for its configured
providers. Do not start that profile against an unfenced production clone.

Install `backup-postgres.sh` as executable in the production directory and
the service/timer under `/etc/systemd/system`. The daily 02:30 UTC logical
backup precedes the host's 10:00–14:00 UTC snapshot window. Run and restore
the first logical backup before declaring cutover complete. Backup retention
is deliberate; monitor free space and preserve an off-host verified copy.

Event ingestion changes originally tracked in PRs #460, #463 and #464 have
entered main through their reviewed successors, including #475 and #465.
Repository integration does not establish their production rollout or authorize
enabling ingestion or event operations flags.

## Remaining operational follow-up

Daily artist enrichment and the course publisher target `https://api.tdfrecords.net`.
The hourly messaging workflow now performs the existing read-only token check:
missing, invalid, expired, or soon-expiring credentials still fail and notify.
It cannot exchange credentials or update the retired Fly app. Automatic token
rotation for Hetzner is pending a reviewed integration with the current secret
store. Until then, an authorized operator must rotate credentials in the current
Hetzner deployment and synchronize the GitHub health-check credentials; a check
of GitHub credentials alone does not verify the running service's credentials.
Do not use the legacy no-argument Fly refresh command after this cutover.

Datadog API synthetic test `r2d-i82-3jy` now targets
`https://api.tdfrecords.net/health`: root workflow run `37209188359` on
2026-10-04 passed that request and web test `rv2-x2n-epx`, with zero critical
errors. The web test still targets `https://tdf-app.pages.dev/`; its owner must
verify coverage of the canonical `https://www.tdfrecords.net` surface as well.
Mobile's separate Datadog credentials were rejected with HTTP 403 in run
`37184643181`; its green status was caused by disabled critical-error failure,
not successful synthetic tests. Correct those credentials and require critical
errors to fail before treating Mobile's synthetic result as release evidence.
These repository changes do not deploy, rotate production credentials, or
claim that the pending authentication/upload gates have been performed.

## Read-only operational tools after cutover

The catalog inventory and daily mail monitor use the existing dedicated TDF SSH
connection through `scripts/production_access.py`. The recorded connection is
`root@178.105.93.101` with `~/.ssh/tdf_hetzner_deploy_20260928`; set
`TDF_PRODUCTION_SSH_HOST` / `TDF_PRODUCTION_SSH_KEY` only when moving that
already-authorized connection. Strict host-key checking, batch authentication
and the dedicated identity are required. Never disable host verification.

Both tools require the running `tdf-production` API and database under
`/opt/tdf/production`, their expected database/network binding, and the configured
immutable API and PostgreSQL images (`TDF_IMAGE` and `POSTGRES_IMAGE`). Both
references must match the running container or its registry digest. They fail
closed instead of falling back to Fly or the
quarantined restore database. Catalog inventory also compares the public API
commit/health with the inspected deployment before and after its existing bounded,
anonymized read-only SQL, making fresh DNS/TLS/peer-bound public requests on both
sides of the query. PostgreSQL defaults to read-only before the transaction;
statement/lock timeouts and sensitive-column exclusions remain in force.

The mail monitor reads only `SMTP_USERNAME` and `SMTP_PASSWORD` from the protected
mode-0600 `api.env` into memory and requires agreement with the running API.
Neither subprocess diagnostics nor credentials are logged. It retains read-only
IMAP selection, BODY.PEEK, size limits, aggregate-only reports and error status.
The installed LaunchAgent copy needs both `monitor.py` (from
`scripts/mail-deliverability-monitor.py`) and `production_access.py` alongside it;
verify their hashes and a read-only run after an update. Updating these local
tools does not deploy the application or rotate runtime credentials.


Catalog SQL runs as the dedicated `tdf_catalog_inventory` role, never `postgres`.
Its reviewed operational setup is `catalog-readonly-role.sql`: no superuser,
role/database creation, inherited roles or row-security bypass; SELECT only in
public, with no password or new network access. Provisioning an existing role
name fails instead of changing it. Existing roles retain their effective CREATE capability explicitly before the
ambient PUBLIC schema-CREATE grant is removed. Their login, membership and other
privileges remain unchanged; the new reader cannot create persistent objects.
The helper also accepts only the exact reviewed inventory SQL digest and refuses
coverage gaps after new tables are added. Review SELECT grants for those tables
before the next inventory; do not silently omit them or use the application role.
An explicit read-write transaction must still receive permission denied on a
zero-row UPDATE probe. The live setup/negative control is recorded by the audit.

Immutable-image checks compare `TDF_IMAGE` with the container's configured image
reference or the image's matching registry RepoDigest. Docker's local image/config
ID is recorded for race detection, but is not assumed to equal a manifest digest.
The root-level legacy Instagram diagnostic is also retired; use the existing
read-only `scripts/check-messaging-token.mjs` instead.

The catalog public health/version reads require normal certificate/hostname TLS
validation and require every DNS answer and the actual HTTPS socket peer to match
the server address reported by the authenticated SSH connection. A healthy copy of
the same Git SHA on another host is rejected before the inventory query runs.
Introducing a CDN or load balancer requires a reviewed replacement for this direct
origin binding; the inventory intentionally fails closed in that topology.

The shared access helper also requires the database container to mount the named
`tdf_production_postgres_data` volume at `/var/lib/postgresql/data`, with no child
mount shadowing that store. A replacement volume, bind mount or missing mount is
rejected before metadata, credentials or inventory are returned.
