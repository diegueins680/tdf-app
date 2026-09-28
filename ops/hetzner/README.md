# TDF portable restore rehearsal

This configuration deploys a quarantined copy of TDF on a dedicated Hetzner
host. It does **not** switch production writers, frontend traffic, payment
callbacks, or mobile clients. Never promote the rehearsal database: take a
fresh consistent export after quiescing every production writer.

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
These estimates exclude additional traffic, snapshots, taxes, and other
account charges. Savings occur only after the corresponding Fly resources
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

## Cutover gates

The operator explicitly chose a web-first cutover on 2026-09-28 and accepted
downtime for older installed mobile clients until updated. Preserve this
decision: no temporary Fly forwarding service is required. Keep production
traffic on Fly until the following implementation and verification steps are
ready and the repository's required review/checks pass.

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

The event ingestion changes in PRs #460, #463, and #464 remain separate from
this hosting configuration and require their own review and rollout.
