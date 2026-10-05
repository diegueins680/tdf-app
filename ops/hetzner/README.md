# TDF production operations on Hetzner

The observed production target is the dedicated Hetzner host, Compose project
`tdf-production`, directory `/opt/tdf/production`, private PostgreSQL17 database
`tdf_hq`, and volume `tdf_production_postgres_data`. The public backend is
`https://api.tdfrecords.net`. These identities must be re-observed before any
production operation. The authoritative system index is
[formal/system/README.md](../../formal/system/README.md).

Google interactive login and ordinary inventory-photo upload passed on October5
against production web merge `c0a154cd4cbfcd3a5dc5167634be30b7691803cc`.
The deployed entry asset matched the immutable Cloudflare deployment; both
99-byte synthetic PNG uploads returned HTTP200 and downloaded with matching
hashes. The form was cancelled without creating an inventory record. These
checks used the existing backend, not a rollout of this candidate. See also
[the cutover validation record](validation-2026-09-28.md). Repository integration
does not activate experimental event, payment or ticket-email flags.

## Read-only inspection

Run from the repository root with the operator's dedicated SSH identity:

```sh
node scripts/inspect-hetzner-production.mjs --identity-file /absolute/private/key --output /absolute/private/new-runtime.json
```

The command pins the host, requires existing SSH host trust and a specific key,
checks Compose identity, immutable image references, private database networking
and the expected volume. It reads the ledger and production capability flags
through the existing `tdf_catalog_inventory` role in read-only transactions. The nine merchandise-reputation database flags are observed separately, with
missing rows listed as unknown rather than disabled. Interaction runtime/entity-kind
switches and the optional event-operations API flag are also recorded. An absent
event-operations flag table is represented as null; it is not inferred disabled. Social
observations contain only runtime enablement and activation history, never pair,
consent or message data. A missing singleton remains unknown rather than inferred
disabled. Only the container's pinned Unix socket and port are used; inherited libpq routing
overrides are cleared and SQL verifies that the connection is local. Only
allowlisted identity/configuration facts leave the host. Credentials and raw
Docker/SQL errors are not printed. Output creation is exclusive with mode0600;
receipts record collector hashes and whether its source checkout was dirty.
Source provenance is captured before the remote read and checked afterward;
changed sources invalidate the observation rather than receiving a passing receipt.
Observations are sequential, not an atomic fleet snapshot. Missing flags retain
unknown effective defaults. A successful observation is **not release readiness**.

## Source and migration preparation

After a clean commit, use a runtime receipt from the same collector implementation,
begun within the last fifteen minutes:

```sh
node scripts/prepare-hetzner-release.mjs --runtime /absolute/private/recent-runtime.json --output /absolute/private/new-plan.json
```

This local command reads immutable Git blobs, including SQL includes, checks each
migration introduction's ancestry and compares the observed ledger against the
exact manifest. Unknown/duplicate entries and unapproved checksum differences
reject preparation. Pending entries retain manifest order, including holes in the
observed ledger. Historical compatible checksums remain explicit in the manifest.
The plan records current flags, the required coordinated CORS configuration repair,
and outstanding release gates. It always reports `executionAllowed: false`.
It does not invoke SSH, Docker, a database, or a deployment service. Receipts are
local evidence, not signed attestations or approval; an executor must re-observe
runtime state under its release lock.

## Isolated online database restore rehearsal

From a clean reviewed source revision:

```sh
node scripts/rehearse-hetzner-restore.mjs --identity-file /absolute/private/key --output /absolute/private/new-restore.json
```

The collector and rehearsal pin the host's local Docker Unix socket and discard
Docker context/TLS routing overrides. The helper holds a nonblocking exclusive
host lock for its entire operation, verifies available memory/disk and the source
identity, then exports a read-only PostgreSQL snapshot. Table counts and the custom
archive use that same snapshot. Source queries are read-only, use the explicit
local database socket, and have database and in-container process timeouts.

Database and password-free role archives remain in a new root-private directory
under `/opt/tdf/backups`. Only hashes, relation counts, migration identities and
operational metadata leave the host. Ownership and ACL definitions are restored;
role credentials remain a separate recovery obligation. The existing `postgres`
role is retained from initdb, then its dumped attributes are applied.

Restoration targets a unique nonce-owned container running the exact production
PostgreSQL image. It has no external network, published ports, host mounts or
production credentials, a read-only root filesystem, 256MiB tmpfs data, 384MiB
memory with no extra swap allowance, half a CPU and 64-process limit. PostgreSQL
uses ten connections and `max_locks_per_transaction=1024`: a real restore with
the default64 exhausted its shared lock table. The memory ceiling stays384MiB. Rehearsal
requires at least 1GiB available memory, 2GiB disk and a source database no larger
than 128MiB. These conservative bounds deliberately reject growth; do not silently
raise them on a production host. Archives are each capped at 256MiB.

A pass requires successful role/archive replay, matching snapshot relation counts,
matching migration identities, a stable production database identity/ledger, and
confirmed removal of the fully inspected isolate. No count-only or archive-listing
shortcut can pass. Local tests inject dump, role, restore, count, ledger, interruption
and cleanup failures; invalid target controls and source/receipt drift must reject.
This is empirical conformance of this boundary, not a formal proof of recovery.

The 540-second operation deadline reserves another 90 seconds for bounded cleanup.
A lost create response is recovered using the unique name, then full identity and
isolation validation. On failure, partial archives and a stage-only failure record
remain private. A host crash, daemon outage or forced process kill can still leave
an isolate. A new rehearsal checks all labelled containers, including stopped ones,
under the exclusive lock. It also rejects any `restore-rehearsal.pending.json`
marker: this root-private nonce/image record is fsynced before Docker creation and
cleared only after identity-checked removal. A delayed create can finish after the
helper dies, so an empty inventory does not authorize clearing a pending marker.
Recovery must establish no request is in flight, then inspect/remove the admitted
nonce target before clearing its matching marker. The helper rejects before source inspection if any remain. This
prevents overlapping resource reservations after process death. Inspect the
`net.tdf.restore-rehearsal` label, nonce name, immutable image
and isolation before removing that specific container. Never prune unrelated
containers or remove the permanent lock inode. A failure is never a passing receipt. The [bounded scheduling model and its five
negative controls](../../formal/system/restore-isolation.md) state the assumptions
and exclusions explicitly.

To rehearse this exact candidate's migrations on the restored isolate, add
`--with-candidate-migrations`. The launcher loads the manifest and recursively
included SQL from immutable Git blobs with introduction-ancestry validation. It
uses the existing canonical migration-batch generator, including its schema
verifier, and binds the manifest and SQL hashes to the receipt. The helper first
rejects unknown or changed applied history, applies the batch twice on the admitted
isolate, and requires complete ledger correspondence, preserved historical entries
and stable second-application results. Any provider/revenue/social, merchandise-reputation, interaction or optional event-operation control changes
are reported explicitly; their presence is not authorization to activate them in
production. By default no application or worker is started against the restored data. SQL and
migration diagnostics stay inside the root-private archive directory.

**This does not establish release readiness.** The online database snapshot is not
coordinated with assets or cluster-global role/schema changes; the rehearsal lock
only excludes other rehearsals. No provider action, production restore or deployment occurs. The default invocation does not run an application canary. Counts do not establish bytewise logical equality.
Keep provider flags disabled and production writes intact. A release still needs
writer drainage, a coordinated database/assets/private-uploads backup, tested secret/off-host
recovery, migration rehearsal and compatible application recovery.

## Private upload storage

The [private upload contract](../../formal/system/private-upload-persistence.md)
requires a pre-provisioned, private host `uploads` directory bound to `/app/uploads`.
The production entrypoint refuses missing, unwritable or known ephemeral storage.
Before replacing the old API, drain writers and preserve any files still in its
writable layer; an earlier empty observation is not permission to discard new
files. Verify image-user ownership and coordinated database/assets/uploads restore.
The inspector reports the canonical bind separately from release eligibility.
Both `api` and the historical shared-database `canary` mount that same
pre-provisioned private directory so production startup checks apply consistently.
That `canary` can access live data and uploads: it is not an isolated release canary
and cannot satisfy these obligations or authorize running a candidate before
writer fencing and recovery qualification.

## Routine release status

The [physical PostgreSQL copy boundary](../../formal/system/physical-recovery.md)
starts a verified cold PG17 copy on isolated disk-backed storage, with exact
cluster identity and clean-control checks and no initdb fallback. It shares the
logical rehearsal reservation. Its synthetic Docker checks do not establish
production capture, off-host custody, coordinated restore or deployment readiness.

The [private file recovery primitive](../../formal/system/recovery-files.md) checks
file content and metadata in new isolated targets. It is a library for the pending
coordinated bundle, not a production backup or deployment command. The
[encryption primitive](../../formal/system/recovery-envelope.md) uses pinned age
with immutable Linux execution and trusted content hashes; it does not yet
provide production key custody, transfer or coordinated restoration.

The [scheduled logical archive contract](../../formal/system/logical-backup.md)
defines the daily timer's narrower guarantee. Install the shell entrypoint and
both Python companions together after review; never treat an online dump or
archive listing as coordinated database/assets recovery. Existing installed
versions require fresh hash and scheduled-run verification after delivery.

Storage observation now rejects shadowing child mounts and noncanonical PGDATA.
A fixed boolean read-only query under the existing local postgres role additionally
checks the server's effective data_directory; the inventory reader receives no
new grants. Command/configuration overrides cannot satisfy this check by merely
retaining an unused canonical volume mount. Privileged concurrent reconfiguration
remains outside the sequential observation boundary.

The [release intent journal](../../formal/system/release-journal.md) persists ordered
intent and rejects uncertain retries in one fixed global control directory. It
does not yet coordinate or verify production effects.

The guarded routine Hetzner release executor remains an open implementation and
verification obligation. Direct invocation of `scripts/production-release.mjs`
is retired and rejects before any provider or database action. The former
`release:backend`, `release:backend:plan` and `release:backend:preflight` commands
are removed. Its reviewed importable source/artifact helpers remain available to
`npm run release:backend:prepare -- FULL_RELEASE_SHA FULL_RECOVERY_SHA NEW_PRIVATE_DIRECTORY`.
That command only prepares portable artifacts; it is not the canonical current-ledger
plan described above and does not deploy or authorize database changes.

A production executor must re-observe identity, flags, approved manifest/checksums,
backups and recovery eligibility under its release lock. It must fence writers,
apply only reviewed pending migrations and verify health, release identity and
critical authenticated flows before reopening traffic. Never restore an old backup
over newer financial, audit, identity or user data to reverse an application release.

## Historical material

The [September28 restore/cutover plan](../../docs/archive/hetzner-restore-rehearsal-2026-09-28.md)
records the original Fly-to-Hetzner transition. It is not current release
instruction. The Compose, Caddy and backup files here remain implementation
inputs; installed configuration must be compared with them before execution.

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

The shared `production_access.py` helper pins the local Docker Unix socket and
runs Docker with a minimal environment. Inventory psql runs with a cleared
container environment, explicit `/var/run/postgresql` socket and port5432, and
read-only transaction defaults. Each SQL connection asserts the database, reader
role, local transport and server port before coverage or catalog queries. Ambient
Docker contexts and libpq service/address overrides cannot choose another target.
These observations remain sequential, not an atomic production snapshot.

### Optional isolated application verification

After a reviewed candidate image is available locally by immutable digest, add
`--canary-image diegueins680/tdf-hq@sha256:…` to the clean-source rehearsal with
`--with-candidate-migrations`. See [DEPLOY-CANARY-001](../../formal/system/isolated-canary.md)
for exact isolation, outage/recovery semantics, cleanup and evidence limitations.
This uses the disposable restored database and new empty mounts, never the
shared-database compose canary. An actual isolated run is required; source tests
alone do not establish Docker compatibility or production eligibility.
