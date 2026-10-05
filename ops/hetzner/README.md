# TDF production operations on Hetzner

The observed production target is the dedicated Hetzner host, Compose project
`tdf-production`, directory `/opt/tdf/production`, private PostgreSQL17 database
`tdf_hq`, and volume `tdf_production_postgres_data`. The public backend is
`https://api.tdfrecords.net`. These identities must be re-observed before any
production operation. The authoritative system index is
[formal/system/README.md](../../formal/system/README.md).

## Read-only inspection

Run from the repository root with the operator's dedicated SSH identity:

```sh
node scripts/inspect-hetzner-production.mjs --identity-file /absolute/private/key --output /absolute/private/new-runtime.json
```

The command pins the host, requires existing SSH host trust and a specific key,
checks Compose identity, immutable image references, private database networking
and the expected volume. It reads the ledger and production capability flags
through the existing `tdf_catalog_inventory` role in read-only transactions. Social
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
an isolate: inspect the `net.tdf.restore-rehearsal` label, nonce name, immutable image
and isolation before removing that specific container. Never prune unrelated
containers or remove the permanent lock inode. A failure is never a passing receipt.

To rehearse this exact candidate's migrations on the restored isolate, add
`--with-candidate-migrations`. The launcher loads the manifest and recursively
included SQL from immutable Git blobs with introduction-ancestry validation. It
uses the existing canonical migration-batch generator, including its schema
verifier, and binds the manifest and SQL hashes to the receipt. The helper first
rejects unknown or changed applied history, applies the batch twice on the admitted
isolate, and requires complete ledger correspondence, preserved historical entries
and stable second-application results. Any provider/revenue/social control changes
are reported explicitly; their presence is not authorization to activate them in
production. No application or worker is started against the restored data. SQL and
migration diagnostics stay inside the root-private archive directory.

**This does not establish release readiness.** The online database snapshot is not
coordinated with assets or cluster-global role/schema changes; the rehearsal lock
only excludes other rehearsals. No provider action, application canary, production
restore or deployment occurs. Counts do not establish bytewise logical equality.
Keep provider flags disabled and production writes intact. A release still needs
writer drainage, a coordinated database/assets backup, tested secret/off-host
recovery, migration rehearsal and compatible application recovery.

## Routine release status

The guarded routine Hetzner release executor remains an open implementation and
verification obligation. `release:backend` and `release:backend:preflight` now
reject before remote action because their former executor targets Fly. Historical
source-only `release:backend:plan` and importable migration/recovery validators
remain available for correspondence and restore preparation. Do not use the
historical executor to redirect production or treat its Fly plan as a Hetzner
release plan.

Before an eligible rollout, the canonical executor must bind reviewed source,
immutable target/recovery images, the exact migration manifest, actual Compose
configuration and current flags; acquire a release lock; drain existing writers;
back up database and assets; verify an actual isolated restore; apply and verify
reviewed migrations; exercise a restricted canary; then verify the live version,
configuration, database and safe smoke behavior. An archive listing alone does
not establish restoration. Existing CORS permissiveness requires coordinated
configuration and enforcing-image rollout. Recovery expiry requires draining
old handlers. Provider and experiment gates must not activate incidentally.

Recovery after accepting writes must preserve the current database and use a
compatible enforcing image. Never restore an old backup over newer financial,
audit, identity or user data merely to reverse an application release.

## Historical material

The [September28 restore/cutover plan](../../docs/archive/hetzner-restore-rehearsal-2026-09-28.md)
records the original Fly-to-Hetzner transition. It is not current release
instruction. The Compose, Caddy and backup files here remain implementation
inputs; installed configuration must be compared with them before execution.
