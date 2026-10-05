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
through the existing `tdf_catalog_inventory` role in read-only transactions. Only
the container's pinned Unix socket and port are used; inherited libpq routing
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
