# Private live music catalog aggregate collector — PREPARATION ONLY

No SSH command, production SQL, storage request, flag change, grant, DDL, or production write has been performed by this preparation. Nothing is added to the repository or privacy candidate.

Review `collect.py`, `fixed-queries.sql`, `query-spec.json`, the byte-exact `canonical-inspect-runtime.py`, and `review-manifest.json` before any execution. The launcher requires the exact reviewed manifest SHA-256 and rejects changed sources. Use the existing dedicated identity and strict host verification; never change host access or the database role to make this pass.

## Identity boundary

The canonical source is copied byte-for-byte from the recorded repository revision. Its Docker command pins `/var/run/docker.sock` and clears inherited Docker routing. Container discovery, Compose project/service/workdir/config-file identity, immutable image forms, exact database network and volume, no published DB ports, PGDATA/no nested mounts, and API network validation use the original `summarize_container` function. All three services must be running. The same sanitized container descriptors must match after collection; only their hash leaves the host, not container or business IDs.

Root explicitly permitted one existing privileged exception: invoke the original, unchanged `verify_storage` function. Its exact fixed boolean SELECT under local `postgres` validates database/user/local socket/port/default read-only/data_directory. It selects no business row and is never used as a fallback for inventory SELECT failures. The canonical function retains its existing 15-second PGOPTIONS statement timeout. Its source/query hashes are captured separately. No other privileged query exists in the extension.

All **new** SQL, including capability/identity preflights, logs in directly as `tdf_catalog_inventory`, begins `BEGIN READ ONLY`, executes `SET LOCAL ROLE tdf_catalog_inventory`, sets a **2-second statement timeout**, and rolls back. The original cleared libpq environment, fixed Unix socket/port/database and role remain. New query processes have an eight-second timeout; lock timeout is 500 ms and idle-in-transaction timeout is three seconds. The remote Python collector has a 120-second alarm and the SSH launcher a 180-second cap. No new dependency is installed.

## Output boundary and observations

Nine fixed aggregate families cover canonical assets by fixed role/processing-state buckets; release totals/version states; job and upload states; named ready/published database-reference gaps; structural reference/orphan counts; and legacy artist-release provider-reference presence. Unexpected code values are counted only in a fixed `__other__` bucket. No titles, raw IDs, owners, filenames, object keys, URLs, metadata or failure text are returned.

Each fixed table is checked for SELECT privilege, relation type and RLS. Missing SELECT, missing relations, unexpected relation type, or unestablished RLS coverage makes the dependent family **UNOBSERVED with null counts**. SQLSTATE 42501 is `permission_denied`; timeout/schema/process errors remain explicit UNOBSERVED outcomes. No privileged retry, grant, schema repair, invented zero, or application fallback exists. Zero is emitted only for a successful validated SELECT with available full table coverage.

The new counters do not prove storage-object existence, MIME/codec validity, rights, public visibility, search completeness or playback authorization. Database constraints can explain zero structural gaps. Unreferenced draft recordings/assets may be legitimate. Transactions are sequential, not one global snapshot. Other social, podcast and arbitrary upload sources remain outside this collector. No repair/delete action is authorized.

## Validation performed locally

Python compilation, JavaScript syntax checking, deterministic wrapper/shape checks and 13 private unit tests use fakes or pure command construction; they perform no network or database operation. The tests cover fixed role/socket/caps, elevated-command rejection, permission failures, process/statement timeouts, output leakage rejection, real observed zero versus UNOBSERVED, RLS/missing relation handling, identity failure, and before/after identity drift. They do not execute PostgreSQL grammar or canonical production identity probes. Actual SQL execution and live catalog counts remain UNOBSERVED until root reviews and explicitly runs the collector.

## Reviewed execution, later and by root only

After review, supply the manifest hash reported by preparation to:

```
node /private/tmp/tdf-audio-live-catalog-prep-20261026/launch.mjs --review-sha256 REVIEWED_MANIFEST_SHA --identity-file EXISTING_PRIVATE_IDENTITY --output NEW_PRIVATE_RECEIPT_JSON
```

The command is an execution recipe, not authorization or evidence that it ran. An inaccessible role/table is the audit blocker to report, not a reason to provision new access. Outputs are created exclusively with mode 0600. No credentials or raw stderr are printed.
