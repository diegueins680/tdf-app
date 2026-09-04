# Automatic production migrations

Production applies database changes automatically from the reviewed SQL
manifest before the backend starts. It does **not** run schema changes inferred
from Persistent models.

## Runtime contract

- `AUTO_APPLY_PRODUCTION_MIGRATIONS=true` enables the production entrypoint.
- `RUN_MIGRATIONS=false` remains mandatory and prevents inferred Persistent
  DDL from becoming a second source of truth.
- `CONTEXTUAL_REPUTATION_ENABLED` is staged as `false` until its additive
  schema, backfill, consent copy, and controlled pilot are approved.
- `EVENT_DISCOVERY_ENABLED` is staged as `false` during a backend rollout and
  is re-enabled only after the schema, fleet and discovery preflight pass.

The image build renders `scripts/production-migrations.json` and every SQL file
it references into `/app/production-migrations.sql`. The rendered bundle embeds
the immutable image commit and the SHA-256 checksum of each migration. It does
not include repository source, Node.js or database credentials.

At container startup, `tdf-hq/production-entrypoint.sh`:

1. Rejects malformed flags and refuses to combine the reviewed runner with
   `RUN_MIGRATIONS=true`.
2. Connects using the same database URL or `DB_*`/`PG*` variables as the app.
3. Acquires the production migration advisory lock without waiting.
4. Creates or verifies `tdf_schema_migration`.
5. Rejects checksum changes to any previously applied migration.
6. Applies pending migrations in manifest order.
7. Runs the complete schema verification contract.
8. Releases the lock and starts the backend with `RUN_MIGRATIONS=false`.

If any migration or verification fails, `psql` exits nonzero and the backend is
never started. A concurrent runner also exits nonzero instead of executing the
same batch twice. Fly's rolling strategy and `max_unavailable = 1` preserve one
serving Machine while the candidate starts.

## Adding a migration

Applied migration files are byte-for-byte immutable. Never edit an applied SQL
file to repair a function, trigger, constraint, or data correction. Restore the
recorded content and add a new idempotent forward migration instead. The release
runner compares every applied checksum before executing any pending SQL and
fails closed on drift.

1. Add an idempotent SQL file under `tdf-hq/sql/` with a transaction and
   explicit safety checks.
2. Add it to `scripts/production-migrations.json` with an immutable
   `introducedBy` commit.
3. Extend the schema verification contract and rollback documentation.
4. Run:

   ```bash
   npm run test:production-release
   npm run test:ci-pipeline
   npm run audit:catalog-lists
   bash scripts/test-automatic-migrations-production-schema.sh
   ```

The PostgreSQL integration test restores the production-shaped fixture, starts
the real entrypoint, verifies the complete ledger and schema, starts it again,
and requires an identical schema fingerprint after the second run.

The 2026-08-25 commerce and distribution row-binding compatibility migrations
are the forward repair for trigger definitions that had previously been edited
inside applied 2026-08-13 files. Their rollback files restore the historical
definitions and must be used only after freezing the corresponding writes and
rolling back the application. The music-directory profile-image migration is
also restored to its recorded bytes; its already-registered host-compatibility
migration remains the forward correction.

## Rollout and rollback

The guarded release lane still performs backup, dry-run, preflight and explicit
schema application before the canary. The startup runner is therefore a
checksum-verified no-op during a normal release, while also protecting restarts
and deployments that discover an unapplied registered migration.

Rollback remains migration-specific. Never remove ledger rows, aliases,
catalog data or audit history to roll back. Restore the recorded snapshot when
the migration's documented reverse procedure cannot safely preserve data. Keep
automatic publication disabled during migration and rollback.
