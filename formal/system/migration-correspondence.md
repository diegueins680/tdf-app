# Production migration correspondence (DEPLOY-MIGRATION-001)

Reconstructed on 2026-10-12 from `scripts/lib/migration-contract.mjs`
(`loadMigrationContract`, `reconcileMigrationLedger`),
`scripts/lib/hetzner-preparation.mjs` (`prepareHetznerRelease`),
`buildMigrationBatchSql` in `scripts/lib/production-release.mjs`,
`scripts/render-production-migration-batch.mjs` and
`tdf-hq/production-entrypoint.sh`. The requirement text was read against the
code, not the other way round.

## Two sides of one ledger

`tdf_schema_migration(migration_id PRIMARY KEY, checksum, source_commit,
applied_at)` records every applied manifest entry with the SHA-256 of its
expanded SQL and the candidate revision that applied it.

1. **Release preparation** (`reconcileMigrationLedger`): every ledger row
   must name a manifest entry once and carry that entry's checksum or one
   the manifest declares compatible; otherwise preparation throws
   (`Applied migration is absent from the candidate manifest`, `Applied
   migration history differs from the reviewed candidate`, duplicates,
   malformed checksums or revisions). The result keeps manifest order and
   marks each entry applied or pending; a hole (a pending entry before an
   applied one) is reported, not omitted. Preparation authorizes nothing.
2. **The batch** (`buildMigrationBatchSql`, rendered by
   `render-production-migration-batch.mjs` and run by the release and by
   `production-entrypoint.sh` with `AUTO_APPLY_PRODUCTION_MIGRATIONS=true`):
   one psql script under `pg_try_advisory_lock('tdf-production-schema-migrations')`.
   Per entry, in manifest order: raise if the ledger holds the id with a
   checksum the candidate does not allow; skip if applied; otherwise apply and
   record. Then the full schema verification runs. The batch does not look at
   ledger rows the manifest does not know: an older image rolled back over a
   newer ledger must still start (AUTHORITY-075 records this asymmetry with
   preparation).

## What was wrong

- **The ledger row was not atomic with the migration.** The batch ran the
  migration source (almost every file carries its own `BEGIN; ... COMMIT;`)
  and then inserted the ledger row as a separate statement. A process death
  or connection loss between the file's COMMIT and that insert left the
  effects applied and unrecorded; the next run re-applied the file, which
  fails for any non-idempotent statement (`already exists`) and needs a
  manual ledger repair. For the twelve manifest files without their own
  transaction a failure midway also left partial effects committed and
  unrecorded. Fixed: the batch opens a transaction, inserts the ledger row,
  then runs the file; a file's own `BEGIN` only warns and its `COMMIT` (or the
  batch's trailing one) commits row and effects together; a failure anywhere
  leaves neither. Precondition, pinned by a unit test over the expanded SQL of every
  registered entry (includes resolved): a file opens at most one transaction,
  ends with its COMMIT, never rolls back, and uses no statement that refuses a
  transaction block (`CONCURRENTLY`, `VACUUM`, `ALTER TYPE ... ADD VALUE`, ...).
  Applied entries are skipped, so the change affects pending entries only.
  `docs/automatic-production-migrations.md` already promised that a concurrent
  runner exits non-zero; the batch did not deliver it.
- **A held lock did not fail the batch.** The refusal branch ended with
  `\quit 3`; psql's `\quit` takes no argument (`warning: \quit: extra
  argument "3" ignored`) and the batch exited 0 after printing "refusing to
  wait", so a caller, including the entrypoint's "applied and schema
  verification passed" message, treated an active concurrent migration as
  success without applying or verifying anything. Reproduced with psql 16.
  Fixed: the branch raises an SQL error under `ON_ERROR_STOP`, which exits 3.

## Model

`formal/event-operations/MigrationLedger.tla`: three manifest entries, two
source versions, three batch runs, one runner (the lock is below the model).
Actions: `Start`, `Skip`, `Halt`, `Apply` (atomic or not), `Record`, `Crash`,
`Finish`, `ChangeSource` (with or without declaring the old checksum
compatible), `Rollback`/`Restore` of an older image, `Prepare`. Invariants:
`LedgerImpliesEffects` (sanity, holds by construction), `EffectsRecorded` (effects are recorded except while
a non-atomic run is writing the row), `SingleApplication` (an entry's SQL runs at most once; after a crash under the
previous layout the next run re-applies it), `NoRunPastDrift`
(a run never walks past an applied entry whose recorded checksum is not
allowed), `PreparationSound`. Positive run: 1,282,056 states, 334,720
distinct, depth 26. Controls: `RecordAfterCommit` (the previous layout; a
crash leaves effects unrecorded), `BatchUnchecked`, `PrepareUnchecked`,
`UnknownAccepted`, each failing on the named invariant.

## Evidence

- `scripts/__tests__/migration-batch-postgres.mjs` (CI step "Verify the
  production migration batch, ledger atomicity and lock refusal"): the
  rendered batch run through psql against the migrated fixture with synthetic
  migrations in their own schema: fresh application in order with bound ledger
  rows, idempotent re-run, checksum mismatch fails closed, compatible checksum
  accepted keeping the recorded one, unknown ledger row tolerated by the batch
  and rejected by preparation, a hole applied after the later entry and
  reported pending by preparation, atomic failure for a file with and without
  its own transaction (the previous layout as a negative control leaves the
  partial table unrecorded), the session dying right after a file's COMMIT
  (ledger row kept with the atomic layout, lost with the previous one),
  unrecorded effects re-applied (idempotent file
  recovers, strict file needs a forward repair), a held lock exits 3 before
  touching anything, ledger shape constraints.
- `scripts/__tests__/production-release.test.mjs`: the apply block's order
  (BEGIN, ledger row, source, COMMIT), the lock refusal shape, and the
  manifest precondition over every registered entry's expanded SQL.
- `scripts/__tests__/production-entrypoint.test.mjs`: a psql status 3 stops
  the entrypoint before the success line and the server start.
- `scripts/test-automatic-migrations-production-schema.sh` (CI, and run
  locally on 2026-10-12): all 202 registered files applied on an empty
  database through the atomic wrapper, verified, re-run idempotent.
- `scripts/__tests__/hetzner-preparation.test.mjs`: preparation negative
  controls (unchanged).

## Bounds and ambiguities

- The model does not refine psql; a file's own `COMMIT` inside the batch
  transaction is the real mechanism and is tested, not modelled.
- Holes are applied after later entries (manifest order is the application
  order of pending work, not a prefix property); independent migrations are
  assumed. The seven open PRs that each add manifest entry 203 produce holes
  only if an entry is inserted before one already applied, which the
  manifest's date-ordered ids make unlikely but do not prevent (several open
  PRs on 2026-10-12 each append an entry 203; appending keeps the prefix).
- A migration whose effects exist without a ledger row (a crash under the old
  layout, or a manual application) still needs a forward repair when its SQL
  is not idempotent; the atomic row prevents new occurrences, it does not
  repair old ones. Entries production has not applied yet are applied with the
  atomic row.
- Two concurrent batches are excluded by the advisory lock; with the atomic
  row the ledger's primary key would also roll back a second application.
