# Canonical catalog production schema cutover

## Purpose

The production application already contains the typed catalog runtime, but the
production database predates its Persistent migrations. This release closes
that gap without enabling startup migrations or accepting legacy string writes.

The authoritative production manifest applies these stages in order:

1. `2026-08-14_catalog_canonical_schema.sql` creates the specialized catalog,
   workflow, security, reference, Records, CMS, and DDEX tables.
2. `2026-08-14_catalog_consumer_expand.sql` adds nullable UUID references and
   typed consumer relations to the production-shaped schema.
3. `2026-08-14_catalog_integrity.sql` installs the same constraints and trigger
   functions used by the Haskell runtime.
4. `2026-08-14_catalog_foundation_seed.sql` inserts the deterministic bilingual
   foundation data with dependency-ordered, duplicate-safe inserts.
5. The reviewed catalog backfills and cutovers run in dependency order. They
   preserve per-value evidence, stop on ambiguity or safety-threshold breaches,
   populate canonical IDs, and disable legacy string/slug writes.

The migration batch supplies its immutable release SHA as
`candidate_revision`. Every manifest entry is ledgered with its SHA-256 digest,
and a changed applied migration is rejected.

## Reproducibility

The canonical schema originates from the four Persistent migrations in
`TDF.Catalog.Models`. `GenCatalogMigration.hs` renders them and
`wrap-catalog-canonical-schema.mjs` adds the fail-closed transactional wrapper.
`GenCatalogIntegrity.hs` renders `catalogIntegrityStatements`. The foundation
seed is produced by running `SeedCatalogFoundation.hs` in an empty canonical
database, exporting only the allowlisted seed tables, and ordering that
data-only dump with `order-catalog-foundation-seed.mjs`.

The checked-in CI baseline is a schema-only, no-row dump. It intentionally
contains column names such as `password_hash` and `token`, but contains no
credentials, hashes, tokens, business rows, or personal data.

## Verification

CI restores `scripts/__tests__/fixtures/production-schema-20260814.sql` into
PostgreSQL 17 with pgvector and then:

- runs the read-only production preflight;
- applies the full authoritative manifest twice;
- requires every ledger entry and checksum to match;
- runs all 14 dry-runs and raw apply scripts;
- runs every apply script a second time with the same revision;
- checks the catalog foundation minimums and canonical UUID columns;
- checks that every non-dry-run backfill completed;
- checks that DDEX legacy values were cleared and canonical IDs were populated;
- reruns the complete release schema verifier and preflight; and
- proves that an altered applied checksum is rejected.

Local command after restoring the fixture and applying the four foundation
migrations:

```bash
TDF_CATALOG_TEST_DATABASE_URL='postgresql://postgres:postgres@127.0.0.1:55441/tdf_catalog_cutover_verify' \
TDF_CATALOG_TEST_REVISION='catalog-local-integration-v1' \
scripts/test-catalog-production-cutover.sh
```

The integration helper refuses every non-localhost database URL.

## Production sequence

1. Confirm Juan's independent emergency-administrator login; being assigned a
   role is not sufficient evidence of recoverability.
2. Capture a fresh Fly volume snapshot and record its identifier without
   credentials.
3. Export aggregate before-counts and run the inventory against a restored copy.
4. Build all applications from the exact candidate SHA and require CI to pass.
5. Run the release tool's read-only preflight and security gate.
6. Apply the ledgered manifest once. Any missing relation, ambiguity,
   incomplete seed, invalid FK, legacy DDEX value, or checksum mismatch aborts.
7. Run the post-migration security gate and schema verification.
8. Deploy one Fly Machine as canary, verify health and representative catalog
   reads/writes, then roll the remaining Machine.
9. Deploy the matching Cloudflare assets and verify web/mobile contract parity.
10. Record after-counts, migration run IDs, image digest, health results, and
    rollback boundary in the release evidence.

## Rollback

Prefer application rollback while leaving additive canonical tables intact.
This preserves aliases, audit history, mappings, and migrated entities. If the
cutover data itself must be reversed, quiesce writers, deploy the compatible
pre-cutover application, and run the reviewed `*_rollback.sql` files in reverse
cutover order. Each rollback refuses post-cutover writes or changed canonical
values; a refusal requires forward repair or restoration of the verified
snapshot, never forced deletion.

Do not drop canonical tables, audit records, translations, aliases, mapping
evidence, or catalog items automatically. The four foundation migrations are
additive and have no destructive automatic rollback.

Automatic stop conditions include unexpected counts, duplicates, unresolved
identity conflicts, broken FKs, an incomplete backfill, administrator lockout,
privilege escalation, schema/client disagreement, and material latency or
offline-synchronization regression.
