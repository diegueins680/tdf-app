#!/usr/bin/env bash
# Verifies canonical directory preview images and artist-profile derived
# listings on the production schema: forward migration (twice), behavior
# assertions, real two-session concurrency, backfill (twice), rollback and
# re-apply. Requires an isolated, empty PostgreSQL database.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
database_url="${TDF_ARTIST_LISTINGS_TEST_DATABASE_URL:?Set TDF_ARTIST_LISTINGS_TEST_DATABASE_URL to an isolated empty PostgreSQL database}"
work_dir="$(mktemp -d "${TMPDIR:-/tmp}/tdf-artist-listings.XXXXXX")"
trap 'rm -rf "${work_dir}"' EXIT INT TERM

sql_dir="${repo_root}/tdf-hq/sql"
test_dir="${repo_root}/tdf-hq/test/integration"
migration="${sql_dir}/2026-10-07_directory_artist_derived_listings.sql"
rollback="${sql_dir}/2026-10-07_directory_artist_derived_listings_rollback.sql"
backfill="${sql_dir}/2026-10-07_directory_artist_listing_backfill_apply.sql"
dry_run="${sql_dir}/2026-10-07_directory_artist_listing_backfill_dry_run.sql"

run_sql() {
  psql "${database_url}" -X -q -v ON_ERROR_STOP=1 "$@" >/dev/null
}
query() {
  psql "${database_url}" -X -qAt -v ON_ERROR_STOP=1 -c "$1"
}

existing_tables="$(query "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p');")"
if [ "${existing_tables}" != "0" ]; then
  echo "Artist listing test requires an empty isolated database; found ${existing_tables} tables" >&2
  exit 1
fi

# Production-shaped base: the 2026-08-14 production schema plus every
# registered production migration that precedes this change.
run_sql -f "${repo_root}/scripts/__tests__/fixtures/production-schema-20260814.sql"
run_sql -f "${repo_root}/scripts/__tests__/fixtures/catalog-production-source-fixture.sql"
SOURCE_COMMIT="0000000000000000000000000000000000000000" \
  node "${repo_root}/scripts/render-production-migration-batch.mjs" > "${work_dir}/batch.sql"
run_sql -f "${work_dir}/batch.sql"

# The manifest already includes this change; re-applying proves idempotence.
run_sql -f "${migration}"
run_sql -f "${migration}"
run_sql -f "${test_dir}/directory_artist_derived_listings_postgres.sql"

# Two sessions race to create the same profile's listing.
race_profile="$(query "ALTER TABLE directory_profile DISABLE TRIGGER directory_profile_artist_listing_sync_trigger;
  INSERT INTO directory_profile (subject_party_id, profile_kind, public_name, slug, bio, profile_status, visibility,
    moderation_status, completeness_score, published_at)
  VALUES (910001,'artist','Race Artist','race-artist','Artista para la prueba de concurrencia real.','published',
    'public','allowed',.8,now()) RETURNING id;
  ALTER TABLE directory_profile ENABLE TRIGGER directory_profile_artist_listing_sync_trigger;" | grep -E '^[0-9a-f-]{36}$')"
psql "${database_url}" -X -q -v ON_ERROR_STOP=1 \
  -c "BEGIN; SELECT directory_sync_profile_listing('${race_profile}'); SELECT pg_sleep(2); COMMIT;" \
  >/dev/null &
first_session=$!
sleep 0.5
second_listing="$(query "SELECT directory_sync_profile_listing('${race_profile}');")"
wait "${first_session}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id='${race_profile}';")" = "1"
test "$(query "SELECT id FROM classified WHERE source_profile_id='${race_profile}';")" = "${second_listing}"

run_sql -f "${dry_run}"
run_sql -f "${backfill}"
run_sql -f "${backfill}"
run_sql -f "${test_dir}/directory_artist_listing_backfill_assertions_postgres.sql"

# Rollback is non-destructive and repeatable; forward re-apply resumes listings.
published_before="$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")"
run_sql -f "${rollback}"
run_sql -f "${rollback}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")" = "0"
run_sql -f "${migration}"
run_sql -f "${backfill}"
test "$(query "SELECT count(*) FROM classified WHERE source_profile_id IS NOT NULL AND status='published';")" = "${published_before}"

echo "Directory artist listings: migration, behavior, concurrency, backfill and rollback verified"
