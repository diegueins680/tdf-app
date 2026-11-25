#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
if [ -n "${TDF_STORAGE_SECURITY_FULL_DATABASE_URL:-}" ]; then
  # The loader validates a disposable local/CI URI and refuses a nonempty DB.
  # Keep full-schema qualification separate from the older bounded fixture.
  TDF_TEST_DATABASE_URL=$TDF_STORAGE_SECURITY_FULL_DATABASE_URL
  sh "$repo_root/scripts/load-payment-test-schema.sh" "$TDF_TEST_DATABASE_URL"
  psql "$TDF_TEST_DATABASE_URL" -X -v ON_ERROR_STOP=1 \
    -f "$repo_root/tdf-hq/test/integration/content_storage_full_security_fixture.sql" >/dev/null
else
  . "$repo_root/scripts/lib/postgres-test-database.sh"
  tdf_test_db_init tdf_storage_security_test
  tdf_test_psql < "$repo_root/tdf-hq/test/integration/content_storage_security_fixture.sql" >/dev/null
  for migration in 2026-11-04_content_storage 2026-11-04_content_storage_drive 2026-11-04_content_storage_deletion 2026-11-04_content_storage_accounting 2026-11-04_content_storage_delete_capacity 2026-11-06_content_storage_security; do
    tdf_test_psql < "$repo_root/tdf-hq/sql/$migration.sql" >/dev/null
  done
  # This reduced fixture models the reference checked by storage deletion.
  tdf_test_psql -c 'CREATE TABLE directory_feed_image(file_id uuid REFERENCES content_storage_upload(id))' >/dev/null
  tdf_test_psql -c 'INSERT INTO content_storage_account(party_id,legacy_usage_verified) SELECT id,true FROM party' >/dev/null
fi
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
TDF_STORAGE_SECURITY_DATABASE_URL="$TDF_TEST_DATABASE_URL" "$test_binary" --match=storage-security --fail-on=empty
