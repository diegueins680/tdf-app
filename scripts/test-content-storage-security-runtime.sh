#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_storage_security_test
tdf_test_psql < "$repo_root/tdf-hq/test/integration/content_storage_security_fixture.sql" >/dev/null
for migration in 2026-11-04_content_storage 2026-11-04_content_storage_drive 2026-11-04_content_storage_deletion 2026-11-04_content_storage_accounting 2026-11-04_content_storage_delete_capacity 2026-11-06_content_storage_security; do
  tdf_test_psql < "$repo_root/tdf-hq/sql/$migration.sql" >/dev/null
done
tdf_test_psql -c 'INSERT INTO content_storage_account(party_id,legacy_usage_verified) SELECT id,true FROM party' >/dev/null
cd "$repo_root/tdf-hq"
test_binary="$(stack path --dist-dir)/build/tdf-hq-test/tdf-hq-test"
TDF_STORAGE_SECURITY_DATABASE_URL="$TDF_TEST_DATABASE_URL" "$test_binary" --match=storage-security --fail-on=empty
