#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
# The existing helper creates this database before owning its cleanup. It never
# adopts, drops or resets an existing caller database.
PGPORT=5432
export PGPORT
batch=
cases=
cleanup_native_import() {
  result=$1
  trap - EXIT INT TERM
  if test -n "$batch" && ! rm -f "$batch"; then
    test "$result" -ne 0 || result=1
  fi
  if test -n "$cases" && ! rm -f "$cases"; then
    test "$result" -ne 0 || result=1
  fi
  if ! tdf_test_db_cleanup; then
    test "$result" -ne 0 || result=1
  fi
  exit "$result"
}
tdf_test_db_init tdf_native_session_import_test
trap 'cleanup_native_import $?' EXIT
trap 'cleanup_native_import 130' INT
trap 'cleanup_native_import 143' TERM
batch=$(mktemp)
cases=$(mktemp)
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" > "$batch"
tdf_test_psql -f "$batch" >/dev/null
expected_rows=$(node -e 'process.stdout.write(String(require(process.argv[1]).migrations.length))' "$repo_root/scripts/production-migrations.json")
test "$expected_rows" -gt 0
actual_rows=$(tdf_test_psql -qAtc 'SELECT count(*) FROM tdf_schema_migration')
test "$actual_rows" = "$expected_rows"
printf 'PASS native canonical migration prefix %s\n' "$actual_rows"
python3 "$repo_root/scripts/test-native-session-import-sql.py" > "$cases"
tdf_test_psql -f "$cases"
printf '%s\n' 'PASS native session import: current canonical schema, correction, revisions, replay and identity guards'
