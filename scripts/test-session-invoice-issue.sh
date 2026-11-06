#!/bin/sh
# PAY-INVOICE-ISSUE-001 on PostgreSQL: the advisory lock runs through the query
# API and, of two concurrent new keys for the same invoice request, exactly one
# reserves. SQLite cannot exercise either, so these run against a disposable
# database that the spec recreates its table in.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database=tdf_session_issue_test
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
dropdb --if-exists "$test_database" >/dev/null
createdb "$test_database"
trap cleanup EXIT INT TERM
export TDF_SESSION_ISSUE_TEST_DB="host=$PGHOST port=${PGPORT:-5432} user=${PGUSER:-postgres} password=${PGPASSWORD:-postgres} dbname=$test_database"
cd "$repo_root/tdf-hq"
output=$(stack test ${TDF_STACK_FLAGS:-} tdf-hq:test:tdf-hq-test --jobs 1 \
  --test-arguments='--match "Session invoice issuance on PostgreSQL" --fail-on=pending' 2>&1) || {
  printf '%s\n' "$output"; exit 1; }
printf '%s\n' "$output"
# A pending example means the database was not configured; a silent pass with
# no examples would not prove the lock; require a positive count.
printf '%s\n' "$output" | grep -Eq '^[1-9][0-9]* examples?, 0 failures' || {
  echo "session invoice PostgreSQL examples did not run" >&2; exit 1; }
