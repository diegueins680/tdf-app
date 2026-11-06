#!/bin/sh
# PAY-INVOICE-ISSUE-001 on PostgreSQL: the advisory lock runs through the query
# API and, of two concurrent new keys for the same invoice request, exactly one
# reserves. SQLite cannot exercise either, so these run against a disposable
# database that the spec recreates its table in.
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
# Refuse libpq routing overrides before any database command. Credentials stay
# in environment variables, never interpolated into a connection string.
if [ -n "${PGHOSTADDR:-}" ] || [ -n "${PGSERVICE:-}" ] || [ -n "${PGSERVICEFILE:-}" ]; then
  echo "libpq routing overrides are forbidden in the invoice test runner" >&2
  exit 1
fi
export PGHOST="${PGHOST:-127.0.0.1}"
export PGPORT="${PGPORT:-5432}"
export PGUSER="${PGUSER:-postgres}"
export PGPASSWORD="${PGPASSWORD:-postgres}"
case "$PGHOST" in
  127.0.0.1|localhost) ;;
  postgres) [ "${CI:-}" = true ] || { echo "postgres host requires CI=true" >&2; exit 1; } ;;
  *) echo "Only isolated local/CI PostgreSQL hosts are allowed" >&2; exit 1 ;;
esac
case "$PGPORT" in
  *[!0-9]*|??????*) echo "Invalid PostgreSQL port" >&2; exit 1 ;;
esac
[ "$PGPORT" -ge 1 ] && [ "$PGPORT" -le 65535 ] || {
  echo "Invalid PostgreSQL port" >&2; exit 1; }
test_database=tdf_session_issue_test_$$
# Do not drop or reuse a pre-existing database. Claim cleanup only after this
# invocation successfully creates its own name; a failed create never drops.
createdb "$test_database"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
export TDF_SESSION_ISSUE_TEST_DB="dbname=$test_database"
cd "$repo_root/tdf-hq"
output=$(stack test ${TDF_STACK_FLAGS:-} tdf-hq:test:tdf-hq-test --jobs 1 \
  --test-arguments='--match "Session invoice issuance on PostgreSQL" --fail-on=pending' 2>&1) || {
  printf '%s\n' "$output"; exit 1; }
printf '%s\n' "$output"
# A pending example means the database was not configured; require both known
# PostgreSQL examples, allowing additional future cases. Preserve raw output
# above; normalize only ANSI SGR styling in the summary-verification stream.
summary_output=$(printf '%s\n' "$output" | sed "s/$(printf '\033')\[[0-9;]*m//g")
printf '%s\n' "$summary_output" | grep -Eq '^([2-9]|[1-9][0-9]+) examples, 0 failures$' || {
  echo "session invoice PostgreSQL examples did not run" >&2; exit 1; }
