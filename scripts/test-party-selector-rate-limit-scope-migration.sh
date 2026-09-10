#!/bin/sh
set -eu

test_container="tdf-party-selector-rate-limit-test-$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-10_party_selector_rate_limit_scopes.sql"
down_migration="$repo_root/tdf-hq/sql/2026-09-10_party_selector_rate_limit_scopes_rollback.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=party-selector-rate-limit-test \
  -e POSTGRES_DB=party_selector_rate_limit_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" \
  psql -v ON_ERROR_STOP=1 -U postgres -d party_selector_rate_limit_test -Atc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Party selector rate-limit migration database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d party_selector_rate_limit_test "$@"
}

apply_file() {
  docker exec -i "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d party_selector_rate_limit_test \
    < "$1" >/dev/null
}

psql_exec -c "CREATE TABLE directory_rate_limit (
  scope TEXT NOT NULL,
  subject_hash TEXT NOT NULL,
  window_started_at TIMESTAMPTZ NOT NULL,
  count INTEGER NOT NULL DEFAULT 1 CHECK (count > 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (scope, subject_hash, window_started_at),
  CONSTRAINT directory_rate_limit_scope_check CHECK (scope IN (
    'search', 'profile_create', 'classified_publish', 'application',
    'invitation', 'contact', 'report', 'review', 'experience-review'
  ))
);" >/dev/null
psql_exec -c "INSERT INTO directory_rate_limit (scope, subject_hash, window_started_at)
  VALUES ('search', 'existing', date_trunc('day', NOW()));" >/dev/null

apply_file "$up_migration"
apply_file "$up_migration"

psql_exec -c "INSERT INTO directory_rate_limit (scope, subject_hash, window_started_at)
  VALUES
    ('party_selector:event_invitation', 'invitation', date_trunc('day', NOW())),
    ('party_selector:social_connection', 'social', date_trunc('day', NOW()));" >/dev/null

if psql_exec -c "INSERT INTO directory_rate_limit (scope, subject_hash, window_started_at)
  VALUES ('unknown', 'unknown', date_trunc('day', NOW()));" >/dev/null 2>&1; then
  echo "Party selector rate-limit migration accepted an unknown scope" >&2
  exit 1
fi

if apply_file "$down_migration" 2>/dev/null; then
  echo "Party selector rate-limit rollback discarded live selector counters" >&2
  exit 1
fi

psql_exec -c "DELETE FROM directory_rate_limit WHERE scope LIKE 'party_selector:%';" >/dev/null
apply_file "$down_migration"
apply_file "$down_migration"

if psql_exec -c "INSERT INTO directory_rate_limit (scope, subject_hash, window_started_at)
  VALUES ('party_selector:social_connection', 'rolled-back', date_trunc('day', NOW()));" \
  >/dev/null 2>&1; then
  echo "Party selector rate-limit rollback left selector scopes enabled" >&2
  exit 1
fi

if [ "$(psql_exec -Atc "SELECT count(*) FROM directory_rate_limit WHERE scope='search';")" != "1" ]; then
  echo "Party selector rate-limit migration did not preserve existing counters" >&2
  exit 1
fi

psql_exec -c 'ALTER TABLE directory_rate_limit DROP CONSTRAINT directory_rate_limit_scope_check;' >/dev/null
if apply_file "$up_migration" 2>/dev/null; then
  echo "Party selector rate-limit migration accepted a missing compatibility constraint" >&2
  exit 1
fi
psql_exec -c "ALTER TABLE directory_rate_limit
  ADD CONSTRAINT directory_rate_limit_scope_check CHECK (scope = 'search');" >/dev/null
if apply_file "$up_migration" 2>/dev/null; then
  echo "Party selector rate-limit migration broadened a malformed compatibility constraint" >&2
  exit 1
fi

echo "Party selector rate-limit scope migration passed forward, idempotency,"
echo "allowlist, preservation, rollback-safety, and drift checks."
