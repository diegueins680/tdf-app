#!/bin/sh
set -eu

onboarding_container="tdf-onboarding-progress-test-$$"
onboarding_database="tdf_onboarding_progress_test"
onboarding_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$onboarding_root/tdf-hq/sql/2026-09-06_user_onboarding_progress.sql"
rollback_migration="$onboarding_root/tdf-hq/sql/2026-09-06_user_onboarding_progress_rollback.sql"

cleanup() {
  docker rm -f "$onboarding_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$onboarding_container" \
  -e POSTGRES_PASSWORD=onboarding-progress-test \
  -e POSTGRES_DB="$onboarding_database" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$onboarding_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$onboarding_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$onboarding_container" >&2
    echo "PostgreSQL onboarding migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$onboarding_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$onboarding_database" "$@"
}

apply_sql() {
  docker exec -i "$onboarding_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$onboarding_database" < "$1" >/dev/null
}

psql_exec -c 'CREATE TABLE party (id BIGSERIAL PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party DEFAULT VALUES; INSERT INTO party DEFAULT VALUES;' >/dev/null

apply_sql "$up_migration"
apply_sql "$up_migration"

constraint_count=$(psql_exec -qAt -c "SELECT count(*) FROM pg_constraint WHERE conrelid='public.user_onboarding_progress'::regclass AND convalidated AND contype IN ('f','u','c');")
test "$constraint_count" = "8"
index_count=$(psql_exec -qAt -c "SELECT count(*) FROM pg_indexes WHERE schemaname='public' AND indexname='user_onboarding_progress_eligible_idx';")
test "$index_count" = "1"

psql_exec -c "INSERT INTO user_onboarding_progress (party_id, signup_completed_at, intent) VALUES (1, '2026-09-06T10:00:00Z', 'follow_artists');" >/dev/null
psql_exec -c "UPDATE user_onboarding_progress SET completed_at='2026-09-06T10:05:00Z', first_value='artist_followed', first_value_completed_at='2026-09-06T10:05:00Z' WHERE party_id=1;" >/dev/null

if psql_exec -c "INSERT INTO user_onboarding_progress (party_id) VALUES (1);" >/dev/null 2>&1; then
  echo "expected duplicate party onboarding progress to be rejected" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO user_onboarding_progress (party_id, intent) VALUES (2, 'administrator');" >/dev/null 2>&1; then
  echo "expected unsupported onboarding intent to be rejected" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO user_onboarding_progress (party_id, first_value, first_value_completed_at) VALUES (2, 'artist_followed', now());" >/dev/null 2>&1; then
  echo "expected first value without onboarding completion to be rejected" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO user_onboarding_progress (party_id, signup_completed_at, completed_at) VALUES (2, '2026-09-06T10:00:00Z', '2026-09-06T09:59:00Z');" >/dev/null 2>&1; then
  echo "expected completion before signup to be rejected" >&2
  exit 1
fi

psql_exec -c 'DELETE FROM party WHERE id=1;' >/dev/null
remaining=$(psql_exec -qAt -c 'SELECT count(*) FROM user_onboarding_progress WHERE party_id=1;')
if [ "$remaining" != "0" ]; then
  echo "expected party deletion to cascade to onboarding progress" >&2
  exit 1
fi

psql_exec -c "INSERT INTO user_onboarding_progress (party_id, signup_completed_at, intent) VALUES (2, '2026-09-06T11:00:00Z', 'events');" >/dev/null
apply_sql "$rollback_migration"
apply_sql "$rollback_migration"
remaining=$(psql_exec -qAt -c "SELECT count(*) FROM user_onboarding_progress WHERE party_id=2 AND intent='events';")
if [ "$remaining" != "1" ]; then
  echo "onboarding progress rollback did not preserve durable account history" >&2
  exit 1
fi

apply_sql "$up_migration"

echo "User onboarding progress migration passed forward, idempotency, constraint, cascade, rollback, and reapply checks."
