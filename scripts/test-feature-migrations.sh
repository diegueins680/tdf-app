#!/bin/sh
set -eu

TDF_MIGRATION_CONTAINER="tdf-feature-migration-test-$$"
TDF_MIGRATION_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_MIGRATION_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_MIGRATION_CONTAINER" \
  -e POSTGRES_PASSWORD=feature-migration-test \
  -e POSTGRES_DB=tdf_feature_migration_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker logs "$TDF_MIGRATION_CONTAINER" 2>&1 | grep -q 'PostgreSQL init process complete; ready for start up.'; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "PostgreSQL migration test container did not complete initialization" >&2
    exit 1
  fi
  sleep 1
done

# The official image briefly exposes an initialization server and then restarts
# PostgreSQL. Waiting for the init-complete marker prevents pg_isready from
# racing that intentional shutdown.
attempt=0
until docker exec "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test -Atc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "PostgreSQL migration test database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test "$@"
}

psql_exec -c 'CREATE TABLE party (id BIGSERIAL PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party DEFAULT VALUES;' >/dev/null
docker exec -i "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test \
  < "$TDF_MIGRATION_ROOT/tdf-hq/sql/2026-08-06_feature_access_requests.sql" >/dev/null
docker exec -i "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test \
  < "$TDF_MIGRATION_ROOT/tdf-hq/sql/2026-08-06_feature_navigation_preferences.sql" >/dev/null

psql_exec -c "INSERT INTO feature_access_requests (requester_party_id, feature_id, action, role_context, module_context, status, reviewer_group, requested_at, updated_at) VALUES (1, 'crm.contacts', 'view', '[]', '[]', 'pending', 'crm-reviewers', now(), now());" >/dev/null
if psql_exec -c "INSERT INTO feature_access_requests (requester_party_id, feature_id, action, role_context, module_context, status, reviewer_group, requested_at, updated_at) VALUES (1, 'crm.contacts', 'view', '[]', '[]', 'pending', 'crm-reviewers', now(), now());" >/dev/null 2>&1; then
  echo "Duplicate pending access-request constraint did not reject a duplicate" >&2
  exit 1
fi
psql_exec -c "INSERT INTO feature_navigation_preferences (party_id, feature_id, favorite, pinned, pin_order, use_count, updated_at) VALUES (1, 'crm.contacts', true, true, 0, 1, now());" >/dev/null
if psql_exec -c "INSERT INTO feature_navigation_preferences (party_id, feature_id, favorite, pinned, pin_order, use_count, updated_at) VALUES (1, 'crm.contacts', false, false, null, 0, now());" >/dev/null 2>&1; then
  echo "Navigation preference uniqueness constraint did not reject a duplicate" >&2
  exit 1
fi

psql_exec -c 'DROP TABLE feature_access_request_history; DROP TABLE feature_access_requests; DROP TABLE feature_navigation_preferences;' >/dev/null
remaining=$(psql_exec -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public' AND table_name IN ('feature_access_requests', 'feature_access_request_history', 'feature_navigation_preferences');")
if [ "$remaining" != "0" ]; then
  echo "Feature migration rollback left tables behind" >&2
  exit 1
fi

docker exec -i "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test \
  < "$TDF_MIGRATION_ROOT/tdf-hq/sql/2026-08-06_feature_access_requests.sql" >/dev/null
docker exec -i "$TDF_MIGRATION_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_feature_migration_test \
  < "$TDF_MIGRATION_ROOT/tdf-hq/sql/2026-08-06_feature_navigation_preferences.sql" >/dev/null

echo "Feature migrations passed forward, constraint, rollback, and reapply checks."
