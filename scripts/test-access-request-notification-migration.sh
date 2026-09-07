#!/bin/sh
set -eu

test_container="tdf-access-request-notification-test-$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-04_access_request_notification_types.sql"
down_migration="$repo_root/tdf-hq/sql/2026-09-04_access_request_notification_types_rollback.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=access-request-notification-test \
  -e POSTGRES_DB=access_request_notification_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" \
  psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test -Atc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Access-request notification migration database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test "$@"
}

insert_notification() {
  notification_type="$1"
  psql_exec -c "INSERT INTO notification (recipient_party_id, notif_type, title, body)
    VALUES (1, '$notification_type', 'test', 'test');" >/dev/null
}

apply_file() {
  docker exec -i "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d access_request_notification_test \
    < "$1" >/dev/null
}

psql_exec -c 'CREATE TABLE party (id BIGSERIAL PRIMARY KEY);' >/dev/null
psql_exec -c 'INSERT INTO party DEFAULT VALUES;' >/dev/null
apply_file "$repo_root/tdf-hq/sql/2026-07-12_notification_table.sql"
apply_file "$up_migration"
apply_file "$up_migration"

for notification_type in \
  reaction_received post_trending weekly_top artist_liked \
  access_request_submitted access_request_review access_request_decided; do
  insert_notification "$notification_type"
done

if insert_notification unknown_type 2>/dev/null; then
  echo "Expanded notification constraint accepted an unknown type" >&2
  exit 1
fi

if apply_file "$down_migration" 2>/dev/null; then
  echo "Rollback discarded access-request notification compatibility with live rows present" >&2
  exit 1
fi

psql_exec -c "DELETE FROM notification WHERE notif_type LIKE 'access_request_%';" >/dev/null
apply_file "$down_migration"
apply_file "$down_migration"

if insert_notification access_request_submitted 2>/dev/null; then
  echo "Rollback left access-request notification types enabled" >&2
  exit 1
fi

apply_file "$up_migration"
apply_file "$up_migration"
for notification_type in access_request_submitted access_request_review access_request_decided; do
  insert_notification "$notification_type"
done

access_request_rows=$(psql_exec -Atc \
  "SELECT count(*) FROM notification WHERE notif_type LIKE 'access_request_%';")
if [ "$access_request_rows" != "3" ]; then
  echo "Expected all three access-request notification types to persist" >&2
  exit 1
fi

echo "Access-request notification migration passed forward, idempotency,"
echo "constraint, rollback-safety, and reapply checks."
