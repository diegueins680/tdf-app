#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-event-operations-http-test-$$"
test_database=tdf_event_operations_http_test
test_container_id=''
cleanup() {
  if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null 2>&1 || true; fi
}
trap cleanup EXIT INT TERM
test_container_id=$(docker run --rm -d --name "$test_container" -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=event-http-test-only -e POSTGRES_DB="$test_database" postgres:16-alpine)
attempt=0
until docker exec "$test_container" psql -h 127.0.0.1 -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then echo 'Disposable HTTP database did not become ready' >&2; exit 1; fi
  sleep 1
done
apply_sql() {
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" < "$1" >/dev/null
}
apply_sql "$repo_root/tdf-hq/test/integration/event_operations_foundation_fixture.sql"
apply_sql "$repo_root/tdf-hq/sql/2026-09-14_event_operations_foundation.sql"
apply_sql "$repo_root/tdf-hq/sql/2026-09-14_event_operations_api.sql"
apply_sql "$repo_root/tdf-hq/sql/2026-09-14_event_task_commit.sql"
apply_sql "$repo_root/tdf-hq/sql/2026-09-14_event_task_read.sql"
apply_sql "$repo_root/tdf-hq/test/integration/event_operations_http_fixture.sql"
test_port=$(docker port "$test_container" 5432/tcp)
test_port=${test_port##*:}
cd "$repo_root/tdf-hq"
EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST=1 \
EVENT_OPERATIONS_TEST_DSN="host=127.0.0.1 port=$test_port user=postgres password=event-http-test-only dbname=$test_database" \
  sh "$repo_root/scripts/run-event-operations-http-harness.sh"
echo 'Event operations production auth/subrouter HTTP tests passed against disposable PostgreSQL.'
