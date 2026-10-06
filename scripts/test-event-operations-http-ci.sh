#!/bin/sh
set -eu
# Fixed GitHub service endpoint and fresh database only. No arbitrary external DSN or drop command.
test "${GITHUB_ACTIONS:-}" = true || { echo 'This runner requires the CI service container' >&2; exit 1; }
export PGHOST=postgres PGPORT=5432 PGUSER=postgres PGPASSWORD=postgres
export PGDATABASE=tdf_event_operations_http_test
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
createdb "$PGDATABASE"
for fixture in \
  tdf-hq/test/integration/event_operations_foundation_fixture.sql \
  tdf-hq/sql/2026-09-14_event_operations_foundation.sql \
  tdf-hq/sql/2026-09-14_event_operations_api.sql \
  tdf-hq/sql/2026-09-14_event_task_commit.sql \
  tdf-hq/sql/2026-09-14_event_task_read.sql \
  tdf-hq/sql/2026-09-15_event_task_revision.sql \
  tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql \
  tdf-hq/sql/2026-09-15_event_raci_reassignment.sql \
  tdf-hq/sql/2026-09-15_event_raci_editor_context.sql \
  tdf-hq/sql/2026-09-16_event_task_completion.sql \
  tdf-hq/test/integration/event_operations_http_fixture.sql; do
  psql -X -v ON_ERROR_STOP=1 -f "$repo_root/$fixture" >/dev/null
done
EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST=1 \
EVENT_OPERATIONS_TEST_DSN='host=postgres port=5432 user=postgres password=postgres dbname=tdf_event_operations_http_test' \
  sh "$repo_root/scripts/run-event-operations-http-harness.sh"
