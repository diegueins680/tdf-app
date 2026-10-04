#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
# Resolve and freeze a local Docker endpoint; never allocate fixtures on a remote context.
if [ -n "${DOCKER_CONTEXT:-}" ] || [ -z "${DOCKER_HOST:-}" ]; then
  test_docker_endpoint=$(docker context inspect "$(docker context show)" --format '{{.Endpoints.docker.Host}}')
else
  test_docker_endpoint=$DOCKER_HOST
fi
case "$test_docker_endpoint" in
  unix:///*) ;;
  *) echo 'A local Unix-socket Docker endpoint is required' >&2; exit 1 ;;
esac
export DOCKER_HOST="$test_docker_endpoint"
unset DOCKER_CONTEXT
test_container_id=''
cleanup() {
  test_exit_code=$?
  trap - EXIT
  if [ -n "$test_container_id" ]; then
    if docker rm -f "$test_container_id" >/dev/null; then
      echo "Removed owned disposable database container: $test_container_id"
    else
      echo "Failed to remove owned test container: $test_container_id" >&2
      test_exit_code=1
    fi
  fi
  exit "$test_exit_code"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
# Compile before allocating a database; no application environment or .env is sourced.
cd "$repo_root/tdf-hq"
test_dist=$(stack path --dist-dir)
test_autogen="$test_dist/build/tdf-hq-exe/autogen"
test -f "$test_autogen/Paths_tdf_hq.hs" || {
  echo 'Build the canonical backend first: cd tdf-hq && stack build tdf-hq:exe:tdf-hq-exe' >&2
  exit 1
}
mkdir -p .stack-work/event-raci-browser
stack exec -- ghc -O0 -threaded -isrc -itest -i"$test_autogen" -outputdir .stack-work/event-raci-browser \
  test/EventRaciBrowserMain.hs -o .stack-work/event-raci-browser/event-raci-browser
test_database=tdf_event_raci_browser_test
test_container_id=$(docker run --rm -d -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=event-browser-test-only -e POSTGRES_DB="$test_database" postgres:16-alpine)
attempt=0
until docker exec "$test_container_id" psql -h 127.0.0.1 -X -U postgres -d "$test_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then echo 'Disposable browser database did not become ready' >&2; exit 1; fi
  sleep 1
done
for fixture in \
  test/integration/event_operations_foundation_fixture.sql \
  sql/2026-09-14_event_operations_foundation.sql \
  sql/2026-09-14_event_operations_api.sql \
  sql/2026-09-14_event_task_commit.sql \
  sql/2026-09-14_event_task_read.sql \
  sql/2026-09-15_event_task_revision.sql \
  sql/2026-09-15_event_task_revisioned_read.sql \
  sql/2026-09-15_event_raci_reassignment.sql \
  sql/2026-09-15_event_raci_editor_context.sql \
  test/integration/event_operations_http_fixture.sql \
  test/integration/event_raci_browser_fixture.sql; do
  docker exec -i "$test_container_id" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" < "$fixture" >/dev/null
done
test_port=$(docker port "$test_container_id" 5432/tcp)
test_port=${test_port##*:}
EVENT_RACI_DISPOSABLE_BROWSER_TEST=1 \
EVENT_RACI_TEST_CONTAINER="$test_container_id" \
EVENT_RACI_BROWSER_RUNNER="$repo_root/scripts/run-event-raci-browser.mjs" \
EVENT_OPERATIONS_TEST_DSN="host=127.0.0.1 port=$test_port user=postgres password=event-browser-test-only dbname=$test_database" \
  .stack-work/event-raci-browser/event-raci-browser
echo 'Real RACI session/API/PostgreSQL browser verification passed.'
