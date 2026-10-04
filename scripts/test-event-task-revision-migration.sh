#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-task-revision.XXXXXX")
cleanup() {
  if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-task-revision \
  -e POSTGRES_PASSWORD=task-revision-test-only -e POSTGRES_DB=tdf_task_revision_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_task_revision_test "$@"
}
apply() { sql < "$repo_root/$1" >/dev/null; }
attempt=0
until sql -h 127.0.0.1 -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1)); test "$attempt" -lt 30 || exit 1
  sleep 1
done
wait_backend() {
  attempt=0
  until [ "$(sql -qAtc "SELECT count(*) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='$1' AND wait_event_type='$2'")" = 1 ]; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 100 ]; then echo "Missing barrier $1/$2; logs: $test_logs" >&2; exit 1; fi
    sleep 0.1
  done
}
start_coordinator() {
  sql -c "SET application_name='task_revision_coordinator';
    SELECT pg_advisory_lock(889,2); SELECT pg_sleep(60);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend task_revision_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='task_revision_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
up=tdf-hq/sql/2026-09-15_event_task_revision.sql
down=tdf-hq/sql/2026-09-15_event_task_revision_rollback.sql
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply tdf-hq/sql/2026-09-14_event_task_read.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_task_revision_assertions.sql
apply "$down"
apply "$down"
sql -qAtc "SELECT task_revision_test.check_that(
  to_regprocedure('event_operation_lock_task_revision(bigint,bigint,bigint)') IS NULL,
  'rollback removes guard'); SELECT task_revision_test.check_that(
  (SELECT revisions=(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t)
   FROM task_revision_test.preserved), 'rollback retains counters');" >/dev/null
# Passive tracking continues after rollback, so reapply cannot resurrect old versions.
old_revision=$(sql -qAtc 'SELECT task_revision_test.rev(100)')
sql -c "UPDATE event_operation_raci_assignment SET valid_from=valid_from-interval '1 day'
  WHERE activity_id=100 AND raci_role='responsible';" >/dev/null
test "$(sql -qAtc 'SELECT task_revision_test.rev(100)')" = "$((old_revision+1))"
apply "$up"
apply "$up"
test "$(sql -qAtc 'SELECT task_revision_test.rev(100)')" = "$((old_revision+1))"

# Observe both guarded commands and raw legacy RACI writers on the same fence.
# Two commands capture the SAME revision; the waiting command must reject it.
for writer_kind in guarded legacy; do
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  old_revision=$(sql -qAtc 'SELECT task_revision_test.rev(100)')
  if [ "$writer_kind" = guarded ]; then
    guard_statement="SELECT event_operation_lock_task_revision(10,100,$old_revision);"
  else
    guard_statement=''
  fi
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='task_revision_first';
    $guard_statement
    UPDATE event_operation_raci_assignment SET valid_from=valid_from-interval '1 day'
      WHERE activity_id=100 AND raci_role='responsible';
    SELECT pg_advisory_xact_lock(889,2); COMMIT;" > "$test_logs/first-$writer_kind-$isolation.log" 2>&1 &
  first_pid=$!
  wait_backend task_revision_first Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='task_revision_second';
    SELECT event_operation_lock_task_revision(10,100,$old_revision);
    UPDATE event_operation_raci_assignment SET valid_from=valid_from-interval '1 day'
      WHERE activity_id=100 AND raci_role='responsible'; COMMIT;" > "$test_logs/second-$writer_kind-$isolation.log" 2>&1 &
  second_pid=$!
  wait_backend task_revision_second Lock
  release_coordinator
  wait "$first_pid"
  if wait "$second_pid"; then echo "Stale $isolation command unexpectedly committed" >&2; exit 1; fi
  grep -q '40001' "$test_logs/second-$writer_kind-$isolation.log"
  test "$(sql -qAtc 'SELECT task_revision_test.rev(100)')" = "$((old_revision+1))"
done
done

# An aborted first command releases its fence and leaves the captured revision valid.
old_revision=$(sql -qAtc 'SELECT task_revision_test.rev(100)')
start_coordinator
sql -c "BEGIN; SET LOCAL application_name='task_revision_abort';
  SELECT event_operation_lock_task_revision(10,100,$old_revision);
  UPDATE event_logistics_activity SET status='in_progress' WHERE id=100;
  SELECT pg_advisory_xact_lock(889,2); ROLLBACK;" > "$test_logs/abort.log" 2>&1 &
first_pid=$!
wait_backend task_revision_abort Lock
sql -c "BEGIN; SET LOCAL application_name='task_revision_retry';
  SELECT event_operation_lock_task_revision(10,100,$old_revision);
  UPDATE event_logistics_activity SET status='in_progress' WHERE id=100; COMMIT;" > "$test_logs/retry.log" 2>&1 &
second_pid=$!
wait_backend task_revision_retry Lock
release_coordinator
wait "$first_pid"
wait "$second_pid"
test "$(sql -qAtc 'SELECT task_revision_test.rev(100)')" = "$((old_revision+1))"
echo 'Task revision PASS: tracked relations, ABA, no cross-task advance, invalid/opaque targets, overflow, rollback/reapply, RC/RR/Serializable guarded and legacy writer conflicts, aborted-command recovery.'
echo "Diagnostic logs: $test_logs"
