#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-task-revision-read.XXXXXX")
cleanup() {
  if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-task-revisioned-read \
  -e POSTGRES_PASSWORD=revision-read-test-only -e POSTGRES_DB=tdf_task_revision_read_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning -c statement_timeout=20000' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_task_revision_read_test "$@"
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
assert_blocked_by() {
  sql -qAtc "SELECT task_revision_read_test.check_that(EXISTS(
    SELECT 1 FROM pg_stat_activity holder, pg_stat_activity waiter
    WHERE holder.datname=current_database() AND waiter.datname=current_database()
      AND holder.application_name='$1' AND waiter.application_name='$2'
      AND holder.pid=ANY(pg_blocking_pids(waiter.pid))), 'observed exact lock blocker');" >/dev/null
}
start_coordinator() {
  sql -c "SET statement_timeout='60s'; SET application_name='revision_read_coordinator';
    SELECT pg_advisory_lock(889,3); SELECT pg_sleep(50);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend revision_read_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='revision_read_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
up=tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql
down=tdf-hq/sql/2026-09-15_event_task_revisioned_read_rollback.sql
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply tdf-hq/sql/2026-09-14_event_task_read.sql
apply tdf-hq/sql/2026-09-15_event_task_revision.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_task_revisioned_read_assertions.sql
apply "$down"
apply "$down"
sql -qAtc "SELECT task_revision_read_test.check_that(
  to_regprocedure('event_operation_read_task_with_revision(bigint,bigint,bigint)') IS NULL
  AND event_operation_read_task(10,100,1) IS NOT NULL
  AND (SELECT snapshot=task_revision_read_test.rows() FROM task_revision_read_test.preserved),
  'rollback preserves old reads and all domain records');" >/dev/null
apply "$up"
sql -qAtc "SELECT task_revision_read_test.check_that(
  event_operation_read_task_with_revision(10,100,1)->>'aggregateRevision'='4'
  AND (SELECT snapshot=task_revision_read_test.rows() FROM task_revision_read_test.preserved),
  'reapply restores envelope without resetting data');" >/dev/null

for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  sql -c 'UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=100;' >/dev/null
  old_revision=$(sql -qAtc 'SELECT revision FROM event_operation_task_revision WHERE activity_id=100')
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='revision_read_writer';
    UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
      valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=100 AND raci_role='responsible';
    SELECT pg_advisory_xact_lock(889,3); COMMIT;" > "$test_logs/writer-$isolation.log" 2>&1 &
  writer_pid=$!
  wait_backend revision_read_writer Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='revision_read_waiter';
    WITH result AS MATERIALIZED (SELECT event_operation_read_task_with_revision(10,100,1) AS value)
    SELECT task_revision_read_test.check_that(value->>'aggregateRevision'='$((old_revision+1))'
      AND jsonb_array_length(value->'task'->'raci')=1
      AND value->'task'->'accountabilityNeedsAttention'='true'::JSONB, 'post-wait coherent task and revision') FROM result;
    COMMIT;" > "$test_logs/read-$isolation.log" 2>&1 &
  reader_pid=$!
  wait_backend revision_read_waiter Lock
  assert_blocked_by revision_read_writer revision_read_waiter
  release_coordinator
  wait "$writer_pid"
  if [ "$isolation" = 'READ COMMITTED' ]; then
    wait "$reader_pid"
  else
    if wait "$reader_pid"; then echo "Expected stale $isolation abort" >&2; exit 1; fi
    grep -q '40001' "$test_logs/read-$isolation.log"
  fi
done

# Force the exact inter-statement gap: a legacy RACI write must wait for the
# reader's metadata lock while an unrelated same-event task remains writable.
sql -c 'UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=100;' >/dev/null
old_revision=$(sql -qAtc 'SELECT revision FROM event_operation_task_revision WHERE activity_id=100')
apply tdf-hq/test/integration/event_task_revisioned_read_barrier.sql
start_coordinator
sql -qAtc "SET application_name='revision_read_first';
  WITH result AS MATERIALIZED (SELECT event_operation_read_task_with_revision(10,100,1) AS value)
  SELECT task_revision_read_test.check_that(value->>'aggregateRevision'='$old_revision'
    AND jsonb_array_length(value->'task'->'raci')=2, 'reader-first coherent old projection') FROM result;" \
  > "$test_logs/read-first.log" 2>&1 &
reader_pid=$!
wait_backend revision_read_first Lock
sql -c "SET statement_timeout='5s'; UPDATE event_logistics_activity SET status='confirmed' WHERE id=101;" >/dev/null
sql -c "SET application_name='revision_read_second';
  UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
    valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=100 AND raci_role='responsible';" \
  > "$test_logs/write-second.log" 2>&1 &
writer_pid=$!
wait_backend revision_read_second Lock
assert_blocked_by revision_read_first revision_read_second
release_coordinator
wait "$reader_pid"
wait "$writer_pid"
sql -c 'DROP FUNCTION event_operation_read_task(BIGINT,BIGINT,BIGINT);
  ALTER FUNCTION task_revision_read_saved(BIGINT,BIGINT,BIGINT) RENAME TO event_operation_read_task;' >/dev/null
sql -qAtc "WITH result AS MATERIALIZED (SELECT event_operation_read_task_with_revision(10,100,1) AS value)
  SELECT task_revision_read_test.check_that(value->>'aggregateRevision'='$((old_revision+1))'
    AND jsonb_array_length(value->'task'->'raci')=1, 'next read sees committed replacement') FROM result;" >/dev/null

# Permission can expire while waiting only on metadata, even with the event
# authorization fence already held. The canonical projector must recheck the clock.
sql -c "UPDATE event_operation_grant SET valid_until=clock_timestamp()+interval '10 seconds';" >/dev/null
start_coordinator
sql -c "BEGIN; SET LOCAL application_name='revision_read_expiry_holder';
  SELECT 1 FROM event_operation_task_revision WHERE activity_id=100 FOR UPDATE;
  SELECT pg_advisory_xact_lock(889,3); COMMIT;" > "$test_logs/expiry-holder.log" 2>&1 &
writer_pid=$!
wait_backend revision_read_expiry_holder Lock
sql -qAtc "BEGIN; SET LOCAL application_name='revision_read_expiry_waiter';
  SELECT task_revision_read_test.check_that(now() < (SELECT valid_until FROM event_operation_grant), 'starts before expiry');
  SELECT task_revision_read_test.check_that(event_operation_read_task_with_revision(10,100,2) IS NULL,
    'grant expired while awaiting metadata'); COMMIT;" > "$test_logs/expiry-waiter.log" 2>&1 &
reader_pid=$!
wait_backend revision_read_expiry_waiter Lock
assert_blocked_by revision_read_expiry_holder revision_read_expiry_waiter
attempt=0
until [ "$(sql -qAtc 'SELECT clock_timestamp() >= valid_until FROM event_operation_grant')" = t ]; do
  attempt=$((attempt + 1)); test "$attempt" -lt 150 || exit 1
  sleep 0.1
done
release_coordinator
wait "$writer_pid"
wait "$reader_pid"
echo 'Revisioned task read PASS: exact private envelope, lossless BIGINT, no read writes, apply/down/up, writer-first RC/RR/Serializable, reader-first metadata barrier, unrelated task writes, expiry during metadata wait.'
echo "Diagnostic logs: $test_logs"
