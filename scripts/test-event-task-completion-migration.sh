#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-task-completion.XXXXXX")
cleanup() { if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-task-completion \
  -e POSTGRES_PASSWORD=completion-test-only -e POSTGRES_DB=tdf_completion_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning -c statement_timeout=25000' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_completion_test "$@"
}
apply() { sql < "$repo_root/$1" >/dev/null; }
attempt=0
until sql -h 127.0.0.1 -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt+1)); test "$attempt" -lt 30 || exit 1
  sleep 1
done
wait_backend() {
  attempt=0
  until [ "$(sql -qAtc "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database()
    AND application_name='$1' AND wait_event_type='$2'")" = 1 ]; do
    attempt=$((attempt+1))
    if [ "$attempt" -ge 100 ]; then echo "Missing barrier $1/$2; logs: $test_logs" >&2; exit 1; fi
    sleep 0.1
  done
}
assert_blocked_by() {
  sql -qAtc "SELECT raci_command_test.check_that(EXISTS(SELECT 1 FROM pg_stat_activity holder,pg_stat_activity waiter
    WHERE holder.datname=current_database() AND waiter.datname=current_database()
      AND holder.application_name='$1' AND waiter.application_name='$2'
      AND holder.pid=ANY(pg_blocking_pids(waiter.pid))), 'exact blocking session observed');" >/dev/null
}
start_coordinator() {
  sql -c "SET statement_timeout='60s'; SET application_name='completion_coordinator';
    SELECT pg_advisory_lock(889,6); SELECT pg_sleep(50);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend completion_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='completion_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
up=tdf-hq/sql/2026-09-16_event_task_completion.sql
down=tdf-hq/sql/2026-09-16_event_task_completion_rollback.sql
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply tdf-hq/sql/2026-09-14_event_task_read.sql
apply tdf-hq/sql/2026-09-15_event_task_revision.sql
apply tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql
apply tdf-hq/sql/2026-09-15_event_raci_reassignment.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_raci_reassignment_fixture.sql
apply tdf-hq/test/integration/event_task_completion_fixture.sql
apply tdf-hq/test/integration/event_task_completion_assertions.sql
before_rows=$(sql -qAtc 'SELECT completion_test.rows()')
apply "$down"
apply "$down"
sql -qAtc "SELECT raci_command_test.check_that(to_regprocedure(
  'event_operation_complete_task(bigint,bigint,bigint,uuid,bigint,text,text)') IS NULL,
  'rollback removes only private entry point');" >/dev/null
test "$(sql -qAtc 'SELECT completion_test.rows()')" = "$before_rows"
apply "$up"
sql -qAtc "SELECT raci_command_test.check_that(completion_test.command(400,1,1,4)->>'replayed'='true',
  'reapply never resets accepted completion keys');" >/dev/null
test "$(sql -qAtc 'SELECT completion_test.rows()')" = "$before_rows"

task=6000
for kind in retry competing legacy dependency; do
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  expected=4
  expected_count=1
  resulting_revision=5
  error=operation_not_ready
  first="SELECT completion_test.command($task,1,1,4);"
  if [ "$kind" = legacy ]; then
    first="UPDATE event_logistics_activity SET version=version+1 WHERE id=$task;"
    expected_count=0
    error=version_conflict
  elif [ "$kind" = dependency ]; then
    prerequisite=$((task+1000))
    sql -qAtc "INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES($prerequisite,10,'completed',1);
      INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES($task,$prerequisite);" >/dev/null
    expected=5
    expected_count=0
    first="UPDATE event_logistics_activity SET status='planned',version=version+1 WHERE id=$prerequisite;"
    error=dependencies_not_ready
  fi
  second_key=2
  if [ "$kind" = retry ]; then second_key=1; fi
  start_coordinator
  sql -qAtc "BEGIN; SET LOCAL application_name='completion_first'; $first
    SELECT pg_advisory_xact_lock(889,6); COMMIT;" > "$test_logs/first-$kind-$isolation.log" 2>&1 &
  first_pid=$!
  wait_backend completion_first Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation; SET LOCAL application_name='completion_second';
    SELECT completion_test.command($task,1,$second_key,$expected); COMMIT;" \
    > "$test_logs/second-$kind-$isolation.log" 2>&1 &
  second_pid=$!
  wait_backend completion_second Lock
  assert_blocked_by completion_first completion_second
  release_coordinator
  wait "$first_pid"
  if [ "$isolation" = 'READ COMMITTED' ]; then
    wait "$second_pid"
    if [ "$kind" = retry ]; then
      grep -q '"replayed": true' "$test_logs/second-$kind-$isolation.log"
    else
      grep -q "\"error\": \"$error\"" "$test_logs/second-$kind-$isolation.log"
    fi
  else
    if wait "$second_pid"; then echo "Expected stale $isolation rejection" >&2; exit 1; fi
    grep -q '40001' "$test_logs/second-$kind-$isolation.log"
  fi
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = "$resulting_revision"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = "$expected_count"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_command_receipt WHERE operation_code='event.task.complete/$task'")" = "$expected_count"
done
done

for expiry in metadata activity party raci; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  # Establish the coordinator before starting the real-time validity window.
  # Docker startup/exec latency must not consume the original four-second window.
  # Still assert that the waiter starts valid, blocks on the exact holder, and
  # resumes only after actual expiry; do not replace those checks with sleeps.
  start_coordinator
  expiry_query="SELECT valid_until FROM event_operation_grant WHERE resource_id='$task' AND grantee_party_id=2"
  if [ "$expiry" = raci ]; then
    sql -c "UPDATE event_operation_raci_assignment SET valid_until=clock_timestamp()+interval '15 seconds'
      WHERE activity_id=$task AND raci_role='responsible';" >/dev/null
    expiry_query="SELECT valid_until FROM event_operation_raci_assignment WHERE activity_id=$task AND raci_role='responsible'"
  else
    sql -c "UPDATE event_operation_grant SET valid_until=clock_timestamp()+interval '15 seconds'
      WHERE resource_id='$task' AND grantee_party_id=2;" >/dev/null
  fi
  hold="SELECT 1 FROM event_logistics_activity WHERE id=$task FOR UPDATE;"
  if [ "$expiry" = metadata ]; then hold="SELECT 1 FROM event_operation_task_revision WHERE activity_id=$task FOR SHARE;"; fi
  if [ "$expiry" = party ]; then hold="SELECT 1 FROM party WHERE id=2 FOR UPDATE;"; fi
  expected=$(sql -qAtc "SELECT raci_command_test.rev($task)")
  sql -c "BEGIN; SET LOCAL application_name='completion_expiry_holder'; $hold
    SELECT pg_advisory_xact_lock(889,6); COMMIT;" > "$test_logs/expiry-holder-$expiry.log" 2>&1 &
  first_pid=$!
  wait_backend completion_expiry_holder Lock
  sql -qAtc "BEGIN; SET LOCAL application_name='completion_expiry_waiter';
    SELECT raci_command_test.check_that(clock_timestamp()<($expiry_query),'command starts before expiry');
    SELECT completion_test.command($task,2,1,$expected); COMMIT;" > "$test_logs/expiry-waiter-$expiry.log" 2>&1 &
  second_pid=$!
  wait_backend completion_expiry_waiter Lock
  assert_blocked_by completion_expiry_holder completion_expiry_waiter
  attempt=0
  until [ "$(sql -qAtc "SELECT clock_timestamp()>=($expiry_query)")" = t ]; do
    attempt=$((attempt+1)); test "$attempt" -lt 100 || exit 1
    sleep 0.1
  done
  release_coordinator
  wait "$first_pid"
  wait "$second_pid"
  error=not_found
  if [ "$expiry" = raci ]; then error=accountability_not_ready; fi
  grep -q "\"error\": \"$error\"" "$test_logs/expiry-waiter-$expiry.log"
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = "$expected"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = 0
done

for ordering in revoke-first command-first aborted-first; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  revoke="UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
    revocation_reason='synthetic concurrent revocation' WHERE resource_id='$task' AND grantee_party_id=2;"
  command="SELECT completion_test.command($task,2,1,4);"
  first="$command"; second="$revoke"; terminal=COMMIT
  if [ "$ordering" = revoke-first ]; then first="$revoke"; second="$command"; fi
  if [ "$ordering" = aborted-first ]; then terminal=ROLLBACK; second="$command"; fi
  start_coordinator
  sql -qAtc "BEGIN; SET LOCAL application_name='completion_order_first'; $first
    SELECT pg_advisory_xact_lock(889,6); $terminal;" > "$test_logs/order-first-$ordering.log" 2>&1 &
  first_pid=$!
  wait_backend completion_order_first Lock
  sql -qAtc "BEGIN; SET LOCAL application_name='completion_order_second'; $second COMMIT;" \
    > "$test_logs/order-second-$ordering.log" 2>&1 &
  second_pid=$!
  wait_backend completion_order_second Lock
  assert_blocked_by completion_order_first completion_order_second
  release_coordinator
  wait "$first_pid"
  wait "$second_pid"
  expected_count=1; expected_revision=5
  if [ "$ordering" = revoke-first ]; then
    expected_count=0; expected_revision=4
    grep -q '"error": "not_found"' "$test_logs/order-second-$ordering.log"
  elif [ "$ordering" = aborted-first ]; then
    grep -q '"replayed": false' "$test_logs/order-second-$ordering.log"
  fi
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = "$expected_revision"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = "$expected_count"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_command_receipt WHERE operation_code='event.task.complete/$task'")" = "$expected_count"
  if [ "$ordering" != aborted-first ]; then
    sql -qAtc "SELECT raci_command_test.check_that(completion_test.command($task,2,1,4)->>'error'='not_found',
      'revocation hides history after either ordering');" >/dev/null
  fi
done
echo 'Task completion PASS: exact scoped guards/retries/history, 32 decision cases, persistence/overflow/outer rollback, down/up, 12 RC/RR/Serializable races, four observed expiry waits, both revocation orders and aborted-first recovery.'
echo "Diagnostic logs: $test_logs"
