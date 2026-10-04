#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-raci-command.XXXXXX")
cleanup() { if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-raci-reassignment \
  -e POSTGRES_PASSWORD=raci-command-test-only -e POSTGRES_DB=tdf_raci_command_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning -c statement_timeout=25000' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_raci_command_test "$@"
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
  sql -qAtc "SELECT raci_command_test.check_that(EXISTS(SELECT 1 FROM pg_stat_activity holder, pg_stat_activity waiter
    WHERE holder.datname=current_database() AND waiter.datname=current_database()
      AND holder.application_name='$1' AND waiter.application_name='$2'
      AND holder.pid=ANY(pg_blocking_pids(waiter.pid))), 'exact blocking session observed');" >/dev/null
}
start_coordinator() {
  sql -c "SET statement_timeout='60s'; SET application_name='raci_coordinator';
    SELECT pg_advisory_lock(889,4); SELECT pg_sleep(50);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend raci_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='raci_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
up=tdf-hq/sql/2026-09-15_event_raci_reassignment.sql
down=tdf-hq/sql/2026-09-15_event_raci_reassignment_rollback.sql
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply tdf-hq/sql/2026-09-14_event_task_read.sql
apply tdf-hq/sql/2026-09-15_event_task_revision.sql
apply tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_raci_reassignment_fixture.sql
apply tdf-hq/test/integration/event_raci_reassignment_assertions.sql
apply tdf-hq/test/integration/event_raci_reassignment_faults.sql
before_rows=$(sql -qAtc 'SELECT raci_command_test.rows()')
apply "$down"
apply "$down"
sql -qAtc "SELECT raci_command_test.check_that(to_regprocedure(
  'event_operation_reassign_raci(bigint,bigint,bigint,uuid,bigint,text,bigint,bigint,text,text)') IS NULL,
  'rollback removes only private entry point');" >/dev/null
test "$(sql -qAtc 'SELECT raci_command_test.rows()')" = "$before_rows"
apply "$up"
test "$(sql -qAtc 'SELECT raci_command_test.rows()')" = "$before_rows"
sql -qAtc "SELECT raci_command_test.check_that(
  raci_command_test.command(300,1,1,4,'responsible',2,3)->>'replayed'='true', 'reapply never resets accepted keys');" >/dev/null
test "$(sql -qAtc 'SELECT raci_command_test.rows()')" = "$before_rows"

task=1000
for kind in retry competing legacy; do
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  if [ "$kind" = legacy ]; then
    first="UPDATE event_operation_raci_assignment SET valid_from=valid_from-interval '1 day'
      WHERE activity_id=$task AND raci_role='responsible';"
    resulting_revision=5
    expected_audits=0
  else
    first="SELECT raci_command_test.command($task,1,1,4,'responsible',2,3);"
    resulting_revision=6
    expected_audits=1
  fi
  second_key=2
  if [ "$kind" = retry ]; then second_key=1; fi
  start_coordinator
  sql -qAtc "BEGIN; SET LOCAL application_name='raci_first'; $first
    SELECT pg_advisory_xact_lock(889,4); COMMIT;" > "$test_logs/first-$kind-$isolation.log" 2>&1 &
  first_pid=$!
  wait_backend raci_first Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation; SET LOCAL application_name='raci_second';
    SELECT raci_command_test.command($task,1,$second_key,4,'responsible',2,3); COMMIT;" \
    > "$test_logs/second-$kind-$isolation.log" 2>&1 &
  second_pid=$!
  wait_backend raci_second Lock
  assert_blocked_by raci_first raci_second
  release_coordinator
  wait "$first_pid"
  if [ "$isolation" = 'READ COMMITTED' ]; then
    wait "$second_pid"
    if [ "$kind" = retry ]; then
      grep -q '"replayed": true' "$test_logs/second-$kind-$isolation.log"
    else
      grep -q '"error": "version_conflict"' "$test_logs/second-$kind-$isolation.log"
    fi
  else
    if wait "$second_pid"; then echo "Expected stale $isolation rejection" >&2; exit 1; fi
    grep -q '40001' "$test_logs/second-$kind-$isolation.log"
  fi
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = "$resulting_revision"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = "$expected_audits"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_command_receipt WHERE operation_code='event.task.raci.reassign/$task'")" = "$expected_audits"
done
done

# Permissions expire during distinct blocking points; no frozen pre-wait clock.
for expiry in metadata source recipient party; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  expiring_party=2
  if [ "$expiry" = recipient ]; then expiring_party=3; fi
  sql -c "UPDATE event_operation_grant SET valid_until=clock_timestamp()+interval '10 seconds'
    WHERE resource_id='$task' AND grantee_party_id=$expiring_party;" >/dev/null
  if [ "$expiry" = metadata ]; then
    hold="SELECT 1 FROM event_operation_task_revision WHERE activity_id=$task FOR SHARE;"
  elif [ "$expiry" = party ]; then
    hold="SELECT 1 FROM party WHERE id=3 FOR UPDATE;"
  else
    hold="SELECT 1 FROM event_operation_raci_assignment WHERE activity_id=$task AND raci_role='responsible' FOR UPDATE;"
  fi
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='raci_expiry_holder'; $hold
    SELECT pg_advisory_xact_lock(889,4); COMMIT;" > "$test_logs/expiry-holder-$expiry.log" 2>&1 &
  first_pid=$!
  wait_backend raci_expiry_holder Lock
  sql -qAtc "BEGIN; SET LOCAL application_name='raci_expiry_waiter';
    SELECT raci_command_test.check_that(clock_timestamp()<(SELECT valid_until FROM event_operation_grant
      WHERE resource_id='$task' AND grantee_party_id=$expiring_party), 'command starts before expiry');
    SELECT raci_command_test.command($task,2,1,4,'responsible',2,3); COMMIT;" \
    > "$test_logs/expiry-waiter-$expiry.log" 2>&1 &
  second_pid=$!
  wait_backend raci_expiry_waiter Lock
  assert_blocked_by raci_expiry_holder raci_expiry_waiter
  attempt=0
  until [ "$(sql -qAtc "SELECT clock_timestamp()>=valid_until FROM event_operation_grant
    WHERE resource_id='$task' AND grantee_party_id=$expiring_party")" = t ]; do
    attempt=$((attempt+1)); test "$attempt" -lt 150 || exit 1
    sleep 0.1
  done
  release_coordinator
  wait "$first_pid"
  wait "$second_pid"
  if [ "$expiry" = recipient ]; then
    grep -q '"error": "assignee_unavailable"' "$test_logs/expiry-waiter-$expiry.log"
  else
    grep -q '"error": "not_found"' "$test_logs/expiry-waiter-$expiry.log"
  fi
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = 4
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = 0
done

# The canonical authorization fence determines a serial order for explicit revocation.
for ordering in revoke-first command-first aborted-first; do
  task=$((task+1))
  sql -qAtc "SELECT raci_command_test.seed($task);" >/dev/null
  revoke="UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
    revocation_reason='synthetic concurrent revocation' WHERE resource_id='$task' AND grantee_party_id=2;"
  command="SELECT raci_command_test.command($task,2,1,4,'responsible',2,3);"
  first="$command"
  second="$revoke"
  terminal=COMMIT
  if [ "$ordering" = revoke-first ]; then first="$revoke"; second="$command"; fi
  if [ "$ordering" = aborted-first ]; then terminal=ROLLBACK; second="$command"; fi
  start_coordinator
  sql -qAtc "BEGIN; SET LOCAL application_name='raci_order_first'; $first
    SELECT pg_advisory_xact_lock(889,4); $terminal;" > "$test_logs/order-first-$ordering.log" 2>&1 &
  first_pid=$!
  wait_backend raci_order_first Lock
  sql -qAtc "BEGIN; SET LOCAL application_name='raci_order_second'; $second COMMIT;" \
    > "$test_logs/order-second-$ordering.log" 2>&1 &
  second_pid=$!
  wait_backend raci_order_second Lock
  assert_blocked_by raci_order_first raci_order_second
  release_coordinator
  wait "$first_pid"
  wait "$second_pid"
  expected_count=1
  expected_revision=6
  if [ "$ordering" = revoke-first ]; then
    expected_count=0
    expected_revision=4
    grep -q '"error": "not_found"' "$test_logs/order-second-$ordering.log"
  elif [ "$ordering" = aborted-first ]; then
    grep -q '"replayed": false' "$test_logs/order-second-$ordering.log"
  else
    grep -q '"replayed": false' "$test_logs/order-first-$ordering.log"
  fi
  test "$(sql -qAtc "SELECT raci_command_test.rev($task)")" = "$expected_revision"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_audit_event WHERE resource_id='$task'")" = "$expected_count"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_command_receipt WHERE operation_code='event.task.raci.reassign/$task'")" = "$expected_count"
  if [ "$ordering" != aborted-first ]; then
    sql -qAtc "SELECT raci_command_test.check_that(
      raci_command_test.command($task,2,1,4,'responsible',2,3)->>'error'='not_found',
      'revoked actor cannot replay or mutate after either serial ordering');" >/dev/null
  fi
done
echo 'RACI reassignment PASS: scoped authority, atomic history, exact retry, private task keys, lifecycle/time guards, 40 generated replacements, audit/receipt/overflow rollback, down/up, nine RC/RR/Serializable races, four observed expiry waits, both revocation orders and aborted-first retry recovery.'
echo "Diagnostic logs: $test_logs"
