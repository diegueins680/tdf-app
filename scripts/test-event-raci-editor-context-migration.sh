#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-raci-context.XXXXXX")
cleanup() { if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi; }
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-raci-editor-context \
  -e POSTGRES_PASSWORD=context-test-only -e POSTGRES_DB=tdf_raci_context_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning -c statement_timeout=25000' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_raci_context_test "$@"
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
    attempt=$((attempt+1)); test "$attempt" -lt 100 || exit 1
    sleep 0.1
  done
}
assert_blocked_by() {
  sql -qAtc "SELECT raci_command_test.check_that(EXISTS(SELECT 1 FROM pg_stat_activity a,pg_stat_activity b
    WHERE a.datname=current_database() AND b.datname=current_database() AND a.application_name='$1'
    AND b.application_name='$2' AND a.pid=ANY(pg_blocking_pids(b.pid))), 'observed exact metadata blocker');" >/dev/null
}
start_coordinator() {
  sql -c "SET statement_timeout='60s'; SET application_name='raci_context_coordinator';
    SELECT pg_advisory_lock(889,5); SELECT pg_sleep(50);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend raci_context_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='raci_context_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply tdf-hq/sql/2026-09-14_event_task_read.sql
apply tdf-hq/sql/2026-09-15_event_task_revision.sql
apply tdf-hq/sql/2026-09-15_event_task_revisioned_read.sql
apply tdf-hq/sql/2026-09-15_event_raci_reassignment.sql
up=tdf-hq/sql/2026-09-15_event_raci_editor_context.sql
down=tdf-hq/sql/2026-09-15_event_raci_editor_context_rollback.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_raci_reassignment_fixture.sql
apply tdf-hq/test/integration/event_raci_editor_context_assertions.sql
before_rows=$(sql -qAtc 'SELECT raci_command_test.rows()')
apply "$down"
apply "$down"
sql -qAtc "SELECT raci_command_test.check_that(to_regprocedure(
  'event_operation_read_raci_editor_context(bigint,bigint,bigint,bigint)') IS NULL,
  'rollback removes context only');" >/dev/null
test "$(sql -qAtc 'SELECT raci_command_test.rows()')" = "$before_rows"
apply "$up"
test "$(sql -qAtc 'SELECT raci_command_test.rows()')" = "$before_rows"

for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  sql -c "UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=300;" >/dev/null
  old_revision=$(sql -qAtc 'SELECT raci_command_test.rev(300)')
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='raci_context_writer';
    UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
      valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=300 AND raci_role='responsible';
    SELECT pg_advisory_xact_lock(889,5); COMMIT;" > "$test_logs/writer-$isolation.log" 2>&1 &
  writer_pid=$!
  wait_backend raci_context_writer Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='raci_context_reader';
    WITH result AS MATERIALIZED (SELECT raci_command_test.context(1) value)
    SELECT raci_command_test.check_that(value->>'aggregateRevision'='$((old_revision+1))'
      AND value->>'operationReady'='false' AND value->'eligiblePartyIds'='[]'::jsonb,
      'post-wait coherent revision and unavailable operation') FROM result; COMMIT;" \
    > "$test_logs/reader-$isolation.log" 2>&1 &
  reader_pid=$!
  wait_backend raci_context_reader Lock
  assert_blocked_by raci_context_writer raci_context_reader
  release_coordinator
  wait "$writer_pid"
  if [ "$isolation" = 'READ COMMITTED' ]; then wait "$reader_pid";
  else
    if wait "$reader_pid"; then echo "Expected stale $isolation failure" >&2; exit 1; fi
    grep -q '40001' "$test_logs/reader-$isolation.log"
  fi
done
sql -c "UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=300;" >/dev/null
for expiry in actor recipient manage; do
  expiring_party=2
  if [ "$expiry" = recipient ]; then expiring_party=3; fi
  if [ "$expiry" = manage ]; then
    sql -c "INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
      VALUES(10,2,'task.read','task','300',1);" >/dev/null
  fi
  sql -c "UPDATE event_operation_grant SET valid_until=clock_timestamp()+interval '10 seconds'
    WHERE resource_id='300' AND grantee_party_id=$expiring_party
      AND scope_code='task.$([ "$expiry" = recipient ] && echo read || echo manage)';" >/dev/null
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='raci_context_holder';
    SELECT 1 FROM event_operation_task_revision WHERE activity_id=300 FOR UPDATE;
    SELECT pg_advisory_xact_lock(889,5); COMMIT;" > "$test_logs/holder-$expiry.log" 2>&1 &
  holder_pid=$!
  wait_backend raci_context_holder Lock
  reader_actor=2
  if [ "$expiry" = recipient ]; then reader_actor=1; fi
  sql -qAtc "BEGIN; SET LOCAL application_name='raci_context_expiry_reader';
    SELECT raci_command_test.context($reader_actor); COMMIT;" > "$test_logs/expiry-$expiry.log" 2>&1 &
  reader_pid=$!
  wait_backend raci_context_expiry_reader Lock
  assert_blocked_by raci_context_holder raci_context_expiry_reader
  attempt=0
  until [ "$(sql -qAtc "SELECT bool_and(clock_timestamp()>=valid_until) FROM event_operation_grant
    WHERE resource_id='300' AND grantee_party_id=$expiring_party AND valid_until IS NOT NULL")" = t ]; do
    attempt=$((attempt+1)); test "$attempt" -lt 150 || exit 1
    sleep 0.1
  done
  release_coordinator
  wait "$holder_pid"
  wait "$reader_pid"
  if [ "$expiry" = actor ]; then test -z "$(tr -d '\n\r ' < "$test_logs/expiry-$expiry.log")";
  elif [ "$expiry" = recipient ]; then grep -q '"eligiblePartyIds": \[1, 2\]' "$test_logs/expiry-$expiry.log";
  else grep -q '"canManage": false' "$test_logs/expiry-$expiry.log"; fi
  sql -c "UPDATE event_operation_grant SET valid_until=NULL WHERE resource_id='300';" >/dev/null
done
echo 'RACI context PASS: scope, pagination, exact revision, temporal options, no writes, down/up, three writer isolation races and three post-wait expiry cases.'
echo "Diagnostic logs: $test_logs"
