#!/bin/sh
set -eu
test "$#" = 0 || exit 2
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container_id=''
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-task-read.XXXXXX")
cleanup() {
  if [ -n "$test_container_id" ]; then docker rm -f "$test_container_id" >/dev/null; fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
test_container_id=$(docker run --rm -d --network none --label tdf.test=event-task-read \
  -e POSTGRES_PASSWORD=task-read-test-only -e POSTGRES_DB=tdf_task_read_test postgres:16-alpine)
sql() {
  docker exec -i -e PGOPTIONS='-c client_min_messages=warning' "$test_container_id" \
    psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_task_read_test "$@"
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
  sql -c "SET application_name='task_read_coordinator';
    SELECT pg_advisory_lock(889,1); SELECT pg_sleep(60);" > "$test_logs/coordinator.log" 2>&1 &
  coordinator_pid=$!
  wait_backend task_read_coordinator Timeout
}
release_coordinator() {
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='task_read_coordinator'" >/dev/null
  wait "$coordinator_pid" || true
}
up=tdf-hq/sql/2026-09-14_event_task_read.sql
down=tdf-hq/sql/2026-09-14_event_task_read_rollback.sql
apply tdf-hq/test/integration/event_operations_foundation_fixture.sql
apply tdf-hq/sql/2026-09-14_event_operations_foundation.sql
apply tdf-hq/sql/2026-09-14_event_operations_api.sql
apply tdf-hq/sql/2026-09-14_event_task_commit.sql
apply "$up"
apply "$up"
apply tdf-hq/test/integration/event_task_read_assertions.sql
apply "$down"
apply "$down"
sql -qAtc "SELECT task_read_test.check_that(to_regprocedure('event_operation_read_task(bigint,bigint,bigint)') IS NULL
  AND to_regprocedure('event_operation_actor_can_read_task(bigint,bigint,bigint,timestamp with time zone)') IS NULL,
  'rollback removes only new entry points');
  SELECT task_read_test.check_that((SELECT snapshot=task_read_test.rows() FROM task_read_test.preserved),
  'rollback preserves domain rows');" >/dev/null
apply "$up"
sql -qAtc "SELECT task_read_test.check_that(event_operation_read_task(10,100,1) IS NOT NULL,
  'reapply restores read'); SELECT task_read_test.check_that(
  (SELECT snapshot=task_read_test.rows() FROM task_read_test.preserved), 'reapply preserves records');" >/dev/null

# A reader begins while revocation is uncommitted. RC sees the committed denial;
# RR/SERIALIZABLE abort on the changed authorization fence instead of disclosing.
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  sql -c "DELETE FROM event_operation_grant;
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
    VALUES (10,2,'task.read','task','100',1);" >/dev/null
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='task_read_revoke';
    UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='race';
    SELECT pg_advisory_xact_lock(889,1); COMMIT;" > "$test_logs/revoke-$isolation.log" 2>&1 &
  writer_pid=$!
  wait_backend task_read_revoke Lock
  sql --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='task_read_reader';
    SELECT COALESCE(event_operation_read_task(10,100,2)::text,'opaque'); COMMIT;" \
    > "$test_logs/read-$isolation.log" 2>&1 &
  reader_pid=$!
  wait_backend task_read_reader Lock
  release_coordinator
  wait "$writer_pid"
  if [ "$isolation" = 'READ COMMITTED' ]; then
    wait "$reader_pid"
    test "$(sed '/^$/d' "$test_logs/read-$isolation.log")" = opaque
  else
    if wait "$reader_pid"; then echo "Expected stale $isolation abort" >&2; exit 1; fi
    grep -q '40001' "$test_logs/read-$isolation.log"
    if grep -q 'activityId' "$test_logs/read-$isolation.log"; then exit 1; fi
  fi
done

# The same fence covers resource narrowing and co-owner revocation; feature disable
# has its own shared lock. Each blocked reader must re-evaluate after the commit.
for change in scope owner flag; do
  sql -c "DELETE FROM event_operation_grant;
    DELETE FROM event_operation_relationship WHERE party_id=2;
    UPDATE event_operation_feature_flag SET enabled=TRUE;
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
      VALUES (10,2,'task.read','task','100',1);" >/dev/null
  case "$change" in
    scope) mutation="UPDATE event_operation_grant SET resource_id='101'" ;;
    owner)
      sql -c "DELETE FROM event_operation_grant;
        INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES (10,2,'co_owner');" >/dev/null
      mutation="UPDATE event_operation_relationship SET revoked_at=clock_timestamp() WHERE party_id=2" ;;
    flag) mutation="UPDATE event_operation_feature_flag SET enabled=FALSE" ;;
  esac
  start_coordinator
  sql -c "BEGIN; SET LOCAL application_name='task_read_change'; $mutation;
    SELECT pg_advisory_xact_lock(889,1); COMMIT;" > "$test_logs/change-$change.log" 2>&1 &
  writer_pid=$!
  wait_backend task_read_change Lock
  sql -qAtc "SET application_name='task_read_changed';
    SELECT COALESCE(event_operation_read_task(10,100,2)::text,'opaque');" > "$test_logs/changed-$change.log" 2>&1 &
  reader_pid=$!
  wait_backend task_read_changed Lock
  release_coordinator
  wait "$writer_pid"
  wait "$reader_pid"
  test "$(sed '/^$/d' "$test_logs/changed-$change.log")" = opaque
done
sql -c 'UPDATE event_operation_feature_flag SET enabled=TRUE;' >/dev/null

# Expiry during an observed row-lock wait: transaction-start now() would authorize.
sql -c "DELETE FROM event_operation_grant;
  INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,
    issued_by_party_id,valid_until) VALUES (10,2,'task.read','task','100',1,clock_timestamp()+interval '10 seconds');" >/dev/null
start_coordinator
sql -c "BEGIN; SET LOCAL application_name='task_read_expiry_lock';
  SELECT 1 FROM event_operation_event_state WHERE event_id=10 FOR UPDATE;
  SELECT pg_advisory_xact_lock(889,1); COMMIT;" > "$test_logs/expiry-lock.log" 2>&1 &
writer_pid=$!
wait_backend task_read_expiry_lock Lock
sql -qAtc "BEGIN; SET LOCAL application_name='task_read_expiry';
  SELECT task_read_test.check_that(now() < (SELECT valid_until FROM event_operation_grant), 'read begins before expiry');
  SELECT COALESCE(event_operation_read_task(10,100,2)::text,'opaque'); COMMIT;" > "$test_logs/expiry.log" 2>&1 &
reader_pid=$!
wait_backend task_read_expiry Lock
attempt=0
until [ "$(sql -qAtc 'SELECT clock_timestamp() >= valid_until FROM event_operation_grant')" = t ]; do
  attempt=$((attempt + 1)); test "$attempt" -lt 150 || exit 1
  sleep 0.1
done
release_coordinator
wait "$writer_pid"
wait "$reader_pid"
test "$(sed '/^$/d' "$test_logs/expiry.log")" = opaque

# Task writes use their own fence. Uncommitted activity+RACI updates must neither
# block this read nor leak half of the new snapshot; the next read sees both.
start_coordinator
sql -c "BEGIN; SET LOCAL application_name='task_read_task_writer';
  UPDATE event_logistics_activity SET status='in_progress',version=2 WHERE id=100;
  UPDATE event_operation_raci_assignment SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='replacement'
    WHERE activity_id=100 AND raci_role='responsible';
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (100,3,'responsible',1);
  SET CONSTRAINTS ALL IMMEDIATE;
  SELECT pg_advisory_xact_lock(889,1); COMMIT;" > "$test_logs/task-writer.log" 2>&1 &
writer_pid=$!
wait_backend task_read_task_writer Lock
sql -qAtc "SET statement_timeout='5s'; SELECT task_read_test.check_that(
  event_operation_read_task(10,100,1) @> '{\"version\":1,\"status\":\"planned\",\"raci\":[{\"partyId\":2,\"role\":\"responsible\"}]}',
  'reader sees coherent committed pre-write snapshot');" >/dev/null
release_coordinator
wait "$writer_pid"
sql -qAtc "SELECT task_read_test.check_that(
  event_operation_read_task(10,100,1) @> '{\"version\":2,\"status\":\"in_progress\",\"raci\":[{\"partyId\":3,\"role\":\"responsible\"}]}',
  'reader sees coherent post-write snapshot');" >/dev/null
echo 'Task read PASS: scoped/temporal permissions, opaque targets, allowlist, attention, no side effects, apply/rollback/reapply, revocation RC/RR/Serializable, scope/owner/feature races, expiry wait, concurrent task/RACI snapshots.'
echo "Diagnostic logs: $test_logs"
