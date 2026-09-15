#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_container="tdf-event-task-commit-test-$$"
test_database=tdf_event_task_commit_test
test_logs=$(mktemp -d "${TMPDIR:-/tmp}/tdf-task-commit.XXXXXX")
cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
  # Retain small diagnostic logs; only this run's disposable container is removed.
}
trap cleanup EXIT INT TERM
docker run --rm -d --name "$test_container" -e POSTGRES_PASSWORD=task-test \
  -e POSTGRES_DB="$test_database" postgres:16-alpine >/dev/null
sql() { docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" "$@"; }
apply() { docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" < "$1" >/dev/null; }
attempt=0
until sql -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  test "$attempt" -lt 30 || exit 1
  sleep 1
done
reject() {
  expected="$1"
  statement="$2"
  if sql --set=VERBOSITY=verbose -c "$statement" > "$test_logs/rejection.log" 2>&1; then
    echo "Expected $expected rejection: $statement" >&2
    exit 1
  fi
  if ! grep -q "$expected" "$test_logs/rejection.log"; then
    sed -n '1,60p' "$test_logs/rejection.log" >&2
    exit 1
  fi
}
wait_for_backend() {
  backend_name="$1"
  wait_kind="$2"
  attempt=0
  until [ "$(sql -qAtc "SELECT count(*) FROM pg_stat_activity WHERE application_name='$backend_name' AND wait_event_type='$wait_kind'")" = 1 ]; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 50 ]; then
      echo "Backend did not reach $wait_kind barrier: $backend_name; logs: $test_logs" >&2
      exit 1
    fi
    sleep 0.1
  done
}
race() {
  race_task="$1"; isolation="$2"; first="$3"; second="$4"; expected="$5"
  # The coordinator owns a test-only advisory lock. Release it only after BOTH
  # writers are observably blocked, so a slow CI host cannot turn a race into a
  # serial test with a different (but still safe) error code.
  sql -c "SET application_name='task_test_coordinator_$race_task';
    SELECT pg_advisory_lock(884,$race_task); SELECT pg_sleep(60);" > "$test_logs/coordinator-$race_task.log" 2>&1 &
  coordinator_pid=$!
  wait_for_backend "task_test_coordinator_$race_task" Timeout
  sql --set=VERBOSITY=verbose -c "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='task_test_first_$race_task';
    $first; SET CONSTRAINTS ALL IMMEDIATE;
    SELECT pg_advisory_xact_lock(884,$race_task); COMMIT;" > "$test_logs/first-$race_task.log" 2>&1 &
  first_pid=$!
  wait_for_backend "task_test_first_$race_task" Lock
  sql --set=VERBOSITY=verbose -c "BEGIN ISOLATION LEVEL $isolation;
    SET LOCAL application_name='task_test_second_$race_task';
    $second; COMMIT;" > "$test_logs/second-$race_task.log" 2>&1 &
  second_pid=$!
  wait_for_backend "task_test_second_$race_task" Lock
  # Only this disposable database's precisely named coordinator is terminated.
  sql -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='task_test_coordinator_$race_task'" >/dev/null
  wait "$coordinator_pid" || true
  wait "$first_pid" || { sed -n '1,60p' "$test_logs/first-$race_task.log" >&2; exit 1; }
  if wait "$second_pid"; then
    echo "Both competing task writes committed: $race_task" >&2
    exit 1
  fi
  if ! grep -q "$expected" "$test_logs/second-$race_task.log"; then
    sed -n '1,60p' "$test_logs/second-$race_task.log" >&2
    exit 1
  fi
}
apply "$repo_root/tdf-hq/test/integration/event_operations_foundation_fixture.sql"
apply "$repo_root/tdf-hq/sql/2026-09-14_event_operations_foundation.sql"
up="$repo_root/tdf-hq/sql/2026-09-14_event_task_commit.sql"
down="$repo_root/tdf-hq/sql/2026-09-14_event_task_commit_rollback.sql"
if [ "${1:-}" != '--foundation-only' ]; then
  apply "$up"
  apply "$up"
fi
sql -c "BEGIN;
 INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
 VALUES (100,1,'accountable',1),(100,2,'responsible',1);
 INSERT INTO event_operation_task_policy(activity_id) VALUES (100);
 COMMIT;" >/dev/null

# A status guard before relation replacement misses this exact legacy-handler order.
reject 23514 "BEGIN;
 UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=100;
 INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,101);
 COMMIT;"
test "$(sql -qAtc "SELECT status || ':' || version FROM event_logistics_activity WHERE id=100")" = planned:1
test "$(sql -qAtc 'SELECT count(*) FROM event_logistics_dependency WHERE activity_id=100')" = 0

# Final-state validation must permit either statement order for a valid transaction.
sql -c "BEGIN;
 UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=100;
 INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,101);
 UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=101;
 COMMIT;" >/dev/null
reject 23514 "UPDATE event_logistics_activity SET status='planned',version=3 WHERE id=101;"
reject 23514 "INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,103);"
reject 23514 "BEGIN; SET CONSTRAINTS ALL IMMEDIATE;
 INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,103); COMMIT;"
reject 23514 'DELETE FROM event_operation_task_policy WHERE activity_id=100;'
reject 23514 'UPDATE event_operation_task_policy SET dependencies_gate_completion=false WHERE activity_id=100;'
reject 23514 'UPDATE event_logistics_activity SET event_id=11 WHERE id=100;'
reject 23514 'DELETE FROM event_logistics_activity WHERE id=100;'
reject 23514 "UPDATE event_operation_raci_assignment SET activity_id=104 WHERE activity_id=100 AND raci_role='responsible';"

# Insertion as completed followed by policy activation also validates the final graph.
reject 23514 "BEGIN;
 INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (105,10,'completed',1);
 INSERT INTO event_operation_task_policy(activity_id,requires_accountability) VALUES (105,false);
 INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (105,103);
 COMMIT;"
test "$(sql -qAtc 'SELECT count(*) FROM event_logistics_activity WHERE id=105')" = 0

# Concurrent removals must not each count the other transaction's uncommitted Responsible.
# A real write fence also rejects stale REPEATABLE READ snapshots with SQLSTATE 40001.
task=199
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
  task=$((task + 1))
  sql -c "BEGIN;
   INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES ($task,10,'planned',1);
   INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES ($task,1,'accountable',1),($task,2,'responsible',1),($task,3,'responsible',1);
   INSERT INTO event_operation_task_policy(activity_id) VALUES ($task);
   COMMIT;" >/dev/null
  if [ "$isolation" = 'READ COMMITTED' ]; then expected=23514; else expected=40001; fi
  race "$task" "$isolation" \
    "UPDATE event_operation_raci_assignment SET revoked_at=now(),revoked_by_party_id=1,revocation_reason='replacement' WHERE activity_id=$task AND party_id=2" \
    "UPDATE event_operation_raci_assignment SET revoked_at=now(),revoked_by_party_id=1,revocation_reason='replacement' WHERE activity_id=$task AND party_id=3" \
    "$expected"
  test "$(sql -qAtc "SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=$task AND raci_role='responsible' AND revoked_at IS NULL")" = 1
done

# Race the status writer against the relation writer, in both orders. The loser must
# reject, not commit a completed task with a pending prerequisite.
task=299
for first_operation in complete dependency; do
  task=$((task + 1))
  sql -c "BEGIN;
   INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES ($task,10,'planned',1);
   INSERT INTO event_operation_task_policy(activity_id,requires_accountability) VALUES ($task,false);
   COMMIT;" >/dev/null
  complete="UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=$task"
  dependency="INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES ($task,103)"
  if [ "$first_operation" = complete ]; then first="$complete"; second="$dependency";
  else first="$dependency"; second="$complete"; fi
  race "$task" 'READ COMMITTED' "$first" "$second" 23514
done

# An override is exact-version-bound; old consent does not authorize later versions.
sql -c "BEGIN;
 INSERT INTO event_operation_task_override(activity_id,activity_version,override_kind,reason,policy_reference,authorized_by_party_id)
 VALUES (100,2,'blocked_completion','Emergency access','test-policy',1);
 UPDATE event_logistics_activity SET version=3 WHERE id=100;
 UPDATE event_logistics_activity SET status='planned',version=3 WHERE id=101;
 COMMIT;" >/dev/null
reject 23514 'UPDATE event_logistics_activity SET version=4 WHERE id=100;'
reject 55000 "UPDATE event_operation_task_override SET reason='changed' WHERE activity_id=100;"

apply "$down"
apply "$down"
test "$(sql -qAtc 'SELECT count(*) FROM event_operation_task_override WHERE activity_id=100')" = 1
# Simulate incompatible data written under the rolled-back, weaker guard. A forward
# migration must refuse it and roll its DDL back, not silently grandfather the violation.
sql -c 'UPDATE event_logistics_activity SET version=4 WHERE id=100;' >/dev/null
if apply "$up" > "$test_logs/invalid-up.log" 2>&1; then
  echo 'Expected migration to reject incompatible existing task' >&2
  exit 1
fi
grep -q 'completed task 100 has incomplete dependencies' "$test_logs/invalid-up.log"
test "$(sql -qAtc "SELECT count(*) FROM pg_trigger WHERE tgname='event_operation_task_completion_guard'")" = 1
test "$(sql -qAtc "SELECT count(*) FROM pg_trigger WHERE tgname='event_operation_task_commit_guard'")" = 0
sql -c 'UPDATE event_logistics_activity SET version=3 WHERE id=100;' >/dev/null
apply "$up"
reject 23514 'UPDATE event_logistics_activity SET version=4 WHERE id=100;'
echo 'Task commit migration PASS: final-state/immediate constraints, policy bypass guards, RACI write skew (RC/RR/Serializable), status/dependency races in both orders, versioned override, apply twice, rollback twice, incompatible-data rejection, reapply.'
echo "Diagnostic logs: $test_logs"
