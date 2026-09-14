#!/bin/sh
set -eu

test_container="tdf-event-operations-foundation-test-$$"
test_database="tdf_event_operations_foundation_test"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-14_event_operations_foundation.sql"
rollback_migration="$repo_root/tdf-hq/sql/2026-09-14_event_operations_foundation_rollback.sql"
fixture_sql="$repo_root/tdf-hq/test/integration/event_operations_foundation_fixture.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=event-operations-test \
  -e POSTGRES_DB="$test_database" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$test_container" >&2
    echo "PostgreSQL event operations migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" "$@"
}

apply_sql() {
  docker exec -i "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" < "$1" >/dev/null
}

apply_sql "$fixture_sql"

apply_sql "$up_migration"
apply_sql "$up_migration"

state_rows=$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_event_state;')
test "$state_rows" = "2"
planning_state=$(psql_exec -qAt -c 'SELECT canonical_state FROM event_operation_event_state WHERE event_id=10;')
test "$planning_state" = "planning"
live_state=$(psql_exec -qAt -c 'SELECT canonical_state FROM event_operation_event_state WHERE event_id=11;')
test "$live_state" = "in_progress"
owner_rows=$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_relationship WHERE event_id=10 AND relationship_kind='primary_owner';")
test "$owner_rows" = "1"
issue_rows=$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_migration_issue WHERE event_id=11 AND issue_code='owner_missing';")
test "$issue_rows" = "1"
transition_rows=$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_lifecycle_transition_policy WHERE active;')
test "$transition_rows" = "28"

psql_exec -c "INSERT INTO event_operation_session(event_id,name,attendance_mode,timezone,starts_at,ends_at) VALUES (10,'Doors','physical','America/Guayaquil','2026-09-14T18:00:00Z','2026-09-14T19:00:00Z');" >/dev/null
if psql_exec -c "INSERT INTO event_operation_session(event_id,name,attendance_mode,timezone,starts_at,ends_at) VALUES (10,'Bad zone','virtual','Mars/Olympus','2026-09-14T18:00:00Z','2026-09-14T19:00:00Z');" >/dev/null 2>&1; then
  echo "expected unknown timezone to be rejected" >&2
  exit 1
fi

psql_exec -c "
  BEGIN;
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (100,1,'accountable',1), (100,2,'responsible',1);
  INSERT INTO event_operation_task_policy(activity_id) VALUES (100);
  COMMIT;
" >/dev/null

if psql_exec -c "INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES (100,3,'accountable',1);" >/dev/null 2>&1; then
  echo "expected a second active Accountable party to be rejected" >&2
  exit 1
fi
if psql_exec -c "DELETE FROM event_operation_raci_assignment WHERE activity_id=100 AND raci_role='responsible';" >/dev/null 2>&1; then
  echo "expected removal of the required Responsible party to be rejected" >&2
  exit 1
fi

psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,101);' >/dev/null
if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null 2>&1; then
  echo "expected circular task dependency to be rejected" >&2
  exit 1
fi

if psql_exec -c "BEGIN; UPDATE event_logistics_activity SET status='in_progress',version=2 WHERE id=101 AND version=1; DELETE FROM event_logistics_dependency WHERE activity_id=101; INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100); COMMIT;" >/dev/null 2>&1; then
  echo "expected an activity/relation update cycle to roll back atomically" >&2
  exit 1
fi
test "$(psql_exec -qAt -c "SELECT status || ':' || version FROM event_logistics_activity WHERE id=101;")" = "planned:1"
test "$(psql_exec -qAt -c 'SELECT count(*) FROM event_logistics_dependency WHERE activity_id=101;')" = "0"

if psql_exec -c "BEGIN; INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (105,10,'planned',1); INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (105,102); COMMIT;" >/dev/null 2>&1; then
  echo "expected a cross-event activity/dependency create to roll back atomically" >&2
  exit 1
fi
test "$(psql_exec -qAt -c 'SELECT count(*) FROM event_logistics_activity WHERE id=105;')" = "0"

docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" \
  -c "BEGIN; INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (103,104); SELECT pg_sleep(2); COMMIT;" \
  >/dev/null 2>&1 &
first_dependency_pid=$!
sleep 1
concurrent_reverse_succeeded=false
if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (104,103);' >/dev/null 2>&1; then
  concurrent_reverse_succeeded=true
fi
wait "$first_dependency_pid"
if [ "$concurrent_reverse_succeeded" = true ]; then
  echo "expected concurrent reverse dependencies to serialize and reject a cycle" >&2
  exit 1
fi

if psql_exec -c "UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=100;" >/dev/null 2>&1; then
  echo "expected completion with an incomplete dependency to be rejected" >&2
  exit 1
fi

psql_exec -c "INSERT INTO event_operation_task_override(activity_id,activity_version,override_kind,reason,policy_reference,authorized_by_party_id) VALUES (100,1,'blocked_completion','Emergency venue access','event-ops-emergency-v1',1);" >/dev/null
psql_exec -c "UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=100;" >/dev/null

psql_exec -c "INSERT INTO event_operation_audit_event(event_id,actor_party_id,actor_reference,operation_code,resource_kind,resource_id,outcome,reason,correlation_id) VALUES (10,1,'party:1','task.override','task','100','override','Emergency venue access','test:override');" >/dev/null
if psql_exec -c "UPDATE event_operation_audit_event SET operation_code='tampered';" >/dev/null 2>&1; then
  echo "expected audit history mutation to be rejected" >&2
  exit 1
fi

digest="$(printf 'secure-invitation-token' | shasum -a 256 | awk '{print $1}')"
psql_exec -c "INSERT INTO event_invitation_security(event_invitation_id,token_digest,invited_scopes,expires_at,created_by_party_id) VALUES (50,decode('$digest','hex'),ARRAY['task.read'],now()+interval '1 hour',1);" >/dev/null
if psql_exec -c "INSERT INTO event_invitation_security(event_invitation_id,token_digest,invited_scopes,expires_at,created_by_party_id) VALUES (50,decode('$digest','hex'),ARRAY['event.manage'],now()+interval '1 hour',1);" >/dev/null 2>&1; then
  echo "expected invitation token replay row to be rejected" >&2
  exit 1
fi

apply_sql "$rollback_migration"
preserved_audit=$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_audit_event;')
test "$preserved_audit" = "1"
psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null
psql_exec -c 'DELETE FROM event_logistics_dependency WHERE activity_id=101 AND depends_on_activity_id=100;' >/dev/null
apply_sql "$up_migration"

if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null 2>&1; then
  echo "expected cycle guard to be restored after reapply" >&2
  exit 1
fi

echo "Event operations foundation migration passed apply, idempotency, lifecycle mapping, ownership issue, timezone, RACI, sequential/concurrent DAG, atomic activity/relation rollback, completion override, invitation token, immutable audit, rollback, and reapply checks."
