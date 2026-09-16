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
# Rehearse upgrading an already installed restrictive fence FK.
psql_exec -c 'ALTER TABLE event_operation_task_write_fence DROP CONSTRAINT event_operation_task_write_fence_event_id_fkey; ALTER TABLE event_operation_task_write_fence ADD CONSTRAINT event_operation_task_write_fence_event_id_fkey FOREIGN KEY(event_id) REFERENCES social_event(id) ON DELETE RESTRICT;' >/dev/null
apply_sql "$up_migration"
assert_unprotected_event_deletion() {
  psql_exec -c "BEGIN;
    INSERT INTO social_event(id) VALUES ($1);
    INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES ($2,$1,'planned',1);
    DELETE FROM event_logistics_activity WHERE id=$2;
    DELETE FROM social_event WHERE id=$1;
    COMMIT;" >/dev/null
  test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_task_write_fence WHERE event_id=$1;")" = 0
}
assert_unprotected_event_deletion 20 200

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
# Match the handler's single transaction: the old graph permits completion,
# then replacement introduces an incomplete prerequisite. The failed insertion
# must roll back both the activity/version and the relation deletion.
psql_exec -c "BEGIN;
  INSERT INTO event_logistics_activity(id,event_id,status,version)
    VALUES (150,10,'planned',1),(151,10,'completed',1);
  INSERT INTO event_operation_task_policy(activity_id,requires_accountability)
    VALUES (150,false);
  INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id)
    VALUES (150,151);
  COMMIT;" >/dev/null
if psql_exec -c "BEGIN;
  UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=150 AND version=1;
  DELETE FROM event_logistics_dependency WHERE activity_id=150;
  INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (150,101);
  COMMIT;" >/dev/null 2>&1; then
  echo 'expected replacement with an incomplete prerequisite to reject completion' >&2
  exit 1
fi
test "$(psql_exec -qAt -c "SELECT status || ':' || version FROM event_logistics_activity WHERE id=150;")" = 'planned:1'
test "$(psql_exec -qAt -c 'SELECT depends_on_activity_id FROM event_logistics_dependency WHERE activity_id=150;')" = '151'
# A valid replacement still commits the requested version and graph together.
psql_exec -c "BEGIN;
  UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=150 AND version=1;
  DELETE FROM event_logistics_dependency WHERE activity_id=150;
  INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (150,151);
  COMMIT;" >/dev/null
test "$(psql_exec -qAt -c "SELECT status || ':' || version FROM event_logistics_activity WHERE id=150;")" = 'completed:2'

if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null 2>&1; then
  echo "expected circular task dependency to be rejected" >&2
  exit 1
fi

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

# A historical override cannot authorize a newly inserted or retargeted edge.
for edge_sql in \
  'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,103);' \
  'UPDATE event_logistics_dependency SET depends_on_activity_id=103 WHERE activity_id=100 AND depends_on_activity_id=101;'; do
  if psql_exec -c "$edge_sql" >/dev/null 2>&1; then
    echo 'expected historical override to reject a new incomplete prerequisite' >&2
    exit 1
  fi
done
psql_exec -c 'UPDATE event_logistics_dependency SET depends_on_activity_id=101 WHERE activity_id=100 AND depends_on_activity_id=101;' >/dev/null
test "$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_task_override WHERE activity_id=100;')" = 1

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

# A completed protected task cannot acquire an incomplete prerequisite later.
psql_exec -c "
  BEGIN;
  INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (110,10,'completed',1);
  INSERT INTO event_operation_task_policy(activity_id,requires_accountability) VALUES (110,false);
  COMMIT;
" >/dev/null
if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (110,101);' >/dev/null 2>&1; then
  echo "expected dependency added after completion to be rejected" >&2
  exit 1
fi

# Time passing cannot keep expired accountability valid. Retirement is explicit,
# attributed and transactional with replacement; expiry never invents an actor.
psql_exec -c "
  BEGIN;
  INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (120,10,'planned',1);
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id,valid_until)
    VALUES (120,1,'accountable',1,clock_timestamp()+interval '5 seconds'),
           (120,2,'responsible',1,clock_timestamp()+interval '5 seconds');
  INSERT INTO event_operation_task_policy(activity_id) VALUES (120);
  COMMIT;
" >/dev/null
psql_exec -qAt -c "SELECT pg_sleep(GREATEST(0,EXTRACT(EPOCH FROM
  ((SELECT max(valid_until) FROM event_operation_raci_assignment WHERE activity_id=120) - clock_timestamp()))) + 0.1);" >/dev/null
if psql_exec -c "UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=120;" >/dev/null 2>&1; then
  echo "expected expired RACI coverage to reject completion" >&2
  exit 1
fi
if psql_exec -c "SELECT event_operation_retire_expired_raci(120,NULL,'missing actor');" >/dev/null 2>&1; then
  echo "expected unattributed retirement to be rejected" >&2
  exit 1
fi
psql_exec -c "
  BEGIN;
  SELECT event_operation_retire_expired_raci(120,1,'Expired assignment replacement');
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (120,1,'accountable',1),(120,2,'responsible',1);
  UPDATE event_logistics_activity SET status='completed',version=2 WHERE id=120;
  COMMIT;
" >/dev/null
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=120 AND revoked_by_party_id=1 AND revocation_reason='Expired assignment replacement' AND valid_until <= revoked_at;")" = 2
test "$(psql_exec -qAt -c "SELECT event_operation_retire_expired_raci(120,1,'Do not revoke current assignments');")" = 0

# Future intent grants no current responsibility and cannot be retired as expired.
psql_exec -c "
  BEGIN;
  INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (125,10,'planned',1);
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id,valid_from)
    VALUES (125,1,'accountable',1,now()),(125,2,'responsible',1,now()+interval '1 day');
  COMMIT;
" >/dev/null
if psql_exec -c 'INSERT INTO event_operation_task_policy(activity_id) VALUES (125);' >/dev/null 2>&1; then
  echo 'expected future responsibility to fail current coverage' >&2
  exit 1
fi
test "$(psql_exec -qAt -c "SELECT event_operation_retire_expired_raci(125,1,'Preserve future intent');")" = 0
test "$(psql_exec -qAt -c "SELECT count(*) FROM pg_proc p, LATERAL aclexplode(p.proacl) acl WHERE p.oid='event_operation_retire_expired_raci(bigint,bigint,text)'::regprocedure AND acl.grantee=0 AND acl.privilege_type='EXECUTE';")" = 0

# Two writers cannot each remove a different last Responsible. A separate
# advisory gate holds writer one after its UPDATE; observe writer two waiting
# on the event fence before releasing that gate. Only this private DB is used.
psql_exec -c "
  BEGIN;
  INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (130,10,'planned',1);
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (130,1,'accountable',1),(130,2,'responsible',1),(130,3,'responsible',1);
  INSERT INTO event_operation_task_policy(activity_id) VALUES (130);
  COMMIT;
" >/dev/null
wait_for_state() {
  expected_query="$1"
  poll=0
  until [ "$(psql_exec -qAt -c "$expected_query")" = 1 ]; do
    poll=$((poll+1))
    if [ "$poll" -ge 30 ]; then echo 'expected database synchronization state was not observed' >&2; exit 1; fi
    sleep 1
  done
}
for isolation in 'READ COMMITTED' 'REPEATABLE READ' 'SERIALIZABLE'; do
psql_exec -c "UPDATE event_operation_raci_assignment SET revoked_at=NULL,revoked_by_party_id=NULL,revocation_reason=NULL WHERE activity_id=130 AND party_id=2;" >/dev/null
psql_exec -c "SET application_name='tdf_raci_gate'; SELECT pg_advisory_lock(891231); SELECT pg_sleep(120);" >/dev/null 2>&1 &
gate_pid=$!
wait_for_state "SELECT count(*) FROM pg_stat_activity WHERE application_name='tdf_raci_gate' AND wait_event='PgSleep';"
psql_exec -c "SET application_name='tdf_raci_first'; BEGIN ISOLATION LEVEL $isolation;
  UPDATE event_operation_raci_assignment SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='First writer' WHERE activity_id=130 AND party_id=2;
  SELECT pg_advisory_xact_lock(891231); COMMIT;" >/dev/null 2>&1 &
first_raci_pid=$!
wait_for_state "SELECT count(*) FROM pg_stat_activity WHERE application_name='tdf_raci_first' AND wait_event_type='Lock';"
psql_exec -c "SET application_name='tdf_raci_second'; BEGIN ISOLATION LEVEL $isolation;
  UPDATE event_operation_raci_assignment SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='Second writer' WHERE activity_id=130 AND party_id=3;
  COMMIT;" >/dev/null 2>&1 &
second_raci_pid=$!
wait_for_state "SELECT count(*) FROM pg_stat_activity WHERE application_name='tdf_raci_second' AND wait_event_type='Lock';"
psql_exec -qAt -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE application_name='tdf_raci_gate';" >/dev/null
wait "$gate_pid" || true
wait "$first_raci_pid"
if wait "$second_raci_pid"; then
  echo 'expected the second Responsible removal to fail after the first commit' >&2
  exit 1
fi
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_raci_assignment WHERE activity_id=130 AND raci_role='responsible' AND revoked_at IS NULL;")" = 1
  echo "Observed concurrent RACI removal passed at $isolation"
done

apply_sql "$rollback_migration"
assert_unprotected_event_deletion 21 210
preserved_audit=$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_audit_event;')
test "$preserved_audit" = "1"
psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null
psql_exec -c 'DELETE FROM event_logistics_dependency WHERE activity_id=101 AND depends_on_activity_id=100;' >/dev/null
apply_sql "$up_migration"

if psql_exec -c 'INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (101,100);' >/dev/null 2>&1; then
  echo "expected cycle guard to be restored after reapply" >&2
  exit 1
fi

echo "Event operations foundation migration passed apply, idempotency, lifecycle mapping, ownership issue, timezone, RACI, sequential/concurrent DAG, completion override/post-completion dependency, expiry/attributed replacement, observed concurrent RACI removal, invitation token, immutable audit, rollback, and reapply checks."
