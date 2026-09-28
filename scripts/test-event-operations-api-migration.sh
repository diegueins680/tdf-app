#!/bin/sh
set -eu

test_container="tdf-event-operations-api-test-$$"
test_database="tdf_event_operations_api_test"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
foundation_migration="$repo_root/tdf-hq/sql/2026-09-14_event_operations_foundation.sql"
api_migration="$repo_root/tdf-hq/sql/2026-09-14_event_operations_api.sql"
api_rollback="$repo_root/tdf-hq/sql/2026-09-14_event_operations_api_rollback.sql"
fixture_sql="$repo_root/tdf-hq/test/integration/event_operations_foundation_fixture.sql"
result_dir=$(mktemp -d "${TMPDIR:-/tmp}/tdf-event-operations-api.XXXXXX")

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
  case "$result_dir" in
    */tdf-event-operations-api.*) rm -rf -- "$result_dir" ;;
    *) echo "Refusing to remove unexpected test result path: $result_dir" >&2 ;;
  esac
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -p 127.0.0.1::5432 \
  -e POSTGRES_PASSWORD=event-operations-api-test \
  -e POSTGRES_DB="$test_database" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$test_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$test_container" >&2
    echo "PostgreSQL event operations API test database did not become ready" >&2
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

json_field() {
  printf '%s' "$1" | jq -r "$2"
}

transition_json() {
  psql_exec -qAt -c "SELECT event_operation_apply_transition($1,$2,'$3',$4,'$5',$6,'$7',encode(digest('$8','sha256'),'hex'));"
}

wait_for_replay_barrier() {
  replay_backend="$1"; replay_wait="$2"; replay_attempt=0
  until [ "$(psql_exec -qAtc "SELECT count(*) FROM pg_stat_activity WHERE application_name='$replay_backend' AND wait_event_type='$replay_wait'")" = 1 ]; do
    replay_attempt=$((replay_attempt + 1))
    if [ "$replay_attempt" -ge 50 ]; then
      echo "Replay test backend did not reach $replay_wait: $replay_backend" >&2
      exit 1
    fi
    sleep 0.1
  done
}

test_replay_revocation_race() {
  replay_event="$1"; replay_isolation="$2"
  replay_command="30000000-0000-4000-8000-0000000000$replay_event"
  psql_exec -c "BEGIN;
    INSERT INTO social_event(id,organizer_party_id) VALUES ($replay_event,'1');
    INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
      VALUES ($replay_event,'planning',1,'replay race');
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
      VALUES ($replay_event,2,'event.manage',1); COMMIT;" >/dev/null
  replay_original=$(transition_json "$replay_event" 2 "$replay_command" 1 pending_approval NULL replay-race request-race)
  test "$(json_field "$replay_original" '.version')" = 2
  transition_json "$replay_event" 2 "$replay_command" 1 pending_approval NULL replay-race request-race > "$result_dir/replay-a.json" &
  replay_a=$!
  transition_json "$replay_event" 2 "$replay_command" 1 pending_approval NULL replay-race request-race > "$result_dir/replay-b.json" &
  replay_b=$!
  wait "$replay_a"; wait "$replay_b"
  test "$(jq -s '[.[] | select(.replayed == true and .version == 2)] | length' "$result_dir/replay-a.json" "$result_dir/replay-b.json")" = 2

  psql_exec -c "SET application_name='replay_coordinator_$replay_event';
    SELECT pg_advisory_lock(885,$replay_event); SELECT pg_sleep(60);" > "$result_dir/coordinator.log" 2>&1 &
  replay_coordinator=$!
  wait_for_replay_barrier "replay_coordinator_$replay_event" Timeout
  psql_exec -c "BEGIN; SET LOCAL application_name='replay_revoke_$replay_event';
    UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,
      revocation_reason='concurrent replay test' WHERE event_id=$replay_event;
    SELECT pg_advisory_xact_lock(885,$replay_event); COMMIT;" > "$result_dir/revoke.log" 2>&1 &
  replay_revoke=$!
  wait_for_replay_barrier "replay_revoke_$replay_event" Lock
  psql_exec --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $replay_isolation;
    SET LOCAL application_name='replay_read_$replay_event';
    SELECT event_operation_apply_transition($replay_event,2,'$replay_command',1,'pending_approval',NULL,'replay-race',encode(digest('request-race','sha256'),'hex'));
    COMMIT;" > "$result_dir/read.log" 2>&1 &
  replay_read=$!
  wait_for_replay_barrier "replay_read_$replay_event" Lock
  psql_exec --set=VERBOSITY=verbose -qAtc "BEGIN ISOLATION LEVEL $replay_isolation;
    SET LOCAL application_name='snapshot_read_$replay_event';
    SELECT event_operation_read_snapshot($replay_event,2) IS NULL;
    COMMIT;" > "$result_dir/snapshot.log" 2>&1 &
  snapshot_read=$!
  wait_for_replay_barrier "snapshot_read_$replay_event" Lock
  psql_exec -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='replay_coordinator_$replay_event'" >/dev/null
  wait "$replay_coordinator" || true
  wait "$replay_revoke"
  if [ "$replay_isolation" = 'READ COMMITTED' ]; then
    wait "$replay_read"
    test "$(jq -r 'keys|join(",")' "$result_dir/read.log")" = error
    test "$(jq -r '.error' "$result_dir/read.log")" = not_found
    wait "$snapshot_read"
    test "$(tail -n 1 "$result_dir/snapshot.log")" = t
  else
    if wait "$replay_read"; then
      echo "Stale $replay_isolation command did not abort" >&2
      exit 1
    fi
    grep -q 40001 "$result_dir/read.log" || { sed -n '1,50p' "$result_dir/read.log" >&2; exit 1; }
    if wait "$snapshot_read"; then
      echo "Stale $replay_isolation snapshot did not abort" >&2
      exit 1
    fi
    grep -q 40001 "$result_dir/snapshot.log" || { sed -n '1,50p' "$result_dir/snapshot.log" >&2; exit 1; }
  fi
  replay_retry=$(transition_json "$replay_event" 2 "$replay_command" 1 pending_approval NULL replay-race request-race)
  test "$(json_field "$replay_retry" '.error')" = not_found
  test "$(psql_exec -qAtc "SELECT version FROM event_operation_event_state WHERE event_id=$replay_event")" = 2
  test "$(psql_exec -qAtc "SELECT count(*) FROM event_operation_transition WHERE event_id=$replay_event")" = 1
  test "$(psql_exec -qAtc "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=$replay_event")" = 1
}

test_snapshot_disable_race() {
  psql_exec -c "SET application_name='snapshot_flag_coordinator';
    SELECT pg_advisory_lock(886,1); SELECT pg_sleep(60);" > "$result_dir/flag-coordinator.log" 2>&1 &
  flag_coordinator=$!
  wait_for_replay_barrier snapshot_flag_coordinator Timeout
  psql_exec -c "BEGIN; SET LOCAL application_name='snapshot_flag_writer';
    UPDATE event_operation_feature_flag SET enabled=FALSE,updated_at=clock_timestamp(),
      change_reason='snapshot race disable' WHERE feature_code='event.operations.api';
    SELECT pg_advisory_xact_lock(886,1); COMMIT;" > "$result_dir/flag-writer.log" 2>&1 &
  flag_writer=$!
  wait_for_replay_barrier snapshot_flag_writer Lock
  psql_exec -qAtc "BEGIN; SET LOCAL application_name='snapshot_flag_reader';
    SELECT event_operation_read_snapshot(10,1) IS NULL; COMMIT;" > "$result_dir/flag-reader.log" 2>&1 &
  flag_reader=$!
  wait_for_replay_barrier snapshot_flag_reader Lock
  psql_exec -qAtc "SELECT pg_terminate_backend(pid) FROM pg_stat_activity
    WHERE datname=current_database() AND application_name='snapshot_flag_coordinator'" >/dev/null
  wait "$flag_coordinator" || true
  wait "$flag_writer"; wait "$flag_reader"
  test "$(tail -n 1 "$result_dir/flag-reader.log")" = t
  psql_exec -c "UPDATE event_operation_feature_flag SET enabled=TRUE,updated_at=clock_timestamp(),
    updated_by_party_id=1,change_reason='snapshot test restore' WHERE feature_code='event.operations.api';" >/dev/null
}

apply_sql "$fixture_sql"
apply_sql "$foundation_migration"
apply_sql "$api_migration"
apply_sql "$api_migration"
test "$(psql_exec -qAtc 'SELECT event_operation_read_snapshot(10,1) IS NULL')" = t

flag_enabled=$(psql_exec -qAt -c "SELECT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api';")
test "$flag_enabled" = "f"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_feature_flag_history WHERE feature_code='event.operations.api' AND enabled=FALSE;")" = "1"
if psql_exec -qAt -c "UPDATE event_operation_feature_flag SET enabled=TRUE, updated_at=now() WHERE feature_code='event.operations.api';" >/dev/null 2>&1; then
  echo "Feature flag activation without actor and reason unexpectedly succeeded" >&2
  exit 1
fi
disabled_result=$(transition_json 10 1 10000000-0000-4000-8000-000000000001 1 planning NULL api-disabled request-disabled)
test "$(json_field "$disabled_result" '.error')" = "feature_disabled"

psql_exec -c "
  INSERT INTO social_event(id,organizer_party_id,workflow_state_id) VALUES
    (12,'1','00000000-0000-0000-0000-000000000001'),
    (13,'1','00000000-0000-0000-0000-000000000001');
  INSERT INTO event_operation_event_state(event_id,canonical_state,version,legacy_state_code,migration_evidence) VALUES
    (12,'planning',1,'planning','API transition test'),
    (13,'planning',1,'planning','API concurrency test');
  INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind,created_by_party_id,provenance) VALUES
    (12,1,'primary_owner',1,'{\"source\":\"api-test\"}'),
    (13,1,'primary_owner',1,'{\"source\":\"api-test\"}');
  UPDATE event_operation_feature_flag
  SET enabled=TRUE, updated_by_party_id=1, change_reason='ephemeral API migration verification'
  WHERE feature_code='event.operations.api';
  INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
  VALUES (10,2,'event.approve',1), (12,1,'event.approve',1);
" >/dev/null

owner_can_read=$(psql_exec -qAt -c 'SELECT event_operation_actor_can_read(10,1);')
apply_sql "$repo_root/tdf-hq/test/integration/event_operations_command_privacy_assertions.sql"
outsider_can_read=$(psql_exec -qAt -c 'SELECT event_operation_actor_can_read(10,3);')
test "$owner_can_read" = "t"
test "$outsider_can_read" = "f"

invalid_command=$(transition_json 10 1 10000000-0000-4000-8000-000000000009 0 pending_approval NULL invalid-command request-invalid)
test "$(json_field "$invalid_command" '.error')" = "invalid_request"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_audit_event WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000009' AND outcome='rejected';")" = "1"

submitted=$(transition_json 10 1 10000000-0000-4000-8000-000000000010 1 pending_approval NULL submit-review request-submit)
test "$(json_field "$submitted" '.canonicalState')" = "pending_approval"
test "$(json_field "$submitted" '.version')" = "2"
test "$(json_field "$submitted" '.replayed')" = "false"

replayed=$(transition_json 10 1 10000000-0000-4000-8000-000000000010 1 pending_approval NULL submit-review request-submit)
test "$(json_field "$replayed" '.replayed')" = "true"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_transition WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000010';")" = "1"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_audit_event WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000010';")" = "1"

reused_key=$(transition_json 10 1 10000000-0000-4000-8000-000000000010 1 pending_approval NULL submit-review changed-request)
test "$(json_field "$reused_key" '.error')" = "idempotency_conflict"
reused_key_actor=$(transition_json 10 3 10000000-0000-4000-8000-000000000010 1 pending_approval NULL submit-review request-submit)
test "$(json_field "$reused_key_actor" '.error')" = "not_found"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_audit_event WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000010' AND outcome='conflict';")" = "1"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_audit_event WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000010' AND outcome='rejected';")" = "1"

forbidden=$(transition_json 10 3 10000000-0000-4000-8000-000000000011 2 approved NULL outsider-approval request-outsider)
test "$(json_field "$forbidden" '.error')" = "not_found"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_audit_event WHERE event_id=10 AND command_id='10000000-0000-4000-8000-000000000011' AND outcome='rejected';")" = "1"

approved=$(transition_json 10 2 10000000-0000-4000-8000-000000000012 2 approved NULL independent-approval request-approval)
test "$(json_field "$approved" '.canonicalState')" = "approved"
test "$(json_field "$approved" '.version')" = "3"

effects_not_ready=$(transition_json 10 1 10000000-0000-4000-8000-000000000013 3 published NULL blocked-publish request-publish)
test "$(json_field "$effects_not_ready" '.error')" = "transition_effects_not_ready"

reason_required=$(transition_json 10 1 10000000-0000-4000-8000-000000000014 3 planning NULL rollback-no-reason request-rollback-empty)
test "$(json_field "$reason_required" '.error')" = "reason_required"
rolled_back=$(transition_json 10 1 10000000-0000-4000-8000-000000000015 3 planning "'Venue plan changed'" rollback-reason request-rollback)
test "$(json_field "$rolled_back" '.canonicalState')" = "planning"
test "$(json_field "$rolled_back" '.version')" = "4"

same_actor_submit=$(transition_json 12 1 10000000-0000-4000-8000-000000000020 1 pending_approval NULL same-actor-submit request-same-submit)
test "$(json_field "$same_actor_submit" '.version')" = "2"
same_actor_approval=$(transition_json 12 1 10000000-0000-4000-8000-000000000021 2 approved NULL same-actor-approval request-same-approval)
test "$(json_field "$same_actor_approval" '.error')" = "separation_of_duties"

transition_json 13 1 10000000-0000-4000-8000-000000000030 1 pending_approval NULL concurrent-a request-concurrent-a > "$result_dir/a.json" &
first_pid=$!
transition_json 13 1 10000000-0000-4000-8000-000000000031 1 pending_approval NULL concurrent-b request-concurrent-b > "$result_dir/b.json" &
second_pid=$!
wait "$first_pid"
wait "$second_pid"

accepted_count=$(jq -s '[.[] | select(.canonicalState == "pending_approval" and .version == 2)] | length' "$result_dir/a.json" "$result_dir/b.json")
conflict_count=$(jq -s '[.[] | select(.error == "version_conflict")] | length' "$result_dir/a.json" "$result_dir/b.json")
test "$accepted_count" = "1"
test "$conflict_count" = "1"
test "$(psql_exec -qAt -c 'SELECT version FROM event_operation_event_state WHERE event_id=13;')" = "2"
test "$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_transition WHERE event_id=13;')" = "1"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_command_receipt WHERE event_id=13 AND operation_code='event.lifecycle.transition';")" = "2"

apply_sql "$repo_root/tdf-hq/test/integration/event_operations_replay_assertions.sql"
apply_sql "$repo_root/tdf-hq/test/integration/event_operations_snapshot_assertions.sql"
test_replay_revocation_race 30 'READ COMMITTED'
test_replay_revocation_race 31 'REPEATABLE READ'
test_replay_revocation_race 32 'SERIALIZABLE'
test_snapshot_disable_race
replay_epoch=$(psql_exec -qAtc 'SELECT authorization_version FROM event_operation_event_state WHERE event_id=20')

if [ "${RUN_EVENT_OPERATIONS_HASKELL_TESTS:-0}" = 1 ]; then
  snapshot_port=$(docker port "$test_container" 5432/tcp)
  snapshot_port=${snapshot_port##*:}
  (
    cd "$repo_root/tdf-hq"
    EVENT_OPERATIONS_TEST_DSN="host=127.0.0.1 port=$snapshot_port user=postgres password=event-operations-api-test dbname=$test_database" \
      stack exec -- runghc -isrc -itest test/EventOperationsBoundaryMain.hs
  )
fi

apply_sql "$api_rollback"
apply_sql "$api_rollback"
test "$(psql_exec -qAt -c "SELECT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api';")" = "f"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_feature_flag_history WHERE feature_code='event.operations.api';")" = "5"
test "$(psql_exec -qAtc "SELECT to_regprocedure('event_operation_read_snapshot(bigint,bigint)') IS NULL")" = t
if psql_exec -qAt -c "UPDATE event_operation_feature_flag_history SET change_reason='tampered' WHERE id=(SELECT min(id) FROM event_operation_feature_flag_history);" >/dev/null 2>&1; then
  echo "Feature flag history update unexpectedly succeeded" >&2
  exit 1
fi
test "$(psql_exec -qAt -c "SELECT to_regprocedure('event_operation_apply_transition(bigint,bigint,uuid,bigint,text,text,text,text)') IS NULL;")" = "t"
test "$(psql_exec -qAt -c 'SELECT count(*) FROM event_operation_transition WHERE event_id IN (10,12,13);')" = "5"

apply_sql "$api_migration"
test "$(psql_exec -qAtc 'SELECT authorization_version FROM event_operation_event_state WHERE event_id=20')" = "$replay_epoch"
test "$(psql_exec -qAtc 'SELECT count(*) FROM event_operation_transition WHERE event_id=20')" = 1
test "$(psql_exec -qAtc 'SELECT count(*) FROM event_operation_command_receipt WHERE event_id=20')" = 3
test "$(psql_exec -qAtc 'SELECT count(*) FROM event_operation_command_receipt WHERE event_id IN (80,81,82)')" = 3
test "$(psql_exec -qAtc 'SELECT count(*) FROM event_operation_transition WHERE event_id IN (80,81,82)')" = 1
test "$(psql_exec -qAtc 'SELECT count(*) FROM event_operation_audit_event WHERE event_id IN (80,81,82)')" = 9
test "$(psql_exec -qAt -c "SELECT enabled FROM event_operation_feature_flag WHERE feature_code='event.operations.api';")" = "f"
test "$(psql_exec -qAt -c "SELECT count(*) FROM event_operation_feature_flag_history WHERE feature_code='event.operations.api';")" = "5"
test "$(psql_exec -qAtc 'SELECT event_operation_read_snapshot(10,1) IS NULL')" = t
test "$(psql_exec -qAt -c "SELECT to_regprocedure('event_operation_apply_transition(bigint,bigint,uuid,bigint,text,text,text,text)') IS NOT NULL;")" = "t"

echo "Event operations API migration passed replay/snapshot privacy, fresh-clock coherent projection, approval visibility, concurrent revocation (RC/RR/Serializable), flag-disable race, immutable history, idempotency, lifecycle guards, rollback and reapply."
