#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
. "$repo_root/scripts/lib/postgres-test-database.sh"
tdf_test_db_init tdf_event_memory_policy_test
migration_batch=$(mktemp)
dispatch_output=$(mktemp)
rollback_output=$(mktemp)
# Keep database ownership cleanup from the shared helper, and remove only our file.
cleanup_memories() { rm -f "$migration_batch" "$dispatch_output" "$rollback_output"; tdf_test_db_cleanup; }
trap cleanup_memories EXIT INT TERM
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
tdf_test_psql -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" > "$migration_batch"
tdf_test_psql -f "$migration_batch" >/dev/null
# Direct replay is deliberate: manifest application and repeated SQL must agree.
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_invitations.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_invitations.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_invitations.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_invitations_rollback.sql" >/dev/null
test "$(tdf_test_psql -qAtc "SELECT to_regprocedure('event_memory_dispatch_batch(integer)') IS NULL")" = t
tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_invitations.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_invitations.sql" >/dev/null
tdf_test_psql -f "$repo_root/tdf-hq/test/integration/event_memory_invitation_concurrency.sql" >/dev/null
tdf_test_psql -qAt > "$dispatch_output" <<'SQL' &
BEGIN;
SELECT pg_advisory_xact_lock(hashtextextended('event-memory-dispatch',0));
SELECT pg_sleep(2);
SELECT event_memory_dispatch_batch(20);
COMMIT;
SQL
dispatch_pid=$!
lock_observed=false
for attempt in 1 2 3 4 5 6 7 8 9 10; do
 if test "$(tdf_test_psql -qAtc "SELECT NOT pg_try_advisory_lock(hashtextextended('event-memory-dispatch',0))")" = t; then
  lock_observed=true; break
 fi
 sleep 0.1
done
test "$lock_observed" = true
test "$(tdf_test_psql -qAtc 'SELECT event_memory_dispatch_batch(20)')" = 0
wait "$dispatch_pid"
test "$(tr -d '[:space:]' < "$dispatch_output")" = 1
test "$(tdf_test_psql -qAtc 'SELECT count(*)=1 FROM event_memory_delivery WHERE event_id=919142001 AND step=1')" = t
if tdf_test_psql -f "$repo_root/tdf-hq/sql/2026-10-25_event_memory_invitations_rollback.sql" > "$rollback_output" 2>&1; then
 echo 'Rollback must refuse after an actual synthetic delivery' >&2; exit 1
fi
grep -q 'Retain the installed privacy authority' "$rollback_output"
test "$(tdf_test_psql -qAtc 'SELECT count(*)=1 FROM event_memory_delivery WHERE event_id=919142001 AND step=1')" = t
python3 "$repo_root/formal/event-memories/check.py"
printf '%s\n' 'Event memories: full schema, replay, rollback/reapply, concurrent dispatch, retained-delivery refusal and policy assertions passed'
