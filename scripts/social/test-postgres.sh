#!/usr/bin/env bash
set -euo pipefail
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
TDF_SOCIAL_CONTAINER="tdf-social-verification-$$"
python3 - "$TDF_SOCIAL_ROOT" <<'PYHASH'
import hashlib,sys
from pathlib import Path
root=Path(sys.argv[1])
for p in sorted((root/'tdf-hq/sql').glob('*social_v2*.sql')):
    print(hashlib.sha256(p.read_bytes()).hexdigest(),p.relative_to(root))
PYHASH
if [ "${TDF_SOCIAL_NATIVE:-0}" = 1 ]; then
  TDF_SOCIAL_PG_BIN=${TDF_SOCIAL_PG_BIN:-/usr/local/opt/postgresql@16/bin}
  TDF_SOCIAL_PG_DATA=$(mktemp -d)
  TDF_SOCIAL_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
  "$TDF_SOCIAL_PG_BIN/initdb" -D "$TDF_SOCIAL_PG_DATA" -U postgres -A trust --no-locale -E UTF8 >/dev/null
  trap '"$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -m immediate -w stop >/dev/null 2>&1 || true' EXIT
  "$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -l "$TDF_SOCIAL_PG_DATA/server.log" \
    -o "-h 127.0.0.1 -p $TDF_SOCIAL_PORT -k $TDF_SOCIAL_PG_DATA" -w start >/dev/null
  "$TDF_SOCIAL_PG_BIN/createdb" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -U postgres social_test
  psql_test() { "$TDF_SOCIAL_PG_BIN/psql" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -X -v ON_ERROR_STOP=1 -U postgres -d social_test "$@"; }
else
trap 'docker rm -f "$TDF_SOCIAL_CONTAINER" >/dev/null 2>&1 || true' EXIT
 docker run --rm -d --name "$TDF_SOCIAL_CONTAINER" -e POSTGRES_PASSWORD=synthetic-only \
  -e POSTGRES_DB=social_test postgres:16-alpine >/dev/null
for attempt in $(seq 1 30); do
  if docker exec "$TDF_SOCIAL_CONTAINER" pg_isready -h 127.0.0.1 -U postgres -d social_test >/dev/null 2>&1; then break; fi
  sleep 1
done
psql_test() { docker exec -i "$TDF_SOCIAL_CONTAINER" psql -X -v ON_ERROR_STOP=1 -U postgres -d social_test "$@"; }
fi
psql_test <<'SQL'
CREATE TABLE party(id bigint PRIMARY KEY, display_name text NOT NULL, is_org boolean NOT NULL DEFAULT false);
CREATE TABLE user_credential(id bigint PRIMARY KEY,party_id bigint REFERENCES party(id),active boolean NOT NULL DEFAULT true);
INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(1,5) n;
INSERT INTO user_credential SELECT n,n,true FROM generate_series(1,5) n;
SQL
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_test <<'SQL'
DO $$ BEGIN
  ASSERT social_v2_mutate(1,2,'request',0,'disabled')->>'error'='disabled';
END $$;
UPDATE social_v2_runtime SET enabled=true;
DO $$ BEGIN
  ASSERT social_v2_mutate(1,1,'request',0,'self')->>'error'='invalid';
  ASSERT social_v2_mutate(1,999,'request',0,'missing')->>'error'='unavailable';
  ASSERT social_v2_mutate(1,2,'accept',0,'early')->>'error'='no_request';
  ASSERT social_v2_mutate(1,2,'request',0,'request')->>'connected'='false';
  ASSERT social_v2_mutate(1,2,'request',0,'request')->>'revision'='1';
  ASSERT social_v2_mutate(1,2,'follow',0,'request')->>'error'='request_key_conflict';
  ASSERT social_v2_mutate(2,1,'accept',0,'stale')->>'error'='revision_conflict';
  ASSERT social_v2_mutate(2,1,'accept',1,'accept')->>'connected'='true';
  ASSERT social_v2_mutate(1,2,'block',2,'block')->>'connected'='false';
  ASSERT social_v2_mutate(2,1,'accept',1,'accept')->>'error'='unavailable';
  ASSERT social_v2_mutate(1,2,'unblock',3,'unblock')->>'connected'='false';
  ASSERT social_v2_mutate(2,1,'accept',1,'accept')->>'connected'='false';
  ASSERT social_v2_mutate(2,1,'accept',1,'accept')->>'revision'='4';
  ASSERT social_v2_mutate(1,2,'request',0,'late')->>'error'='revision_conflict';
  ASSERT social_v2_mutate(1,2,'mute',4,'mute')->>'muted'='true';
  ASSERT social_v2_allowed(1,2);
  ASSERT social_v2_mutate(1,2,'dismiss',5,'dismiss')->>'dismissed'='true';
  ASSERT social_v2_preferences(1,true,false,0)->>'personalized'='false';
  ASSERT social_v2_preferences(1,true,true,0)->>'error'='revision_conflict';
  ASSERT social_v2_close(1)->>'closed'='true';
  ASSERT NOT social_v2_allowed(2,1);
  ASSERT social_v2_mutate(1,2,'request',0,'request')->>'error'='unavailable';
  ASSERT social_v2_preferences(1,true,true,1)->>'error'='unavailable';
END $$;
SQL
# Coordinate with a separate row-lock barrier, rather than hoping a short
# sleep is observed under Docker/CI contention. Only this disposable DB is used.
psql_test -c "CREATE TABLE social_race_barrier(id integer PRIMARY KEY); INSERT INTO social_race_barrier VALUES(1);" >/dev/null
wait_for_session() {
  for attempt in $(seq 1 100); do
    waiting=$(psql_test -Atc "SELECT count(*) FROM pg_stat_activity WHERE application_name='$1' AND wait_event_type='$2'")
    if [ "$waiting" = 1 ]; then return; fi
  done
  echo "Race barrier not reached: $1 / $2" >&2
  return 1
}
psql_test -c "SET application_name='social-race-gate'; BEGIN; SELECT id FROM social_race_barrier FOR UPDATE; SELECT pg_sleep(300);" >/dev/null 2>&1 &
gate_pid=$!
wait_for_session social-race-gate Timeout
psql_test -c "SELECT social_v2_mutate(3,4,'request',0,'race-request')" >/dev/null
psql_test -c "SET application_name='social-race-block'; BEGIN; SELECT social_v2_mutate(3,4,'block',1,'race-block'); SELECT id FROM social_race_barrier FOR UPDATE; COMMIT;" > /tmp/tdf-social-race-block-$$.txt &
race_pid=$!
wait_for_session social-race-block Lock
psql_test -Atc "SET application_name='social-race-accept'; SELECT social_v2_mutate(4,3,'accept',1,'race-accept')->>'error';" > /tmp/tdf-social-race-accept-$$.txt &
accept_pid=$!
wait_for_session social-race-accept Lock
# Releasing this controller connection lets block commit before accept resumes.
psql_test -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE application_name='social-race-gate'" >/dev/null
wait "$gate_pid" || true # Explicit, expected controller termination only.
wait "$race_pid"
wait "$accept_pid"
[ "$(tail -1 /tmp/tdf-social-race-accept-$$.txt)" = unavailable ]
psql_test -c "DO \$\$ BEGIN ASSERT NOT EXISTS(SELECT 1 FROM social_v2_pair WHERE party_a=3 AND party_b=4 AND (consent_a OR consent_b)); END \$\$;"
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_pause.sql"
psql_test <<'SQL'
DO $$ BEGIN
  ASSERT social_v2_mutate(3,4,'unblock',2,'pause')->>'error'='disabled';
  ASSERT (SELECT block_a FROM social_v2_pair WHERE party_a=3 AND party_b=4);
  ASSERT (SELECT closed FROM social_v2_preference WHERE party_id=1);
  ASSERT (SELECT count(*) FROM social_v2_command)>0;
END $$;
SQL
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
psql_test -c "DO \$\$ BEGIN ASSERT NOT (SELECT enabled FROM social_v2_runtime); ASSERT (SELECT block_a FROM social_v2_pair WHERE party_a=3 AND party_b=4); END \$\$;"
psql_test < "$TDF_SOCIAL_ROOT/scripts/social/fixture.sql"
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_read_models.sql"
psql_test < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_read_models.sql"
psql_test < "$TDF_SOCIAL_ROOT/scripts/social/read-model-tests.sql"
psql_test < "$TDF_SOCIAL_ROOT/scripts/social/model-cases.sql"
if [ "${TDF_SOCIAL_BENCHMARK:-0}" = 1 ]; then
  if [ "${TDF_SOCIAL_FEED_BENCHMARK:-0}" = 1 ]; then
    cat "$TDF_SOCIAL_ROOT/scripts/social/benchmark.sql" "$TDF_SOCIAL_ROOT/scripts/social/feed-benchmark.sql" | psql_test
  else
    psql_test < "$TDF_SOCIAL_ROOT/scripts/social/benchmark.sql"
  fi
fi
echo 'PASS: feed ordering, eligibility, edits, deletion, backdated publish, revocation, discovery opt-out/exclusions'
echo 'PASS: additive reapply, denied defaults, consent, retry, block race, preferences, closure, pause preserves new writes'
