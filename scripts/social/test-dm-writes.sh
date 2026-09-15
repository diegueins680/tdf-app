#!/usr/bin/env bash
set -euo pipefail
TDF_SOCIAL_ROOT=$(cd "$(dirname "$0")/../.." && pwd)
if [ "${TDF_SOCIAL_NATIVE:-0}" = 1 ]; then
  TDF_SOCIAL_PG_BIN=${TDF_SOCIAL_PG_BIN:-/usr/local/opt/postgresql@16/bin}
  TDF_SOCIAL_PG_DATA=$(mktemp -d)
  TDF_SOCIAL_PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1]); s.close()')
  "$TDF_SOCIAL_PG_BIN/initdb" -D "$TDF_SOCIAL_PG_DATA" -U postgres -A trust --no-locale -E UTF8 >/dev/null
  trap '"$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -m immediate -w stop >/dev/null 2>&1 || true' EXIT
  "$TDF_SOCIAL_PG_BIN/pg_ctl" -D "$TDF_SOCIAL_PG_DATA" -l "$TDF_SOCIAL_PG_DATA/server.log" -o "-h 127.0.0.1 -p $TDF_SOCIAL_PORT -k $TDF_SOCIAL_PG_DATA" -w start >/dev/null
  "$TDF_SOCIAL_PG_BIN/createdb" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -U postgres social_dm
  psql_dm() { "$TDF_SOCIAL_PG_BIN/psql" -h 127.0.0.1 -p "$TDF_SOCIAL_PORT" -X -v ON_ERROR_STOP=1 -v dm_benchmark="${TDF_SOCIAL_DM_BENCHMARK:-0}" -U postgres -d social_dm "$@"; }
else
  TDF_SOCIAL_CONTAINER="tdf-social-dm-$$"
  trap 'docker rm -f "$TDF_SOCIAL_CONTAINER" >/dev/null 2>&1 || true' EXIT
  docker run --rm -d --name "$TDF_SOCIAL_CONTAINER" -e POSTGRES_PASSWORD=synthetic-only -e POSTGRES_DB=social_dm postgres:17-alpine >/dev/null
  for attempt in $(seq 1 30); do
    if docker exec "$TDF_SOCIAL_CONTAINER" pg_isready -h 127.0.0.1 -U postgres -d social_dm >/dev/null 2>&1; then break; fi
    sleep 1
  done
  psql_dm() { docker exec -i "$TDF_SOCIAL_CONTAINER" psql -X -v ON_ERROR_STOP=1 -v dm_benchmark="${TDF_SOCIAL_DM_BENCHMARK:-0}" -U postgres -d social_dm "$@"; }
fi
psql_dm <<'SQL'
CREATE TABLE party(id bigint PRIMARY KEY,display_name text NOT NULL,is_org boolean NOT NULL DEFAULT false);
CREATE TABLE user_credential(id bigint PRIMARY KEY,party_id bigint REFERENCES party(id),active boolean NOT NULL DEFAULT true);
INSERT INTO party SELECT n,'Synthetic '||n,false FROM generate_series(1,5) n;
INSERT INTO user_credential SELECT n,n,true FROM generate_series(1,5) n;
CREATE TABLE chat_thread(id bigint PRIMARY KEY,dm_party_a bigint NOT NULL REFERENCES party(id),dm_party_b bigint NOT NULL REFERENCES party(id));
CREATE TABLE chat_message(id bigserial PRIMARY KEY,thread_id bigint NOT NULL REFERENCES chat_thread(id),sender_party_id bigint NOT NULL REFERENCES party(id),body text NOT NULL);
INSERT INTO chat_thread VALUES(1,1,2);
CREATE FUNCTION dm_try_send(actor bigint, message text) RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO chat_message(thread_id,sender_party_id,body) VALUES(1,actor,message);
  RETURN true;
EXCEPTION WHEN insufficient_privilege THEN RETURN false;
END $$;
CREATE TABLE dm_benchmark(label text,ms double precision);
CREATE FUNCTION dm_measure(run_label text) RETURNS void LANGUAGE plpgsql AS $$
DECLARE started timestamptz;
BEGIN
  FOR n IN 1..55 LOOP
    started := clock_timestamp();
    ASSERT dm_try_send(1,'synthetic benchmark');
    IF n>5 THEN INSERT INTO dm_benchmark VALUES(run_label,extract(epoch FROM clock_timestamp()-started)*1000); END IF;
  END LOOP;
  DELETE FROM chat_message WHERE body='synthetic benchmark';
END $$;
\if :dm_benchmark
SELECT dm_measure('before');
\endif
SQL
psql_dm < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_foundation.sql"
if [ "${TDF_SOCIAL_DM_UNSAFE_CONTROL:-0}" != 1 ]; then
  psql_dm < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
  psql_dm < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
else
  # Deliberately reproduce the old unguarded INSERT using the same generated tests.
  psql_dm -c 'ALTER TABLE social_v2_runtime ADD COLUMN activated_once boolean NOT NULL DEFAULT false;'
fi
if [ "${TDF_SOCIAL_DM_BENCHMARK:-0}" = 1 ]; then
  psql_dm -c "SELECT dm_measure('inactive');"
fi
psql_dm < "$TDF_SOCIAL_ROOT/scripts/social/dm-model-cases.sql"
echo 'PASS: 27 generated observed INSERT outcomes'
psql_dm <<'SQL'
DO $$ BEGIN
  ASSERT NOT (SELECT activated_once OR enabled FROM social_v2_runtime);
  ASSERT dm_try_send(1,'legacy before activation');
END $$;
UPDATE social_v2_runtime SET enabled=true;
UPDATE social_v2_runtime SET enabled=false,activated_once=false;
DO $$ BEGIN
  ASSERT (SELECT activated_once AND NOT enabled FROM social_v2_runtime);
  ASSERT NOT dm_try_send(1,'paused without new consent');
  BEGIN DELETE FROM social_v2_runtime; RAISE EXCEPTION 'activation delete succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN TRUNCATE social_v2_runtime; RAISE EXCEPTION 'activation truncate succeeded';
    EXCEPTION WHEN check_violation THEN NULL; END;
END $$;
UPDATE social_v2_runtime SET enabled=true;
SELECT social_v2_mutate(1,2,'request',0,'dm-request');
SELECT social_v2_mutate(2,1,'accept',1,'dm-accept');
\if :dm_benchmark
SELECT dm_measure('active');
-- Predeclared criterion: <=10ms added p95 server-side INSERT time on this fixture.
SELECT label, count(*) AS samples,percentile_disc(0.5) WITHIN GROUP(ORDER BY ms) AS p50_ms,
  percentile_disc(0.95) WITHIN GROUP(ORDER BY ms) AS p95_ms FROM dm_benchmark GROUP BY label ORDER BY label;
DO $$ DECLARE baseline double precision; BEGIN
  SELECT percentile_disc(0.95) WITHIN GROUP(ORDER BY ms) INTO baseline FROM dm_benchmark WHERE label='before';
  ASSERT (SELECT max(p95)-baseline<=10 FROM (SELECT percentile_disc(0.95) WITHIN GROUP(ORDER BY ms) p95 FROM dm_benchmark WHERE label<>'before' GROUP BY label) runs), 'fixture INSERT overhead exceeds declared 10ms';
END $$;
\endif
DO $$ BEGIN
  ASSERT dm_try_send(1,'connected a');
  ASSERT dm_try_send(2,'connected b');
  ASSERT NOT dm_try_send(3,'not a participant');
END $$;
BEGIN;
UPDATE user_credential SET active=false WHERE party_id=2;
DO $$ BEGIN ASSERT NOT dm_try_send(1,'inactive peer'); END $$;
ROLLBACK;
BEGIN;
UPDATE party SET is_org=true WHERE id=2;
DO $$ BEGIN ASSERT NOT dm_try_send(1,'unsupported managed entity'); END $$;
ROLLBACK;
BEGIN;
SELECT social_v2_close(2);
DO $$ BEGIN ASSERT NOT dm_try_send(1,'closed peer'); END $$;
ROLLBACK;
BEGIN ISOLATION LEVEL REPEATABLE READ;
DO $$ BEGIN
  BEGIN PERFORM dm_try_send(1,'unsupported stale snapshot');
    RAISE EXCEPTION 'repeatable read send succeeded';
  EXCEPTION WHEN feature_not_supported THEN NULL; END;
END $$;
ROLLBACK;
BEGIN ISOLATION LEVEL SERIALIZABLE;
DO $$ BEGIN
  BEGIN PERFORM dm_try_send(1,'unsupported serializable mode');
    RAISE EXCEPTION 'serializable send succeeded';
  EXCEPTION WHEN feature_not_supported THEN NULL; END;
END $$;
ROLLBACK;
SELECT social_v2_mutate(1,2,'block',2,'dm-block');
DO $$ BEGIN
  ASSERT NOT dm_try_send(1,'blocked outgoing');
  ASSERT NOT dm_try_send(2,'blocked incoming');
  BEGIN UPDATE chat_message SET body='blocked edit' WHERE body='connected a';
    RAISE EXCEPTION 'blocked edit succeeded'; EXCEPTION WHEN insufficient_privilege THEN NULL; END;
  BEGIN INSERT INTO chat_message(id,thread_id,sender_party_id,body)
    SELECT id,thread_id,sender_party_id,'blocked upsert' FROM chat_message WHERE body='connected a'
    ON CONFLICT(id) DO UPDATE SET body=excluded.body;
    RAISE EXCEPTION 'blocked upsert succeeded'; EXCEPTION WHEN insufficient_privilege THEN NULL; END;
END $$;
SELECT social_v2_mutate(1,2,'unblock',3,'dm-unblock');
DO $$ BEGIN ASSERT NOT dm_try_send(1,'unblock does not restore consent'); END $$;
CREATE TABLE dm_race_barrier(id integer PRIMARY KEY);
INSERT INTO dm_race_barrier VALUES(1);
SQL
wait_for_session() {
  for attempt in $(seq 1 100); do
    [ "$(psql_dm -Atc "SELECT count(*) FROM pg_stat_activity WHERE application_name='$1' AND wait_event_type='$2'")" = 1 ] && return
    sleep 0.05
  done
  echo "Race barrier not reached: $1 / $2" >&2
  return 1
}
TDF_DM_RESULTS=$(mktemp -d)
# Restore consent via explicit independent requests.
psql_dm -c "SELECT social_v2_mutate(1,2,'request',4,'race-request'); SELECT social_v2_mutate(2,1,'accept',5,'race-accept');" >/dev/null
psql_dm -c "SET application_name='dm-gate'; BEGIN; SELECT id FROM dm_race_barrier FOR UPDATE; SELECT pg_sleep(300);" > "$TDF_DM_RESULTS/gate.txt" 2>&1 &
gate_pid=$!
wait_for_session dm-gate Timeout
psql_dm -c "SET application_name='dm-block'; BEGIN; SELECT social_v2_mutate(2,1,'block',6,'race-block'); SELECT id FROM dm_race_barrier FOR UPDATE; COMMIT;" > "$TDF_DM_RESULTS/block.txt" &
block_pid=$!
wait_for_session dm-block Lock
psql_dm -Atc "SET application_name='dm-send'; SELECT dm_try_send(1,'block wins');" > "$TDF_DM_RESULTS/send.txt" &
send_pid=$!
wait_for_session dm-send Lock
psql_dm -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE application_name='dm-gate';" >/dev/null
wait "$gate_pid" || true
wait "$block_pid"
wait "$send_pid"
[ "$(tail -1 "$TDF_DM_RESULTS/send.txt")" = f ]
# Writer wins: block waits for the same account locks until message commit.
psql_dm -c "SELECT social_v2_mutate(2,1,'unblock',7,'race-unblock'); SELECT social_v2_mutate(1,2,'request',8,'race-request2'); SELECT social_v2_mutate(2,1,'accept',9,'race-accept2');" >/dev/null
psql_dm -c "SET application_name='dm-gate'; BEGIN; SELECT id FROM dm_race_barrier FOR UPDATE; SELECT pg_sleep(300);" > "$TDF_DM_RESULTS/gate2.txt" 2>&1 &
gate_pid=$!
wait_for_session dm-gate Timeout
psql_dm -Atc "SET application_name='dm-send'; BEGIN; SELECT dm_try_send(1,'send wins'); SELECT id FROM dm_race_barrier FOR UPDATE; COMMIT;" > "$TDF_DM_RESULTS/send2.txt" &
send_pid=$!
wait_for_session dm-send Lock
psql_dm -c "SET application_name='dm-block'; SELECT social_v2_mutate(2,1,'block',10,'race-block2');" > "$TDF_DM_RESULTS/block2.txt" &
block_pid=$!
wait_for_session dm-block Lock
psql_dm -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE application_name='dm-gate';" >/dev/null
wait "$gate_pid" || true
wait "$send_pid"
wait "$block_pid"
psql_dm < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-14_social_v2_pause.sql"
psql_dm < "$TDF_SOCIAL_ROOT/tdf-hq/sql/2026-09-15_social_v2_dm_write_boundary.sql"
psql_dm <<'SQL'
DO $$ BEGIN
  ASSERT (SELECT activated_once AND NOT enabled FROM social_v2_runtime);
  ASSERT NOT dm_try_send(1,'paused blocked retry');
  ASSERT (SELECT count(*) FROM chat_message WHERE body='block wins')=0;
  ASSERT (SELECT count(*) FROM chat_message WHERE body='send wins')=1;
  ASSERT (SELECT count(*) FROM chat_message)=4; -- legacy + two connected + winner
  ASSERT (SELECT count(*) FROM social_v2_command)=11;
  ASSERT (SELECT block_b FROM social_v2_pair WHERE party_a=1 AND party_b=2);
END $$;
SQL
echo 'PASS: inactive compatibility, persistent activation, consent, both directions, participant/lifecycle denials, edit/upsert protection, both real block/send race orders, preserved-write pause and reapply'
