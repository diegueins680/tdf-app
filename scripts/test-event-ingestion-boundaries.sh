#!/bin/sh
set -eu
records_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
event_pg_bin="${TDF_EVENT_TEST_PG_BIN:-/usr/local/opt/postgresql@16/bin}"
event_temp=""
cleanup() {
  if [ -n "$event_temp" ]; then
    "$event_pg_bin/pg_ctl" -D "$event_temp/data" -m immediate stop >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT INT TERM
if [ -z "${TDF_EVENT_BOUNDARY_TEST_DATABASE_URL:-}" ]; then
  event_temp=$(mktemp -d "${TMPDIR:-/tmp}/tdf-event-boundary.XXXXXX")
  event_port=$((56432 + ($$ % 1000)))
  LC_ALL=C "$event_pg_bin/initdb" -D "$event_temp/data" --locale=C --encoding=UTF8 -A trust -U postgres >/dev/null
  "$event_pg_bin/pg_ctl" -D "$event_temp/data" -o "-h 127.0.0.1 -k $event_temp -p $event_port" -w start >/dev/null
  "$event_pg_bin/createdb" -h 127.0.0.1 -p "$event_port" -U postgres event_boundary_test
  TDF_EVENT_BOUNDARY_TEST_DATABASE_URL="host=127.0.0.1 port=$event_port user=postgres dbname=event_boundary_test"
fi
psql_exec() { psql "$TDF_EVENT_BOUNDARY_TEST_DATABASE_URL" -X -v ON_ERROR_STOP=1 "$@"; }
test "$(psql_exec -Atc "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")" = 0
psql_exec >/dev/null <<'SQL'
CREATE TABLE party(id bigint PRIMARY KEY);
CREATE TABLE social_event(id bigint PRIMARY KEY);
CREATE TABLE event_discovery_source(id bigserial PRIMARY KEY,source_key text UNIQUE,name text,source_type text,feed_url text,city_id bigint,enabled boolean DEFAULT true,priority integer DEFAULT 100,configuration text,etag text,last_modified text,consecutive_failures integer DEFAULT 0,last_success_at timestamptz,last_error text,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now());
CREATE TABLE external_event_ref(id bigserial PRIMARY KEY,provider text,external_id text,event_id bigint REFERENCES social_event(id),source_status text,UNIQUE(provider,external_id));
SQL
psql_exec -f "$records_root/tdf-hq/sql/2026-08-16_event_research_ingestion.sql" >/dev/null
psql_exec -f "$records_root/tdf-hq/sql/2026-09-27_event_ingestion_boundaries.sql" >/dev/null
psql_exec -f "$records_root/tdf-hq/test/sql/event_ingestion_boundaries.sql" >/dev/null
# Leave one capacity slot, then race the research and automated writers.
psql_exec -c "UPDATE event_research_candidate SET review_state='discarded' WHERE external_id='research-3'; UPDATE event_research_pilot_control SET approved=false; INSERT INTO social_event VALUES(4);" >/dev/null
psql_exec -c "BEGIN; SELECT 1 FROM event_research_pilot_control WHERE control_key='default' FOR UPDATE; SELECT pg_sleep(1); INSERT INTO external_event_ref(provider,external_id,event_id,source_status) VALUES('race','event',4,'draft:on_sale'); COMMIT;" >/dev/null &
event_first=$!
psql_exec -c "INSERT INTO event_research_candidate(provider,external_id,run_id,review_state,title,timezone,country_code,source_url,payload,evidence,confidence,content_hash,verified_at,created_at,updated_at) VALUES('race','candidate',1,'draft','Fixture','America/Guayaquil','EC','https://official.example/event','{}','[{}]','medium',repeat('a',64),now(),now(),now());" >/dev/null 2>&1 &
event_second=$!
event_success=0
if wait "$event_first"; then event_success=$((event_success+1)); fi
if wait "$event_second"; then event_success=$((event_success+1)); fi
test "$event_success" = 1
test "$(psql_exec -Atc 'SELECT count(*) FROM tdf_event_pilot_keys()')" = 20
psql_exec -f "$records_root/tdf-hq/sql/2026-09-27_event_ingestion_boundaries_rollback.sql" >/dev/null
psql_exec -f "$records_root/tdf-hq/sql/2026-09-27_event_ingestion_boundaries.sql" >/dev/null
test "$(psql_exec -Atc 'SELECT count(*) FROM tdf_event_pilot_keys()')" = 20
test "$(psql_exec -Atc 'SELECT count(*) FROM event_discovery_publication_approval WHERE revoked_at IS NOT NULL')" = 2
echo 'Shared event pilot: mixed-entry cap, replay/link identities, discard, concurrent writers, separate publication authority, revocation, rollback/reapply passed.'
