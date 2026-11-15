#!/bin/sh
# Applies the six-city venue discovery migration to a disposable PostgreSQL
# cluster and checks seeding, idempotent replay, operator choices and rollback.
set -eu
records_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
venue_pg_bin="${TDF_EVENT_TEST_PG_BIN:-/usr/local/opt/postgresql@16/bin}"
venue_temp=""
cleanup() {
  if [ -n "$venue_temp" ]; then
    "$venue_pg_bin/pg_ctl" -D "$venue_temp/data" -m immediate stop >/dev/null 2>&1 || true
    rm -rf "$venue_temp"
  fi
}
trap cleanup EXIT INT TERM
venue_temp=$(mktemp -d "${TMPDIR:-/tmp}/tdf-venue-discovery.XXXXXX")
venue_port=$((57432 + ($$ % 1000)))
LC_ALL=C "$venue_pg_bin/initdb" -D "$venue_temp/data" --locale=C --encoding=UTF8 -A trust -U postgres >/dev/null
LC_ALL=C "$venue_pg_bin/pg_ctl" -D "$venue_temp/data" -o "-h 127.0.0.1 -c unix_socket_directories= -p $venue_port" -l "$venue_temp/server.log" -w start >/dev/null
"$venue_pg_bin/createdb" -h 127.0.0.1 -p "$venue_port" -U postgres venue_discovery_test
venue_url="host=127.0.0.1 port=$venue_port user=postgres dbname=venue_discovery_test"
psql_exec() { psql "$venue_url" -X -v ON_ERROR_STOP=1 "$@"; }
count() { psql_exec -Atc "$1"; }
migration="$records_root/tdf-hq/sql/2026-11-14_latam_venue_discovery_sources.sql"
rollback="$records_root/tdf-hq/sql/2026-11-14_latam_venue_discovery_sources_rollback.sql"

# The registry as production has it before this migration: Quito already
# registered without a zone, the built-in providers, and one manual web source.
psql_exec >/dev/null <<'SQL'
CREATE TABLE event_city (
  id BIGSERIAL PRIMARY KEY, name TEXT NOT NULL, normalized_name TEXT NOT NULL,
  country_code TEXT NOT NULL, time_zone TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT unique_event_city UNIQUE (normalized_name, country_code));
CREATE TABLE event_discovery_source (
  id BIGSERIAL PRIMARY KEY, source_key TEXT NOT NULL, name TEXT NOT NULL, source_type TEXT NOT NULL,
  feed_url TEXT, city_id BIGINT REFERENCES event_city(id), enabled BOOLEAN NOT NULL DEFAULT TRUE,
  priority INTEGER NOT NULL DEFAULT 100, configuration TEXT, etag TEXT, last_modified TEXT,
  consecutive_failures INTEGER NOT NULL DEFAULT 0, last_success_at TIMESTAMPTZ, last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT unique_event_discovery_source UNIQUE (source_key),
  CONSTRAINT event_discovery_source_type_check
    CHECK (source_type IN ('ticketmaster', 'buenplan', 'ical', 'json', 'web')));
INSERT INTO event_city (name, normalized_name, country_code) VALUES ('Quito', 'quito', 'EC');
INSERT INTO event_discovery_source (source_key, name, source_type, enabled, priority) VALUES
  ('ticketmaster', 'Ticketmaster', 'ticketmaster', TRUE, 300),
  ('buenplan', 'Buen Plan', 'buenplan', TRUE, 200);
INSERT INTO event_discovery_source (source_key, name, source_type, feed_url, enabled, priority)
  VALUES ('meet2go-web', 'Meet2Go', 'web', 'https://meet2go.com/', FALSE, 250);
SQL

psql_exec -f "$migration" >/dev/null
test "$(count "SELECT count(*) FROM event_city")" = 6
test "$(count "SELECT string_agg(name, ',' ORDER BY name) FROM event_city WHERE country_code='CO' AND time_zone='America/Bogota'")" = "Bogotá,Cali,Medellín"
test "$(count "SELECT count(*) FROM event_city WHERE country_code='EC' AND time_zone='America/Guayaquil'")" = 3
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_type IN ('ical','jsonld') AND enabled")" = 9
# Meet2Go's terms prohibit crawlers, so its source is registered but off.
test "$(count "SELECT enabled::text FROM event_discovery_source WHERE source_key='meet2go-jsonld'")" = "false"
# Every single-venue source is bound to its city; the two platforms are not.
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_type IN ('ical','jsonld') AND city_id IS NULL")" = 2
test "$(count "SELECT string_agg(source_key, ',' ORDER BY source_key) FROM event_discovery_source WHERE city_id IS NULL AND source_type='jsonld'")" = "latiquetera-jsonld,meet2go-jsonld"
test "$(count "SELECT city.name FROM event_discovery_source s JOIN event_city city ON city.id=s.city_id WHERE s.source_key='tuboleta-daviarena'")" = "Medellín"
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_type='jsonld' AND (configuration IS NULL OR configuration::jsonb IS NULL)")" = 0
test "$(count "SELECT count(*) FROM event_discovery_source WHERE feed_url NOT LIKE 'https://%' AND source_type IN ('ical','jsonld')")" = 0
# Existing rows are untouched.
test "$(count "SELECT enabled::text || ':' || source_type FROM event_discovery_source WHERE source_key='meet2go-web'")" = "false:web"
test "$(count "SELECT count(*) FROM event_discovery_source")" = 13

# An operator's later choice survives replay, and replay adds nothing.
psql_exec -c "UPDATE event_discovery_source SET enabled=FALSE, feed_url='https://example.org/changed.ics' WHERE source_key='teatro-sucre-ical'" >/dev/null
psql_exec -f "$migration" >/dev/null
test "$(count "SELECT count(*) FROM event_discovery_source")" = 13
test "$(count "SELECT count(*) FROM event_city")" = 6
test "$(count "SELECT enabled::text || ':' || feed_url FROM event_discovery_source WHERE source_key='teatro-sucre-ical'")" = "false:https://example.org/changed.ics"

# The widened type check still rejects unknown types.
if psql_exec -c "INSERT INTO event_discovery_source (source_key, name, source_type) VALUES ('x', 'x', 'html')" >/dev/null 2>&1; then
  echo 'unknown source type was accepted' >&2
  exit 1
fi

# Rollback stops the seeded sources and keeps cities, rows and other sources.
psql_exec -f "$rollback" >/dev/null
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_type IN ('ical','jsonld') AND enabled")" = 0
test "$(count "SELECT count(*) FROM event_discovery_source")" = 13
test "$(count "SELECT count(*) FROM event_city")" = 6
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_key IN ('ticketmaster','buenplan') AND enabled")" = 2
psql_exec -f "$rollback" >/dev/null
# Reapplying after rollback does not silently re-enable anything.
psql_exec -f "$migration" >/dev/null
test "$(count "SELECT count(*) FROM event_discovery_source WHERE source_type IN ('ical','jsonld') AND enabled")" = 0
echo 'Six-city venue discovery migration: seed, replay, operator choices, type check, rollback/reapply passed.'
