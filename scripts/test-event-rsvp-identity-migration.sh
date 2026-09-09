#!/bin/sh
set -eu

TDF_RSVP_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_RSVP_CONTAINER="tdf-event-rsvp-migration-test-$$"
TDF_RSVP_DATABASE="tdf_event_rsvp_test"
TDF_RSVP_MODE=""
TDF_RSVP_TEMP_DIR=""
TDF_RSVP_PG_BIN="/usr/local/opt/postgresql@16/bin"
TDF_RSVP_PORT=$((55432 + ($$ % 1000)))

cleanup() {
  if [ "$TDF_RSVP_MODE" = "docker" ]; then
    docker rm -f "$TDF_RSVP_CONTAINER" >/dev/null 2>&1 || true
  elif [ "$TDF_RSVP_MODE" = "local" ] && [ -n "$TDF_RSVP_TEMP_DIR" ]; then
    "$TDF_RSVP_PG_BIN/pg_ctl" -D "$TDF_RSVP_TEMP_DIR/data" -m immediate stop >/dev/null 2>&1 || true
    rm -rf -- "$TDF_RSVP_TEMP_DIR"
  fi
}
trap cleanup EXIT INT TERM

if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
  TDF_RSVP_MODE="docker"
  docker run --rm -d \
    --name "$TDF_RSVP_CONTAINER" \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -e POSTGRES_DB="$TDF_RSVP_DATABASE" \
    postgres:16-alpine >/dev/null

  attempt=0
  until docker exec "$TDF_RSVP_CONTAINER" pg_isready -U postgres -d "$TDF_RSVP_DATABASE" >/dev/null 2>&1; do
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 45 ]; then
      echo "RSVP migration test database did not become ready" >&2
      exit 1
    fi
    sleep 1
  done
elif [ -x "$TDF_RSVP_PG_BIN/initdb" ] && [ -x "$TDF_RSVP_PG_BIN/pg_ctl" ]; then
  TDF_RSVP_MODE="local"
  TDF_RSVP_TEMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/tdf-rsvp-migration.XXXXXX")
  LC_ALL=C "$TDF_RSVP_PG_BIN/initdb" -D "$TDF_RSVP_TEMP_DIR/data" --locale=C --encoding=UTF8 -A trust -U postgres >/dev/null
  "$TDF_RSVP_PG_BIN/pg_ctl" -D "$TDF_RSVP_TEMP_DIR/data" -o "-h 127.0.0.1 -p $TDF_RSVP_PORT" -w start >/dev/null
  "$TDF_RSVP_PG_BIN/createdb" -h 127.0.0.1 -p "$TDF_RSVP_PORT" -U postgres "$TDF_RSVP_DATABASE"
else
  echo "Docker daemon and PostgreSQL 16 binaries are both unavailable" >&2
  exit 1
fi

psql_exec() {
  if [ "$TDF_RSVP_MODE" = "docker" ]; then
    docker exec -i "$TDF_RSVP_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$TDF_RSVP_DATABASE" "$@"
  else
    "$TDF_RSVP_PG_BIN/psql" -h 127.0.0.1 -p "$TDF_RSVP_PORT" -v ON_ERROR_STOP=1 -U postgres -d "$TDF_RSVP_DATABASE" "$@"
  fi
}
psql_file() {
  psql_exec < "$1"
}

psql_exec <<'SQL' >/dev/null
CREATE EXTENSION pgcrypto;
CREATE TABLE party(id BIGINT PRIMARY KEY);
CREATE TABLE user_locale_preferences(id BIGSERIAL PRIMARY KEY,user_id BIGINT UNIQUE REFERENCES party(id));
CREATE TABLE workflow_definition(id UUID PRIMARY KEY,code TEXT NOT NULL,active BOOLEAN NOT NULL);
CREATE TABLE workflow_state(id UUID PRIMARY KEY,workflow_id UUID NOT NULL REFERENCES workflow_definition(id),code TEXT NOT NULL,active BOOLEAN NOT NULL);
CREATE TABLE workflow_state_capability(id UUID PRIMARY KEY,state_id UUID NOT NULL REFERENCES workflow_state(id),capability_code TEXT NOT NULL,enabled BOOLEAN NOT NULL,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL,version INTEGER NOT NULL,UNIQUE(state_id,capability_code));
CREATE TABLE venue(id BIGINT PRIMARY KEY,name TEXT,city TEXT,city_id UUID,country_id UUID);
CREATE TABLE city_reference(id UUID PRIMARY KEY,name_es TEXT,country_id UUID,latitude DOUBLE PRECISION,longitude DOUBLE PRECISION);
CREATE TABLE country_reference(id UUID PRIMARY KEY,alpha2 TEXT);
CREATE TABLE social_event(id BIGINT PRIMARY KEY,title TEXT NOT NULL,description TEXT,start_time TIMESTAMPTZ NOT NULL,end_time TIMESTAMPTZ,timezone TEXT,price_cents INTEGER,currency_id UUID,capacity INTEGER,venue_id BIGINT REFERENCES venue(id),workflow_state_id UUID REFERENCES workflow_state(id),metadata TEXT,updated_at TIMESTAMPTZ NOT NULL);
CREATE TABLE event_rsvp(id BIGINT PRIMARY KEY,event_id BIGINT NOT NULL REFERENCES social_event(id),party_id VARCHAR NOT NULL,status VARCHAR NOT NULL,metadata VARCHAR,created_at TIMESTAMPTZ NOT NULL,updated_at TIMESTAMPTZ NOT NULL);

INSERT INTO party VALUES (7),(8);
INSERT INTO user_locale_preferences(user_id) VALUES (7);
INSERT INTO workflow_definition VALUES ('00000000-0000-4000-8000-000000000104','social-event-lifecycle',TRUE);
INSERT INTO workflow_state VALUES
 ('00000000-0000-4000-8000-000000000232','00000000-0000-4000-8000-000000000104','announced',TRUE),
 ('00000000-0000-4000-8000-000000000239','00000000-0000-4000-8000-000000000104','cancelled',TRUE);
INSERT INTO workflow_state_capability VALUES
 ('10000000-0000-4000-8000-000000000001','00000000-0000-4000-8000-000000000232','public-listable',TRUE,now(),now(),1);
INSERT INTO social_event VALUES
 (11,'Public event',NULL,now()+interval '1 day',NULL,'UTC',NULL,NULL,NULL,NULL,'00000000-0000-4000-8000-000000000232','{"isPublic":true,"imageUrl":"https://example.test/poster.png"}',now()),
 (12,'Private event',NULL,now()+interval '1 day',NULL,'UTC',NULL,NULL,NULL,NULL,'00000000-0000-4000-8000-000000000232','{"isPublic":false}',now()),
 (13,'Malformed metadata event',NULL,now()+interval '1 day',NULL,'UTC',NULL,NULL,NULL,NULL,'00000000-0000-4000-8000-000000000232','{not-json',now()),
 (14,'Cancelled public event',NULL,now()+interval '1 day',NULL,'UTC',NULL,NULL,NULL,NULL,'00000000-0000-4000-8000-000000000239','{"isPublic":true}',now()),
 (15,'Unsafe image event',NULL,now()+interval '1 day',NULL,'UTC',NULL,NULL,NULL,NULL,'00000000-0000-4000-8000-000000000232','{"isPublic":true,"imageUrl":"javascript:alert(1)"}',now());
INSERT INTO event_rsvp VALUES
 (101,11,'007','Accepted',NULL,now()-interval '3 days',now()-interval '2 days'),
 (102,11,'7','maybe',NULL,now()-interval '1 day',now()-interval '1 hour'),
 (103,11,'8','invalid',NULL,now(),now()),
 (104,11,'not-a-party','accepted',NULL,now(),now()),
 (105,11,'0','accepted',NULL,now(),now()),
 (106,11,'999999999999999999999999','accepted',NULL,now(),now());
CREATE VIEW directory_public_event AS SELECT event.id,event.title,event.description,event.start_time,event.end_time,event.timezone,event.price_cents,event.currency_id,event.capacity,event.venue_id,venue.name AS venue_name,city.id AS city_id,coalesce(city.name_es,venue.city) AS city_name,country.alpha2 AS country_code,city.latitude AS public_latitude,city.longitude AS public_longitude,event.updated_at FROM social_event event JOIN workflow_state state ON state.id=event.workflow_state_id JOIN workflow_state_capability capability ON capability.state_id=state.id AND capability.capability_code='public-listable' AND capability.enabled LEFT JOIN venue ON venue.id=event.venue_id LEFT JOIN city_reference city ON city.id=venue.city_id LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id,city.country_id) WHERE state.active;
CREATE VIEW directory_public_venue AS SELECT DISTINCT venue.id,venue.name,city.id AS city_id,coalesce(city.name_es,venue.city) AS city_name,country.alpha2 AS country_code,city.latitude AS public_latitude,city.longitude AS public_longitude,NULL::integer AS capacity,now() AS updated_at FROM venue JOIN directory_public_event event ON event.venue_id=venue.id LEFT JOIN city_reference city ON city.id=venue.city_id LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id,city.country_id);
SQL

psql_file "$TDF_RSVP_ROOT/tdf-hq/sql/2026-09-08_event_rsvp_identity_privacy_feed.sql" >/dev/null
psql_file "$TDF_RSVP_ROOT/tdf-hq/sql/2026-09-08_event_rsvp_identity_privacy_feed.sql" >/dev/null

test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp WHERE event_id=11 AND party_id='7';")" = "1"
test "$(psql_exec -Atc "SELECT status FROM event_rsvp WHERE event_id=11 AND party_id='7';")" = "maybe"
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp_migration_evidence WHERE reason='duplicate';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp_migration_evidence WHERE reason IN ('invalid_party','invalid_status');")" = "4"
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp WHERE show_on_profile OR visibility_decided_at IS NOT NULL;")" = "0"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_public_event WHERE id=11;")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_public_event WHERE id=12;")" = "0"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_public_event WHERE id=13;")" = "0"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_public_event WHERE id=14 AND NOT rsvp_eligible AND NOT public_share_eligible;")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM directory_public_event WHERE id=15 AND image_url IS NULL;")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM workflow_state_capability WHERE capability_code='rsvp' AND enabled;")" = "1"

if psql_exec -c "INSERT INTO event_rsvp(event_id,party_id,status,created_at,updated_at) VALUES (11,'7','accepted',now(),now());" >/dev/null 2>&1; then
  echo "Expected event/party uniqueness to reject a duplicate RSVP" >&2
  exit 1
fi
if psql_exec -c "INSERT INTO event_rsvp(event_id,party_id,status,created_at,updated_at) VALUES (11,'8','NONE',now(),now());" >/dev/null 2>&1; then
  echo "Expected canonical status constraint to reject NONE" >&2
  exit 1
fi

psql_exec -c "INSERT INTO party(id) VALUES (9) ON CONFLICT DO NOTHING;" >/dev/null
psql_exec -c "INSERT INTO event_rsvp(id,event_id,party_id,status,show_on_profile,visibility_decided_at,created_at,updated_at) VALUES (107,11,'9','accepted',TRUE,now(),now(),now()) ON CONFLICT(event_id,party_id) DO UPDATE SET status=excluded.status,updated_at=excluded.updated_at;" >/dev/null &
TDF_RSVP_FIRST_PID=$!
psql_exec -c "INSERT INTO event_rsvp(id,event_id,party_id,status,show_on_profile,visibility_decided_at,created_at,updated_at) VALUES (108,11,'9','maybe',TRUE,now(),now(),now()) ON CONFLICT(event_id,party_id) DO UPDATE SET status=excluded.status,updated_at=excluded.updated_at;" >/dev/null &
TDF_RSVP_SECOND_PID=$!
wait "$TDF_RSVP_FIRST_PID"
wait "$TDF_RSVP_SECOND_PID"
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp WHERE event_id=11 AND party_id='9';")" = "1"
test "$(psql_exec -Atc "SELECT status IN ('accepted','maybe') FROM event_rsvp WHERE event_id=11 AND party_id='9';")" = "t"

psql_exec -c "INSERT INTO party(id) VALUES (10); INSERT INTO event_rsvp(id,event_id,party_id,status,show_on_profile,visibility_decided_at,created_at,updated_at) VALUES (109,11,'10','accepted',TRUE,now(),now(),now()); DELETE FROM party WHERE id=10;" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp WHERE party_id='10';")" = "0"

psql_file "$TDF_RSVP_ROOT/tdf-hq/sql/2026-09-08_event_rsvp_identity_privacy_feed_rollback.sql" >/dev/null
test "$(psql_exec -Atc "SELECT count(*) FROM event_rsvp WHERE event_id=11 AND party_id='7';")" = "1"
test "$(psql_exec -Atc "SELECT count(*) FROM workflow_state_capability WHERE capability_code='rsvp' AND enabled;")" = "0"

echo "Event RSVP identity/privacy migration checks passed."
