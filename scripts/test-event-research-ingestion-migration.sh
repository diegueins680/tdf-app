#!/bin/sh
set -eu

TDF_EVENT_RESEARCH_CONTAINER="tdf-event-research-migration-test-$$"
TDF_EVENT_RESEARCH_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cleanup() {
  docker rm -f "$TDF_EVENT_RESEARCH_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_EVENT_RESEARCH_CONTAINER" \
  -e POSTGRES_PASSWORD=event-research-test \
  -e POSTGRES_DB=tdf_event_research_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_EVENT_RESEARCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_research_test -Atc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "PostgreSQL migration test database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec "$TDF_EVENT_RESEARCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_research_test "$@"
}

psql_exec -c 'CREATE TABLE social_event (id BIGSERIAL PRIMARY KEY);' >/dev/null
psql_exec -c 'CREATE TABLE event_discovery_source (id BIGSERIAL PRIMARY KEY, source_key TEXT NOT NULL UNIQUE, name TEXT NOT NULL, source_type TEXT NOT NULL, feed_url TEXT, city_id BIGINT, enabled BOOLEAN NOT NULL DEFAULT TRUE, priority INTEGER NOT NULL DEFAULT 100, configuration TEXT, etag TEXT, last_modified TEXT, consecutive_failures INTEGER NOT NULL DEFAULT 0, last_success_at TIMESTAMPTZ, last_error TEXT, created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now());' >/dev/null

apply_migration() {
  docker exec -i "$TDF_EVENT_RESEARCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_research_test \
    < "$TDF_EVENT_RESEARCH_ROOT/tdf-hq/sql/2026-08-16_event_research_ingestion.sql" >/dev/null
}

rollback_migration() {
  docker exec -i "$TDF_EVENT_RESEARCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d tdf_event_research_test \
    < "$TDF_EVENT_RESEARCH_ROOT/tdf-hq/sql/2026-08-16_event_research_ingestion.down.sql" >/dev/null
}

apply_migration

source_count=$(psql_exec -Atc "SELECT count(*) FROM event_discovery_source WHERE source_type = 'web' AND NOT enabled;")
if [ "$source_count" != "7" ]; then
  echo "Expected seven disabled web research sources, got $source_count" >&2
  exit 1
fi

psql_exec -c "INSERT INTO event_research_run (run_key, status, reconciliation, counters, started_at, updated_at, created_by_party_id) VALUES ('test-run', 'running', true, '{}', now(), now(), '1');" >/dev/null

psql_exec -c "INSERT INTO event_research_candidate (provider, external_id, run_id, review_state, title, timezone, country_code, source_url, payload, evidence, confidence, managed_fields, content_hash, verified_at, is_pilot, created_at, updated_at) SELECT 'fixture', 'event-' || value, 1, 'draft', 'Event ' || value, 'America/Guayaquil', 'EC', 'https://official.example/event/' || value, '{}', '[{\"url\":\"https://official.example\"}]', 'medium', '[]', repeat('a', 64), now(), true, now(), now() FROM generate_series(1, 20) AS value;" >/dev/null

if psql_exec -c "INSERT INTO event_research_candidate (provider, external_id, run_id, review_state, title, timezone, country_code, source_url, payload, evidence, confidence, managed_fields, content_hash, verified_at, is_pilot, created_at, updated_at) VALUES ('fixture', 'event-21', 1, 'draft', 'Event 21', 'America/Guayaquil', 'EC', 'https://official.example/event/21', '{}', '[{\"url\":\"https://official.example\"}]', 'medium', '[]', repeat('b', 64), now(), true, now(), now());" >/dev/null 2>&1; then
  echo "Pilot limit did not reject candidate 21" >&2
  exit 1
fi

psql_exec -c "INSERT INTO event_research_candidate (provider, external_id, run_id, review_state, title, timezone, country_code, source_url, payload, evidence, confidence, managed_fields, content_hash, verified_at, is_pilot, created_at, updated_at) VALUES ('fixture', 'event-1', 1, 'draft', 'Event 1 retry', 'America/Guayaquil', 'EC', 'https://official.example/event/1', '{}', '[{\"url\":\"https://official.example\"}]', 'medium', '[]', repeat('c', 64), now(), true, now(), now()) ON CONFLICT (provider, external_id) DO UPDATE SET title = EXCLUDED.title, content_hash = EXCLUDED.content_hash;" >/dev/null

candidate_count=$(psql_exec -Atc 'SELECT count(*) FROM event_research_candidate;')
if [ "$candidate_count" != "20" ]; then
  echo "Idempotent retry created a duplicate candidate" >&2
  exit 1
fi

psql_exec -c "UPDATE event_research_candidate SET review_state = 'discarded' WHERE provider = 'fixture' AND external_id = 'event-2';" >/dev/null
psql_exec -c "INSERT INTO event_research_candidate (provider, external_id, run_id, review_state, title, timezone, country_code, source_url, payload, evidence, confidence, managed_fields, content_hash, verified_at, is_pilot, created_at, updated_at) VALUES ('fixture', 'replacement', 1, 'review', 'Replacement', 'America/Guayaquil', 'EC', 'https://official.example/replacement', '{}', '[{\"url\":\"https://official.example\"}]', 'medium', '[]', repeat('d', 64), now(), true, now(), now());" >/dev/null

active_count=$(psql_exec -Atc "SELECT count(*) FROM event_research_candidate WHERE is_pilot AND review_state <> 'discarded';")
if [ "$active_count" != "20" ]; then
  echo "Discard-and-replace did not preserve the active pilot limit" >&2
  exit 1
fi

rollback_migration

remaining=$(psql_exec -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'event_research_%';")
if [ "$remaining" != "0" ]; then
  echo "Rollback left event research tables behind" >&2
  exit 1
fi

apply_migration

echo "Event research migration passed source seed, pilot cap, idempotency, replacement, rollback, and reapply checks."
