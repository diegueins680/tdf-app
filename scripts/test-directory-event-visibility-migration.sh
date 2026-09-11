#!/bin/sh
set -eu

visibility_container="tdf-directory-visibility-test-$$"
visibility_database="tdf_directory_visibility_test"
visibility_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
visibility_migration="$visibility_root/tdf-hq/sql/2026-09-07_directory_event_visibility_and_favorite_evidence.sql"

cleanup() {
  docker rm -f "$visibility_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$visibility_container" \
  -e POSTGRES_PASSWORD=directory-visibility-test \
  -e POSTGRES_DB="$visibility_database" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$visibility_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$visibility_database" -qAtc 'SELECT 1' >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    docker logs "$visibility_container" >&2
    echo "PostgreSQL directory visibility test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -i "$visibility_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$visibility_database" "$@"
}

apply_sql() {
  docker exec -i "$visibility_container" psql -X -v ON_ERROR_STOP=1 -U postgres -d "$visibility_database" < "$1" >/dev/null
}

psql_exec <<'SQL' >/dev/null
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE workflow_state (
  id UUID PRIMARY KEY,
  active BOOLEAN NOT NULL
);
CREATE TABLE workflow_state_capability (
  state_id UUID NOT NULL REFERENCES workflow_state(id),
  capability_code TEXT NOT NULL,
  enabled BOOLEAN NOT NULL
);
CREATE TABLE country_reference (
  id UUID PRIMARY KEY,
  alpha2 TEXT NOT NULL
);
CREATE TABLE city_reference (
  id UUID PRIMARY KEY,
  country_id UUID REFERENCES country_reference(id),
  name_es TEXT NOT NULL,
  latitude DOUBLE PRECISION,
  longitude DOUBLE PRECISION
);
CREATE TABLE venue (
  id BIGINT PRIMARY KEY,
  name TEXT NOT NULL,
  city TEXT,
  city_id UUID REFERENCES city_reference(id),
  country_id UUID REFERENCES country_reference(id),
  capacity INTEGER,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE social_event (
  id BIGINT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  start_time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  timezone TEXT,
  price_cents INTEGER,
  currency_id UUID,
  capacity INTEGER,
  venue_id BIGINT REFERENCES venue(id),
  workflow_state_id UUID REFERENCES workflow_state(id),
  metadata TEXT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE directory_search_document (
  entity_kind TEXT NOT NULL,
  entity_id TEXT NOT NULL,
  PRIMARY KEY (entity_kind, entity_id)
);
CREATE TABLE directory_audit_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  actor_party_id BIGINT,
  action TEXT NOT NULL,
  entity_kind TEXT NOT NULL DEFAULT 'event',
  entity_id TEXT NOT NULL DEFAULT '1',
  correlation_id TEXT NOT NULL DEFAULT 'fixture-correlation',
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE directory_favorite (
  account_party_id BIGINT NOT NULL,
  target_kind TEXT NOT NULL,
  target_id TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (account_party_id, target_kind, target_id)
);

INSERT INTO workflow_state(id, active) VALUES
  ('10000000-0000-4000-8000-000000000001', TRUE),
  ('10000000-0000-4000-8000-000000000002', TRUE);
INSERT INTO workflow_state_capability(state_id, capability_code, enabled) VALUES
  ('10000000-0000-4000-8000-000000000001', 'public-listable', TRUE),
  ('10000000-0000-4000-8000-000000000002', 'public-listable', FALSE);
INSERT INTO country_reference(id, alpha2) VALUES
  ('20000000-0000-4000-8000-000000000001', 'EC');
INSERT INTO city_reference(id, country_id, name_es, latitude, longitude) VALUES
  ('30000000-0000-4000-8000-000000000001', '20000000-0000-4000-8000-000000000001', 'Quito', -0.1807, -78.4678);
INSERT INTO venue(id, name, city_id, country_id) VALUES
  (1, 'Visible venue', '30000000-0000-4000-8000-000000000001', '20000000-0000-4000-8000-000000000001'),
  (2, 'Private-only venue', '30000000-0000-4000-8000-000000000001', '20000000-0000-4000-8000-000000000001');

INSERT INTO social_event(id, title, start_time, venue_id, workflow_state_id, metadata) VALUES
  (1, 'Missing metadata', now() + interval '1 day', 1, '10000000-0000-4000-8000-000000000001', NULL),
  (2, 'Blank metadata', now() + interval '2 days', 1, '10000000-0000-4000-8000-000000000001', '  '),
  (3, 'Empty object', now() + interval '3 days', 1, '10000000-0000-4000-8000-000000000001', '{}'),
  (4, 'Explicit public', now() + interval '4 days', 1, '10000000-0000-4000-8000-000000000001', '{"isPublic":true,"currency":"USD"}'),
  (5, 'Explicit private', now() + interval '5 days', 2, '10000000-0000-4000-8000-000000000001', '{"isPublic":false}'),
  (6, 'Malformed', now() + interval '6 days', 1, '10000000-0000-4000-8000-000000000001', '{'),
  (7, 'Duplicate field', now() + interval '7 days', 1, '10000000-0000-4000-8000-000000000001', '{"isPublic":false,"isPublic":true}'),
  (8, 'Unsupported field', now() + interval '8 days', 1, '10000000-0000-4000-8000-000000000001', '{"isPublic":true,"secret":"do-not-project"}'),
  (9, 'Wrong privacy type', now() + interval '9 days', 1, '10000000-0000-4000-8000-000000000001', '{"isPublic":"true"}'),
  (10, 'Non-public workflow', now() + interval '10 days', 1, '10000000-0000-4000-8000-000000000002', '{"isPublic":true}');

-- Reproduce the old visibility defect before applying the repair.
CREATE VIEW directory_public_event AS
SELECT
  event.id, event.title, event.description, event.start_time, event.end_time,
  event.timezone, event.price_cents, event.currency_id, event.capacity,
  event.venue_id, venue.name AS venue_name, city.id AS city_id,
  coalesce(city.name_es, venue.city) AS city_name, country.alpha2 AS country_code,
  city.latitude AS public_latitude, city.longitude AS public_longitude,
  event.updated_at
FROM social_event event
JOIN workflow_state state ON state.id = event.workflow_state_id
JOIN workflow_state_capability capability
  ON capability.state_id = state.id
 AND capability.capability_code = 'public-listable'
 AND capability.enabled
LEFT JOIN venue ON venue.id = event.venue_id
LEFT JOIN city_reference city ON city.id = venue.city_id
LEFT JOIN country_reference country ON country.id = coalesce(venue.country_id, city.country_id)
WHERE state.active;

CREATE VIEW directory_public_venue AS
SELECT DISTINCT
  venue.id, venue.name, city.id AS city_id, coalesce(city.name_es, venue.city) AS city_name,
  country.alpha2 AS country_code, city.latitude AS public_latitude,
  city.longitude AS public_longitude, venue.capacity, venue.updated_at
FROM venue
JOIN directory_public_event event ON event.venue_id = venue.id
LEFT JOIN city_reference city ON city.id = venue.city_id
LEFT JOIN country_reference country ON country.id = coalesce(venue.country_id, city.country_id);

INSERT INTO directory_search_document(entity_kind, entity_id)
SELECT 'event', id::text FROM social_event;
INSERT INTO directory_search_document(entity_kind, entity_id) VALUES ('venue', '1'), ('venue', '2');
INSERT INTO directory_favorite(account_party_id, target_kind, target_id, created_at) VALUES
  (42, 'event', '0042', '2026-09-07T09:00:00Z'),
  (42, 'event', '42', '2026-09-07T10:00:00Z'),
  (42, 'profile', 'D1000000-0000-4000-8000-000000000001', '2026-09-07T11:00:00Z'),
  (42, 'event', 'not-an-event', '2026-09-07T12:00:00Z');
SQL

before_private=$(psql_exec -qAt -c "SELECT count(*) FROM directory_public_event WHERE id=5;")
test "$before_private" = "1"

apply_sql "$visibility_migration"
apply_sql "$visibility_migration"

visible_ids=$(psql_exec -qAt -c "SELECT string_agg(id::text, ',' ORDER BY id) FROM directory_public_event;")
test "$visible_ids" = "1,2,3,4"
projected_events=$(psql_exec -qAt -c "SELECT string_agg(entity_id, ',' ORDER BY entity_id::bigint) FROM directory_search_document WHERE entity_kind='event';")
test "$projected_events" = "1,2,3,4"
projected_venues=$(psql_exec -qAt -c "SELECT string_agg(entity_id, ',' ORDER BY entity_id::bigint) FROM directory_search_document WHERE entity_kind='venue';")
test "$projected_venues" = "1"
malformed_is_hidden=$(psql_exec -qAt -c "SELECT NOT directory_social_event_metadata_is_public('{');")
test "$malformed_is_hidden" = "t"
evidence_index=$(psql_exec -qAt -c "SELECT count(*) FROM pg_indexes WHERE indexname='directory_audit_actor_action_created_idx';")
test "$evidence_index" = "1"
canonical_events=$(psql_exec -qAt -c "SELECT string_agg(target_id, ',' ORDER BY target_id) FROM directory_favorite WHERE account_party_id=42 AND target_kind='event';")
test "$canonical_events" = "42,not-an-event"
canonical_event_created_at=$(psql_exec -qAt -c "SELECT created_at FROM directory_favorite WHERE account_party_id=42 AND target_kind='event' AND target_id='42';")
test "$canonical_event_created_at" = "2026-09-07 09:00:00+00"
canonical_profile=$(psql_exec -qAt -c "SELECT target_id FROM directory_favorite WHERE account_party_id=42 AND target_kind='profile';")
test "$canonical_profile" = "d1000000-0000-4000-8000-000000000001"
invalid_favorite_preserved=$(psql_exec -qAt -c "SELECT count(*) FROM directory_favorite WHERE account_party_id=42 AND target_id='not-an-event';")
test "$invalid_favorite_preserved" = "1"

favorite_write_status=$(psql_exec -qAt -c "WITH requested AS (SELECT 'event'::text AS target_kind,'4'::text AS target_id), eligible AS (SELECT requested.target_kind,requested.target_id FROM requested WHERE EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id=CAST(requested.target_id AS bigint) AND event.start_time>=CURRENT_TIMESTAMP)), inserted AS (INSERT INTO directory_favorite(account_party_id,target_kind,target_id) SELECT 77,eligible.target_kind,eligible.target_id FROM eligible WHERE NOT EXISTS (SELECT 1 FROM directory_favorite existing WHERE existing.account_party_id=77 AND existing.target_kind=eligible.target_kind AND directory_canonical_favorite_target(existing.target_kind,existing.target_id)=eligible.target_id) ON CONFLICT DO NOTHING RETURNING target_kind,target_id), audited AS (INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,correlation_id,metadata) SELECT 77,'favorite.saved',inserted.target_kind,inserted.target_id,'favorite-save-test-1',jsonb_build_object('source','directory.favorite.put','visibility','public-upcoming') FROM inserted RETURNING id) SELECT EXISTS(SELECT 1 FROM eligible),EXISTS(SELECT 1 FROM inserted),EXISTS(SELECT 1 FROM audited);")
test "$favorite_write_status" = "t|t|t"
favorite_repeat_status=$(psql_exec -qAt -c "WITH requested AS (SELECT 'event'::text AS target_kind,'4'::text AS target_id), eligible AS (SELECT requested.target_kind,requested.target_id FROM requested WHERE EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id=CAST(requested.target_id AS bigint) AND event.start_time>=CURRENT_TIMESTAMP)), inserted AS (INSERT INTO directory_favorite(account_party_id,target_kind,target_id) SELECT 77,eligible.target_kind,eligible.target_id FROM eligible WHERE NOT EXISTS (SELECT 1 FROM directory_favorite existing WHERE existing.account_party_id=77 AND existing.target_kind=eligible.target_kind AND directory_canonical_favorite_target(existing.target_kind,existing.target_id)=eligible.target_id) ON CONFLICT DO NOTHING RETURNING target_kind,target_id), audited AS (INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,correlation_id,metadata) SELECT 77,'favorite.saved',inserted.target_kind,inserted.target_id,'favorite-save-test-2',jsonb_build_object('source','directory.favorite.put','visibility','public-upcoming') FROM inserted RETURNING id) SELECT EXISTS(SELECT 1 FROM eligible),EXISTS(SELECT 1 FROM inserted),EXISTS(SELECT 1 FROM audited);")
test "$favorite_repeat_status" = "t|f|f"
private_write_status=$(psql_exec -qAt -c "WITH requested AS (SELECT 'event'::text AS target_kind,'5'::text AS target_id), eligible AS (SELECT requested.target_kind,requested.target_id FROM requested WHERE EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id=CAST(requested.target_id AS bigint) AND event.start_time>=CURRENT_TIMESTAMP)), inserted AS (INSERT INTO directory_favorite(account_party_id,target_kind,target_id) SELECT 77,eligible.target_kind,eligible.target_id FROM eligible ON CONFLICT DO NOTHING RETURNING target_kind,target_id), audited AS (INSERT INTO directory_audit_event(actor_party_id,action,entity_kind,entity_id,correlation_id,metadata) SELECT 77,'favorite.saved',inserted.target_kind,inserted.target_id,'favorite-save-private',jsonb_build_object('source','directory.favorite.put') FROM inserted RETURNING id) SELECT EXISTS(SELECT 1 FROM eligible),EXISTS(SELECT 1 FROM inserted),EXISTS(SELECT 1 FROM audited);")
test "$private_write_status" = "f|f|f"
favorite_audit_count=$(psql_exec -qAt -c "SELECT count(*) FROM directory_audit_event WHERE actor_party_id=77 AND action='favorite.saved' AND entity_kind='event' AND entity_id='4';")
test "$favorite_audit_count" = "1"

psql_exec -qAt -c "INSERT INTO directory_favorite(account_party_id,target_kind,target_id) VALUES (88,'event','0004');" >/dev/null
legacy_put_status=$(psql_exec -qAt -c "WITH requested AS (SELECT 'event'::text AS target_kind,'4'::text AS target_id), eligible AS (SELECT requested.target_kind,requested.target_id FROM requested WHERE EXISTS (SELECT 1 FROM directory_public_event event WHERE event.id=CAST(requested.target_id AS bigint) AND event.start_time>=CURRENT_TIMESTAMP)), inserted AS (INSERT INTO directory_favorite(account_party_id,target_kind,target_id) SELECT 88,eligible.target_kind,eligible.target_id FROM eligible WHERE NOT EXISTS (SELECT 1 FROM directory_favorite existing WHERE existing.account_party_id=88 AND existing.target_kind=eligible.target_kind AND directory_canonical_favorite_target(existing.target_kind,existing.target_id)=eligible.target_id) ON CONFLICT DO NOTHING RETURNING target_kind,target_id) SELECT EXISTS(SELECT 1 FROM eligible),EXISTS(SELECT 1 FROM inserted);")
test "$legacy_put_status" = "t|f"
psql_exec -qAt -c "DELETE FROM directory_favorite WHERE account_party_id=88 AND target_kind='event' AND (target_id='4' OR directory_canonical_favorite_target(target_kind,target_id)='4');" >/dev/null
legacy_delete_count=$(psql_exec -qAt -c "SELECT count(*) FROM directory_favorite WHERE account_party_id=88 AND target_kind='event';")
test "$legacy_delete_count" = "0"

echo "Directory event visibility migration passed defect reproduction, fail-closed projection, stale-search cleanup, legacy-favorite convergence, atomic save/audit, evidence-index, and idempotency checks."
