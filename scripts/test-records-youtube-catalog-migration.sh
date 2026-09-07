#!/bin/sh
set -eu

test_container="tdf-records-youtube-catalog-test-$$"
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
up_migration="$repo_root/tdf-hq/sql/2026-09-06_records_youtube_catalog.sql"
down_migration="$repo_root/tdf-hq/sql/2026-09-06_records_youtube_catalog_rollback.sql"

cleanup() {
  docker rm -f "$test_container" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$test_container" \
  -e POSTGRES_PASSWORD=records-youtube-catalog-test \
  -e POSTGRES_DB=records_youtube_catalog_test \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$test_container" \
  psql -v ON_ERROR_STOP=1 -U postgres -d records_youtube_catalog_test -Atc 'SELECT 1' \
  >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 30 ]; then
    echo "Records YouTube catalog test database did not become queryable" >&2
    exit 1
  fi
  sleep 1
done

psql_exec() {
  docker exec -i "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d records_youtube_catalog_test "$@"
}

apply_file() {
  docker exec -i "$test_container" \
    psql -v ON_ERROR_STOP=1 -U postgres -d records_youtube_catalog_test \
    < "$1" >/dev/null
}

psql_exec >/dev/null <<'SQL'
CREATE EXTENSION pgcrypto;

CREATE TABLE workflow_state (
  id UUID PRIMARY KEY,
  workflow_id UUID NOT NULL,
  code TEXT NOT NULL,
  active BOOLEAN NOT NULL,
  UNIQUE (workflow_id, code)
);
CREATE TABLE catalog_definition (
  id UUID PRIMARY KEY,
  code TEXT UNIQUE NOT NULL,
  workflow_id UUID NOT NULL,
  active BOOLEAN NOT NULL,
  source_name TEXT,
  source_version TEXT,
  source_effective_date DATE,
  last_synced_at TIMESTAMPTZ,
  cache_revision BIGINT NOT NULL DEFAULT 0,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1
);
CREATE TABLE recording_type_reference (
  id UUID PRIMARY KEY,
  code TEXT UNIQUE NOT NULL,
  active BOOLEAN NOT NULL
);
CREATE TABLE external_provider (
  id UUID PRIMARY KEY,
  code TEXT UNIQUE NOT NULL,
  active BOOLEAN NOT NULL
);
CREATE TABLE editorial_collection (
  id UUID PRIMARY KEY,
  code TEXT UNIQUE NOT NULL,
  collection_type TEXT NOT NULL,
  active BOOLEAN NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  version BIGINT NOT NULL DEFAULT 1
);
CREATE TABLE record_contributor (
  id UUID PRIMARY KEY,
  catalog_id UUID NOT NULL,
  code TEXT UNIQUE NOT NULL,
  contributor_kind TEXT NOT NULL,
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  sort_order BIGINT NOT NULL,
  active BOOLEAN NOT NULL,
  workflow_state_id UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  version BIGINT NOT NULL
);
CREATE TABLE record_external_resource (
  id UUID PRIMARY KEY,
  provider_id UUID NOT NULL,
  external_code TEXT NOT NULL,
  resource_kind TEXT NOT NULL,
  canonical_url TEXT NOT NULL,
  duration_ms BIGINT,
  thumbnail_url TEXT,
  active BOOLEAN NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  version BIGINT NOT NULL,
  UNIQUE (provider_id, resource_kind, external_code)
);
CREATE TABLE recording (
  id UUID PRIMARY KEY,
  catalog_id UUID NOT NULL,
  code TEXT UNIQUE NOT NULL,
  recording_type_id UUID NOT NULL,
  title_es TEXT NOT NULL,
  title_en TEXT NOT NULL,
  description_es TEXT,
  description_en TEXT,
  duration_ms BIGINT,
  current_slug TEXT UNIQUE,
  sort_order BIGINT NOT NULL,
  active BOOLEAN NOT NULL,
  workflow_state_id UUID NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  published_revision BIGINT NOT NULL,
  usage_count BIGINT NOT NULL,
  version BIGINT NOT NULL
);
CREATE TABLE recording_contributor (
  id UUID PRIMARY KEY,
  recording_id UUID NOT NULL,
  contributor_id UUID NOT NULL,
  credit_role TEXT NOT NULL,
  sort_order BIGINT NOT NULL,
  primary_credit BOOLEAN NOT NULL,
  UNIQUE (recording_id, contributor_id, credit_role),
  UNIQUE (recording_id, credit_role, sort_order)
);
CREATE TABLE recording_external_resource (
  id UUID PRIMARY KEY,
  recording_id UUID NOT NULL,
  resource_id UUID NOT NULL,
  relation_kind TEXT NOT NULL,
  sort_order BIGINT NOT NULL,
  primary_resource BOOLEAN NOT NULL,
  UNIQUE (recording_id, resource_id, relation_kind),
  UNIQUE (recording_id, relation_kind, sort_order)
);
CREATE TABLE collection_recording (
  id UUID PRIMARY KEY,
  collection_id UUID NOT NULL,
  recording_id UUID NOT NULL,
  sort_order BIGINT NOT NULL,
  featured BOOLEAN NOT NULL,
  UNIQUE (collection_id, recording_id),
  UNIQUE (collection_id, sort_order)
);
CREATE TABLE catalog_backfill_run (
  id UUID PRIMARY KEY,
  run_code TEXT NOT NULL,
  candidate_revision TEXT NOT NULL,
  dry_run BOOLEAN NOT NULL,
  status TEXT NOT NULL,
  safety_threshold BIGINT NOT NULL,
  scanned_rows BIGINT NOT NULL,
  mapped_rows BIGINT NOT NULL,
  ambiguous_rows BIGINT NOT NULL,
  rejected_rows BIGINT NOT NULL,
  started_at TIMESTAMPTZ NOT NULL,
  completed_at TIMESTAMPTZ,
  report TEXT,
  correlation_id TEXT NOT NULL,
  UNIQUE (run_code, candidate_revision, dry_run)
);
CREATE TABLE catalog_migration_mapping (
  id UUID PRIMARY KEY,
  run_id UUID NOT NULL,
  source_table TEXT NOT NULL,
  source_column TEXT NOT NULL,
  source_record_id TEXT NOT NULL,
  original_value TEXT NOT NULL,
  normalized_value TEXT NOT NULL,
  catalog_id UUID NOT NULL,
  entity_id UUID,
  status TEXT NOT NULL,
  evidence TEXT,
  source_count BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  UNIQUE (run_id, source_table, source_column, source_record_id, original_value)
);

INSERT INTO workflow_state VALUES
  ('00000000-0000-4000-8000-000000000205', '00000000-0000-4000-8000-000000000101', 'published', TRUE);
INSERT INTO catalog_definition (id, code, workflow_id, active) VALUES
  ('10000000-0000-4000-8000-000000000014', 'records-recordings', '00000000-0000-4000-8000-000000000101', TRUE),
  ('10000000-0000-4000-8000-000000000030', 'record-contributors', '00000000-0000-4000-8000-000000000101', TRUE);
INSERT INTO recording_type_reference VALUES
  ('7977bab9-0e03-4999-8183-414aab322585', 'music-video', TRUE);
INSERT INTO external_provider VALUES
  ('00000000-0000-4000-8000-000000000401', 'youtube', TRUE);
INSERT INTO editorial_collection (id, code, collection_type, active) VALUES
  ('00000000-0000-4000-8000-000000000501', 'tdf-records-recordings', 'recording', TRUE);

WITH source(sort_order, youtube_id, title, contributor, duration_ms, description) AS (
  VALUES
    (1, 'f2BabxM1Pjc', 'Federico Molinari @ TDF Electro Sessions', 'Federico Molinari', 2654000, 'DJ set publicado en el canal TDF Records.'),
    (2, 'rRkAeNB0R14', 'Just One Nite @ TDF Electro Sessions', 'Just One Nite', 3381000, 'DJ set publicado en el canal TDF Records.'),
    (3, 'wZQAlIqllQY', 'Morex DJ Set @ TDF Electro Sessions', 'Morex', 3741000, 'DJ set publicado en el canal TDF Records.'),
    (4, 'YDODXZ4lyRk', 'Diego Saá @ TDF Electro Sessions', 'Diego Saá', 2889000, 'Live set publicado en el canal TDF Records.'),
    (5, '1hKWOram3aw', 'Everaldo Vasco @ TDF Sessions', 'Everaldo Vasco', 5178000, 'Sesión publicada en el canal TDF Records.'),
    (6, 'xqeey8SrH8M', 'COHEMA @ TDF Sessions', 'COHEMA', 3600000, 'Sesión publicada en el canal TDF Records.')
), contributors AS (
  INSERT INTO record_contributor (
    id, catalog_id, code, contributor_kind, name_es, name_en, sort_order,
    active, workflow_state_id, created_at, updated_at, version
  )
  SELECT DISTINCT ON (lower(contributor))
    gen_random_uuid(),
    '10000000-0000-4000-8000-000000000030',
    'legacy-credit-' || left(encode(digest(lower(btrim(contributor)), 'sha256'), 'hex'), 20),
    'artist', contributor, contributor, 0, TRUE,
    '00000000-0000-4000-8000-000000000205', now(), now(), 1
  FROM source
  ORDER BY lower(contributor), sort_order
), resources AS (
  INSERT INTO record_external_resource (
    id, provider_id, external_code, resource_kind, canonical_url, duration_ms,
    active, created_at, updated_at, version
  )
  SELECT gen_random_uuid(), '00000000-0000-4000-8000-000000000401', youtube_id,
    'video', 'https://www.youtube.com/watch?v=' || youtube_id, duration_ms,
    TRUE, now(), now(), 1
  FROM source
), recordings AS (
  INSERT INTO recording (
    id, catalog_id, code, recording_type_id, title_es, title_en,
    description_es, description_en, duration_ms, current_slug, sort_order,
    active, workflow_state_id, created_at, updated_at, published_revision,
    usage_count, version
  )
  SELECT gen_random_uuid(), '10000000-0000-4000-8000-000000000014',
    'youtube-recording-' || youtube_id,
    '7977bab9-0e03-4999-8183-414aab322585', title, title, description,
    description, duration_ms, 'youtube-recording-' || youtube_id, sort_order,
    TRUE, '00000000-0000-4000-8000-000000000205', now(), now(), 1, 0, 1
  FROM source
)
INSERT INTO collection_recording (id, collection_id, recording_id, sort_order, featured)
SELECT gen_random_uuid(), '00000000-0000-4000-8000-000000000501', recording.id,
  source.sort_order, FALSE
FROM source
JOIN recording ON recording.code = 'youtube-recording-' || source.youtube_id;

INSERT INTO recording_contributor (
  id, recording_id, contributor_id, credit_role, sort_order, primary_credit
)
SELECT gen_random_uuid(), recording.id, contributor.id, 'primary-artist', 0, TRUE
FROM recording
JOIN record_contributor contributor
  ON contributor.code = 'legacy-credit-' || left(
      encode(digest(lower(btrim(
        CASE recording.code
          WHEN 'youtube-recording-f2BabxM1Pjc' THEN 'Federico Molinari'
          WHEN 'youtube-recording-rRkAeNB0R14' THEN 'Just One Nite'
          WHEN 'youtube-recording-wZQAlIqllQY' THEN 'Morex'
          WHEN 'youtube-recording-YDODXZ4lyRk' THEN 'Diego Saá'
          WHEN 'youtube-recording-1hKWOram3aw' THEN 'Everaldo Vasco'
          ELSE 'COHEMA'
        END
      )), 'sha256'), 'hex'), 20);

INSERT INTO recording_external_resource (
  id, recording_id, resource_id, relation_kind, sort_order, primary_resource
)
SELECT gen_random_uuid(), recording.id, resource.id, 'primary-media', 0, TRUE
FROM recording
JOIN record_external_resource resource
  ON recording.code = 'youtube-recording-' || resource.external_code;
SQL

test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active;")" = "6"

apply_file "$up_migration"

test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active AND code LIKE 'youtube-recording-%';")" = "33"
test "$(psql_exec -Atc "SELECT count(*) FROM collection_recording membership JOIN editorial_collection collection ON collection.id=membership.collection_id JOIN recording ON recording.id=membership.recording_id WHERE collection.code='tdf-records-recordings' AND recording.active;")" = "33"
test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active AND code IN ('youtube-recording-9387ent0ELc','youtube-recording-5SpnEELSNqw','youtube-recording-97PnHRn8IGs','youtube-recording-e24-id_Ix8s','youtube-recording-z7RpdrL4P4A');")" = "0"
test "$(psql_exec -Atc "SELECT recording.title_es FROM collection_recording membership JOIN recording ON recording.id=membership.recording_id WHERE membership.collection_id='00000000-0000-4000-8000-000000000501' ORDER BY membership.sort_order LIMIT 1;")" = "Llama Este Pez @ Sereno Moreno Live Set Pt 1"
test "$(psql_exec -Atc "SELECT recording.title_es FROM collection_recording membership JOIN recording ON recording.id=membership.recording_id WHERE membership.collection_id='00000000-0000-4000-8000-000000000501' ORDER BY membership.sort_order DESC LIMIT 1;")" = "Diego Saá @ TDF ESTUDIO"
test "$(psql_exec -Atc "SELECT duration_ms FROM recording WHERE code='youtube-recording-Re3lL-myniY';")" = "9062000"
test "$(psql_exec -Atc "SELECT count(*) FROM catalog_migration_mapping WHERE status='mapped';")" = "33"
test "$(psql_exec -Atc "SELECT status || ':' || scanned_rows || ':' || mapped_rows FROM catalog_backfill_run WHERE run_code='records-youtube-catalog-2026-09-06';")" = "complete:33:33"
test "$(psql_exec -Atc "SELECT source_version FROM catalog_definition WHERE code='records-recordings';")" = "UCx9Jpaw_XDrMtIdzWYlU51g/videos@2026-09-06"

version_before_replay=$(psql_exec -Atc "SELECT sum(version) FROM recording;")
resource_version_before_replay=$(psql_exec -Atc "SELECT sum(version) FROM record_external_resource;")
apply_file "$up_migration"
test "$(psql_exec -Atc "SELECT sum(version) FROM recording;")" = "$version_before_replay"
test "$(psql_exec -Atc "SELECT sum(version) FROM record_external_resource;")" = "$resource_version_before_replay"
test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active;")" = "33"
test "$(psql_exec -Atc "SELECT count(*) FROM catalog_migration_mapping;")" = "33"

apply_file "$down_migration"
test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active;")" = "6"
test "$(psql_exec -Atc "SELECT string_agg(recording.code || ':' || membership.sort_order, ',' ORDER BY membership.sort_order) FROM collection_recording membership JOIN recording ON recording.id=membership.recording_id WHERE membership.collection_id='00000000-0000-4000-8000-000000000501' AND recording.active;")" = "youtube-recording-f2BabxM1Pjc:1,youtube-recording-rRkAeNB0R14:2,youtube-recording-wZQAlIqllQY:3,youtube-recording-YDODXZ4lyRk:4,youtube-recording-1hKWOram3aw:5,youtube-recording-xqeey8SrH8M:6"
test "$(psql_exec -Atc "SELECT duration_ms FROM recording WHERE code='youtube-recording-f2BabxM1Pjc';")" = "2654000"
test "$(psql_exec -Atc "SELECT status FROM catalog_backfill_run WHERE run_code='records-youtube-catalog-2026-09-06';")" = "rolled-back"

apply_file "$up_migration"
test "$(psql_exec -Atc "SELECT count(*) FROM recording WHERE active;")" = "33"
test "$(psql_exec -Atc "SELECT count(DISTINCT membership.sort_order) FROM collection_recording membership JOIN recording ON recording.id=membership.recording_id WHERE membership.collection_id='00000000-0000-4000-8000-000000000501' AND recording.active;")" = "33"

echo "Records YouTube catalog migration passed 27-video ingestion, exact ordering, evidence, replay, rollback, and reapply checks."
