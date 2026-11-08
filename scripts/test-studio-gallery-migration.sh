#!/usr/bin/env bash
set -euo pipefail
# Use an empty disposable PostgreSQL database; never production or a shared DB.
: "${TDF_GALLERY_TEST_DATABASE_URL:?Point this at an empty local disposable database}"
# This runner accepts one explicit disposable-loopback database, not libpq
# service/query routing or an environment-supplied address override.
if [[ -n "${PGHOSTADDR:-}${PGSERVICE:-}${PGSERVICEFILE:-}" ]]; then
  echo 'Refusing libpq routing overrides for the disposable gallery database' >&2; exit 2
fi
if [[ ! "$TDF_GALLERY_TEST_DATABASE_URL" =~ ^postgresql://[a-zA-Z0-9_-]+(:[a-zA-Z0-9_-]+)?@(127[.]0[.]0[.]1|localhost):([0-9]{1,5})/tdf_studio_gallery_test$ ]]; then
  echo 'Use the explicit loopback port and tdf_studio_gallery_test database, without URI options' >&2; exit 2
fi
gallery_test_port=${BASH_REMATCH[3]}
if (( 10#$gallery_test_port < 1 || 10#$gallery_test_port > 65535 )); then
  echo 'Invalid disposable gallery database port' >&2; exit 2
fi
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
psql_gallery() { psql "$TDF_GALLERY_TEST_DATABASE_URL" -X -q -v ON_ERROR_STOP=1 "$@"; }
if [[ "$(psql_gallery -Atc "SELECT count(*) FROM pg_class WHERE relnamespace='public'::regnamespace AND relkind IN ('r','p')")" != 0 ]]; then
  echo 'An empty database is required' >&2; exit 2
fi
psql_gallery -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql_gallery -f "$repo_root/tdf-hq/sql/2026-08-14_catalog_canonical_schema.sql" >/dev/null
psql_gallery -f "$repo_root/tdf-hq/sql/2026-08-14_catalog_consumer_expand.sql" >/dev/null
psql_gallery -f "$repo_root/tdf-hq/sql/2026-08-14_catalog_integrity.sql" >/dev/null
psql_gallery -f "$repo_root/tdf-hq/sql/2026-08-14_catalog_foundation_seed.sql" >/dev/null
psql_gallery <<'SQL'
INSERT INTO cms_content(slug,locale,version,status,payload)
VALUES ('unrelated','es',1,'published','{"keep":true}');
SQL
migration="$repo_root/tdf-hq/sql/2026-11-07_studio_gallery.sql"
psql_gallery -f "$migration" >/dev/null
psql_gallery -f "$migration" >/dev/null
psql_gallery <<'SQL'
DO $$
DECLARE gallery jsonb;
BEGIN
 SELECT payload::jsonb INTO STRICT gallery FROM cms_content WHERE slug='studio-gallery';
 IF jsonb_array_length(gallery->'photos') <> 13
 OR gallery->'photos'->0->>'id' <> 'maps-preamps'
 OR (SELECT count(DISTINCT photo->>'id') FROM jsonb_array_elements(gallery->'photos') photo) <> 13
 OR (SELECT count(*) FROM jsonb_array_elements(gallery->'photos') photo WHERE photo->>'credit'='Daniel Escudero · Google Maps' AND photo->>'capturedAt'='2024-06') <> 4
 THEN RAISE EXCEPTION 'Initial gallery/provenance mismatch'; END IF;
 IF NOT EXISTS (SELECT 1 FROM cms_content c JOIN authored_content a ON a.id=c.authored_content_id JOIN content_type t ON t.id=a.content_type_id JOIN workflow_state w ON w.id=a.workflow_state_id WHERE c.slug='studio-gallery' AND c.status='published' AND a.active AND t.active AND t.entity_kind='authored_page' AND t.public_read AND w.code='published')
 THEN RAISE EXCEPTION 'Gallery is not discoverable through the authored CMS'; END IF;
END $$;
-- A saved draft must leave the published gallery intact.
INSERT INTO cms_content(authored_content_id,slug,locale,version,status,payload)
SELECT authored_content_id,slug,locale,2,'draft','{"schemaVersion":1,"photos":[]}' FROM cms_content WHERE slug='studio-gallery';
DO $$ BEGIN
 IF (SELECT jsonb_array_length(payload::jsonb->'photos') FROM cms_content WHERE slug='studio-gallery' AND status='published')<>13
 THEN RAISE EXCEPTION 'Draft replaced public content'; END IF;
END $$;
-- Model the existing CMS replacement transaction, including an intentionally empty gallery.
BEGIN;
UPDATE cms_content SET status='archived' WHERE slug='studio-gallery' AND status='published';
UPDATE cms_content SET status='published',published_at=CURRENT_TIMESTAMP WHERE slug='studio-gallery' AND version=2;
COMMIT;
SQL
psql_gallery -f "$migration" >/dev/null
psql_gallery <<'SQL'
DO $$ BEGIN
 IF (SELECT count(*) FROM cms_content WHERE slug='studio-gallery')<>2
 OR (SELECT payload::jsonb FROM cms_content WHERE slug='studio-gallery' AND status='published') <> '{"schemaVersion":1,"photos":[]}'::jsonb
 OR (SELECT payload::jsonb FROM cms_content WHERE slug='unrelated') <> '{"keep":true}'::jsonb
 THEN RAISE EXCEPTION 'Migration reseeded removed photos or altered unrelated content'; END IF;
END $$;
SQL
echo 'Studio gallery migration: seed, public lookup, draft isolation, empty publication, rerun preservation PASS'
