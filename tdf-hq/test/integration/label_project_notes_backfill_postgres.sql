-- PostgreSQL integration test for the typed label-project note cutover.
-- Run only against a disposable database initialized by the candidate app.

\set ON_ERROR_STOP on

INSERT INTO cms_content
  (slug, locale, version, status, title, payload, created_at, updated_at, published_at)
VALUES
  ('label-projects', 'es-test', 9001, 'published', 'Fixture label projects',
   '{"items":[{"id":"11111111-1111-4111-8111-111111111111","text":"Distribuir master","done":false},{"id":"legacy-note-2","text":"Revisar créditos","done":true}]}'::text,
   '2030-01-01T00:00:00Z', '2030-01-01T00:00:00Z', '2030-01-01T00:00:00Z')
ON CONFLICT (slug, locale, version) DO UPDATE SET
  status = EXCLUDED.status,
  payload = EXCLUDED.payload,
  updated_at = EXCLUDED.updated_at,
  published_at = EXCLUDED.published_at;

CREATE TEMP TABLE label_project_fixture_source AS
SELECT id,
  md5(id::text || ':' || slug || ':' || locale || ':' || version::text || ':' ||
    status || ':' || COALESCE(payload::text, '')) AS digest
FROM cms_content
WHERE slug = 'label-projects' AND locale = 'es-test' AND version = 9001;

\ir ../../sql/2026-08-07_label_project_notes_backfill_dry_run.sql
\ir ../../sql/2026-08-07_label_project_notes_backfill_apply.sql

DO $integration$
DECLARE
  fixture label_project_fixture_source%ROWTYPE;
  persisted_count bigint;
  current_digest text;
BEGIN
  SELECT * INTO fixture FROM label_project_fixture_source;
  SELECT count(*) INTO persisted_count
  FROM label_project_note
  WHERE source_cms_content_id = fixture.id;
  SELECT md5(id::text || ':' || slug || ':' || locale || ':' || version::text || ':' ||
    status || ':' || COALESCE(payload::text, '')) INTO current_digest
  FROM cms_content WHERE id = fixture.id;

  IF persisted_count <> 2 THEN
    RAISE EXCEPTION 'expected two typed project notes, got %', persisted_count;
  END IF;
  IF current_digest <> fixture.digest THEN
    RAISE EXCEPTION 'backfill mutated the preserved CMS source row';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM label_project_note
    WHERE source_cms_content_id = fixture.id
      AND id = '11111111-1111-4111-8111-111111111111'::uuid
      AND text = 'Distribuir master'
      AND completed = false
  ) THEN
    RAISE EXCEPTION 'valid source UUID or values were not preserved';
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM label_project_note
    WHERE source_cms_content_id = fixture.id
      AND source_item_id = 'legacy-note-2'
      AND text = 'Revisar créditos'
      AND completed = true
  ) THEN
    RAISE EXCEPTION 'legacy item was not deterministically mapped with provenance';
  END IF;
END
$integration$;

CREATE TEMP TABLE label_project_after_first AS
SELECT count(*) AS row_count, min(id::text) AS first_id, max(id::text) AS last_id
FROM label_project_note
WHERE source_cms_content_id = (SELECT id FROM label_project_fixture_source);

\ir ../../sql/2026-08-07_label_project_notes_backfill_apply.sql

DO $integration$
DECLARE
  before_row label_project_after_first%ROWTYPE;
  after_count bigint;
  after_first text;
  after_last text;
BEGIN
  SELECT * INTO before_row FROM label_project_after_first;
  SELECT count(*), min(id::text), max(id::text)
    INTO after_count, after_first, after_last
  FROM label_project_note
  WHERE source_cms_content_id = (SELECT id FROM label_project_fixture_source);
  IF (after_count, after_first, after_last) IS DISTINCT FROM
     (before_row.row_count, before_row.first_id, before_row.last_id) THEN
    RAISE EXCEPTION 'label project note apply is not idempotent';
  END IF;
END
$integration$;

\ir ../../sql/2026-08-07_label_project_notes_backfill_rollback.sql
\ir ../../sql/2026-08-07_label_project_notes_backfill_rollback.sql

DO $integration$
BEGIN
  IF EXISTS (SELECT 1 FROM label_project_note WHERE source_cms_content_id IS NOT NULL) THEN
    RAISE EXCEPTION 'rollback left migrated project notes';
  END IF;
END
$integration$;

SELECT 'Label project note PostgreSQL backfill checks passed' AS result;
