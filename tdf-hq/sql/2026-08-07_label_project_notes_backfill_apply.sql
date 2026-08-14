\set ON_ERROR_STOP on
BEGIN;
SELECT pg_advisory_xact_lock(hashtextextended('tdf-label-project-notes-backfill-v1', 0));
\ir 2026-08-07_label_project_notes_backfill_source.sql

DO $safety$
BEGIN
  IF EXISTS (
    SELECT 1 FROM label_project_note_latest
    WHERE jsonb_typeof(payload->'items') IS DISTINCT FROM 'array'
  ) THEN
    RAISE EXCEPTION 'label-projects backfill stopped: payload.items is not an array';
  END IF;
  IF EXISTS (
    SELECT 1 FROM label_project_note_candidates
    WHERE mapping_status <> 'mapped'
  ) THEN
    RAISE EXCEPTION 'label-projects backfill stopped: ambiguous or rejected source items require review';
  END IF;
END
$safety$;

INSERT INTO label_project_note
  (id, text, completed, active, created_by, updated_by, created_at, updated_at,
   version, source_cms_content_id, source_item_id)
SELECT
  entity_id,
  note_text,
  completed,
  true,
  created_by,
  created_by,
  cms_created_at,
  cms_created_at,
  1,
  cms_content_id,
  source_item_id
FROM label_project_note_candidates
WHERE mapping_status = 'mapped'
ON CONFLICT (source_cms_content_id, source_item_id) DO NOTHING;

DO $safety$
DECLARE
  expected_count bigint;
  persisted_count bigint;
BEGIN
  SELECT count(*) INTO expected_count
  FROM label_project_note_candidates
  WHERE mapping_status = 'mapped';

  SELECT count(*) INTO persisted_count
  FROM label_project_note note
  JOIN label_project_note_candidates candidate
    ON candidate.cms_content_id = note.source_cms_content_id
   AND candidate.source_item_id = note.source_item_id
  WHERE candidate.mapping_status = 'mapped';

  IF expected_count <> persisted_count THEN
    RAISE EXCEPTION 'label-projects backfill count mismatch: expected %, persisted %', expected_count, persisted_count;
  END IF;
END
$safety$;

SELECT
  count(*) FILTER (WHERE source_cms_content_id IS NOT NULL) AS migrated_project_notes,
  count(*) FILTER (WHERE source_cms_content_id IS NULL) AS native_project_notes
FROM label_project_note;

COMMIT;
