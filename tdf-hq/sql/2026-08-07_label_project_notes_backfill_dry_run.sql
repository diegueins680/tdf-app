\set ON_ERROR_STOP on
-- Temporary extraction tables require a writable transaction in PostgreSQL.
-- This transaction performs no persistent writes and always rolls back.
BEGIN;
\ir 2026-08-07_label_project_notes_backfill_source.sql

SELECT
  (SELECT count(*) FROM label_project_note_latest) AS selected_cms_versions,
  (SELECT count(*) FROM label_project_note_candidates) AS source_items,
  count(*) FILTER (WHERE mapping_status = 'mapped') AS mapped_items,
  count(*) FILTER (WHERE mapping_status = 'ambiguous') AS ambiguous_items,
  count(*) FILTER (WHERE mapping_status = 'rejected') AS rejected_items
FROM label_project_note_candidates;

SELECT
  cms_content_id,
  locale,
  source_order,
  source_item_id,
  entity_id,
  mapping_status,
  mapping_evidence
FROM label_project_note_candidates
ORDER BY cms_content_id, source_order;

SELECT cms_content_id, locale, 'payload.items must be an array' AS error
FROM label_project_note_latest
WHERE jsonb_typeof(payload->'items') IS DISTINCT FROM 'array';

ROLLBACK;
