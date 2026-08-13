\set ON_ERROR_STOP on
BEGIN;
SELECT pg_advisory_xact_lock(hashtextextended('tdf-label-project-notes-backfill-v1', 0));

DELETE FROM label_project_note
WHERE source_cms_content_id IS NOT NULL;

SELECT count(*) AS remaining_migrated_project_notes
FROM label_project_note
WHERE source_cms_content_id IS NOT NULL;

COMMIT;
