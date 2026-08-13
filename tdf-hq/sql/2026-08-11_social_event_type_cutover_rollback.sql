\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'social-event-type-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-social-event-type-cutover-v1', 0));

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

DROP TRIGGER IF EXISTS social_event_type_integrity ON social_event;

UPDATE social_event target SET
  event_type_id=source.original_event_type_id,
  metadata=source.original_metadata
FROM catalog_social_event_type_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.social_event_id=target.id
  AND target.event_type_id=source.target_event_type_id
  AND target.metadata IS NOT DISTINCT FROM NULLIF((source.original_metadata::jsonb - 'eventType')::text, '{}');

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object('rolledBackAt', now()))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
