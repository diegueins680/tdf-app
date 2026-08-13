\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'feedback-catalog-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-feedback-catalog-cutover-v1', 0));

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

DROP TRIGGER IF EXISTS feedback_reference_integrity ON feedback;

UPDATE feedback target SET
  category=source.original_category,
  severity=source.original_severity,
  category_id=source.original_category_id,
  severity_id=source.original_severity_id
FROM catalog_feedback_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.feedback_id=target.id
  AND target.category_id=source.target_category_id
  AND target.severity_id=source.target_severity_id
  AND target.category IS NULL AND target.severity IS NULL;

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object('rolledBackAt', now()))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
