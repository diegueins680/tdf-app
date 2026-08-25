\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'records-cms-cutover-2026-08-07'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout='120s';
SET LOCAL lock_timeout='2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-records-cms-backfill-v1',0));

-- Rollback is intentionally non-destructive. The unchanged cms_content rows
-- remain the source for the previous application revision; normalized entities,
-- aliases, source mappings, and audit evidence are retained for investigation
-- and a safe re-cutover.
UPDATE catalog_backfill_run
SET status='rolled-back',completed_at=now(),
  report=jsonb_set(COALESCE(report::jsonb,'{}'::jsonb),'{rollback}',
    jsonb_build_object('mode','application-revision-rollback','normalizedRowsRetained',TRUE,
      'legacyCmsRowsPreserved',TRUE,'rolledBackAt',now()))::text
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run;

SELECT jsonb_build_object(
  'runId',id,'status',status,'rollbackMode','deploy-previous-application-revision',
  'normalizedRowsRetained',TRUE,'legacyCmsRowsPreserved',TRUE
)
FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run;

COMMIT;
