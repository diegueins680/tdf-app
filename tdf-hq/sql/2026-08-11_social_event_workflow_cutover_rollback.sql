\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'social-event-workflow-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?batch_size}
\else
  \set batch_size 500
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-social-event-workflow-cutover-v1', 0));
SELECT set_config('tdf.catalog_batch_size', :'batch_size', TRUE);

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id', :'backfill_run_id', TRUE);

DO $gate$
DECLARE unsafe_rows bigint;
BEGIN
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000 THEN
    RAISE EXCEPTION 'rollback batch size must be between 1 and 5000' USING ERRCODE='23514';
  END IF;
  SELECT count(*) INTO unsafe_rows
  FROM catalog_social_event_workflow_cutover_source source
  JOIN social_event target ON target.id=source.social_event_id
  WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND (target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id
      OR target.metadata IS DISTINCT FROM NULLIF((source.original_metadata::jsonb - 'eventStatus')::text, '{}'));
  IF unsafe_rows<>0 THEN
    RAISE EXCEPTION 'social-event workflow rollback refused because % rows changed after cutover', unsafe_rows
      USING ERRCODE='23514';
  END IF;
END
$gate$;

DROP TRIGGER IF EXISTS social_event_workflow_state_integrity ON social_event;

DO $batches$
DECLARE changed_rows integer;
BEGIN
  LOOP
    WITH batch AS (
      SELECT target.id
      FROM social_event target
      JOIN catalog_social_event_workflow_cutover_source source
        ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
          AND source.social_event_id=target.id
      WHERE target.workflow_state_id=source.target_workflow_state_id
        AND target.metadata IS NOT DISTINCT FROM NULLIF((source.original_metadata::jsonb - 'eventStatus')::text, '{}')
      ORDER BY target.id
      LIMIT current_setting('tdf.catalog_batch_size')::integer
      FOR UPDATE OF target SKIP LOCKED
    )
    UPDATE social_event target SET
      workflow_state_id=source.original_workflow_state_id,
      metadata=source.original_metadata
    FROM catalog_social_event_workflow_cutover_source source, batch
    WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.social_event_id=target.id;
    GET DIAGNOSTICS changed_rows = ROW_COUNT;
    EXIT WHEN changed_rows=0;
  END LOOP;
END
$batches$;

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object(
    'rolledBackAt', now(), 'rollbackBatchSize', current_setting('tdf.catalog_batch_size')::integer
  ))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
