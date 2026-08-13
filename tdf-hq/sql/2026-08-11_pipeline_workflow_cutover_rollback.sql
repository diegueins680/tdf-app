\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'pipeline-workflow-cutover-2026-08-11'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-pipeline-workflow-cutover-v1', 0));
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
  SELECT count(*) INTO unsafe_rows FROM catalog_pipeline_workflow_cutover_source source
  JOIN pipeline_card target ON target.id=source.pipeline_card_id
  WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND (target.service_kind IS NOT NULL OR target.stage IS NOT NULL
      OR target.service_offering_id IS DISTINCT FROM source.target_service_offering_id
      OR target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id);
  IF unsafe_rows<>0 THEN
    RAISE EXCEPTION 'pipeline workflow rollback refused because % rows changed after cutover', unsafe_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

DROP TRIGGER IF EXISTS catalog_pipeline_card_integrity ON pipeline_card;

DO $batches$
DECLARE changed_rows integer;
BEGIN
  LOOP
    WITH batch AS (
      SELECT target.id FROM pipeline_card target
      JOIN catalog_pipeline_workflow_cutover_source source
        ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
          AND source.pipeline_card_id=target.id
      WHERE target.service_kind IS NULL AND target.stage IS NULL
        AND target.service_offering_id=source.target_service_offering_id
        AND target.workflow_state_id=source.target_workflow_state_id
      ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer
      FOR UPDATE OF target SKIP LOCKED
    )
    UPDATE pipeline_card target SET service_kind=source.original_service_kind,
      stage=source.original_stage, service_offering_id=source.original_service_offering_id,
      workflow_state_id=source.original_workflow_state_id, updated_at=now()
    FROM catalog_pipeline_workflow_cutover_source source, batch
    WHERE target.id=batch.id AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.pipeline_card_id=target.id;
    GET DIAGNOSTICS changed_rows = ROW_COUNT;
    EXIT WHEN changed_rows=0;
  END LOOP;
END
$batches$;

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object(
    'rolledBackAt', now(), 'rollbackBatchSize', current_setting('tdf.catalog_batch_size')::integer,
    'pipelineIntegrityTriggerDisabledForLegacyRelease', true
  ))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
