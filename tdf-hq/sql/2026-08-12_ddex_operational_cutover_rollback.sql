\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-operational-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-operational-cutover-v1',0));
SELECT set_config('tdf.catalog_batch_size',:'batch_size',TRUE);

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id',:'backfill_run_id',TRUE);

DO $gate$ DECLARE unsafe bigint; BEGIN
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000 THEN
    RAISE EXCEPTION 'rollback batch size must be between 1 and 5000' USING ERRCODE='23514';
  END IF;
  SELECT
    (SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_validation_run target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_validation_run'
        AND target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id)
    +(SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_import_plan target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_import_plan'
        AND (target.status IS NOT NULL OR target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id))
    +(SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_import_run target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_import_run'
        AND (target.status IS NOT NULL OR target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id))
    +(SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_export target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_export'
        AND target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id)
    +(SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_job target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_job'
        AND (target.job_type IS NOT NULL OR target.status IS NOT NULL
          OR target.operation_id IS DISTINCT FROM source.target_operation_id
          OR target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id))
    +(SELECT count(*) FROM catalog_ddex_operational_cutover_source source
      JOIN ddex_import_change target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_import_change'
        AND (target.operation IS NOT NULL OR target.operation_id IS DISTINCT FROM source.target_operation_id))
  INTO unsafe;
  IF unsafe<>0 THEN
    RAISE EXCEPTION 'DDEX operational rollback refused because % rows changed after cutover',unsafe
      USING ERRCODE='23514';
  END IF;
END $gate$;

DROP TRIGGER IF EXISTS ddex_operational_state_integrity ON ddex_validation_run;
DROP TRIGGER IF EXISTS ddex_operational_state_integrity ON ddex_import_plan;
DROP TRIGGER IF EXISTS ddex_operational_state_integrity ON ddex_import_run;
DROP TRIGGER IF EXISTS ddex_operational_state_integrity ON ddex_job;
DROP TRIGGER IF EXISTS ddex_import_change_canonical_integrity ON ddex_import_change;
DROP TRIGGER IF EXISTS ddex_export_canonical_integrity ON ddex_export;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_validation_run target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id
  WHERE target.workflow_state_id IS NOT DISTINCT FROM source.target_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_run target SET workflow_state_id=source.original_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_plan target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_plan' AND source.source_record_id=target.id
  WHERE target.status IS NULL AND target.workflow_state_id IS NOT DISTINCT FROM source.target_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_plan target SET status=source.original_status,
    workflow_state_id=source.original_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_plan' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_run target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_run' AND source.source_record_id=target.id
  WHERE target.status IS NULL AND target.workflow_state_id IS NOT DISTINCT FROM source.target_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_run target SET status=source.original_status,
    workflow_state_id=source.original_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_export target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_export' AND source.source_record_id=target.id
  WHERE target.workflow_state_id IS NOT DISTINCT FROM source.target_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_export target SET workflow_state_id=source.original_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_export' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_job target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_job' AND source.source_record_id=target.id
  WHERE target.job_type IS NULL AND target.status IS NULL
    AND target.operation_id IS NOT DISTINCT FROM source.target_operation_id
    AND target.workflow_state_id IS NOT DISTINCT FROM source.target_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_job target SET job_type=source.original_operation,status=source.original_status,
    operation_id=source.original_operation_id,workflow_state_id=source.original_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_job' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_change target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_change' AND source.source_record_id=target.id
  WHERE target.operation IS NULL AND target.operation_id IS NOT DISTINCT FROM source.target_operation_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_change target SET operation=source.original_operation,
    operation_id=source.original_operation_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_change' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

UPDATE catalog_backfill_run SET status='rolled-back',completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object(
    'rolledBackAt',now(),'rollbackBatchSize',current_setting('tdf.catalog_batch_size')::integer,
    'canonicalDdexOperationalGuardsDisabledForLegacyRelease',TRUE
  ))::text WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
