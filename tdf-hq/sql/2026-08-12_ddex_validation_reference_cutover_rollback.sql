\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-validation-reference-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-validation-reference-cutover-v1',0));
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
    (SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source source
      JOIN ddex_validation_run target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_validation_run'
        AND (target.result IS NOT NULL OR target.result_id IS DISTINCT FROM source.target_reference_id))
    +(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source source
      JOIN ddex_validation_issue target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_validation_issue' AND source.source_column='severity'
        AND (target.severity IS NOT NULL OR target.severity_id IS DISTINCT FROM source.target_reference_id))
    +(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source source
      JOIN ddex_validation_issue target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_validation_issue' AND source.source_column='layer'
        AND (target.layer IS NOT NULL OR target.layer_id IS DISTINCT FROM source.target_reference_id))
    +(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source source
      JOIN ddex_export target ON target.id=source.source_record_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND source.source_table='ddex_export'
        AND (target.validation_result IS NOT NULL OR target.validation_result_id IS DISTINCT FROM source.target_reference_id))
  INTO unsafe;
  IF unsafe<>0 THEN
    RAISE EXCEPTION 'DDEX validation reference rollback refused because % rows changed after cutover',unsafe
      USING ERRCODE='23514';
  END IF;
END $gate$;

DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_validation_run;
DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_validation_issue;
DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_export;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_validation_run target JOIN catalog_ddex_validation_reference_cutover_source source
    ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id
  WHERE target.result IS NULL AND target.result_id IS NOT DISTINCT FROM source.target_reference_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_run target SET result=source.original_value,
  result_id=source.original_reference_id
  FROM catalog_ddex_validation_reference_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_validation_issue target
  JOIN catalog_ddex_validation_reference_cutover_source severity ON severity.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND severity.source_table='ddex_validation_issue' AND severity.source_column='severity' AND severity.source_record_id=target.id
  JOIN catalog_ddex_validation_reference_cutover_source layer ON layer.run_id=severity.run_id AND layer.source_table='ddex_validation_issue' AND layer.source_column='layer' AND layer.source_record_id=target.id
  WHERE target.severity IS NULL AND target.layer IS NULL
    AND target.severity_id IS NOT DISTINCT FROM severity.target_reference_id
    AND target.layer_id IS NOT DISTINCT FROM layer.target_reference_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_issue target SET severity=severity.original_value,
  severity_id=severity.original_reference_id,layer=layer.original_value,
  layer_id=layer.original_reference_id
  FROM catalog_ddex_validation_reference_cutover_source severity,
    catalog_ddex_validation_reference_cutover_source layer,batch WHERE target.id=batch.id
  AND severity.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND severity.source_table='ddex_validation_issue' AND severity.source_column='severity' AND severity.source_record_id=target.id
  AND layer.run_id=severity.run_id AND layer.source_table='ddex_validation_issue'
  AND layer.source_column='layer' AND layer.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_export target JOIN catalog_ddex_validation_reference_cutover_source source
    ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_export' AND source.source_record_id=target.id
  WHERE target.validation_result IS NULL AND target.validation_result_id IS NOT DISTINCT FROM source.target_reference_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_export target SET validation_result=source.original_value,
  validation_result_id=source.original_reference_id
  FROM catalog_ddex_validation_reference_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_export' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

UPDATE catalog_backfill_run SET status='rolled-back',completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object('rolledBackAt',now(),
    'rollbackBatchSize',current_setting('tdf.catalog_batch_size')::integer,
    'canonicalDdexValidationReferenceGuardsDisabledForLegacyRelease',TRUE))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;
COMMIT;
