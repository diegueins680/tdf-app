\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-validation-reference-cutover-2026-08-12'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif
\if :{?safety_threshold}
\else
  \set safety_threshold 10000
\endif
\if :{?batch_size}
\else
  \set batch_size 500
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-validation-reference-cutover-v1',0));
SELECT set_config('tdf.catalog_safety_threshold',:'safety_threshold',TRUE);
SELECT set_config('tdf.catalog_batch_size',:'batch_size',TRUE);

INSERT INTO catalog_backfill_run (
  id,run_code,candidate_revision,dry_run,status,safety_threshold,started_at,correlation_id
) VALUES (
  gen_random_uuid(),:'run_code',:'candidate_revision',FALSE,'mapping',:safety_threshold,
  now(),:'run_code' || ':' || :'candidate_revision'
)
ON CONFLICT (run_code,candidate_revision,dry_run)
DO UPDATE SET status='mapping',safety_threshold=EXCLUDED.safety_threshold,completed_at=NULL;

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id',:'backfill_run_id',TRUE);

CREATE TABLE IF NOT EXISTS catalog_ddex_validation_reference_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  source_table text NOT NULL,source_record_id bigint NOT NULL,source_column text NOT NULL,
  original_value text,original_reference_id uuid,target_reference_id uuid NOT NULL,
  normalized_code text NOT NULL,catalog_code text NOT NULL,evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id,source_table,source_record_id,source_column)
);
DROP TRIGGER IF EXISTS catalog_no_hard_delete ON catalog_ddex_validation_reference_cutover_source;
CREATE TRIGGER catalog_no_hard_delete BEFORE DELETE ON catalog_ddex_validation_reference_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

CREATE TEMP TABLE resolved_ddex_validation_reference ON COMMIT DROP AS
WITH source AS (
  SELECT 'ddex_validation_run'::text source_table,id::bigint source_record_id,
    'result'::text source_column,result::text original_value,result_id current_id,
    'ddex-validation-results'::text catalog_code,
    CASE regexp_replace(lower(COALESCE(result::text,'')),'[^a-z]','','g')
      WHEN 'resultsuccess' THEN 'success' WHEN 'success' THEN 'success' WHEN 'valid' THEN 'success'
      WHEN 'resultfailure' THEN 'failure' WHEN 'failure' THEN 'failure' WHEN 'invalid' THEN 'failure'
      WHEN 'resultwarning' THEN 'warning' WHEN 'warning' THEN 'warning' WHEN 'warnings' THEN 'warning' END normalized_code
  FROM ddex_validation_run WHERE result IS NOT NULL OR (finished_at IS NOT NULL AND result_id IS NULL)
  UNION ALL
  SELECT 'ddex_validation_issue',id,'severity',severity::text,severity_id,'ddex-validation-severities',
    CASE regexp_replace(lower(COALESCE(severity::text,'')),'[^a-z]','','g')
      WHEN 'severityerror' THEN 'error' WHEN 'error' THEN 'error'
      WHEN 'severitywarning' THEN 'warning' WHEN 'warning' THEN 'warning'
      WHEN 'severityinfo' THEN 'info' WHEN 'info' THEN 'info' WHEN 'information' THEN 'info' END
  FROM ddex_validation_issue WHERE severity IS NOT NULL OR severity_id IS NULL
  UNION ALL
  SELECT 'ddex_validation_issue',id,'layer',layer::text,layer_id,'ddex-validation-layers',
    CASE regexp_replace(lower(COALESCE(layer::text,'')),'[^a-z]','','g')
      WHEN 'layerxml' THEN 'xml' WHEN 'xml' THEN 'xml'
      WHEN 'layerxsd' THEN 'xsd' WHEN 'xsd' THEN 'xsd'
      WHEN 'layeravs' THEN 'avs' WHEN 'avs' THEN 'avs'
      WHEN 'layerbusiness' THEN 'business' WHEN 'business' THEN 'business' END
  FROM ddex_validation_issue WHERE layer IS NOT NULL OR layer_id IS NULL
  UNION ALL
  SELECT 'ddex_export',id,'validation_result',validation_result,validation_result_id,'ddex-validation-results',
    CASE regexp_replace(lower(COALESCE(validation_result,'')),'[^a-z]','','g')
      WHEN 'resultsuccess' THEN 'success' WHEN 'success' THEN 'success' WHEN 'valid' THEN 'success'
      WHEN 'resultfailure' THEN 'failure' WHEN 'failure' THEN 'failure' WHEN 'invalid' THEN 'failure'
      WHEN 'resultwarning' THEN 'warning' WHEN 'warning' THEN 'warning' WHEN 'warnings' THEN 'warning' END
  FROM ddex_export WHERE validation_result IS NOT NULL
)
SELECT source.*,candidate.candidate_count,candidate.target_id
FROM source
LEFT JOIN LATERAL (
  SELECT count(*) candidate_count,(array_agg(item.id ORDER BY item.id))[1] target_id
  FROM (
    SELECT id FROM ddex_validation_result WHERE source.catalog_code='ddex-validation-results' AND active AND code=source.normalized_code
    UNION ALL SELECT id FROM ddex_validation_severity WHERE source.catalog_code='ddex-validation-severities' AND active AND code=source.normalized_code
    UNION ALL SELECT id FROM ddex_validation_layer WHERE source.catalog_code='ddex-validation-layers' AND active AND code=source.normalized_code
  ) item
) candidate ON TRUE;

DO $gate$ DECLARE source_rows bigint; invalid_rows bigint; BEGIN
  SELECT count(*),count(*) FILTER (WHERE candidate_count<>1 OR normalized_code IS NULL
    OR (current_id IS NOT NULL AND current_id<>target_id))
  INTO source_rows,invalid_rows FROM resolved_ddex_validation_reference;
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000
    OR source_rows>current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0
    OR (SELECT count(*) FROM ddex_validation_result WHERE active)<>3
    OR (SELECT count(*) FROM ddex_validation_severity WHERE active)<>3
    OR (SELECT count(*) FROM ddex_validation_layer WHERE active)<>4 THEN
    RAISE EXCEPTION 'DDEX validation reference cutover safety gate failed: rows=%, invalidOrAmbiguous=%',
      source_rows,invalid_rows USING ERRCODE='23514';
  END IF;
END $gate$;

INSERT INTO catalog_ddex_validation_reference_cutover_source (
  run_id,source_table,source_record_id,source_column,original_value,original_reference_id,
  target_reference_id,normalized_code,catalog_code,evidence
)
SELECT :'backfill_run_id'::uuid,source_table,source_record_id,source_column,original_value,
  current_id,target_id,normalized_code,catalog_code,
  'exact reviewed legacy constructor matched one active persisted DDEX validation reference'
FROM resolved_ddex_validation_reference
ON CONFLICT (run_id,source_table,source_record_id,source_column) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),source.run_id,source.source_table,source.source_column,
  source.source_record_id::text,COALESCE(source.original_value,'<missing>'),source.normalized_code,
  catalog.id,source.target_reference_id,'mapped',source.evidence,1,now()
FROM catalog_ddex_validation_reference_cutover_source source
JOIN catalog_definition catalog ON catalog.code=source.catalog_code
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,normalized_value=EXCLUDED.normalized_value,
  status='mapped',evidence=EXCLUDED.evidence;

DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_validation_run;
DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_validation_issue;
DROP TRIGGER IF EXISTS ddex_validation_reference_integrity ON ddex_export;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_validation_run target JOIN catalog_ddex_validation_reference_cutover_source source
    ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id
  WHERE target.result_id IS DISTINCT FROM source.target_reference_id OR target.result IS NOT NULL
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_run target SET result_id=source.target_reference_id,result=NULL
  FROM catalog_ddex_validation_reference_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_validation_issue target
  JOIN catalog_ddex_validation_reference_cutover_source severity ON severity.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND severity.source_table='ddex_validation_issue' AND severity.source_column='severity' AND severity.source_record_id=target.id
  JOIN catalog_ddex_validation_reference_cutover_source layer ON layer.run_id=severity.run_id AND layer.source_table='ddex_validation_issue' AND layer.source_column='layer' AND layer.source_record_id=target.id
  WHERE target.severity_id IS DISTINCT FROM severity.target_reference_id OR target.layer_id IS DISTINCT FROM layer.target_reference_id OR target.severity IS NOT NULL OR target.layer IS NOT NULL
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_issue target SET severity_id=severity.target_reference_id,
  layer_id=layer.target_reference_id,severity=NULL,layer=NULL
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
  WHERE target.validation_result_id IS DISTINCT FROM source.target_reference_id OR target.validation_result IS NOT NULL
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_export target SET validation_result_id=source.target_reference_id,validation_result=NULL
  FROM catalog_ddex_validation_reference_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_export' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

CREATE TRIGGER ddex_validation_reference_integrity BEFORE INSERT OR UPDATE ON ddex_validation_run
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_validation_reference();
CREATE TRIGGER ddex_validation_reference_integrity BEFORE INSERT OR UPDATE ON ddex_validation_issue
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_validation_reference();
CREATE TRIGGER ddex_validation_reference_integrity BEFORE INSERT OR UPDATE ON ddex_export
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_validation_reference();

DO $verify$ BEGIN
  IF EXISTS (SELECT 1 FROM catalog_ddex_validation_reference_cutover_source source
    LEFT JOIN catalog_definition catalog ON catalog.code=source.catalog_code
    LEFT JOIN catalog_migration_mapping mapping ON mapping.run_id=source.run_id
      AND mapping.source_table=source.source_table AND mapping.source_column=source.source_column
      AND mapping.source_record_id=source.source_record_id::text
      AND mapping.original_value=COALESCE(source.original_value,'<missing>')
    WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND (catalog.id IS NULL OR mapping.entity_id IS DISTINCT FROM source.target_reference_id))
    OR EXISTS (SELECT 1 FROM ddex_validation_run WHERE result IS NOT NULL OR (finished_at IS NOT NULL AND result_id IS NULL))
    OR EXISTS (SELECT 1 FROM ddex_validation_issue WHERE severity IS NOT NULL OR layer IS NOT NULL OR severity_id IS NULL OR layer_id IS NULL)
    OR EXISTS (SELECT 1 FROM ddex_export WHERE validation_result IS NOT NULL) THEN
    RAISE EXCEPTION 'DDEX validation reference verification failed' USING ERRCODE='23514';
  END IF;
END $verify$;

UPDATE catalog_backfill_run SET status='completed',
  scanned_rows=(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  ambiguous_rows=0,rejected_rows=0,completed_at=now(),
  report=jsonb_build_object('batchSize',:batch_size,'evidenceRows',(SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source WHERE run_id=:'backfill_run_id'::uuid))
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,
  'scannedRows',scanned_rows,'mappedRows',mapped_rows,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;
COMMIT;
