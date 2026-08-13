\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-operational-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-operational-cutover-v1',0));
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

CREATE TABLE IF NOT EXISTS catalog_ddex_operational_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  source_table text NOT NULL,source_record_id bigint NOT NULL,
  original_operation text,original_status text,
  original_workflow_state_id uuid,original_operation_id uuid,
  target_workflow_state_id uuid,target_operation_id uuid,
  normalized_state text,normalized_operation text,evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id,source_table,source_record_id)
);
DROP TRIGGER IF EXISTS catalog_no_hard_delete ON catalog_ddex_operational_cutover_source;
CREATE TRIGGER catalog_no_hard_delete BEFORE DELETE ON catalog_ddex_operational_cutover_source
  FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete();

CREATE TEMP TABLE resolved_ddex_operational ON COMMIT DROP AS
WITH source AS (
  SELECT 'ddex_validation_run'::text source_table,id::bigint source_record_id,
    NULL::text legacy_operation,result::text legacy_status,workflow_state_id,
    NULL::uuid operation_id,'ddex-validation-lifecycle' workflow_code,
    CASE regexp_replace(lower(COALESCE(result::text,'pending')),'[^a-z]','','g')
      WHEN 'resultsuccess' THEN 'succeeded' WHEN 'success' THEN 'succeeded'
      WHEN 'resultfailure' THEN 'failed' WHEN 'failure' THEN 'failed'
      WHEN 'resultwarning' THEN 'warning' WHEN 'warning' THEN 'warning'
      ELSE 'pending' END state_code,NULL::text operation_code
  FROM ddex_validation_run WHERE workflow_state_id IS NULL
  UNION ALL
  SELECT 'ddex_import_plan',id,NULL,status,workflow_state_id,NULL,'ddex-import-plan-lifecycle',
    CASE regexp_replace(lower(COALESCE(status,'')),'[^a-z]','','g')
      WHEN 'plandraft' THEN 'draft' WHEN 'draft' THEN 'draft'
      WHEN 'planresolved' THEN 'resolved' WHEN 'resolved' THEN 'resolved'
      WHEN 'plancommitted' THEN 'committed' WHEN 'committed' THEN 'committed'
      WHEN 'planabandoned' THEN 'abandoned' WHEN 'abandoned' THEN 'abandoned' END,NULL
  FROM ddex_import_plan WHERE status IS NOT NULL OR workflow_state_id IS NULL
  UNION ALL
  SELECT 'ddex_import_run',id,NULL,status,workflow_state_id,NULL,'ddex-import-run-lifecycle',
    CASE regexp_replace(lower(COALESCE(status,'')),'[^a-z]','','g')
      WHEN 'runpending' THEN 'pending' WHEN 'pending' THEN 'pending'
      WHEN 'runrunning' THEN 'running' WHEN 'running' THEN 'running'
      WHEN 'runsuccess' THEN 'succeeded' WHEN 'success' THEN 'succeeded'
      WHEN 'runfailed' THEN 'failed' WHEN 'failed' THEN 'failed'
      WHEN 'runrolledback' THEN 'rolled_back' WHEN 'rolledback' THEN 'rolled_back' END,NULL
  FROM ddex_import_run WHERE status IS NOT NULL OR workflow_state_id IS NULL
  UNION ALL
  SELECT 'ddex_export',id,NULL,NULL,workflow_state_id,NULL,'ddex-export-lifecycle','ready',NULL
  FROM ddex_export WHERE workflow_state_id IS NULL
  UNION ALL
  SELECT 'ddex_job',id,job_type,status,workflow_state_id,operation_id,'ddex-job-lifecycle',
    CASE regexp_replace(lower(COALESCE(status,'')),'[^a-z]','','g')
      WHEN 'jobpending' THEN 'pending' WHEN 'pending' THEN 'pending'
      WHEN 'jobprocessing' THEN 'processing' WHEN 'processing' THEN 'processing'
      WHEN 'jobcompleted' THEN 'completed' WHEN 'completed' THEN 'completed'
      WHEN 'jobfailed' THEN 'failed' WHEN 'failed' THEN 'failed'
      WHEN 'jobretry' THEN 'retry' WHEN 'retry' THEN 'retry' END,
    lower(regexp_replace(regexp_replace(COALESCE(job_type,''),'^Job','','i'),'[^a-zA-Z]','','g'))
  FROM ddex_job WHERE job_type IS NOT NULL OR status IS NOT NULL
    OR workflow_state_id IS NULL OR operation_id IS NULL
  UNION ALL
  SELECT 'ddex_import_change',id,operation,NULL,NULL,operation_id,NULL,NULL,
    lower(regexp_replace(regexp_replace(COALESCE(operation,''),'^Op','','i'),'[^a-zA-Z]','','g'))
  FROM ddex_import_change WHERE operation IS NOT NULL OR operation_id IS NULL
)
SELECT source.*,state_match.candidate_count state_candidates,state_match.target_id target_workflow_state_id,
  operation_match.candidate_count operation_candidates,operation_match.target_id target_operation_id
FROM source
LEFT JOIN LATERAL (
  SELECT count(*) candidate_count,(array_agg(state.id ORDER BY state.id))[1] target_id
  FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE source.workflow_code IS NOT NULL AND workflow.code=source.workflow_code
    AND workflow.active AND state.active AND state.code=source.state_code
) state_match ON TRUE
LEFT JOIN LATERAL (
  SELECT count(*) candidate_count,(array_agg(candidate.id ORDER BY candidate.id))[1] target_id
  FROM (
    SELECT id FROM ddex_job_operation WHERE source.source_table='ddex_job'
      AND active AND code=source.operation_code
    UNION ALL
    SELECT id FROM ddex_import_operation WHERE source.source_table='ddex_import_change'
      AND active AND code=source.operation_code
  ) candidate
) operation_match ON TRUE;

DO $gate$ DECLARE source_rows bigint; invalid_rows bigint; BEGIN
  SELECT count(*),count(*) FILTER (WHERE
    (source_table<>'ddex_import_change' AND state_candidates<>1)
    OR (source_table IN ('ddex_job','ddex_import_change') AND operation_candidates<>1)
    OR (workflow_state_id IS NOT NULL AND workflow_state_id<>target_workflow_state_id)
    OR (operation_id IS NOT NULL AND operation_id<>target_operation_id))
  INTO source_rows,invalid_rows FROM resolved_ddex_operational;
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000
    OR source_rows>current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0
    OR (SELECT count(*) FROM workflow_definition WHERE code LIKE 'ddex-%-lifecycle' AND active)<>6
    OR (SELECT count(*) FROM workflow_state state JOIN workflow_definition workflow
      ON workflow.id=state.workflow_id WHERE workflow.code IN ('ddex-validation-lifecycle',
        'ddex-import-plan-lifecycle','ddex-import-run-lifecycle','ddex-export-lifecycle','ddex-job-lifecycle')
        AND workflow.active AND state.active)<>23
    OR (SELECT count(*) FROM ddex_job_operation WHERE active)<>4
    OR (SELECT count(*) FROM ddex_import_operation WHERE active)<>3 THEN
    RAISE EXCEPTION 'DDEX operational cutover safety gate failed: rows=%, invalidOrAmbiguous=%',
      source_rows,invalid_rows USING ERRCODE='23514';
  END IF;
END $gate$;

INSERT INTO catalog_ddex_operational_cutover_source (
  run_id,source_table,source_record_id,original_operation,original_status,
  original_workflow_state_id,original_operation_id,target_workflow_state_id,target_operation_id,
  normalized_state,normalized_operation,evidence
)
SELECT :'backfill_run_id'::uuid,source_table,source_record_id,legacy_operation,legacy_status,
  workflow_state_id,operation_id,target_workflow_state_id,target_operation_id,state_code,operation_code,
  'exact normalized legacy discriminant matched one active persisted workflow state or operation'
FROM resolved_ddex_operational
ON CONFLICT (run_id,source_table,source_record_id) DO NOTHING;

INSERT INTO workflow_migration_mapping (
  id,run_id,workflow_id,source_table,source_column,source_record_id,original_value,
  normalized_value,state_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),source.run_id,workflow.id,source.source_table,'status',
  source.source_record_id::text,COALESCE(source.original_status,'<implicit>'),source.normalized_state,
  source.target_workflow_state_id,'mapped',source.evidence,1,now()
FROM catalog_ddex_operational_cutover_source source
JOIN workflow_state state ON state.id=source.target_workflow_state_id
JOIN workflow_definition workflow ON workflow.id=state.workflow_id
WHERE source.run_id=:'backfill_run_id'::uuid AND source.target_workflow_state_id IS NOT NULL
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET state_id=EXCLUDED.state_id,normalized_value=EXCLUDED.normalized_value,
  status='mapped',evidence=EXCLUDED.evidence;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),source.run_id,source.source_table,'operation',source.source_record_id::text,
  COALESCE(source.original_operation,'<missing>'),source.normalized_operation,catalog.id,source.target_operation_id,
  'mapped',source.evidence,1,now()
FROM catalog_ddex_operational_cutover_source source
JOIN catalog_definition catalog ON catalog.code=CASE source.source_table
  WHEN 'ddex_job' THEN 'ddex-job-operations' ELSE 'ddex-import-operations' END
WHERE source.run_id=:'backfill_run_id'::uuid AND source.target_operation_id IS NOT NULL
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,normalized_value=EXCLUDED.normalized_value,
  status='mapped',evidence=EXCLUDED.evidence;

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
  WHERE target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_validation_run target SET workflow_state_id=source.target_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_validation_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_plan target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_plan' AND source.source_record_id=target.id
  WHERE target.status IS NOT DISTINCT FROM source.original_status
    AND target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_plan target SET status=NULL,workflow_state_id=source.target_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_plan' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_run target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_run' AND source.source_record_id=target.id
  WHERE target.status IS NOT DISTINCT FROM source.original_status
    AND target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_run target SET status=NULL,workflow_state_id=source.target_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_run' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_export target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_export' AND source.source_record_id=target.id
  WHERE target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_export target SET workflow_state_id=source.target_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_export' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_job target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_job' AND source.source_record_id=target.id
  WHERE target.job_type IS NOT DISTINCT FROM source.original_operation
    AND target.status IS NOT DISTINCT FROM source.original_status
    AND target.operation_id IS NOT DISTINCT FROM source.original_operation_id
    AND target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_job target SET job_type=NULL,status=NULL,operation_id=source.target_operation_id,
    workflow_state_id=source.target_workflow_state_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_job' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed integer; BEGIN LOOP WITH batch AS (
  SELECT target.id FROM ddex_import_change target JOIN catalog_ddex_operational_cutover_source source
  ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
    AND source.source_table='ddex_import_change' AND source.source_record_id=target.id
  WHERE target.operation IS NOT DISTINCT FROM source.original_operation
    AND target.operation_id IS NOT DISTINCT FROM source.original_operation_id
  ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
) UPDATE ddex_import_change target SET operation=NULL,operation_id=source.target_operation_id
  FROM catalog_ddex_operational_cutover_source source,batch WHERE target.id=batch.id
  AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
  AND source.source_table='ddex_import_change' AND source.source_record_id=target.id;
GET DIAGNOSTICS changed=ROW_COUNT; EXIT WHEN changed=0; END LOOP; END $batches$;

CREATE TRIGGER ddex_operational_state_integrity BEFORE INSERT OR UPDATE ON ddex_validation_run
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_operational_state();
CREATE TRIGGER ddex_operational_state_integrity BEFORE INSERT OR UPDATE ON ddex_import_plan
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_operational_state();
CREATE TRIGGER ddex_operational_state_integrity BEFORE INSERT OR UPDATE ON ddex_import_run
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_operational_state();
CREATE TRIGGER ddex_operational_state_integrity BEFORE INSERT OR UPDATE ON ddex_job
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_operational_state();
CREATE TRIGGER ddex_import_change_canonical_integrity BEFORE INSERT OR UPDATE ON ddex_import_change
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_import_change();
CREATE TRIGGER ddex_export_canonical_integrity BEFORE INSERT OR UPDATE ON ddex_export
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_export();

DO $final$ DECLARE invalid bigint; BEGIN
  SELECT
    (SELECT count(*) FROM ddex_validation_run item JOIN workflow_state state ON state.id=item.workflow_state_id
      JOIN workflow_definition workflow ON workflow.id=state.workflow_id WHERE workflow.code<>'ddex-validation-lifecycle')
    +(SELECT count(*) FROM ddex_validation_run WHERE workflow_state_id IS NULL)
    +(SELECT count(*) FROM ddex_import_plan item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
      LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id
      WHERE item.status IS NOT NULL OR workflow.code IS DISTINCT FROM 'ddex-import-plan-lifecycle')
    +(SELECT count(*) FROM ddex_import_run item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
      LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id
      WHERE item.status IS NOT NULL OR workflow.code IS DISTINCT FROM 'ddex-import-run-lifecycle')
    +(SELECT count(*) FROM ddex_export item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
      LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id
      WHERE workflow.code IS DISTINCT FROM 'ddex-export-lifecycle')
    +(SELECT count(*) FROM ddex_job item LEFT JOIN workflow_state state ON state.id=item.workflow_state_id
      LEFT JOIN workflow_definition workflow ON workflow.id=state.workflow_id
      LEFT JOIN ddex_job_operation operation ON operation.id=item.operation_id
      WHERE item.job_type IS NOT NULL OR item.status IS NOT NULL
        OR workflow.code IS DISTINCT FROM 'ddex-job-lifecycle' OR operation.active IS DISTINCT FROM TRUE)
    +(SELECT count(*) FROM ddex_import_change item LEFT JOIN ddex_import_operation operation
      ON operation.id=item.operation_id WHERE item.operation IS NOT NULL OR operation.active IS DISTINCT FROM TRUE)
  INTO invalid;
  IF invalid<>0 THEN RAISE EXCEPTION 'DDEX operational final gate found % invalid rows',invalid USING ERRCODE='23514'; END IF;
END $final$;

UPDATE catalog_backfill_run SET status='completed',completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_ddex_operational_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_ddex_operational_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  ambiguous_rows=0,rejected_rows=0,
  report=jsonb_build_object('batchSize',current_setting('tdf.catalog_batch_size')::integer,
    'evidenceRows',(SELECT count(*) FROM catalog_ddex_operational_cutover_source WHERE run_id=:'backfill_run_id'::uuid))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,
  'scannedRows',scanned_rows,'mappedRows',mapped_rows,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
