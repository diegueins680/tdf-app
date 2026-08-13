\set ON_ERROR_STOP on

BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '5min';
SET LOCAL lock_timeout = '1s';

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
), resolved AS (
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
  ) operation_match ON TRUE
)
SELECT jsonb_build_object(
  'report','ddex-operational-cutover','sourceRows',count(*),
  'mapped',count(*) FILTER (WHERE
    (source_table='ddex_import_change' OR state_candidates=1)
    AND (source_table NOT IN ('ddex_job','ddex_import_change') OR operation_candidates=1)
    AND (workflow_state_id IS NULL OR workflow_state_id=target_workflow_state_id)
    AND (operation_id IS NULL OR operation_id=target_operation_id)),
  'unresolved',count(*) FILTER (WHERE
    (source_table<>'ddex_import_change' AND state_candidates=0)
    OR (source_table IN ('ddex_job','ddex_import_change') AND operation_candidates=0)),
  'ambiguous',count(*) FILTER (WHERE state_candidates>1 OR operation_candidates>1),
  'conflicts',count(*) FILTER (WHERE
    (workflow_state_id IS NOT NULL AND workflow_state_id<>target_workflow_state_id)
    OR (operation_id IS NOT NULL AND operation_id<>target_operation_id)),
  'rows',COALESCE(jsonb_agg(jsonb_build_object(
    'table',source_table,'id',source_record_id,'legacyOperation',legacy_operation,
    'legacyStatus',legacy_status,'stateCode',state_code,'operationCode',operation_code,
    'targetWorkflowStateId',target_workflow_state_id,'targetOperationId',target_operation_id,
    'stateCandidates',state_candidates,'operationCandidates',operation_candidates
  ) ORDER BY source_table,source_record_id),'[]'::jsonb)
) FROM resolved;

SELECT jsonb_build_object(
  'report','ddex-operational-registry',
  'workflows',(SELECT count(*) FROM workflow_definition WHERE code LIKE 'ddex-%-lifecycle' AND active),
  'operationalStates',(SELECT count(*) FROM workflow_state state JOIN workflow_definition workflow
    ON workflow.id=state.workflow_id WHERE workflow.code IN ('ddex-validation-lifecycle',
      'ddex-import-plan-lifecycle','ddex-import-run-lifecycle','ddex-export-lifecycle','ddex-job-lifecycle')
      AND workflow.active AND state.active),
  'jobOperations',(SELECT count(*) FROM ddex_job_operation WHERE active),
  'importOperations',(SELECT count(*) FROM ddex_import_operation WHERE active)
);

ROLLBACK;
