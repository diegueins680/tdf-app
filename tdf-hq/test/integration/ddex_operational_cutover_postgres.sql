\set ON_ERROR_STOP on

SELECT 'ddex-operational-postgres-integration-' || gen_random_uuid() AS run_code,
  'integration-ddex-operational-v1-20260812' AS candidate_revision
\gset
SELECT set_config('tdf.test_ddex_operational_run_code', :'run_code', FALSE);
\set safety_threshold 20
\set batch_size 1

CREATE TEMP TABLE ddex_operational_fixture (
  validation_run_id bigint NOT NULL,
  import_plan_id bigint NOT NULL,
  import_run_id bigint NOT NULL,
  import_change_id bigint NOT NULL,
  export_id bigint NOT NULL,
  job_id bigint NOT NULL
) ON COMMIT PRESERVE ROWS;

ALTER TABLE ddex_validation_run DISABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_plan DISABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_run DISABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_change DISABLE TRIGGER ddex_import_change_canonical_integrity;
ALTER TABLE ddex_export DISABLE TRIGGER ddex_export_canonical_integrity;
ALTER TABLE ddex_job DISABLE TRIGGER ddex_operational_state_integrity;

WITH standard AS (
  SELECT id FROM ddex_standard_version
  WHERE standard_code='ERN' AND version_code='4.3.2' AND active
), received AS (
  SELECT state.id FROM workflow_state state JOIN workflow_definition workflow
    ON workflow.id=state.workflow_id
  WHERE workflow.code='ddex-document-lifecycle' AND state.code='received'
), partner AS (
  INSERT INTO ddex_partner (name,dpid,rules_json,naming_convention,is_active)
  VALUES ('DDEX operational integration partner ' || :'run_code',
    'PADPIDA' || substr(md5(:'run_code'),1,12),NULL,NULL,TRUE)
  RETURNING id
), membership AS (
  INSERT INTO ddex_partner_standard_version (
    partner_id,standard_version_id,sort_order,active,created_at
  ) SELECT partner.id,standard.id,0,TRUE,now() FROM partner,standard
  RETURNING partner_id,standard_version_id
), document AS (
  INSERT INTO ddex_document (
    file_name,private_uri,sha256,size_bytes,standard_version_id,message_type_id,
    workflow_state_id,family,version,namespace,message_type,status,uploaded_by,
    message_id,sender_id,recipient_id,created_at
  ) SELECT 'ddex-operational-' || :'run_code' || '.xml',
    'private://integration/ddex-operational/' || :'run_code',
    'ddex-operational-' || md5(:'run_code'),1024,standard.id,NULL,received.id,
    NULL,NULL,NULL,NULL,NULL,1,'operational-' || :'run_code',NULL,NULL,now()
  FROM standard,received RETURNING id
), validation_run AS (
  INSERT INTO ddex_validation_run (
    document_id,workflow_state_id,validator_version,schema_version,started_at,
    finished_at,result,error_count,warning_count
  ) SELECT document.id,NULL,'integration','4.3.2',now(),now(),'ResultSuccess',0,0
    FROM document RETURNING id
), import_plan AS (
  INSERT INTO ddex_import_plan (
    document_id,workflow_state_id,status,snapshot_json,version,created_at
  ) SELECT document.id,NULL,'PlanDraft','{}',1,now() FROM document RETURNING id
), import_run AS (
  INSERT INTO ddex_import_run (
    plan_id,actor_id,workflow_state_id,status,started_at,finished_at,error_message
  ) SELECT import_plan.id,1,NULL,'RunPending',now(),NULL,NULL FROM import_plan RETURNING id
), import_change AS (
  INSERT INTO ddex_import_change (
    import_run_id,entity_type,entity_id,operation_id,operation,previous_state,new_state
  ) SELECT import_run.id,'record_release',NULL,NULL,'OpCreate',NULL,'{}'
    FROM import_run RETURNING id
), export AS (
  INSERT INTO ddex_export (
    release_id,partner_id,standard_version_id,workflow_state_id,ern_version,
    profile_name,xml_checksum,private_uri,validation_result,created_at
  ) SELECT 987654321,membership.partner_id,membership.standard_version_id,NULL,NULL,
    NULL,'operational-export-' || md5(:'run_code'),
    'private://integration/ddex-operational-export/' || :'run_code',NULL,now()
  FROM membership RETURNING id
), job AS (
  INSERT INTO ddex_job (
    operation_id,job_type,entity_id,workflow_state_id,status,attempts,leased_until,
    last_error,created_at,updated_at
  ) VALUES (NULL,'JobValidate',987654321,NULL,'JobPending',0,NULL,NULL,now(),now())
  RETURNING id
)
INSERT INTO ddex_operational_fixture
SELECT validation_run.id,import_plan.id,import_run.id,import_change.id,export.id,job.id
FROM validation_run,import_plan,import_run,import_change,export,job;

ALTER TABLE ddex_validation_run ENABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_plan ENABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_run ENABLE TRIGGER ddex_operational_state_integrity;
ALTER TABLE ddex_import_change ENABLE TRIGGER ddex_import_change_canonical_integrity;
ALTER TABLE ddex_export ENABLE TRIGGER ddex_export_canonical_integrity;
ALTER TABLE ddex_job ENABLE TRIGGER ddex_operational_state_integrity;

\ir ../../sql/2026-08-12_ddex_operational_cutover_dry_run.sql
\ir ../../sql/2026-08-12_ddex_operational_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_operational_run_code')
    AND candidate_revision='integration-ddex-operational-v1-20260812' AND NOT dry_run;
  IF (SELECT count(*) FROM catalog_ddex_operational_cutover_source
      WHERE run_id=run_id_value)<>6
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value)<>5
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value)<>2 THEN
    RAISE EXCEPTION 'first apply did not preserve all DDEX operational mapping evidence';
  END IF;
  IF EXISTS (
    SELECT 1 FROM ddex_operational_fixture fixture
    JOIN ddex_validation_run validation_run ON validation_run.id=fixture.validation_run_id
    JOIN workflow_state validation_state ON validation_state.id=validation_run.workflow_state_id
    JOIN ddex_import_plan import_plan ON import_plan.id=fixture.import_plan_id
    JOIN workflow_state plan_state ON plan_state.id=import_plan.workflow_state_id
    JOIN ddex_import_run import_run ON import_run.id=fixture.import_run_id
    JOIN workflow_state run_state ON run_state.id=import_run.workflow_state_id
    JOIN ddex_import_change import_change ON import_change.id=fixture.import_change_id
    JOIN ddex_import_operation import_operation ON import_operation.id=import_change.operation_id
    JOIN ddex_export export ON export.id=fixture.export_id
    JOIN workflow_state export_state ON export_state.id=export.workflow_state_id
    JOIN ddex_job job ON job.id=fixture.job_id
    JOIN workflow_state job_state ON job_state.id=job.workflow_state_id
    JOIN ddex_job_operation job_operation ON job_operation.id=job.operation_id
    WHERE validation_state.code<>'succeeded'
      OR plan_state.code<>'draft' OR import_plan.status IS NOT NULL
      OR run_state.code<>'pending' OR import_run.status IS NOT NULL
      OR import_operation.code<>'create' OR import_change.operation IS NOT NULL
      OR export_state.code<>'ready'
      OR job_state.code<>'pending' OR job_operation.code<>'validate'
      OR job.status IS NOT NULL OR job.job_type IS NOT NULL
  ) THEN RAISE EXCEPTION 'first apply produced incorrect canonical operational references'; END IF;
  BEGIN
    DELETE FROM catalog_ddex_operational_cutover_source WHERE run_id=run_id_value;
    RAISE EXCEPTION 'immutable DDEX operational source evidence was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
END
$assert_first_apply$;

CREATE TEMP TABLE ddex_operational_apply_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(concat_ws('|',validation_run.workflow_state_id,import_plan.workflow_state_id,
  import_run.workflow_state_id,import_change.operation_id,export.workflow_state_id,
  job.workflow_state_id,job.operation_id)) AS digest
FROM ddex_operational_fixture fixture
JOIN ddex_validation_run validation_run ON validation_run.id=fixture.validation_run_id
JOIN ddex_import_plan import_plan ON import_plan.id=fixture.import_plan_id
JOIN ddex_import_run import_run ON import_run.id=fixture.import_run_id
JOIN ddex_import_change import_change ON import_change.id=fixture.import_change_id
JOIN ddex_export export ON export.id=fixture.export_id
JOIN ddex_job job ON job.id=fixture.job_id;

\ir ../../sql/2026-08-12_ddex_operational_cutover_apply.sql

DO $assert_rerun$
DECLARE run_id_value uuid; expected_digest text; current_digest text;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_operational_run_code')
    AND candidate_revision='integration-ddex-operational-v1-20260812' AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM ddex_operational_apply_digest;
  SELECT md5(concat_ws('|',validation_run.workflow_state_id,import_plan.workflow_state_id,
    import_run.workflow_state_id,import_change.operation_id,export.workflow_state_id,
    job.workflow_state_id,job.operation_id)) INTO STRICT current_digest
  FROM ddex_operational_fixture fixture
  JOIN ddex_validation_run validation_run ON validation_run.id=fixture.validation_run_id
  JOIN ddex_import_plan import_plan ON import_plan.id=fixture.import_plan_id
  JOIN ddex_import_run import_run ON import_run.id=fixture.import_run_id
  JOIN ddex_import_change import_change ON import_change.id=fixture.import_change_id
  JOIN ddex_export export ON export.id=fixture.export_id
  JOIN ddex_job job ON job.id=fixture.job_id;
  IF current_digest IS DISTINCT FROM expected_digest
    OR (SELECT count(*) FROM catalog_ddex_operational_cutover_source
      WHERE run_id=run_id_value)<>6 THEN
    RAISE EXCEPTION 'idempotent rerun changed rows or duplicated evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-12_ddex_operational_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM ddex_operational_fixture fixture
    JOIN ddex_validation_run validation_run ON validation_run.id=fixture.validation_run_id
    JOIN ddex_import_plan import_plan ON import_plan.id=fixture.import_plan_id
    JOIN ddex_import_run import_run ON import_run.id=fixture.import_run_id
    JOIN ddex_import_change import_change ON import_change.id=fixture.import_change_id
    JOIN ddex_export export ON export.id=fixture.export_id
    JOIN ddex_job job ON job.id=fixture.job_id
    WHERE validation_run.workflow_state_id IS NOT NULL
      OR import_plan.workflow_state_id IS NOT NULL OR import_plan.status<>'PlanDraft'
      OR import_run.workflow_state_id IS NOT NULL OR import_run.status<>'RunPending'
      OR import_change.operation_id IS NOT NULL OR import_change.operation<>'OpCreate'
      OR export.workflow_state_id IS NOT NULL
      OR job.workflow_state_id IS NOT NULL OR job.operation_id IS NOT NULL
      OR job.status<>'JobPending' OR job.job_type<>'JobValidate'
  ) THEN RAISE EXCEPTION 'rollback did not restore exact legacy operational values'; END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-12_ddex_operational_cutover_apply.sql

DO $assert_reapply_and_guards$
DECLARE plan_id_value bigint; job_id_value bigint; change_id_value bigint;
  draft_id uuid; committed_id uuid; validate_operation_id uuid;
BEGIN
  SELECT import_plan_id,job_id,import_change_id INTO STRICT
    plan_id_value,job_id_value,change_id_value FROM ddex_operational_fixture;
  SELECT state.id INTO STRICT draft_id FROM workflow_state state
    JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    WHERE workflow.code='ddex-import-plan-lifecycle' AND state.code='draft';
  SELECT state.id INTO STRICT committed_id FROM workflow_state state
    JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    WHERE workflow.code='ddex-import-plan-lifecycle' AND state.code='committed';
  SELECT id INTO STRICT validate_operation_id FROM ddex_job_operation WHERE code='validate';
  BEGIN
    UPDATE ddex_import_plan SET status='PlanDraft' WHERE id=plan_id_value;
    RAISE EXCEPTION 'legacy plan status update was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE ddex_job SET job_type='JobValidate' WHERE id=job_id_value;
    RAISE EXCEPTION 'legacy job operation update was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE ddex_import_change SET operation='OpCreate' WHERE id=change_id_value;
    RAISE EXCEPTION 'legacy import operation update was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE ddex_import_plan SET workflow_state_id=committed_id WHERE id=plan_id_value;
    RAISE EXCEPTION 'invalid import plan transition was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE workflow_state SET active=FALSE WHERE id=draft_id;
    RAISE EXCEPTION 'referenced operational state was deactivated';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    DELETE FROM ddex_job_operation WHERE id=validate_operation_id;
    RAISE EXCEPTION 'persisted DDEX operation was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  RAISE NOTICE 'DDEX operational dry-run/apply/rerun/rollback/reapply and negative guards passed';
END
$assert_reapply_and_guards$;
