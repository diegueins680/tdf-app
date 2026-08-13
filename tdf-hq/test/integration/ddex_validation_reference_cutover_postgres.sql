\set ON_ERROR_STOP on

SELECT 'ddex-validation-reference-postgres-integration-' || gen_random_uuid() AS run_code,
  'integration-ddex-validation-reference-v1-20260812' AS candidate_revision
\gset
SELECT set_config('tdf.test_ddex_validation_reference_run_code', :'run_code', FALSE);
\set safety_threshold 20
\set batch_size 1

CREATE TEMP TABLE ddex_validation_reference_fixture (
  validation_run_id bigint NOT NULL,
  issue_id bigint NOT NULL,
  export_id bigint NOT NULL
) ON COMMIT PRESERVE ROWS;

ALTER TABLE ddex_validation_run DISABLE TRIGGER ddex_validation_reference_integrity;
ALTER TABLE ddex_validation_issue DISABLE TRIGGER ddex_validation_reference_integrity;
ALTER TABLE ddex_export DISABLE TRIGGER ddex_validation_reference_integrity;

WITH standard AS (
  SELECT id FROM ddex_standard_version WHERE standard_code='ERN' AND version_code='4.3.2' AND active
), received AS (
  SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='ddex-document-lifecycle' AND state.code='received'
), validation_state AS (
  SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='ddex-validation-lifecycle' AND state.code='succeeded'
), export_state AS (
  SELECT state.id FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='ddex-export-lifecycle' AND state.code='ready'
), partner AS (
  INSERT INTO ddex_partner (name,dpid,rules_json,naming_convention,is_active)
  VALUES ('DDEX validation reference integration ' || :'run_code',
    'PADPIDA' || substr(md5(:'run_code'),1,12),NULL,NULL,TRUE) RETURNING id
), membership AS (
  INSERT INTO ddex_partner_standard_version (partner_id,standard_version_id,sort_order,active,created_at)
  SELECT partner.id,standard.id,0,TRUE,now() FROM partner,standard RETURNING partner_id,standard_version_id
), document AS (
  INSERT INTO ddex_document (file_name,private_uri,sha256,size_bytes,standard_version_id,
    message_type_id,workflow_state_id,family,version,namespace,message_type,status,uploaded_by,
    message_id,sender_id,recipient_id,created_at)
  SELECT 'validation-reference-' || :'run_code' || '.xml','private://integration/' || :'run_code',
    'validation-reference-' || md5(:'run_code'),1000,standard.id,NULL,received.id,
    NULL,NULL,NULL,NULL,NULL,1,'validation-reference-' || :'run_code',NULL,NULL,now()
  FROM standard,received RETURNING id
), validation_run AS (
  INSERT INTO ddex_validation_run (document_id,workflow_state_id,result_id,validator_version,
    schema_version,started_at,finished_at,result,error_count,warning_count)
  SELECT document.id,validation_state.id,NULL,'integration','4.3.2',now(),now(),
    'ResultWarning',0,1 FROM document,validation_state RETURNING id
), issue AS (
  INSERT INTO ddex_validation_issue (validation_run_id,severity_id,layer_id,severity,layer,
    code,line_number,column_number,xpath_ref,message,suggestion)
  SELECT validation_run.id,NULL,NULL,'SeverityWarning','LayerXSD','XSD-TEST',12,3,NULL,
    'Integration warning',NULL FROM validation_run RETURNING id
), export AS (
  INSERT INTO ddex_export (release_id,partner_id,standard_version_id,workflow_state_id,
    ern_version,profile_name,xml_checksum,private_uri,validation_result_id,validation_result,created_at)
  SELECT 987654322,membership.partner_id,membership.standard_version_id,export_state.id,
    NULL,NULL,'validation-reference-' || md5(:'run_code'),'private://integration/export/' || :'run_code',
    NULL,'success',now() FROM membership,export_state RETURNING id
)
INSERT INTO ddex_validation_reference_fixture
SELECT validation_run.id,issue.id,export.id FROM validation_run,issue,export;

ALTER TABLE ddex_validation_run ENABLE TRIGGER ddex_validation_reference_integrity;
ALTER TABLE ddex_validation_issue ENABLE TRIGGER ddex_validation_reference_integrity;
ALTER TABLE ddex_export ENABLE TRIGGER ddex_validation_reference_integrity;

\ir ../../sql/2026-08-12_ddex_validation_reference_cutover_dry_run.sql
\ir ../../sql/2026-08-12_ddex_validation_reference_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_validation_reference_run_code')
    AND candidate_revision='integration-ddex-validation-reference-v1-20260812' AND NOT dry_run;
  IF (SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value)<>4 THEN
    RAISE EXCEPTION 'first validation reference apply did not preserve four mappings';
  END IF;
  IF EXISTS (
    SELECT 1 FROM ddex_validation_reference_fixture fixture
    JOIN ddex_validation_run run ON run.id=fixture.validation_run_id
    JOIN ddex_validation_result run_result ON run_result.id=run.result_id
    JOIN ddex_validation_issue issue ON issue.id=fixture.issue_id
    JOIN ddex_validation_severity severity ON severity.id=issue.severity_id
    JOIN ddex_validation_layer layer ON layer.id=issue.layer_id
    JOIN ddex_export export ON export.id=fixture.export_id
    JOIN ddex_validation_result export_result ON export_result.id=export.validation_result_id
    WHERE run_result.code<>'warning' OR run.result IS NOT NULL
      OR severity.code<>'warning' OR layer.code<>'xsd'
      OR issue.severity IS NOT NULL OR issue.layer IS NOT NULL
      OR export_result.code<>'success' OR export.validation_result IS NOT NULL
  ) THEN RAISE EXCEPTION 'validation reference apply produced incorrect canonical identities'; END IF;
END
$assert_first_apply$;

CREATE TEMP TABLE ddex_validation_reference_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(concat_ws('|',run.result_id,issue.severity_id,issue.layer_id,export.validation_result_id)) digest
FROM ddex_validation_reference_fixture fixture
JOIN ddex_validation_run run ON run.id=fixture.validation_run_id
JOIN ddex_validation_issue issue ON issue.id=fixture.issue_id
JOIN ddex_export export ON export.id=fixture.export_id;

\ir ../../sql/2026-08-12_ddex_validation_reference_cutover_apply.sql

DO $assert_rerun$
DECLARE run_id_value uuid; expected_digest text; actual_digest text;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_validation_reference_run_code')
    AND candidate_revision='integration-ddex-validation-reference-v1-20260812' AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM ddex_validation_reference_digest;
  SELECT md5(concat_ws('|',run.result_id,issue.severity_id,issue.layer_id,export.validation_result_id))
    INTO STRICT actual_digest FROM ddex_validation_reference_fixture fixture
  JOIN ddex_validation_run run ON run.id=fixture.validation_run_id
  JOIN ddex_validation_issue issue ON issue.id=fixture.issue_id
  JOIN ddex_export export ON export.id=fixture.export_id;
  IF actual_digest IS DISTINCT FROM expected_digest
    OR (SELECT count(*) FROM catalog_ddex_validation_reference_cutover_source WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value)<>4 THEN
    RAISE EXCEPTION 'validation reference rerun changed identities or duplicated evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-12_ddex_validation_reference_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM ddex_validation_reference_fixture fixture
    JOIN ddex_validation_run run ON run.id=fixture.validation_run_id
    JOIN ddex_validation_issue issue ON issue.id=fixture.issue_id
    JOIN ddex_export export ON export.id=fixture.export_id
    WHERE run.result_id IS NOT NULL OR run.result<>'ResultWarning'
      OR issue.severity_id IS NOT NULL OR issue.layer_id IS NOT NULL
      OR issue.severity<>'SeverityWarning' OR issue.layer<>'LayerXSD'
      OR export.validation_result_id IS NOT NULL OR export.validation_result<>'success'
  ) THEN RAISE EXCEPTION 'validation reference rollback did not restore exact source values'; END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-12_ddex_validation_reference_cutover_apply.sql

DO $assert_guards$
DECLARE run_id_value bigint; issue_id_value bigint; export_id_value bigint;
  result_id_value uuid; severity_id_value uuid; layer_id_value uuid;
BEGIN
  SELECT validation_run_id,issue_id,export_id INTO STRICT run_id_value,issue_id_value,export_id_value
    FROM ddex_validation_reference_fixture;
  SELECT result_id INTO STRICT result_id_value FROM ddex_validation_run WHERE id=run_id_value;
  SELECT severity_id,layer_id INTO STRICT severity_id_value,layer_id_value
    FROM ddex_validation_issue WHERE id=issue_id_value;
  BEGIN UPDATE ddex_validation_run SET result='ResultWarning' WHERE id=run_id_value;
    RAISE EXCEPTION 'legacy validation result was accepted'; EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN UPDATE ddex_validation_issue SET severity='SeverityWarning' WHERE id=issue_id_value;
    RAISE EXCEPTION 'legacy validation severity was accepted'; EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN UPDATE ddex_validation_issue SET layer='LayerXSD' WHERE id=issue_id_value;
    RAISE EXCEPTION 'legacy validation layer was accepted'; EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN UPDATE ddex_export SET validation_result='success' WHERE id=export_id_value;
    RAISE EXCEPTION 'legacy export validation result was accepted'; EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN UPDATE ddex_validation_severity SET active=FALSE WHERE id=severity_id_value;
    RAISE EXCEPTION 'referenced validation severity was deactivated'; EXCEPTION WHEN check_violation THEN NULL; WHEN object_not_in_prerequisite_state THEN NULL; END;
  BEGIN DELETE FROM ddex_validation_layer WHERE id=layer_id_value;
    RAISE EXCEPTION 'validation layer was hard-deleted'; EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  BEGIN DELETE FROM catalog_ddex_validation_reference_cutover_source
      WHERE run_id=(SELECT id FROM catalog_backfill_run WHERE run_code=current_setting('tdf.test_ddex_validation_reference_run_code') AND candidate_revision='integration-ddex-validation-reference-v1-20260812' AND NOT dry_run);
    RAISE EXCEPTION 'validation source evidence was hard-deleted'; EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  RAISE NOTICE 'DDEX validation reference dry-run/apply/rerun/rollback/reapply and negative guards passed';
END
$assert_guards$;
