\set ON_ERROR_STOP on

SELECT 'ddex-reference-postgres-integration-' || gen_random_uuid() AS run_code,
  'integration-ddex-v1-20260812' AS candidate_revision
\gset
SELECT set_config('tdf.test_ddex_run_code', :'run_code', FALSE);
\set safety_threshold 20
\set batch_size 1

ALTER TABLE ddex_partner ADD COLUMN IF NOT EXISTS allowed_versions text[] NOT NULL DEFAULT ARRAY[]::text[];

CREATE TEMP TABLE ddex_reference_fixture (
  document_id bigint NOT NULL,
  export_id bigint NOT NULL,
  partner_id bigint NOT NULL,
  original_family text NOT NULL,
  original_version text NOT NULL,
  original_message_type text NOT NULL,
  original_status text NOT NULL,
  original_export_version text NOT NULL,
  original_partner_versions text[] NOT NULL
) ON COMMIT PRESERVE ROWS;

ALTER TABLE ddex_document DISABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export DISABLE TRIGGER ddex_export_canonical_integrity;

WITH partner AS (
  INSERT INTO ddex_partner (
    name,dpid,rules_json,naming_convention,is_active,allowed_versions
  ) VALUES (
    'DDEX cutover integration partner ' || :'run_code',
    'PADPIDA' || substr(md5(:'run_code'),1,12),NULL,NULL,TRUE,
    ARRAY['4.3.2','4.3.2']::text[]
  ) RETURNING id,allowed_versions
), document AS (
  INSERT INTO ddex_document (
    file_name,private_uri,sha256,size_bytes,standard_version_id,message_type_id,
    workflow_state_id,family,version,namespace,message_type,status,uploaded_by,
    message_id,sender_id,recipient_id,created_at
  ) VALUES (
    'ddex-reference-cutover-' || :'run_code' || '.xml',
    'private://integration/ddex-reference-cutover/' || :'run_code',
    'ddex-reference-cutover-' || md5(:'run_code'),1024,NULL,NULL,NULL,'FamilyERN','432',
    'http://ddex.net/xml/ern/432','NewReleaseMessage','StatusReceived',1,
    'integration-message-' || :'run_code','integration-sender','integration-recipient',now()
  ) RETURNING id
), export AS (
  INSERT INTO ddex_export (
    release_id,partner_id,standard_version_id,ern_version,profile_name,xml_checksum,
    private_uri,validation_result,created_at
  ) SELECT 987654321,partner.id,NULL,'432',NULL,'integration-export-sha',
    'private://integration/ddex-export/' || :'run_code',NULL,now() FROM partner
  RETURNING id,partner_id
)
INSERT INTO ddex_reference_fixture
SELECT document.id,export.id,partner.id,'FamilyERN','432','NewReleaseMessage',
  'StatusReceived','432',partner.allowed_versions
FROM partner,document,export;

ALTER TABLE ddex_document ENABLE TRIGGER ddex_document_canonical_integrity;
ALTER TABLE ddex_export ENABLE TRIGGER ddex_export_canonical_integrity;

\ir ../../sql/2026-08-12_ddex_reference_cutover_dry_run.sql
\ir ../../sql/2026-08-12_ddex_reference_cutover_apply.sql

DO $assert_first_apply$
DECLARE run_id_value uuid;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_run_code')
    AND candidate_revision='integration-ddex-v1-20260812' AND NOT dry_run;
  IF (SELECT count(*) FROM catalog_ddex_document_cutover_source WHERE run_id=run_id_value)<>1
    OR (SELECT count(*) FROM catalog_ddex_export_cutover_source WHERE run_id=run_id_value)<>1
    OR (SELECT count(*) FROM catalog_ddex_partner_cutover_source WHERE run_id=run_id_value)<>2
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value)<>4
    OR (SELECT count(*) FROM workflow_migration_mapping WHERE run_id=run_id_value)<>1 THEN
    RAISE EXCEPTION 'first apply did not preserve complete per-value DDEX evidence';
  END IF;
  IF EXISTS (
    SELECT 1 FROM ddex_reference_fixture fixture
    JOIN ddex_document document ON document.id=fixture.document_id
    JOIN ddex_standard_version standard ON standard.id=document.standard_version_id
    JOIN ddex_message_type message ON message.id=document.message_type_id
    JOIN workflow_state state ON state.id=document.workflow_state_id
    WHERE document.family IS NOT NULL OR document.version IS NOT NULL
      OR document.message_type IS NOT NULL OR document.status IS NOT NULL
      OR standard.standard_code<>'ERN' OR standard.version_code<>'4.3.2'
      OR message.code<>'NewReleaseMessage' OR state.code<>'received'
  ) THEN RAISE EXCEPTION 'first apply produced incorrect canonical DDEX document references'; END IF;
  IF EXISTS (
    SELECT 1 FROM ddex_reference_fixture fixture
    JOIN ddex_export export ON export.id=fixture.export_id
    JOIN ddex_partner partner ON partner.id=fixture.partner_id
    JOIN ddex_partner_standard_version membership
      ON membership.partner_id=partner.id AND membership.standard_version_id=export.standard_version_id
    JOIN ddex_standard_version standard ON standard.id=export.standard_version_id
    WHERE export.ern_version IS NOT NULL OR NOT membership.active
      OR standard.standard_code<>'ERN' OR standard.version_code<>'4.3.2'
      OR jsonb_array_length(COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb))<>0
  ) THEN RAISE EXCEPTION 'first apply produced incorrect export or partner policy references'; END IF;
  BEGIN
    DELETE FROM catalog_ddex_document_cutover_source
    WHERE run_id=run_id_value AND document_id=(SELECT document_id FROM ddex_reference_fixture);
    RAISE EXCEPTION 'immutable DDEX source evidence was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
END
$assert_first_apply$;

CREATE TEMP TABLE ddex_reference_apply_digest ON COMMIT PRESERVE ROWS AS
SELECT md5(concat_ws('|',document.standard_version_id,document.message_type_id,
  document.workflow_state_id,export.standard_version_id,membership.active,
  membership.sort_order,to_jsonb(partner)->'allowed_versions')) AS digest
FROM ddex_reference_fixture fixture
JOIN ddex_document document ON document.id=fixture.document_id
JOIN ddex_export export ON export.id=fixture.export_id
JOIN ddex_partner partner ON partner.id=fixture.partner_id
JOIN ddex_partner_standard_version membership
  ON membership.partner_id=partner.id AND membership.standard_version_id=export.standard_version_id;

\ir ../../sql/2026-08-12_ddex_reference_cutover_apply.sql

DO $assert_rerun$
DECLARE run_id_value uuid; expected_digest text; current_digest text;
BEGIN
  SELECT id INTO STRICT run_id_value FROM catalog_backfill_run
  WHERE run_code=current_setting('tdf.test_ddex_run_code')
    AND candidate_revision='integration-ddex-v1-20260812' AND NOT dry_run;
  SELECT digest INTO STRICT expected_digest FROM ddex_reference_apply_digest;
  SELECT md5(concat_ws('|',document.standard_version_id,document.message_type_id,
    document.workflow_state_id,export.standard_version_id,membership.active,
    membership.sort_order,to_jsonb(partner)->'allowed_versions')) INTO STRICT current_digest
  FROM ddex_reference_fixture fixture
  JOIN ddex_document document ON document.id=fixture.document_id
  JOIN ddex_export export ON export.id=fixture.export_id
  JOIN ddex_partner partner ON partner.id=fixture.partner_id
  JOIN ddex_partner_standard_version membership
    ON membership.partner_id=partner.id AND membership.standard_version_id=export.standard_version_id;
  IF current_digest IS DISTINCT FROM expected_digest THEN
    RAISE EXCEPTION 'idempotent rerun changed canonical DDEX rows';
  END IF;
  IF (SELECT count(*) FROM catalog_ddex_document_cutover_source WHERE run_id=run_id_value)<>1
    OR (SELECT count(*) FROM catalog_ddex_export_cutover_source WHERE run_id=run_id_value)<>1
    OR (SELECT count(*) FROM catalog_ddex_partner_cutover_source WHERE run_id=run_id_value)<>2
    OR (SELECT count(*) FROM catalog_migration_mapping WHERE run_id=run_id_value)<>4 THEN
    RAISE EXCEPTION 'idempotent rerun duplicated DDEX evidence';
  END IF;
END
$assert_rerun$;

\ir ../../sql/2026-08-12_ddex_reference_cutover_rollback.sql

DO $assert_rollback$
BEGIN
  IF EXISTS (
    SELECT 1 FROM ddex_reference_fixture fixture
    JOIN ddex_document document ON document.id=fixture.document_id
    JOIN ddex_export export ON export.id=fixture.export_id
    JOIN ddex_partner partner ON partner.id=fixture.partner_id
    JOIN ddex_partner_standard_version membership ON membership.partner_id=fixture.partner_id
    WHERE document.family IS DISTINCT FROM fixture.original_family
      OR document.version IS DISTINCT FROM fixture.original_version
      OR document.message_type IS DISTINCT FROM fixture.original_message_type
      OR document.status IS DISTINCT FROM fixture.original_status
      OR document.standard_version_id IS NOT NULL OR document.message_type_id IS NOT NULL
      OR document.workflow_state_id IS NOT NULL
      OR export.ern_version IS DISTINCT FROM fixture.original_export_version
      OR export.standard_version_id IS NOT NULL OR membership.active
      OR (to_jsonb(partner)->'allowed_versions') IS DISTINCT FROM to_jsonb(fixture.original_partner_versions)
  ) THEN RAISE EXCEPTION 'rollback did not restore exact legacy DDEX values'; END IF;
END
$assert_rollback$;

\ir ../../sql/2026-08-12_ddex_reference_cutover_apply.sql

DO $assert_reapply_and_guards$
DECLARE document_id_value bigint; received_id uuid; imported_id uuid;
BEGIN
  SELECT document_id INTO STRICT document_id_value FROM ddex_reference_fixture;
  SELECT state.id INTO STRICT received_id FROM workflow_state state
    JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    WHERE workflow.code='ddex-document-lifecycle' AND state.code='received';
  SELECT state.id INTO STRICT imported_id FROM workflow_state state
    JOIN workflow_definition workflow ON workflow.id=state.workflow_id
    WHERE workflow.code='ddex-document-lifecycle' AND state.code='imported';
  BEGIN
    UPDATE ddex_document SET family='ERN' WHERE id=document_id_value;
    RAISE EXCEPTION 'legacy DDEX string update was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE ddex_document SET workflow_state_id=imported_id WHERE id=document_id_value;
    RAISE EXCEPTION 'invalid DDEX workflow transition was accepted';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    UPDATE workflow_state SET active=FALSE WHERE id=received_id;
    RAISE EXCEPTION 'referenced DDEX workflow state was deactivated';
  EXCEPTION WHEN check_violation THEN NULL; END;
  BEGIN
    DELETE FROM ddex_document WHERE id=document_id_value;
    RAISE EXCEPTION 'DDEX document was hard-deleted';
  EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
  RAISE NOTICE 'DDEX dry-run/apply/rerun/rollback/reapply and negative guards passed';
END
$assert_reapply_and_guards$;
