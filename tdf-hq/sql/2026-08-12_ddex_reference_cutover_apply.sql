\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-reference-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-reference-cutover-v1',0));
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

CREATE TABLE IF NOT EXISTS catalog_ddex_document_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id),
  document_id bigint NOT NULL,
  original_family text, original_version text, original_message_type text, original_status text,
  original_standard_version_id uuid, original_message_type_id uuid, original_workflow_state_id uuid,
  target_standard_version_id uuid NOT NULL, target_message_type_id uuid, target_workflow_state_id uuid NOT NULL,
  normalized_family text NOT NULL, normalized_version text NOT NULL, normalized_state text NOT NULL,
  evidence text NOT NULL, captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id,document_id)
);
CREATE TABLE IF NOT EXISTS catalog_ddex_export_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id), export_id bigint NOT NULL,
  original_ern_version text, original_standard_version_id uuid,
  target_standard_version_id uuid NOT NULL, evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(), PRIMARY KEY (run_id,export_id)
);
CREATE TABLE IF NOT EXISTS catalog_ddex_partner_cutover_source (
  run_id uuid NOT NULL REFERENCES catalog_backfill_run(id), partner_id bigint NOT NULL,
  legacy_version text NOT NULL, legacy_ordinality bigint NOT NULL,
  target_standard_version_id uuid NOT NULL, original_membership_active boolean,
  original_membership_sort_order bigint, evidence text NOT NULL,
  captured_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (run_id,partner_id,legacy_ordinality)
);

DO $$ DECLARE table_name text; BEGIN FOREACH table_name IN ARRAY ARRAY[
  'catalog_ddex_document_cutover_source','catalog_ddex_export_cutover_source',
  'catalog_ddex_partner_cutover_source'] LOOP
  EXECUTE format('DROP TRIGGER IF EXISTS catalog_no_hard_delete ON %I',table_name);
  EXECUTE format('CREATE TRIGGER catalog_no_hard_delete BEFORE DELETE ON %I FOR EACH ROW EXECUTE FUNCTION catalog_prevent_hard_delete()',table_name);
END LOOP; END $$;

CREATE TEMP TABLE resolved_ddex_document ON COMMIT DROP AS
WITH source AS (
  SELECT document.*,
    upper(regexp_replace(btrim(COALESCE(document.family,'')),'^Family','','i')) AS family_code,
    CASE regexp_replace(lower(btrim(COALESCE(document.version,''))),'[^0-9]','','g')
      WHEN '432' THEN '4.3.2' WHEN '21' THEN '2.1' WHEN '11' THEN '1.1'
      WHEN '14' THEN '1.4' ELSE btrim(document.version) END AS version_code,
    CASE lower(regexp_replace(btrim(COALESCE(document.status,'')),'[^a-zA-Z0-9]+','','g'))
      WHEN 'statusreceived' THEN 'received' WHEN 'received' THEN 'received'
      WHEN 'statusquarantined' THEN 'quarantined' WHEN 'quarantined' THEN 'quarantined'
      WHEN 'statusqueued' THEN 'queued' WHEN 'queued' THEN 'queued'
      WHEN 'statusvalidating' THEN 'validating' WHEN 'validating' THEN 'validating'
      WHEN 'statusinvalid' THEN 'invalid' WHEN 'invalid' THEN 'invalid'
      WHEN 'statusvalid' THEN 'valid' WHEN 'valid' THEN 'valid'
      WHEN 'statusmappingrequired' THEN 'mapping_required' WHEN 'mappingrequired' THEN 'mapping_required'
      WHEN 'statusreadytoimport' THEN 'ready_to_import' WHEN 'readytoimport' THEN 'ready_to_import'
      WHEN 'statusimporting' THEN 'importing' WHEN 'importing' THEN 'importing'
      WHEN 'statusimported' THEN 'imported' WHEN 'imported' THEN 'imported'
      WHEN 'statusimportfailed' THEN 'import_failed' WHEN 'importfailed' THEN 'import_failed'
      WHEN 'statussuperseded' THEN 'superseded' WHEN 'superseded' THEN 'superseded'
      ELSE NULL END AS state_code
  FROM ddex_document document
  WHERE document.family IS NOT NULL OR document.version IS NOT NULL
    OR document.message_type IS NOT NULL OR document.status IS NOT NULL
    OR document.standard_version_id IS NULL OR document.workflow_state_id IS NULL
)
SELECT source.*, standard_match.candidate_count AS standard_candidates,
  standard_match.target_id AS target_standard_version_id,
  message_match.candidate_count AS message_candidates,
  message_match.target_id AS target_message_type_id,
  state_match.candidate_count AS state_candidates,
  state_match.target_id AS target_workflow_state_id
FROM source
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,(array_agg(standard.id ORDER BY standard.id))[1] AS target_id
  FROM ddex_standard_version standard JOIN ddex_standard_support support
    ON support.standard_version_id=standard.id AND support.deployment_code='default'
    AND support.active AND support.detection_enabled
  WHERE standard.active AND ((source.standard_version_id IS NOT NULL AND standard.id=source.standard_version_id)
    OR (source.standard_version_id IS NULL AND standard.standard_code=source.family_code
      AND standard.version_code=source.version_code))
) standard_match ON TRUE
LEFT JOIN LATERAL (
  SELECT CASE WHEN source.message_type IS NULL AND source.message_type_id IS NULL THEN 0 ELSE count(*) END AS candidate_count,
    (array_agg(message.id ORDER BY message.id))[1] AS target_id
  FROM ddex_message_type message WHERE message.active AND message.runtime_supported
    AND message.standard_version_id=standard_match.target_id
    AND ((source.message_type_id IS NOT NULL AND message.id=source.message_type_id)
      OR (source.message_type_id IS NULL AND lower(regexp_replace(message.code,'[^a-zA-Z0-9]+','','g'))
        =lower(regexp_replace(COALESCE(source.message_type,''),'[^a-zA-Z0-9]+','','g'))))
) message_match ON TRUE
LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,(array_agg(state.id ORDER BY state.id))[1] AS target_id
  FROM workflow_state state JOIN workflow_definition workflow ON workflow.id=state.workflow_id
  WHERE workflow.code='ddex-document-lifecycle' AND workflow.active AND state.active
    AND ((source.workflow_state_id IS NOT NULL AND state.id=source.workflow_state_id)
      OR (source.workflow_state_id IS NULL AND state.code=source.state_code))
) state_match ON TRUE;

CREATE TEMP TABLE resolved_ddex_export ON COMMIT DROP AS
WITH source AS (
  SELECT export.*,CASE regexp_replace(lower(btrim(COALESCE(export.ern_version,''))),'[^0-9]','','g')
    WHEN '432' THEN '4.3.2' ELSE btrim(export.ern_version) END AS version_code
  FROM ddex_export export WHERE export.ern_version IS NOT NULL OR export.standard_version_id IS NULL
)
SELECT source.*,match.candidate_count,match.target_id AS target_standard_version_id
FROM source LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,(array_agg(standard.id ORDER BY standard.id))[1] AS target_id
  FROM ddex_standard_version standard JOIN ddex_standard_support support
    ON support.standard_version_id=standard.id AND support.deployment_code='default'
    AND support.active AND support.export_enabled
  WHERE standard.active AND standard.standard_code='ERN' AND
    ((source.standard_version_id IS NOT NULL AND standard.id=source.standard_version_id)
      OR (source.standard_version_id IS NULL AND standard.version_code=source.version_code))
) match ON TRUE;

CREATE TEMP TABLE resolved_ddex_partner ON COMMIT DROP AS
WITH source AS (
  SELECT partner.id AS partner_id,value.legacy_version,value.ordinality
  FROM ddex_partner partner CROSS JOIN LATERAL jsonb_array_elements_text(
    COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb)
  ) WITH ORDINALITY value(legacy_version,ordinality)
)
SELECT source.*,match.candidate_count,match.target_id AS target_standard_version_id,
  membership.active AS original_membership_active,
  membership.sort_order AS original_membership_sort_order
FROM source LEFT JOIN LATERAL (
  SELECT count(*) AS candidate_count,(array_agg(standard.id ORDER BY standard.id))[1] AS target_id
  FROM ddex_standard_version standard JOIN ddex_standard_support support
    ON support.standard_version_id=standard.id AND support.deployment_code='default'
    AND support.active AND support.detection_enabled
  WHERE standard.active AND regexp_replace(lower(standard.version_code),'[^0-9]','','g')
    =regexp_replace(lower(source.legacy_version),'[^0-9]','','g')
) match ON TRUE
LEFT JOIN ddex_partner_standard_version membership ON membership.partner_id=source.partner_id
  AND membership.standard_version_id=match.target_id;

DO $gate$
DECLARE source_rows bigint; invalid_rows bigint; active_partner_without_policy bigint;
BEGIN
  SELECT (SELECT count(*) FROM resolved_ddex_document)+(SELECT count(*) FROM resolved_ddex_export)
    +(SELECT count(*) FROM resolved_ddex_partner) INTO source_rows;
  SELECT (SELECT count(*) FROM resolved_ddex_document WHERE standard_candidates<>1 OR state_candidates<>1
      OR ((message_type IS NOT NULL OR message_type_id IS NOT NULL) AND message_candidates<>1)
      OR (standard_version_id IS NOT NULL AND standard_version_id<>target_standard_version_id)
      OR (message_type_id IS NOT NULL AND message_type_id<>target_message_type_id)
      OR (workflow_state_id IS NOT NULL AND workflow_state_id<>target_workflow_state_id))
    +(SELECT count(*) FROM resolved_ddex_export export WHERE candidate_count<>1
      OR (standard_version_id IS NOT NULL AND standard_version_id<>target_standard_version_id)
      OR (NOT EXISTS (SELECT 1 FROM ddex_partner_standard_version membership
          JOIN ddex_partner partner ON partner.id=membership.partner_id AND partner.is_active
          WHERE membership.partner_id=export.partner_id
            AND membership.standard_version_id=export.target_standard_version_id AND membership.active)
        AND NOT EXISTS (SELECT 1 FROM resolved_ddex_partner partner_source
          JOIN ddex_partner partner ON partner.id=partner_source.partner_id AND partner.is_active
          WHERE partner_source.partner_id=export.partner_id
            AND partner_source.target_standard_version_id=export.target_standard_version_id
            AND partner_source.candidate_count=1)))
    +(SELECT count(*) FROM resolved_ddex_partner WHERE candidate_count<>1) INTO invalid_rows;
  SELECT count(*) INTO active_partner_without_policy FROM ddex_partner partner WHERE partner.is_active
    AND NOT EXISTS (SELECT 1 FROM resolved_ddex_partner source WHERE source.partner_id=partner.id)
    AND NOT EXISTS (SELECT 1 FROM ddex_partner_standard_version membership
      WHERE membership.partner_id=partner.id AND membership.active);
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000
    OR source_rows>current_setting('tdf.catalog_safety_threshold')::bigint OR invalid_rows<>0
    OR active_partner_without_policy<>0
    OR (SELECT count(*) FROM ddex_standard_version WHERE active)<>4
    OR (SELECT count(*) FROM ddex_message_type WHERE active AND runtime_supported)<>1
    OR (SELECT count(*) FROM workflow_state state JOIN workflow_definition workflow
      ON workflow.id=state.workflow_id WHERE workflow.code='ddex-document-lifecycle'
      AND workflow.active AND state.active)<>12 THEN
    RAISE EXCEPTION 'DDEX cutover safety gate failed: rows=%, invalidOrAmbiguous=%, activePartnersWithoutPolicy=%',
      source_rows,invalid_rows,active_partner_without_policy USING ERRCODE='23514';
  END IF;
END $gate$;

INSERT INTO catalog_ddex_document_cutover_source (
  run_id,document_id,original_family,original_version,original_message_type,original_status,
  original_standard_version_id,original_message_type_id,original_workflow_state_id,
  target_standard_version_id,target_message_type_id,target_workflow_state_id,
  normalized_family,normalized_version,normalized_state,evidence
)
SELECT :'backfill_run_id'::uuid,id,family,version,message_type,status,standard_version_id,
  message_type_id,workflow_state_id,target_standard_version_id,target_message_type_id,
  target_workflow_state_id,family_code,version_code,state_code,
  'unique active governed standard plus exact runtime message and exact persisted lifecycle state'
FROM resolved_ddex_document ON CONFLICT (run_id,document_id) DO NOTHING;

INSERT INTO catalog_ddex_export_cutover_source (
  run_id,export_id,original_ern_version,original_standard_version_id,target_standard_version_id,evidence
)
SELECT :'backfill_run_id'::uuid,id,ern_version,standard_version_id,target_standard_version_id,
  'unique active export-enabled ERN standard version'
FROM resolved_ddex_export ON CONFLICT (run_id,export_id) DO NOTHING;

INSERT INTO catalog_ddex_partner_cutover_source (
  run_id,partner_id,legacy_version,legacy_ordinality,target_standard_version_id,
  original_membership_active,original_membership_sort_order,evidence
)
SELECT :'backfill_run_id'::uuid,partner_id,legacy_version,ordinality,target_standard_version_id,
  original_membership_active,original_membership_sort_order,
  'unique normalized version code among active detection-enabled governed standards'
FROM resolved_ddex_partner ON CONFLICT (run_id,partner_id,legacy_ordinality) DO NOTHING;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,'ddex_document','family+version',source.document_id::text,
  COALESCE(source.original_family,'<missing>') || ':' || COALESCE(source.original_version,'<missing>'),
  source.normalized_family || ':' || source.normalized_version,catalog.id,source.target_standard_version_id,
  'mapped',source.evidence,1,now()
FROM catalog_ddex_document_cutover_source source JOIN catalog_definition catalog
  ON catalog.code='ddex-standard-versions' WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,normalized_value=EXCLUDED.normalized_value,status='mapped',evidence=EXCLUDED.evidence;

INSERT INTO workflow_migration_mapping (
  id,run_id,workflow_id,source_table,source_column,source_record_id,original_value,
  normalized_value,state_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,workflow.id,'ddex_document','status',source.document_id::text,
  COALESCE(source.original_status,'<missing>'),source.normalized_state,source.target_workflow_state_id,
  'mapped',source.evidence,1,now()
FROM catalog_ddex_document_cutover_source source JOIN workflow_definition workflow
  ON workflow.code='ddex-document-lifecycle' WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET state_id=EXCLUDED.state_id,normalized_value=EXCLUDED.normalized_value,status='mapped',evidence=EXCLUDED.evidence;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,'ddex_export','ern_version',source.export_id::text,
  COALESCE(source.original_ern_version,'<missing>'),'ERN:' || standard.version_code,
  catalog.id,source.target_standard_version_id,'mapped',source.evidence,1,now()
FROM catalog_ddex_export_cutover_source source
JOIN ddex_standard_version standard ON standard.id=source.target_standard_version_id
JOIN catalog_definition catalog ON catalog.code='ddex-standard-versions'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,normalized_value=EXCLUDED.normalized_value,
  status='mapped',evidence=EXCLUDED.evidence;

INSERT INTO catalog_migration_mapping (
  id,run_id,source_table,source_column,source_record_id,original_value,normalized_value,
  catalog_id,entity_id,status,evidence,source_count,created_at
)
SELECT gen_random_uuid(),:'backfill_run_id'::uuid,'ddex_partner','allowed_versions',
  source.partner_id::text || ':' || source.legacy_ordinality::text,source.legacy_version,
  standard.standard_code || ':' || standard.version_code,catalog.id,
  source.target_standard_version_id,'mapped',source.evidence,1,now()
FROM catalog_ddex_partner_cutover_source source
JOIN ddex_standard_version standard ON standard.id=source.target_standard_version_id
JOIN catalog_definition catalog ON catalog.code='ddex-standard-versions'
WHERE source.run_id=:'backfill_run_id'::uuid
ON CONFLICT (run_id,source_table,source_column,source_record_id,original_value)
DO UPDATE SET entity_id=EXCLUDED.entity_id,normalized_value=EXCLUDED.normalized_value,
  status='mapped',evidence=EXCLUDED.evidence;

DROP TRIGGER IF EXISTS ddex_document_canonical_integrity ON ddex_document;
DROP TRIGGER IF EXISTS ddex_partner_standard_integrity ON ddex_partner_standard_version;
DROP TRIGGER IF EXISTS ddex_export_canonical_integrity ON ddex_export;
ALTER TABLE ddex_document ALTER COLUMN family DROP NOT NULL;
ALTER TABLE ddex_document ALTER COLUMN version DROP NOT NULL;
ALTER TABLE ddex_document ALTER COLUMN status DROP NOT NULL;
ALTER TABLE ddex_export ALTER COLUMN ern_version DROP NOT NULL;

DO $batches$ DECLARE changed_rows integer; BEGIN LOOP
  WITH batch AS (
    SELECT target.id FROM ddex_document target JOIN catalog_ddex_document_cutover_source source
      ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.document_id=target.id
    WHERE target.family IS NOT DISTINCT FROM source.original_family
      AND target.version IS NOT DISTINCT FROM source.original_version
      AND target.message_type IS NOT DISTINCT FROM source.original_message_type
      AND target.status IS NOT DISTINCT FROM source.original_status
      AND target.standard_version_id IS NOT DISTINCT FROM source.original_standard_version_id
      AND target.message_type_id IS NOT DISTINCT FROM source.original_message_type_id
      AND target.workflow_state_id IS NOT DISTINCT FROM source.original_workflow_state_id
    ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
  ) UPDATE ddex_document target SET family=NULL,version=NULL,message_type=NULL,status=NULL,
      standard_version_id=source.target_standard_version_id,message_type_id=source.target_message_type_id,
      workflow_state_id=source.target_workflow_state_id
    FROM catalog_ddex_document_cutover_source source,batch WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.document_id=target.id;
  GET DIAGNOSTICS changed_rows=ROW_COUNT; EXIT WHEN changed_rows=0; END LOOP; END $batches$;

INSERT INTO ddex_partner_standard_version (partner_id,standard_version_id,sort_order,active,created_at)
SELECT DISTINCT ON (partner_id,target_standard_version_id)
  partner_id,target_standard_version_id,legacy_ordinality-1,TRUE,now()
FROM catalog_ddex_partner_cutover_source WHERE run_id=:'backfill_run_id'::uuid
ORDER BY partner_id,target_standard_version_id,legacy_ordinality
ON CONFLICT (partner_id,standard_version_id) DO UPDATE SET active=TRUE,
  sort_order=EXCLUDED.sort_order;

DO $clear_partner_legacy$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns
      WHERE table_schema=current_schema() AND table_name='ddex_partner'
        AND column_name='allowed_versions') THEN
    EXECUTE format(
      'UPDATE ddex_partner partner SET allowed_versions=ARRAY[]::text[] WHERE EXISTS '
      || '(SELECT 1 FROM catalog_ddex_partner_cutover_source source '
      || 'WHERE source.run_id=%L::uuid AND source.partner_id=partner.id)',
      current_setting('tdf.catalog_backfill_run_id'));
  END IF;
END $clear_partner_legacy$;

DO $batches$ DECLARE changed_rows integer; BEGIN LOOP
  WITH batch AS (
    SELECT target.id FROM ddex_export target JOIN catalog_ddex_export_cutover_source source
      ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.export_id=target.id
    WHERE target.ern_version IS NOT DISTINCT FROM source.original_ern_version
      AND target.standard_version_id IS NOT DISTINCT FROM source.original_standard_version_id
    ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
  ) UPDATE ddex_export target SET ern_version=NULL,standard_version_id=source.target_standard_version_id
    FROM catalog_ddex_export_cutover_source source,batch WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.export_id=target.id;
  GET DIAGNOSTICS changed_rows=ROW_COUNT; EXIT WHEN changed_rows=0; END LOOP; END $batches$;

CREATE TRIGGER ddex_document_canonical_integrity BEFORE INSERT OR UPDATE ON ddex_document
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_document();
CREATE TRIGGER ddex_partner_standard_integrity BEFORE INSERT OR UPDATE ON ddex_partner_standard_version
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_partner_standard();
CREATE TRIGGER ddex_export_canonical_integrity BEFORE INSERT OR UPDATE ON ddex_export
  FOR EACH ROW EXECUTE FUNCTION catalog_validate_ddex_export();

DO $final_gate$ DECLARE invalid_rows bigint; BEGIN
  SELECT (SELECT count(*) FROM ddex_document document LEFT JOIN ddex_standard_version standard
      ON standard.id=document.standard_version_id LEFT JOIN ddex_standard_support support
      ON support.standard_version_id=standard.id AND support.deployment_code='default' AND support.active
      LEFT JOIN workflow_state state ON state.id=document.workflow_state_id LEFT JOIN workflow_definition workflow
      ON workflow.id=state.workflow_id WHERE document.standard_version_id IS NULL OR document.workflow_state_id IS NULL
      OR document.family IS NOT NULL OR document.version IS NOT NULL OR document.message_type IS NOT NULL
      OR document.status IS NOT NULL OR standard.active IS DISTINCT FROM TRUE
      OR support.detection_enabled IS DISTINCT FROM TRUE OR workflow.code IS DISTINCT FROM 'ddex-document-lifecycle')
    +(SELECT count(*) FROM ddex_export export LEFT JOIN ddex_standard_version standard
      ON standard.id=export.standard_version_id LEFT JOIN ddex_standard_support support
      ON support.standard_version_id=standard.id AND support.deployment_code='default' AND support.active
      LEFT JOIN ddex_partner_standard_version membership ON membership.partner_id=export.partner_id
        AND membership.standard_version_id=standard.id AND membership.active
      LEFT JOIN ddex_partner partner ON partner.id=membership.partner_id
      WHERE export.standard_version_id IS NULL OR export.ern_version IS NOT NULL
        OR standard.active IS DISTINCT FROM TRUE OR support.export_enabled IS DISTINCT FROM TRUE
        OR membership.id IS NULL OR partner.is_active IS DISTINCT FROM TRUE)
    +(SELECT count(*) FROM ddex_partner partner WHERE partner.is_active AND NOT EXISTS
      (SELECT 1 FROM ddex_partner_standard_version membership WHERE membership.partner_id=partner.id AND membership.active))
    +(SELECT count(*) FROM ddex_partner partner
      WHERE jsonb_array_length(COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb))<>0)
    INTO invalid_rows;
  IF invalid_rows<>0 THEN RAISE EXCEPTION 'canonical DDEX final gate failed: invalidRows=%',invalid_rows USING ERRCODE='23514'; END IF;
END $final_gate$;

UPDATE catalog_backfill_run SET status='completed',completed_at=now(),
  scanned_rows=(SELECT count(*) FROM catalog_ddex_document_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    +(SELECT count(*) FROM catalog_ddex_export_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    +(SELECT count(*) FROM catalog_ddex_partner_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  mapped_rows=(SELECT count(*) FROM catalog_ddex_document_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    +(SELECT count(*) FROM catalog_ddex_export_cutover_source WHERE run_id=:'backfill_run_id'::uuid)
    +(SELECT count(*) FROM catalog_ddex_partner_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
  rejected_rows=0,ambiguous_rows=0,
  report=jsonb_build_object(
    'documents',(SELECT count(*) FROM catalog_ddex_document_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'exports',(SELECT count(*) FROM catalog_ddex_export_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'partnerPolicies',(SELECT count(*) FROM catalog_ddex_partner_cutover_source WHERE run_id=:'backfill_run_id'::uuid),
    'legacyColumnsCleared',TRUE,'unresolved',0,'ambiguousOrWithheld',0,
    'batchSize',current_setting('tdf.catalog_batch_size')::integer
  )::text WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,
  'scanned',scanned_rows,'mapped',mapped_rows,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
