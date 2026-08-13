\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'ddex-reference-cutover-2026-08-12'
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
SELECT pg_advisory_xact_lock(hashtextextended('tdf-ddex-reference-cutover-v1',0));
SELECT set_config('tdf.catalog_batch_size',:'batch_size',TRUE);

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id',:'backfill_run_id',TRUE);

DO $gate$ DECLARE unsafe_rows bigint; BEGIN
  IF current_setting('tdf.catalog_batch_size')::integer NOT BETWEEN 1 AND 5000 THEN
    RAISE EXCEPTION 'rollback batch size must be between 1 and 5000' USING ERRCODE='23514';
  END IF;
  SELECT (SELECT count(*) FROM catalog_ddex_document_cutover_source source JOIN ddex_document target
      ON target.id=source.document_id WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND (target.family IS NOT NULL OR target.version IS NOT NULL OR target.message_type IS NOT NULL
        OR target.status IS NOT NULL OR target.standard_version_id IS DISTINCT FROM source.target_standard_version_id
        OR target.message_type_id IS DISTINCT FROM source.target_message_type_id
        OR target.workflow_state_id IS DISTINCT FROM source.target_workflow_state_id))
    +(SELECT count(*) FROM catalog_ddex_export_cutover_source source JOIN ddex_export target
      ON target.id=source.export_id WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND (target.ern_version IS NOT NULL OR target.standard_version_id IS DISTINCT FROM source.target_standard_version_id))
    +(SELECT count(*) FROM (
        SELECT partner_id,target_standard_version_id,min(legacy_ordinality)-1 AS target_sort_order
        FROM catalog_ddex_partner_cutover_source
        WHERE run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        GROUP BY partner_id,target_standard_version_id
      ) source LEFT JOIN ddex_partner_standard_version membership
        ON membership.partner_id=source.partner_id
        AND membership.standard_version_id=source.target_standard_version_id
      WHERE membership.id IS NULL OR membership.active IS DISTINCT FROM TRUE
        OR membership.sort_order IS DISTINCT FROM source.target_sort_order)
    +(SELECT count(DISTINCT source.partner_id) FROM catalog_ddex_partner_cutover_source source
      JOIN ddex_partner partner ON partner.id=source.partner_id
      WHERE source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
        AND jsonb_array_length(COALESCE(to_jsonb(partner)->'allowed_versions','[]'::jsonb))<>0)
    INTO unsafe_rows;
  IF unsafe_rows<>0 THEN RAISE EXCEPTION 'DDEX rollback refused because % rows changed after cutover',unsafe_rows USING ERRCODE='23514'; END IF;
END $gate$;

DROP TRIGGER IF EXISTS ddex_document_canonical_integrity ON ddex_document;
DROP TRIGGER IF EXISTS ddex_partner_standard_integrity ON ddex_partner_standard_version;
DROP TRIGGER IF EXISTS ddex_export_canonical_integrity ON ddex_export;

DO $batches$ DECLARE changed_rows integer; BEGIN LOOP
  WITH batch AS (
    SELECT target.id FROM ddex_document target JOIN catalog_ddex_document_cutover_source source
      ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.document_id=target.id
    WHERE target.family IS NULL AND target.version IS NULL AND target.message_type IS NULL AND target.status IS NULL
      AND target.standard_version_id=source.target_standard_version_id
      AND target.message_type_id IS NOT DISTINCT FROM source.target_message_type_id
      AND target.workflow_state_id=source.target_workflow_state_id
    ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
  ) UPDATE ddex_document target SET family=source.original_family,version=source.original_version,
      message_type=source.original_message_type,status=source.original_status,
      standard_version_id=source.original_standard_version_id,message_type_id=source.original_message_type_id,
      workflow_state_id=source.original_workflow_state_id
    FROM catalog_ddex_document_cutover_source source,batch WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.document_id=target.id;
  GET DIAGNOSTICS changed_rows=ROW_COUNT; EXIT WHEN changed_rows=0; END LOOP; END $batches$;

DO $batches$ DECLARE changed_rows integer; BEGIN LOOP
  WITH batch AS (
    SELECT target.id FROM ddex_export target JOIN catalog_ddex_export_cutover_source source
      ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.export_id=target.id
    WHERE target.ern_version IS NULL AND target.standard_version_id=source.target_standard_version_id
    ORDER BY target.id LIMIT current_setting('tdf.catalog_batch_size')::integer FOR UPDATE OF target SKIP LOCKED
  ) UPDATE ddex_export target SET ern_version=source.original_ern_version,
      standard_version_id=source.original_standard_version_id
    FROM catalog_ddex_export_cutover_source source,batch WHERE target.id=batch.id
      AND source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid AND source.export_id=target.id;
  GET DIAGNOSTICS changed_rows=ROW_COUNT; EXIT WHEN changed_rows=0; END LOOP; END $batches$;

UPDATE ddex_partner_standard_version membership SET
  active=COALESCE(source.original_membership_active,FALSE),
  sort_order=COALESCE(source.original_membership_sort_order,membership.sort_order)
FROM (
  SELECT partner_id,target_standard_version_id,
    bool_or(original_membership_active) FILTER (WHERE original_membership_active IS NOT NULL)
      AS original_membership_active,
    min(original_membership_sort_order) AS original_membership_sort_order
  FROM catalog_ddex_partner_cutover_source
  WHERE run_id=:'backfill_run_id'::uuid
  GROUP BY partner_id,target_standard_version_id
) source
WHERE membership.partner_id=source.partner_id
  AND membership.standard_version_id=source.target_standard_version_id;

DO $restore_partner_legacy$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns
      WHERE table_schema=current_schema() AND table_name='ddex_partner'
        AND column_name='allowed_versions') THEN
    EXECUTE format(
      'UPDATE ddex_partner partner SET allowed_versions=source.legacy_versions '
      || 'FROM (SELECT partner_id,array_agg(legacy_version ORDER BY legacy_ordinality) AS legacy_versions '
      || 'FROM catalog_ddex_partner_cutover_source WHERE run_id=%L::uuid GROUP BY partner_id) source '
      || 'WHERE partner.id=source.partner_id',
      current_setting('tdf.catalog_backfill_run_id'));
  END IF;
END $restore_partner_legacy$;

UPDATE catalog_backfill_run SET status='rolled-back',completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object(
    'rolledBackAt',now(),'rollbackBatchSize',current_setting('tdf.catalog_batch_size')::integer,
    'canonicalDDEXGuardsDisabledForLegacyRelease',TRUE
  ))::text WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId',id,'runCode',run_code,'status',status,'report',report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
