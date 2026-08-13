\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'content-reaction-cutover-2026-08-12'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout='10min';
SET LOCAL lock_timeout='2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-content-reaction-cutover-v1',0));
SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.content_reaction_backfill_run_id',:'backfill_run_id',TRUE);

DO $rollback_gate$
DECLARE evidence_rows bigint; canonical_rows bigint; drift_rows bigint;
BEGIN
  SELECT count(*) INTO evidence_rows FROM catalog_content_reaction_cutover_source
    WHERE run_id=current_setting('tdf.content_reaction_backfill_run_id')::uuid;
  SELECT (SELECT count(*) FROM fan_club_post_reaction)
    +(SELECT count(*) FROM fan_club_memory_reaction) INTO canonical_rows;
  SELECT count(*) INTO drift_rows
  FROM catalog_content_reaction_cutover_source source
  LEFT JOIN fan_club_post_reaction post_reaction ON source.target_type='post'
    AND post_reaction.post_id=source.target_id AND post_reaction.reactor_party_id=source.reactor_party_id
  LEFT JOIN fan_club_memory_reaction memory_reaction ON source.target_type='memory'
    AND memory_reaction.memory_id=source.target_id AND memory_reaction.reactor_party_id=source.reactor_party_id
  WHERE source.run_id=current_setting('tdf.content_reaction_backfill_run_id')::uuid AND (
    (source.target_type='post' AND post_reaction.reaction_type_id IS DISTINCT FROM source.target_reaction_type_id)
    OR (source.target_type='memory' AND memory_reaction.reaction_type_id IS DISTINCT FROM source.target_reaction_type_id)
  );
  IF canonical_rows<>evidence_rows OR drift_rows<>0 THEN
    RAISE EXCEPTION 'content reaction rollback blocked: evidence=%, canonical=%, drift=%',
      evidence_rows,canonical_rows,drift_rows USING ERRCODE='23514';
  END IF;
END
$rollback_gate$;

DO $drop_source_trigger$
BEGIN
  IF to_regclass('public.catalog_content_reaction_legacy_source') IS NOT NULL THEN
    DROP TRIGGER IF EXISTS catalog_content_reaction_source_immutable ON catalog_content_reaction_legacy_source;
  END IF;
END
$drop_source_trigger$;
DO $restore_source$
BEGIN
  IF to_regclass('public.content_reaction') IS NOT NULL THEN
    RAISE EXCEPTION 'legacy content_reaction already exists; refusing overwrite' USING ERRCODE='23514';
  END IF;
  IF to_regclass('public.catalog_content_reaction_legacy_source') IS NULL THEN
    RAISE EXCEPTION 'preserved content reaction source is missing' USING ERRCODE='23514';
  END IF;
  ALTER TABLE catalog_content_reaction_legacy_source RENAME TO content_reaction;
END
$restore_source$;

DROP TRIGGER IF EXISTS fan_club_post_reaction_catalog_integrity ON fan_club_post_reaction;
DROP TRIGGER IF EXISTS fan_club_memory_reaction_catalog_integrity ON fan_club_memory_reaction;
DROP TRIGGER IF EXISTS catalog_content_reaction_type_reference_protection ON content_reaction_type;
DROP TABLE fan_club_post_reaction;
DROP TABLE fan_club_memory_reaction;
UPDATE content_reaction_type SET usage_count=0 WHERE usage_count<>0;

UPDATE catalog_backfill_run SET status='rolled-back',completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object(
    'rolledBackAt',now(),
    'preservedMappingRows',(SELECT count(*) FROM catalog_migration_mapping
      WHERE run_id=:'backfill_run_id'::uuid AND source_table='content_reaction'),
    'preservedEvidenceRows',(SELECT count(*) FROM catalog_content_reaction_cutover_source
      WHERE run_id=:'backfill_run_id'::uuid)
  ))::text
WHERE id=:'backfill_run_id'::uuid;
COMMIT;
