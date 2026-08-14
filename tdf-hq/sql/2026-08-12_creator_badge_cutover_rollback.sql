\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'creator-badge-cutover-2026-08-12'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL lock_timeout='2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-creator-badge-cutover-v1',0));
SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.creator_badge_rollback_run_id',:'backfill_run_id',TRUE);

DO $rollback_gate$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type_id')
    OR EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='creator_badge' AND column_name='badge_type') THEN
    RAISE EXCEPTION 'creator badge table is not in the canonical rollback shape' USING ERRCODE='23514';
  END IF;
  IF EXISTS (
    SELECT 1 FROM creator_badge badge
    LEFT JOIN catalog_creator_badge_cutover_source source
      ON source.run_id=current_setting('tdf.creator_badge_rollback_run_id')::uuid AND source.creator_badge_id=badge.id
    WHERE source.creator_badge_id IS NULL OR source.target_badge_type_id<>badge.badge_type_id
  ) THEN
    RAISE EXCEPTION 'creator badge rollback withheld because canonical rows are new or have drifted' USING ERRCODE='23514';
  END IF;
END $rollback_gate$;

ALTER TABLE creator_badge ADD COLUMN badge_type text;
UPDATE creator_badge badge SET badge_type=source.original_badge_type
FROM catalog_creator_badge_cutover_source source
WHERE source.run_id=current_setting('tdf.creator_badge_rollback_run_id')::uuid AND source.creator_badge_id=badge.id;
ALTER TABLE creator_badge ALTER COLUMN badge_type SET NOT NULL;
DROP TRIGGER IF EXISTS creator_badge_catalog_integrity ON creator_badge;
ALTER TABLE creator_badge DROP CONSTRAINT IF EXISTS fk_creator_badge_type;
ALTER TABLE creator_badge DROP CONSTRAINT IF EXISTS unique_creator_badge;
ALTER TABLE creator_badge DROP CONSTRAINT IF EXISTS creator_badge_party_id_club_id_badge_type_id_key;
DROP INDEX IF EXISTS uq_creator_badge_identity;
DROP INDEX IF EXISTS ix_creator_badge_type;
ALTER TABLE creator_badge DROP COLUMN badge_type_id;
DO $legacy_check$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM creator_badge WHERE badge_type NOT IN ('trendsetter','regular','og')) THEN
    ALTER TABLE creator_badge ADD CONSTRAINT creator_badge_badge_type_check
      CHECK (badge_type IN ('trendsetter','regular','og'));
  END IF;
END $legacy_check$;
ALTER TABLE creator_badge ADD CONSTRAINT creator_badge_party_id_club_id_badge_type_key
  UNIQUE(party_id,club_id,badge_type);
CREATE INDEX IF NOT EXISTS idx_creator_badge_club ON creator_badge(club_id,badge_type);
UPDATE creator_badge_type SET usage_count=0 WHERE usage_count<>0;
UPDATE catalog_backfill_run SET status='rolled-back',completed_at=now()
WHERE id=:'backfill_run_id'::uuid;
COMMIT;
