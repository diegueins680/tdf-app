\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'event-moment-reaction-cutover-2026-08-12'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-event-moment-reaction-cutover-v1', 0));

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset
SELECT set_config('tdf.catalog_backfill_run_id', :'backfill_run_id', TRUE);

DO $gate$
DECLARE missing_evidence bigint;
  drifted_rows bigint;
BEGIN
  SELECT count(*) INTO missing_evidence
  FROM event_moment_reaction reaction
  LEFT JOIN catalog_event_moment_reaction_cutover_source source
    ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.reaction_id=reaction.id
  WHERE source.reaction_id IS NULL;
  IF missing_evidence<>0 THEN
    RAISE EXCEPTION 'rollback refused: % event-moment reactions were written after cutover or lack source evidence',
      missing_evidence USING ERRCODE='23514';
  END IF;
  SELECT count(*) INTO drifted_rows
  FROM event_moment_reaction reaction
  JOIN catalog_event_moment_reaction_cutover_source source
    ON source.run_id=current_setting('tdf.catalog_backfill_run_id')::uuid
      AND source.reaction_id=reaction.id
  WHERE reaction.reaction IS NOT NULL OR reaction.reaction_type_id IS DISTINCT FROM source.target_reaction_type_id;
  IF drifted_rows<>0 THEN
    RAISE EXCEPTION 'rollback refused: % event-moment reactions changed after cutover', drifted_rows USING ERRCODE='23514';
  END IF;
END
$gate$;

DROP TRIGGER IF EXISTS event_moment_reaction_catalog_integrity ON event_moment_reaction;
DROP TRIGGER IF EXISTS catalog_reaction_type_reference_protection ON reaction_type;
ALTER TABLE event_moment_reaction ALTER COLUMN reaction_type_id DROP NOT NULL;

UPDATE event_moment_reaction target SET
  reaction=source.original_reaction,
  reaction_type_id=source.original_reaction_type_id
FROM catalog_event_moment_reaction_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.reaction_id=target.id
  AND target.reaction_type_id=source.target_reaction_type_id AND target.reaction IS NULL;

DROP INDEX IF EXISTS uq_event_moment_reaction_identity;
ALTER TABLE event_moment_reaction DROP CONSTRAINT IF EXISTS event_moment_reaction_pkey;
ALTER TABLE event_moment_reaction ADD CONSTRAINT event_moment_reaction_pkey
  PRIMARY KEY (moment_id, reaction, reactor_party_id);
ALTER TABLE event_moment_reaction ALTER COLUMN id DROP NOT NULL;

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=COALESCE(report::jsonb, '{}'::jsonb) || jsonb_build_object('rolledBackAt', now())
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status,
  'restoredRows', (SELECT count(*) FROM catalog_event_moment_reaction_cutover_source WHERE run_id=id))
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
