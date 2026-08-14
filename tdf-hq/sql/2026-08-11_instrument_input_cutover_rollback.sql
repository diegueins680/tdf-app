\set ON_ERROR_STOP on
\if :{?run_code}
\else
  \set run_code 'instrument-input-cutover-2026-08-11'
\endif
\if :{?candidate_revision}
\else
  \set candidate_revision 'UNSET-REQUIRES-RELEASE-SHA'
\endif

BEGIN;
SET LOCAL statement_timeout = '10min';
SET LOCAL lock_timeout = '2s';
SELECT pg_advisory_xact_lock(hashtextextended('tdf-instrument-input-cutover-v1', 0));

SELECT id AS backfill_run_id FROM catalog_backfill_run
WHERE run_code=:'run_code' AND candidate_revision=:'candidate_revision' AND NOT dry_run
\gset

DROP TRIGGER IF EXISTS input_row_instrument_integrity ON input_row;
DROP TRIGGER IF EXISTS live_session_musician_instrument_integrity ON live_session_musician;
DROP TRIGGER IF EXISTS live_session_intake_primary_genre_integrity ON live_session_intake;

UPDATE input_row target SET
  instrument=source.original_instrument,
  instrument_id=source.original_instrument_id,
  mic_id=source.original_mic_id
FROM catalog_input_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.input_row_id=target.id
  AND target.instrument_id=source.target_instrument_id AND target.mic_id=source.target_mic_id
  AND target.instrument IS NULL;

UPDATE live_session_intake target SET
  primary_genre=source.original_value,
  primary_genre_id=source.original_entity_id
FROM catalog_live_session_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.source_table='live_session_intake'
  AND source.source_id=target.id AND target.primary_genre_id=source.target_entity_id
  AND target.primary_genre IS NULL;

UPDATE live_session_musician target SET
  instrument=source.original_value,
  role=source.original_role,
  instrument_id=source.original_entity_id
FROM catalog_live_session_reference_cutover_source source
WHERE source.run_id=:'backfill_run_id'::uuid AND source.source_table='live_session_musician'
  AND source.source_id=target.id AND target.instrument_id=source.target_entity_id
  AND target.instrument IS NULL AND target.role IS NULL;

UPDATE catalog_backfill_run SET status='rolled-back', completed_at=now(),
  report=(COALESCE(NULLIF(report,''),'{}')::jsonb || jsonb_build_object('rolledBackAt', now()))::text
WHERE id=:'backfill_run_id'::uuid;

SELECT jsonb_build_object('runId', id, 'runCode', run_code, 'status', status, 'report', report)
FROM catalog_backfill_run WHERE id=:'backfill_run_id'::uuid;

COMMIT;
