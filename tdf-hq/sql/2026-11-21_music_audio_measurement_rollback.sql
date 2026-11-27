-- Reverses 2026-11-21_music_audio_measurement. Stop the music worker first.
-- Recorded measurements are removed with the table.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TRIGGER IF EXISTS trg_music_asset_supersede_measurements ON music_asset;
DROP FUNCTION IF EXISTS music_asset_supersede_measurements();
DROP FUNCTION IF EXISTS music_finish_audio_measurement(UUID, UUID, INTEGER, JSONB);
DROP FUNCTION IF EXISTS music_begin_audio_measurement(UUID, TEXT, TEXT, TEXT, UUID, INTEGER);
DROP FUNCTION IF EXISTS music_supersede_audio_measurements(UUID, TEXT);
DROP TABLE IF EXISTS music_audio_measurement;
DROP FUNCTION IF EXISTS music_audio_measurement_guard();
DELETE FROM revenue_feature_flag WHERE flag_key = 'music_releases.loudness_check';
COMMIT;
