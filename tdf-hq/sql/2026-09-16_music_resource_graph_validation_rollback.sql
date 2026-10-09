-- Pause authoring/review before rollback: this removes the early graph gate.
-- All data, immutable snapshots and correction guards are retained.
BEGIN;
LOCK TABLE music_release_version, music_asset IN SHARE ROW EXCLUSIVE MODE;
DROP VIEW music_resource_graph_sanitation_queue;
DROP FUNCTION music_check_submission(UUID);
ALTER FUNCTION music_check_submission_before_resource_graph(UUID) RENAME TO music_check_submission;
DROP FUNCTION music_check_ddex_export(UUID);
ALTER FUNCTION music_check_ddex_export_before_resource_graph(UUID) RENAME TO music_check_ddex_export;
DROP FUNCTION music_refresh_validation_flags(UUID);
ALTER FUNCTION music_refresh_validation_flags_before_resource_graph(UUID) RENAME TO music_refresh_validation_flags;
DROP FUNCTION music_check_resource_graph(UUID);
COMMIT;
