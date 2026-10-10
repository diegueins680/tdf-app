BEGIN;
-- Pause DDEX and restore compatible API/renderer/worker BEFORE this rollback.
-- Existing exports/assets and all audit evidence are retained.
DROP FUNCTION IF EXISTS music_check_ddex_operation(UUID,UUID,UUID,TEXT);
DROP FUNCTION IF EXISTS music_ddex_release_identifier(UUID);
COMMIT;
