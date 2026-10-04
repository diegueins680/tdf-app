BEGIN;
SET LOCAL lock_timeout='10s';
SET LOCAL statement_timeout='10min';
DROP FUNCTION IF EXISTS event_operation_read_raci_editor_context(BIGINT,BIGINT,BIGINT,BIGINT);
COMMIT;
