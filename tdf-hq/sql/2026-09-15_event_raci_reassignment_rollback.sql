BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';
DROP FUNCTION IF EXISTS event_operation_reassign_raci(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,BIGINT,BIGINT,TEXT,TEXT);
DROP FUNCTION IF EXISTS event_operation_actor_can_manage_task(BIGINT,BIGINT,BIGINT,TIMESTAMPTZ);
-- Preserve all accepted receipts, audit, old/new assignments and aggregate counters.
COMMIT;
