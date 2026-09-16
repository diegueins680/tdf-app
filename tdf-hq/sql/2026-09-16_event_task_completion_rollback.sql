-- Keep completed work, revisions, immutable audit and idempotency history intact.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP FUNCTION IF EXISTS event_operation_complete_task(BIGINT,BIGINT,BIGINT,UUID,BIGINT,TEXT,TEXT);
COMMIT;
