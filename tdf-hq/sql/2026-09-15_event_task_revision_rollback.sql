-- Disable callers first. Retain passive revision tracking to avoid ABA on roll-forward.
-- Removing counters/triggers requires a separate quiesced migration and token invalidation.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP FUNCTION IF EXISTS event_operation_lock_task_revision(BIGINT,BIGINT,BIGINT);
COMMIT;
