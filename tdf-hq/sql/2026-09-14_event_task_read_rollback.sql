BEGIN;
SET LOCAL lock_timeout = '10s';
DROP FUNCTION IF EXISTS event_operation_read_task(BIGINT, BIGINT, BIGINT);
DROP FUNCTION IF EXISTS event_operation_actor_can_read_task(BIGINT, BIGINT, BIGINT, TIMESTAMPTZ);
-- No domain, permission, version, audit or write-fence record is removed.
COMMIT;
