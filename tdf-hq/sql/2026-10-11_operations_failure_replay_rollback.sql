-- Removes the triggers that make a failure retry re-queue its projection and
-- follow it. Failure and outbox rows are left as they are. Roll the server back
-- first: a server built with this migration refuses retries without them.
-- Safe to apply again.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP INDEX IF EXISTS operations_integration_failure_open_uidx;
DROP TRIGGER IF EXISTS operations_integration_failure_resolved ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_integration_failure_merge ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_integration_failure_retry ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_outbox_settle_failure ON operations_outbox;
DROP FUNCTION IF EXISTS operations_integration_failure_resolved_event();
DROP FUNCTION IF EXISTS operations_integration_failure_merge();
DROP FUNCTION IF EXISTS operations_integration_failure_retry();
DROP FUNCTION IF EXISTS operations_outbox_settle_failure();
DROP FUNCTION IF EXISTS operations_integration_failure_announce(operations_integration_failure, BOOLEAN);
COMMIT;
