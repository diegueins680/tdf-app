-- Removes the triggers that keep a failure in step with its projection. Failure
-- and outbox rows are left as they are. Re-appliable.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TRIGGER IF EXISTS operations_integration_failure_resolved ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_integration_failure_merge ON operations_integration_failure;
DROP TRIGGER IF EXISTS operations_outbox_settle_failure ON operations_outbox;
DROP FUNCTION IF EXISTS operations_integration_failure_resolved_event();
DROP FUNCTION IF EXISTS operations_integration_failure_merge();
DROP FUNCTION IF EXISTS operations_outbox_settle_failure();
COMMIT;
