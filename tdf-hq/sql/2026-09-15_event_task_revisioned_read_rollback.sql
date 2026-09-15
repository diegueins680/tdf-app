BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';
DROP FUNCTION IF EXISTS event_operation_read_task_with_revision(BIGINT, BIGINT, BIGINT);
-- Old reads, domain rows, immutable history and monotonic revision tracking remain.
COMMIT;
