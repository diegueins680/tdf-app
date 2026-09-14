BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Application rollback is deliberately non-destructive. Older application versions do not read
-- these additive relations. Remove enforcement from legacy logistics tables, but retain ownership,
-- invitation, revision, command, transition, override, and audit history for a safe roll-forward.
DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
DROP TRIGGER IF EXISTS event_operation_dependency_cycle_guard ON event_logistics_dependency;

COMMIT;
