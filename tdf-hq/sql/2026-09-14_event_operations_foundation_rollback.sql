BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Application rollback is deliberately non-destructive. Older application versions do not read
-- these additive relations. Remove enforcement from legacy logistics tables, but retain ownership,
-- invitation, revision, command, transition, override, and audit history for a safe roll-forward.
DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
DROP TRIGGER IF EXISTS event_operation_dependency_cycle_guard ON event_logistics_dependency;

-- Preserve fence revisions and RACI retirement history across rollback.
DROP TRIGGER IF EXISTS event_operation_00_task_lock ON event_logistics_activity;
DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON event_logistics_activity;
DROP TRIGGER IF EXISTS event_operation_00_task_lock ON event_logistics_dependency;
DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON event_logistics_dependency;
DROP TRIGGER IF EXISTS event_operation_00_task_lock ON event_operation_task_policy;
DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON event_operation_task_policy;
DROP TRIGGER IF EXISTS event_operation_00_task_lock ON event_operation_raci_assignment;
DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON event_operation_raci_assignment;
DROP FUNCTION IF EXISTS event_operation_retire_expired_raci(BIGINT, BIGINT, TEXT);

COMMIT;
