-- Preserve the current foundation's task integrity boundary during application rollback.
-- The foundation now owns these fences/constraints; removing them would reintroduce
-- stale graph approvals and unaudited task deletion. Quiesce task writers before
-- reverting consumers. Domain data, overrides and audit history remain intact.
BEGIN;
LOCK TABLE event_logistics_activity, event_logistics_dependency,
  event_operation_task_policy, event_operation_raci_assignment IN SHARE ROW EXCLUSIVE MODE;
-- No schema or policy downgrade: a subsequent forward application is idempotent.
COMMIT;
