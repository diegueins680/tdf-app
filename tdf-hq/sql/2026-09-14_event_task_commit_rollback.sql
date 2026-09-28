-- Keep writes quiesced: restoring the old guard restores its documented weaker semantics.
BEGIN;
LOCK TABLE event_logistics_activity, event_logistics_dependency,
  event_operation_task_policy, event_operation_raci_assignment IN SHARE ROW EXCLUSIVE MODE;
DO $$
DECLARE target_table TEXT;
BEGIN
  FOREACH target_table IN ARRAY ARRAY[
    'event_logistics_activity', 'event_logistics_dependency',
    'event_operation_task_policy', 'event_operation_raci_assignment'
  ] LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON %I', target_table);
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_00_task_lock ON %I', target_table);
  END LOOP;
END
$$;
DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
CREATE TRIGGER event_operation_task_completion_guard
  BEFORE UPDATE OF status ON event_logistics_activity
  FOR EACH ROW EXECUTE FUNCTION event_operation_guard_task_completion();
-- Retain fence rows, domain data, overrides and audit history.
COMMIT;
