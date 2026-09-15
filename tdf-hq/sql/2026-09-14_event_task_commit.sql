-- Requires event_operations_foundation. Deliberately not in the production manifest.
-- Formal contract: docs/event-operations/task-commit-contract.md; TaskCommit.tla.
BEGIN;
LOCK TABLE event_logistics_activity, event_logistics_dependency,
  event_operation_task_policy, event_operation_raci_assignment IN SHARE ROW EXCLUSIVE MODE;

-- A write, not just an advisory lock: stale RR/SERIALIZABLE snapshots must abort.
CREATE TABLE IF NOT EXISTS event_operation_task_write_fence (
  event_id BIGINT PRIMARY KEY REFERENCES social_event(id) ON DELETE CASCADE,
  revision BIGINT NOT NULL DEFAULT 1 CHECK (revision > 0)
);
-- The fence is synchronization metadata, not retained audit history. Upgrade
-- earlier opt-in installations too; existing event/task audit restrictions remain.
ALTER TABLE event_operation_task_write_fence
  DROP CONSTRAINT IF EXISTS event_operation_task_write_fence_event_id_fkey;
ALTER TABLE event_operation_task_write_fence
  ADD CONSTRAINT event_operation_task_write_fence_event_id_fkey
  FOREIGN KEY (event_id) REFERENCES social_event(id) ON DELETE CASCADE;

CREATE OR REPLACE FUNCTION event_operation_task_write_lock()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_event_id BIGINT;
  target_activity_id BIGINT;
BEGIN
  IF TG_TABLE_NAME = 'event_logistics_activity' THEN
    IF TG_OP = 'UPDATE' AND (NEW.id <> OLD.id OR NEW.event_id <> OLD.event_id) THEN
      RAISE EXCEPTION 'task identity and event are immutable' USING ERRCODE = '23514';
    END IF;
    IF TG_OP = 'DELETE' AND EXISTS (
      SELECT 1 FROM event_operation_task_policy WHERE activity_id = OLD.id
    ) THEN
      RAISE EXCEPTION 'protected tasks require an audited archival workflow' USING ERRCODE = '23514';
    END IF;
    target_event_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  ELSE
    IF TG_OP = 'UPDATE' AND NEW.activity_id <> OLD.activity_id THEN
      RAISE EXCEPTION 'task relation identity is immutable; replace explicitly' USING ERRCODE = '23514';
    END IF;
    IF TG_TABLE_NAME = 'event_operation_task_policy' THEN
      IF TG_OP = 'DELETE' THEN
        RAISE EXCEPTION 'task policy removal requires an audited workflow' USING ERRCODE = '23514';
      ELSIF TG_OP = 'UPDATE' THEN
        IF (OLD.requires_accountability AND NOT NEW.requires_accountability)
          OR (OLD.dependencies_gate_completion AND NOT NEW.dependencies_gate_completion) THEN
          RAISE EXCEPTION 'task policy weakening requires an audited workflow' USING ERRCODE = '23514';
        END IF;
      END IF;
    END IF;
    target_activity_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.activity_id ELSE NEW.activity_id END;
    SELECT event_id INTO target_event_id FROM event_logistics_activity WHERE id = target_activity_id;
  END IF;
  IF target_event_id IS NOT NULL THEN
    INSERT INTO event_operation_task_write_fence(event_id) VALUES (target_event_id)
      ON CONFLICT (event_id) DO UPDATE
        SET revision = event_operation_task_write_fence.revision + 1;
  END IF;
  IF TG_OP = 'DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_validate_task_event(target_event_id BIGINT)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE
  invalid_activity_id BIGINT;
  checked_at TIMESTAMPTZ := clock_timestamp();
BEGIN
  SELECT activity.id INTO invalid_activity_id
  FROM event_logistics_activity activity
  JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
  WHERE activity.event_id = target_event_id AND policy.requires_accountability
    AND (
      (SELECT count(*) FROM event_operation_raci_assignment assignment
       WHERE assignment.activity_id = activity.id AND assignment.revoked_at IS NULL
         AND assignment.valid_from <= checked_at
         AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)
         AND assignment.raci_role = 'accountable') <> 1
      OR NOT EXISTS (
        SELECT 1 FROM event_operation_raci_assignment assignment
        WHERE assignment.activity_id = activity.id AND assignment.revoked_at IS NULL
         AND assignment.valid_from <= checked_at
         AND (assignment.valid_until IS NULL OR checked_at < assignment.valid_until)
          AND assignment.raci_role = 'responsible'
      )
    ) LIMIT 1;
  IF FOUND THEN
    RAISE EXCEPTION 'task % requires one Accountable and at least one Responsible', invalid_activity_id
      USING ERRCODE = '23514';
  END IF;

  SELECT activity.id INTO invalid_activity_id
  FROM event_logistics_activity activity
  JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
  WHERE activity.event_id = target_event_id AND activity.status = 'completed'
    AND policy.dependencies_gate_completion
    AND EXISTS (
      SELECT 1 FROM event_logistics_dependency dependency
      JOIN event_logistics_activity prerequisite ON prerequisite.id = dependency.depends_on_activity_id
      WHERE dependency.activity_id = activity.id AND prerequisite.status <> 'completed'
    )
    AND NOT EXISTS (
      SELECT 1 FROM event_operation_task_override override_record
      WHERE override_record.activity_id = activity.id
        AND override_record.activity_version = activity.version - 1
        AND override_record.override_kind = 'blocked_completion'
    ) LIMIT 1;
  IF FOUND THEN
    RAISE EXCEPTION 'completed task % has incomplete dependencies', invalid_activity_id
      USING ERRCODE = '23514';
  END IF;
END
$$;

CREATE OR REPLACE FUNCTION event_operation_task_commit_check()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  target_event_id BIGINT;
  target_activity_id BIGINT;
BEGIN
  IF TG_TABLE_NAME = 'event_logistics_activity' THEN
    target_event_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  ELSE
    target_activity_id := CASE WHEN TG_OP = 'DELETE' THEN OLD.activity_id ELSE NEW.activity_id END;
    SELECT event_id INTO target_event_id FROM event_logistics_activity WHERE id = target_activity_id;
  END IF;
  -- An old completion exception covers its existing graph, not new blocked
  -- edges. Check the final state so valid same-transaction prerequisite completion
  -- still works, and ignore deleted edges or unchanged relation updates.
  IF TG_TABLE_NAME = 'event_logistics_dependency' AND TG_OP <> 'DELETE' THEN
    IF TG_OP = 'INSERT' OR NEW.depends_on_activity_id <> OLD.depends_on_activity_id THEN
      IF EXISTS (
        SELECT 1 FROM event_logistics_dependency dependency
        JOIN event_logistics_activity activity ON activity.id = dependency.activity_id
        JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
        JOIN event_logistics_activity prerequisite
          ON prerequisite.id = dependency.depends_on_activity_id
        WHERE dependency.id = NEW.id AND dependency.activity_id = NEW.activity_id
          AND dependency.depends_on_activity_id = NEW.depends_on_activity_id
          AND activity.status = 'completed' AND policy.dependencies_gate_completion
          AND prerequisite.status <> 'completed'
      ) THEN
        RAISE EXCEPTION 'completed task % cannot acquire an incomplete prerequisite', NEW.activity_id
          USING ERRCODE = '23514';
      END IF;
    END IF;
  END IF;
  PERFORM event_operation_validate_task_event(target_event_id);
  RETURN NULL;
END
$$;

DROP TRIGGER IF EXISTS event_operation_task_completion_guard ON event_logistics_activity;
DO $$
DECLARE target_table TEXT;
BEGIN
  FOREACH target_table IN ARRAY ARRAY[
    'event_logistics_activity', 'event_logistics_dependency',
    'event_operation_task_policy', 'event_operation_raci_assignment'
  ] LOOP
    -- Names sort before the existing DAG guard. Constraint checks fire AFTER actual changes,
    -- never on the fence row, which would be too early with SET CONSTRAINTS ALL IMMEDIATE.
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_00_task_lock ON %I', target_table);
    EXECUTE format('CREATE TRIGGER event_operation_00_task_lock BEFORE INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION event_operation_task_write_lock()', target_table);
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_task_commit_guard ON %I', target_table);
    EXECUTE format('CREATE CONSTRAINT TRIGGER event_operation_task_commit_guard
      AFTER INSERT OR UPDATE OR DELETE ON %I DEFERRABLE INITIALLY DEFERRED
      FOR EACH ROW EXECUTE FUNCTION event_operation_task_commit_check()', target_table);
  END LOOP;
  -- Refuse incompatible opt-in data. No automatic repairs or silent policy downgrades.
  PERFORM event_operation_validate_task_event(activity.event_id)
    FROM event_logistics_activity activity
    JOIN event_operation_task_policy policy ON policy.activity_id = activity.id
    GROUP BY activity.event_id;
END
$$;
COMMIT;
