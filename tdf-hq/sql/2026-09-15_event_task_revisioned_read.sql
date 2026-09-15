BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

-- Opt-in representation of the existing task. Actor identity is server supplied.
CREATE OR REPLACE FUNCTION event_operation_read_task_with_revision(
  target_event_id BIGINT, target_activity_id BIGINT, target_actor_party_id BIGINT
)
RETURNS JSONB
LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path = public, pg_temp
AS $$
DECLARE
  aggregate_revision BIGINT;
  task_snapshot JSONB;
BEGIN
  PERFORM 1 FROM event_operation_feature_flag
    WHERE feature_code = 'event.operations.api' AND enabled FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  PERFORM 1 FROM event_operation_event_state WHERE event_id = target_event_id FOR SHARE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  IF NOT event_operation_actor_can_read_task(
    target_event_id, target_activity_id, target_actor_party_id, clock_timestamp()
  ) THEN RETURN NULL; END IF;

  -- Every tracked task mutation advances this row before it can commit. A shared
  -- lock keeps the revision and the following canonical projection coherent.
  -- Never acquire the event task-write fence after this lock; never repair on read.
  SELECT metadata.revision INTO aggregate_revision
  FROM event_operation_task_revision metadata
  JOIN event_logistics_activity activity ON activity.id = metadata.activity_id
  WHERE activity.id = target_activity_id AND activity.event_id = target_event_id
  FOR SHARE OF metadata;
  IF NOT FOUND THEN RETURN NULL; END IF;

  -- Reuses the exact existing projection and rechecks scope/time AFTER metadata wait.
  task_snapshot := event_operation_read_task(target_event_id, target_activity_id, target_actor_party_id);
  IF task_snapshot IS NULL THEN RETURN NULL; END IF;
  RETURN jsonb_build_object('task', task_snapshot, 'aggregateRevision', aggregate_revision::TEXT);
END
$$;
COMMIT;
