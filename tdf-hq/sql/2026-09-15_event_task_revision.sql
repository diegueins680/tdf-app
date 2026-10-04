-- Requires foundation, API, task-commit and task-read migrations. Not a production manifest entry.
-- RV-01–07: docs/event-operations/task-revision-contract.md; checked TaskRevision.tla.
BEGIN;
SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';
LOCK TABLE event_logistics_activity, event_logistics_dependency,
  event_operation_task_policy, event_operation_raci_assignment,
  event_operation_task_override IN SHARE ROW EXCLUSIVE MODE;

CREATE TABLE IF NOT EXISTS event_operation_task_revision (
  activity_id BIGINT PRIMARY KEY REFERENCES event_logistics_activity(id) ON DELETE CASCADE,
  revision BIGINT NOT NULL DEFAULT 1 CHECK (revision > 0)
);
-- Reapply never resets a previously issued revision. Legacy data is not rewritten.
INSERT INTO event_operation_task_revision(activity_id)
  SELECT id FROM event_logistics_activity ON CONFLICT (activity_id) DO NOTHING;

CREATE OR REPLACE FUNCTION event_operation_track_task_revision()
RETURNS trigger LANGUAGE plpgsql SECURITY INVOKER
SET search_path = public, pg_temp AS $$
DECLARE target_activity_id BIGINT;
BEGIN
  IF TG_TABLE_NAME = 'event_logistics_activity' THEN
    target_activity_id := NEW.id;
    IF TG_OP = 'INSERT' THEN
      INSERT INTO event_operation_task_revision(activity_id) VALUES (target_activity_id);
      RETURN NULL;
    END IF;
  ELSE
    target_activity_id := CASE WHEN TG_OP='DELETE' THEN OLD.activity_id ELSE NEW.activity_id END;
    -- Cascading deletion of an unprotected task needs no surviving revision row.
    IF NOT EXISTS (SELECT 1 FROM event_logistics_activity WHERE id=target_activity_id) THEN
      RETURN NULL;
    END IF;
  END IF;
  UPDATE event_operation_task_revision SET revision=revision+1 WHERE activity_id=target_activity_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'task revision unavailable' USING ERRCODE='P0002';
  END IF;
  RETURN NULL;
END $$;

DROP TRIGGER IF EXISTS event_operation_task_revision_track ON event_logistics_activity;
CREATE TRIGGER event_operation_task_revision_track AFTER INSERT OR UPDATE ON event_logistics_activity
  FOR EACH ROW EXECUTE FUNCTION event_operation_track_task_revision();
DO $$ DECLARE target_table TEXT; BEGIN
  FOREACH target_table IN ARRAY ARRAY['event_logistics_dependency',
    'event_operation_task_policy', 'event_operation_raci_assignment', 'event_operation_task_override'] LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS event_operation_task_revision_track ON %I', target_table);
    EXECUTE format('CREATE TRIGGER event_operation_task_revision_track AFTER INSERT OR UPDATE OR DELETE ON %I
      FOR EACH ROW EXECUTE FUNCTION event_operation_track_task_revision()', target_table);
  END LOOP;
END $$;
-- The other four tracked tables already use this BEFORE fence. Overrides are immutable
-- trusted inputs but their insertion must also serialize with aggregate commands.
DROP TRIGGER IF EXISTS event_operation_00_task_lock ON event_operation_task_override;
CREATE TRIGGER event_operation_00_task_lock BEFORE INSERT ON event_operation_task_override
  FOR EACH ROW EXECUTE FUNCTION event_operation_task_write_lock();

CREATE OR REPLACE FUNCTION event_operation_lock_task_revision(
  target_event_id BIGINT, target_activity_id BIGINT, expected_revision BIGINT
) RETURNS VOID LANGUAGE plpgsql VOLATILE SECURITY INVOKER
SET search_path = public, pg_temp AS $$
DECLARE current_revision BIGINT;
BEGIN
  IF expected_revision IS NULL OR expected_revision <= 0 THEN
    RAISE EXCEPTION 'positive task revision required' USING ERRCODE='22023';
  END IF;
  PERFORM 1 FROM event_logistics_activity WHERE id=target_activity_id AND event_id=target_event_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'task revision unavailable' USING ERRCODE='P0002';
  END IF;
  -- Reuse the same WRITE fence as tracked mutations. This also rejects a stale
  -- RR/Serializable snapshot. A plain advisory lock would not provide that property.
  INSERT INTO event_operation_task_write_fence(event_id) VALUES (target_event_id)
    ON CONFLICT (event_id) DO UPDATE SET revision=event_operation_task_write_fence.revision+1;
  -- Separate post-wait statement is essential under READ COMMITTED.
  SELECT metadata.revision INTO current_revision
    FROM event_operation_task_revision metadata
    JOIN event_logistics_activity activity ON activity.id=metadata.activity_id
    WHERE activity.id=target_activity_id AND activity.event_id=target_event_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'task revision unavailable' USING ERRCODE='P0002';
  END IF;
  IF current_revision <> expected_revision THEN
    RAISE EXCEPTION 'task revision conflict' USING ERRCODE='40001';
  END IF;
END $$;
-- No anonymous SQL command surface. Future application grants require explicit review.
REVOKE ALL ON FUNCTION event_operation_lock_task_revision(BIGINT,BIGINT,BIGINT) FROM PUBLIC;
COMMIT;
