CREATE SCHEMA task_revision_test;
CREATE FUNCTION task_revision_test.check_that(ok BOOLEAN, message TEXT) RETURNS VOID
LANGUAGE plpgsql AS $$ BEGIN
  IF ok IS DISTINCT FROM TRUE THEN RAISE EXCEPTION 'Task revision assertion: %', message; END IF;
END $$;
CREATE FUNCTION task_revision_test.rev(task BIGINT) RETURNS BIGINT LANGUAGE sql AS $$
  SELECT revision FROM event_operation_task_revision WHERE activity_id=task
$$;
CREATE ROLE task_revision_untrusted;
SELECT task_revision_test.check_that(NOT has_function_privilege('task_revision_untrusted',
  'event_operation_lock_task_revision(bigint,bigint,bigint)', 'EXECUTE'), 'no implicit PUBLIC command execution');
SELECT task_revision_test.check_that(
  (SELECT count(*)=5 AND bool_and(revision=1) FROM event_operation_task_revision), 'backfill once');

DO $$ DECLARE before_revision BIGINT; saved_fence JSONB; BEGIN
  before_revision := task_revision_test.rev(100);
  UPDATE event_logistics_activity SET status='in_progress' WHERE id=100;
  UPDATE event_logistics_activity SET status='planned' WHERE id=100;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+2
    AND (SELECT version=1 FROM event_logistics_activity WHERE id=100), 'activity ABA without legacy version advance');
  before_revision := task_revision_test.rev(100);
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (100,1,'accountable',1),(100,2,'responsible',1);
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+2, 'each RACI insert');
  before_revision := task_revision_test.rev(100);
  UPDATE event_operation_raci_assignment SET valid_from=valid_from-interval '1 day' WHERE activity_id=100;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+2, 'RACI windows');
  before_revision := task_revision_test.rev(100);
  INSERT INTO event_operation_task_policy(activity_id) VALUES (100);
  UPDATE event_operation_task_policy SET version=version+1 WHERE activity_id=100;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+2, 'policy insert/update');
  before_revision := task_revision_test.rev(100);
  INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES (100,101);
  UPDATE event_logistics_dependency SET depends_on_activity_id=103 WHERE activity_id=100;
  DELETE FROM event_logistics_dependency WHERE activity_id=100;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+3, 'dependency insert/update/delete');
  before_revision := task_revision_test.rev(100);
  INSERT INTO event_operation_task_override(activity_id,activity_version,override_kind,reason,policy_reference,authorized_by_party_id)
    VALUES (100,1,'responsibility_exception','synthetic test only','test-policy',1);
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision+1, 'override insert');
  PERFORM task_revision_test.check_that(task_revision_test.rev(101)=1 AND task_revision_test.rev(102)=1,
    'same-event and cross-event unrelated tasks unchanged');

  before_revision := task_revision_test.rev(100);
  BEGIN
    DELETE FROM event_operation_raci_assignment WHERE activity_id=100 AND raci_role='responsible';
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'missing Responsible deletion unexpectedly succeeded';
  EXCEPTION WHEN check_violation THEN NULL; END;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision
    AND EXISTS(SELECT 1 FROM event_operation_raci_assignment WHERE activity_id=100 AND raci_role='responsible'),
    'deferred constraint failure restores data and revision');
  BEGIN
    UPDATE event_logistics_activity SET status='cancelled' WHERE id=100;
    RAISE EXCEPTION 'synthetic cancellation' USING ERRCODE='57014';
  EXCEPTION WHEN query_canceled THEN NULL; END;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision, 'subtransaction rollback restores revision');

  saved_fence := (SELECT jsonb_agg(to_jsonb(f) ORDER BY event_id) FROM event_operation_task_write_fence f);
  BEGIN
    PERFORM event_operation_lock_task_revision(10,100,before_revision-1);
    RAISE EXCEPTION 'stale version unexpectedly accepted';
  EXCEPTION WHEN serialization_failure THEN NULL; END;
  PERFORM task_revision_test.check_that(saved_fence=(SELECT jsonb_agg(to_jsonb(f) ORDER BY event_id)
    FROM event_operation_task_write_fence f), 'failed guard rolls back fence write');
  BEGIN
    PERFORM event_operation_lock_task_revision(11,100,before_revision);
    RAISE EXCEPTION 'wrong event unexpectedly accepted';
  EXCEPTION WHEN no_data_found THEN NULL; END;
  BEGIN
    PERFORM event_operation_lock_task_revision(10,999,before_revision);
    RAISE EXCEPTION 'missing task unexpectedly accepted';
  EXCEPTION WHEN no_data_found THEN NULL; END;
  BEGIN
    PERFORM event_operation_lock_task_revision(10,100,NULL);
    RAISE EXCEPTION 'null revision unexpectedly accepted';
  EXCEPTION WHEN invalid_parameter_value THEN NULL; END;
  BEGIN
    PERFORM event_operation_lock_task_revision(10,100,0);
    RAISE EXCEPTION 'zero revision unexpectedly accepted';
  EXCEPTION WHEN invalid_parameter_value THEN NULL; END;
  PERFORM event_operation_lock_task_revision(10,100,before_revision);
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision, 'guard alone does not advance task');

  -- Trusted test setup reaches the BIGINT boundary; ordinary writers never set metadata.
  BEGIN
    UPDATE event_operation_task_revision SET revision=9223372036854775807 WHERE activity_id=100;
    UPDATE event_logistics_activity SET status='cancelled' WHERE id=100;
    RAISE EXCEPTION 'overflow unexpectedly accepted';
  EXCEPTION WHEN numeric_value_out_of_range THEN NULL; END;
  PERFORM task_revision_test.check_that(task_revision_test.rev(100)=before_revision
    AND (SELECT status='planned' FROM event_logistics_activity WHERE id=100), 'overflow rolls back rather than wraps');
END $$;

INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (200,10,'planned',1);
SELECT task_revision_test.check_that(task_revision_test.rev(200)=1, 'new task metadata');
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
  VALUES (200,3,'consulted',1);
DELETE FROM event_operation_raci_assignment WHERE activity_id=200;
SELECT task_revision_test.check_that(task_revision_test.rev(200)=3, 'unprotected RACI removal advances');
DELETE FROM event_logistics_activity WHERE id=200;
SELECT task_revision_test.check_that(task_revision_test.rev(200) IS NULL, 'legacy unprotected deletion remains supported');

-- Public JSON remains unchanged and does not reveal the event-wide counter or new metadata.
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_by_party_id=1,
  change_reason='disposable task revision test' WHERE feature_code='event.operations.api';
SELECT task_revision_test.check_that(NOT (event_operation_read_task(10,100,1) ? 'revision'), 'public read shape unchanged');
UPDATE event_operation_feature_flag SET enabled=FALSE WHERE feature_code='event.operations.api';
CREATE TABLE task_revision_test.preserved AS SELECT
  (SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t) AS revisions;
