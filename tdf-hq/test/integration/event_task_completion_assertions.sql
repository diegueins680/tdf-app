DO $$ DECLARE result JSONB; before_rows JSONB; BEGIN
  UPDATE event_operation_feature_flag SET enabled=FALSE WHERE feature_code='event.operations.api';
  PERFORM raci_command_test.check_that(completion_test.command(400,1,1,4)
    ='{"error":"feature_disabled"}'::JSONB, 'feature disabled');
  UPDATE event_operation_feature_flag SET enabled=TRUE WHERE feature_code='event.operations.api';
  result := completion_test.command(400,1,1,4);
  PERFORM raci_command_test.check_that(result=jsonb_build_object('eventId',10,'activityId',400,
    'commandId',raci_command_test.key(1),'status','completed','activityVersion',2,
    'aggregateRevision','5','replayed',FALSE),'exact successful completion receipt');
  PERFORM raci_command_test.check_that((SELECT status='completed' AND version=2 AND updated_at IS NOT NULL
    FROM event_logistics_activity WHERE id=400),'canonical status/version/time updated');
  PERFORM raci_command_test.check_that((SELECT count(*)=1 FROM event_operation_audit_event
    WHERE operation_code='event.task.complete' AND resource_id='400' AND reason='synthetic completion'
      AND before_state='{"status":"planned","activityVersion":1,"aggregateRevision":"4"}'::JSONB
      AND after_state='{"status":"completed","activityVersion":2,"aggregateRevision":"5"}'::JSONB),
    'one exact material-change audit');
  before_rows := completion_test.rows();
  PERFORM raci_command_test.check_that(completion_test.command(400,1,1,4)=result||'{"replayed":true}'::JSONB,
    'exact retry precedes revision and completed-state checks');
  PERFORM raci_command_test.check_that(completion_test.command(400,1,1,4,'changed')->>'error'='idempotency_conflict',
    'changed reason cannot reuse key');
  PERFORM raci_command_test.check_that(completion_test.command(400,1,1,4,'synthetic completion','changed')->>'error'
    ='idempotency_conflict','correlation is bound');
  PERFORM raci_command_test.check_that(completion_test.command(400,1,1,5)->>'error'='idempotency_conflict',
    'expected revision is bound');
  PERFORM raci_command_test.check_that(completion_test.command(400,2,1,4)->>'error'='idempotency_conflict',
    'actor is bound');
  PERFORM raci_command_test.check_that(completion_test.command(400,4,1,4)='{"error":"not_found"}'::JSONB,
    'outsider cannot observe stored receipt');
  PERFORM raci_command_test.check_that(completion_test.command(400,1,2,5)->>'error'='operation_not_ready',
    'a completed task cannot be completed again with a new command');
  PERFORM raci_command_test.check_that(completion_test.rows()=before_rows,'retries/rejections change no business rows');
END $$;

SELECT raci_command_test.check_that(completion_test.command(401,2,1,4)->>'replayed'='false',
  'same key is independent on a sibling task');
BEGIN;
UPDATE event_operation_grant SET scope_code='task.read' WHERE resource_id='401' AND grantee_party_id=2;
SELECT raci_command_test.check_that(completion_test.command(401,2,1,4)->>'replayed'='true',
  'historical replay needs read, not current manage authority');
SELECT raci_command_test.check_that(completion_test.command(401,2,2,5)->>'error'='forbidden','no read-only mutation');
UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='test'
  WHERE resource_id='401' AND grantee_party_id=2;
SELECT raci_command_test.check_that(completion_test.command(401,2,1,4)='{"error":"not_found"}'::JSONB,
  'revocation hides even a historical receipt');
ROLLBACK;

DO $$ DECLARE snapshot JSONB := completion_test.rows(); ident BIGINT; reason TEXT; BEGIN
  FOREACH ident IN ARRAY ARRAY[NULL,0,-1,9007199254740992]::BIGINT[] LOOP
    PERFORM raci_command_test.check_that(event_operation_complete_task(ident,402,1,raci_command_test.key(1),4,'test','test')
      ->>'error'='invalid_request','invalid event identity');
    PERFORM raci_command_test.check_that(completion_test.command(ident,1,1,4)->>'error'='invalid_request','invalid task identity');
    PERFORM raci_command_test.check_that(completion_test.command(402,ident,1,4)->>'error'='invalid_request','invalid actor');
  END LOOP;
  FOREACH ident IN ARRAY ARRAY[NULL,0,-1]::BIGINT[] LOOP
    PERFORM raci_command_test.check_that(completion_test.command(402,1,1,ident)->>'error'='invalid_request','invalid revision');
  END LOOP;
  FOREACH reason IN ARRAY ARRAY[NULL,' ',repeat('x',2001)] LOOP
    PERFORM raci_command_test.check_that(completion_test.command(402,1,1,4,reason)->>'error'='invalid_request','invalid reason');
  END LOOP;
  FOREACH reason IN ARRAY ARRAY[NULL,' ',repeat('x',201)] LOOP
    PERFORM raci_command_test.check_that(completion_test.command(402,1,1,4,'test',reason)->>'error'='invalid_request','invalid correlation');
  END LOOP;
  PERFORM raci_command_test.check_that(event_operation_complete_task(10,402,1,NULL,4,'test','test')->>'error'
    ='invalid_request','null key');
  PERFORM raci_command_test.check_that(event_operation_complete_task(11,402,1,raci_command_test.key(1),4,'test','test')
    ='{"error":"not_found"}'::JSONB,'foreign task identity hidden');
  PERFORM raci_command_test.check_that(completion_test.command(999,1,1,4)='{"error":"not_found"}'::JSONB,'absent task hidden');
  PERFORM raci_command_test.check_that(completion_test.command(402,4,1,4)='{"error":"not_found"}'::JSONB,'existing task hidden');
  PERFORM raci_command_test.check_that(completion_test.command(402,3,1,4)->>'error'='forbidden','scoped reader cannot complete');
  PERFORM raci_command_test.check_that(completion_test.command(402,1,1,3)->>'error'='version_conflict','stale revision');
  PERFORM raci_command_test.check_that(completion_test.command(100,1,1,1)->>'error'='operation_not_ready','legacy task not silently opted in');
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'all invalid inputs have no business effects');
END $$;

-- A stored trusted override cannot bypass this command's no-override contract.
BEGIN;
INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES(402,100);
SELECT raci_command_test.check_that(completion_test.command(402,2,1,5)
  ='{"error":"dependencies_not_ready"}'::JSONB,'private prerequisite causes generic blocked error');
INSERT INTO event_operation_task_override(activity_id,activity_version,override_kind,reason,policy_reference,authorized_by_party_id)
  VALUES(402,1,'blocked_completion','synthetic stored override','completion-test',1);
SELECT raci_command_test.check_that(completion_test.command(402,2,1,6)
  ='{"error":"dependencies_not_ready"}'::JSONB,'stored override is not silently applied');
UPDATE event_logistics_activity SET status='completed',version=version+1 WHERE id=100;
SELECT raci_command_test.check_that(completion_test.command(402,2,1,6)->>'status'='completed',
  'same previously rejected key can succeed once all prerequisites complete');
ROLLBACK;

BEGIN;
INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES(402,100);
UPDATE event_logistics_activity SET status='completed',version=version+1 WHERE id=100;
SELECT raci_command_test.check_that(completion_test.command(402,2,1,5)->>'status'='completed','gated completion');
DO $$ BEGIN
  BEGIN
    UPDATE event_logistics_activity SET status='planned',version=version+1 WHERE id=100;
    SET CONSTRAINTS ALL IMMEDIATE;
    RAISE EXCEPTION 'prerequisite reopened under a completed gated task';
  EXCEPTION WHEN check_violation THEN NULL; END;
END $$;
ROLLBACK;

DO $$ DECLARE state TEXT; BEGIN
  FOREACH state IN ARRAY ARRAY['pending_approval','approved','published','staffing','ready','in_progress',
    'completed','settlement_pending','settled','archived','reprogrammed','cancelled'] LOOP
    UPDATE event_operation_event_state SET canonical_state=state WHERE event_id=10;
    PERFORM raci_command_test.check_that(completion_test.command(402,1,1,4)->>'error'='operation_not_ready','lifecycle: '||state);
  END LOOP;
  UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=10;
  FOREACH state IN ARRAY ARRAY['in_progress','completed','cancelled'] LOOP
    UPDATE event_logistics_activity SET status=state WHERE id=402;
    PERFORM raci_command_test.check_that(completion_test.command(402,1,1,raci_command_test.rev(402))->>'error'
      ='operation_not_ready','task state: '||state);
  END LOOP;
  UPDATE event_logistics_activity SET status='planned' WHERE id=402;
END $$;

-- Other supported/unsupported policy and clock boundaries.
BEGIN;
UPDATE event_operation_event_state SET canonical_state='draft' WHERE event_id=10;
UPDATE event_logistics_activity SET status='confirmed' WHERE id=404;
SELECT raci_command_test.check_that(completion_test.command(404,1,1,5)->>'status'='completed',
  'confirmed preparation can complete in a draft event');
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
  valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=404 AND raci_role='accountable';
SELECT raci_command_test.check_that(completion_test.command(404,1,1,5)->>'replayed'='true',
  'historical replay is not a new fulfillment certification');
ROLLBACK;
BEGIN;
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()+interval '1 day'
  WHERE activity_id=404 AND raci_role='accountable';
SELECT raci_command_test.check_that(completion_test.command(404,1,1,5)->>'error'='accountability_not_ready',
  'future required accountable is not current accountability');
ROLLBACK;
BEGIN;
INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES(406,10,'planned',1);
INSERT INTO event_operation_task_policy(activity_id,requires_accountability,dependencies_gate_completion)
  VALUES(406,FALSE,FALSE);
SELECT raci_command_test.check_that(completion_test.command(406,1,1,2)->>'error'='operation_not_ready',
  'weak opt-in policy does not silently acquire completion semantics');
ROLLBACK;

SELECT raci_command_test.check_that(raci_command_test.command(405,1,1,4,'responsible',2,3)->>'aggregateRevision'='6',
  'preceding independent RACI operation can use the same UUID');
SELECT raci_command_test.check_that(completion_test.command(405,1,1,6)->>'aggregateRevision'='7',
  'completion and RACI operation namespaces do not collide');

-- Exhaustive deterministic decision table over five binary guard inputs.
DO $$ DECLARE n INTEGER; task BIGINT; actor BIGINT; expected BIGINT; result JSONB; code TEXT; snapshot JSONB; BEGIN
  FOR n IN 0..31 LOOP
    task := 2000+n; PERFORM raci_command_test.seed(task);
    INSERT INTO event_logistics_activity(id,event_id,status,version)
      VALUES(5000+n,10,CASE WHEN (n & 1)=1 THEN 'completed' ELSE 'planned' END,1);
    INSERT INTO event_logistics_dependency(activity_id,depends_on_activity_id) VALUES(task,5000+n);
    IF (n & 2)=0 THEN
      UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
        valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=task AND raci_role='responsible';
    END IF;
    UPDATE event_operation_event_state SET canonical_state=CASE WHEN (n & 4)=4 THEN 'planning' ELSE 'approved' END WHERE event_id=10;
    actor := CASE WHEN (n & 8)=8 THEN 2 ELSE 3 END;
    expected := raci_command_test.rev(task)-CASE WHEN (n & 16)=16 THEN 0 ELSE 1 END;
    snapshot := completion_test.rows();
    result := completion_test.command(task,actor,1,expected);
    code := CASE WHEN (n & 8)=0 THEN 'forbidden' WHEN (n & 4)=0 THEN 'operation_not_ready'
      WHEN (n & 16)=0 THEN 'version_conflict' WHEN (n & 2)=0 THEN 'accountability_not_ready'
      WHEN (n & 1)=0 THEN 'dependencies_not_ready' ELSE NULL END;
    IF code IS NULL THEN
      PERFORM raci_command_test.check_that(result->>'status'='completed'
        AND result->>'aggregateRevision'=(expected+1)::TEXT,'generated valid completion');
      snapshot := completion_test.rows();
      PERFORM raci_command_test.check_that(completion_test.command(task,actor,1,expected)=result||'{"replayed":true}'::JSONB,
        'generated exact replay');
    ELSE
      PERFORM raci_command_test.check_that(result=jsonb_build_object('error',code),'generated guard rejection '||n);
    END IF;
    PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'generated rejected/replayed history unchanged');
  END LOOP;
  UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=10;
END $$;

-- Atomic rollback at every persistence boundary and exact large revisions.
CREATE FUNCTION completion_test.fail_write() RETURNS TRIGGER LANGUAGE plpgsql AS $$ BEGIN
  RAISE EXCEPTION 'synthetic persistence fault' USING ERRCODE='ZX001';
END $$;
DO $$ DECLARE target_table TEXT; snapshot JSONB; result JSONB; BEGIN
  FOREACH target_table IN ARRAY ARRAY['event_operation_audit_event','event_operation_command_receipt'] LOOP
    EXECUTE format('CREATE TRIGGER completion_test_failure BEFORE INSERT ON %I FOR EACH ROW EXECUTE FUNCTION completion_test.fail_write()',target_table);
    snapshot := completion_test.rows();
    BEGIN
      PERFORM completion_test.command(403,1,1,4);
      RAISE EXCEPTION 'persistence fault unexpectedly succeeded';
    EXCEPTION WHEN SQLSTATE 'ZX001' THEN NULL; END;
    PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'atomic fault rollback: '||target_table);
    EXECUTE format('DROP TRIGGER completion_test_failure ON %I',target_table);
  END LOOP;
  snapshot := completion_test.rows();
  BEGIN
    result := completion_test.command(403,1,1,4);
    PERFORM raci_command_test.check_that(result->>'status'='completed','accepted before outer rollback');
    RAISE EXCEPTION 'outer abort' USING ERRCODE='ZX002';
  EXCEPTION WHEN SQLSTATE 'ZX002' THEN NULL; END;
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'outer rollback preserves all rows');
  BEGIN
    UPDATE event_operation_task_revision SET revision=9223372036854775807 WHERE activity_id=403;
    PERFORM completion_test.command(403,1,1,9223372036854775807);
    RAISE EXCEPTION 'aggregate overflow succeeded';
  EXCEPTION WHEN numeric_value_out_of_range THEN NULL; END;
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'aggregate overflow rolls back status and audit');
  BEGIN
    UPDATE event_logistics_activity SET version=2147483647 WHERE id=403;
    PERFORM completion_test.command(403,1,1,raci_command_test.rev(403));
    RAISE EXCEPTION 'activity version overflow succeeded';
  EXCEPTION WHEN numeric_value_out_of_range THEN NULL; END;
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'legacy version overflow rolls back');
  BEGIN
    UPDATE event_operation_task_revision SET revision=9223372036854775806 WHERE activity_id=403;
    result := completion_test.command(403,1,1,9223372036854775806);
    PERFORM raci_command_test.check_that(result->'aggregateRevision'='"9223372036854775807"'::JSONB,'exact maximal revision');
    RAISE EXCEPTION 'restore boundary probe' USING ERRCODE='ZX002';
  EXCEPTION WHEN SQLSTATE 'ZX002' THEN NULL; END;
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'boundary probe restores history');
  BEGIN
    UPDATE event_operation_audit_event SET reason='tamper' WHERE operation_code='event.task.complete' AND resource_id='400';
    RAISE EXCEPTION 'audit unexpectedly mutable';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
  BEGIN
    DELETE FROM event_operation_command_receipt WHERE operation_code='event.task.complete/400';
    RAISE EXCEPTION 'receipt unexpectedly deletable';
  EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
  PERFORM raci_command_test.check_that(snapshot=completion_test.rows(),'accepted history is immutable');
END $$;
