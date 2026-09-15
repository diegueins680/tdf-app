DO $$ DECLARE result JSONB; before_rows JSONB; old_record event_operation_raci_assignment%ROWTYPE; BEGIN
  SELECT * INTO old_record FROM event_operation_raci_assignment
    WHERE activity_id=300 AND raci_role='responsible';
  result := raci_command_test.command(300,1,1,4,'responsible',2,3);
  PERFORM raci_command_test.check_that(result=jsonb_build_object('eventId',10,'activityId',300,
    'commandId',raci_command_test.key(1),'role','responsible','fromPartyId',2,'toPartyId',3,
    'aggregateRevision','6','replayed',FALSE), 'exact accepted outcome');
  PERFORM raci_command_test.check_that(raci_command_test.rev(300)=6
    AND (SELECT version=1 AND status='planned' FROM event_logistics_activity WHERE id=300),
    'two tracked writes, no legacy version or status change');
  PERFORM raci_command_test.check_that(EXISTS(SELECT 1 FROM event_operation_raci_assignment
    WHERE id=old_record.id AND party_id=old_record.party_id AND valid_from=old_record.valid_from
      AND assigned_by_party_id=old_record.assigned_by_party_id AND revoked_at IS NOT NULL
      AND revoked_by_party_id=1 AND revocation_reason='synthetic reassignment'), 'old assignment retained');
  PERFORM raci_command_test.check_that((SELECT count(*)=1 FROM event_operation_audit_event
    WHERE resource_kind='task' AND resource_id='300' AND before_state->>'partyId'='2'
      AND after_state->>'partyId'='3' AND before_state->>'aggregateRevision'='4'
      AND after_state->>'aggregateRevision'='6'), 'one complete immutable audit');
  before_rows := raci_command_test.rows();
  PERFORM raci_command_test.check_that(raci_command_test.command(300,1,1,4,'responsible',2,3)
    =result || '{"replayed":true}'::JSONB, 'exact retry before stale version comparison');
  PERFORM raci_command_test.check_that(raci_command_test.command(300,1,1,4,'responsible',2,3,'changed')->>'error'
    ='idempotency_conflict', 'changed payload cannot reuse accepted key');
  PERFORM raci_command_test.check_that(raci_command_test.command(300,1,1,4,'responsible',2,3,
    'synthetic reassignment','changed-correlation')->>'error'='idempotency_conflict',
    'correlation is bound into accepted request identity');
  PERFORM raci_command_test.check_that(raci_command_test.command(300,2,1,4,'responsible',2,3)->>'error'
    ='idempotency_conflict', 'changed readable actor cannot reuse key');
  PERFORM raci_command_test.check_that(raci_command_test.command(300,4,1,4,'responsible',2,3)->>'error'
    ='not_found', 'private key is opaque to outsider');
  PERFORM raci_command_test.check_that(raci_command_test.command(300,1,2,4,'responsible',2,3)->>'error'
    ='version_conflict', 'new stale command rejected');
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'retry/rejections change no business state');
END $$;

-- A private sibling's existing key neither blocks nor discloses this task's new command.
SELECT raci_command_test.check_that(raci_command_test.command(301,2,1,4,'responsible',2,3)->>'replayed'='false',
  'same UUID on another task is a new independently scoped command');
SELECT raci_command_test.check_that((SELECT count(*)=2 FROM event_operation_command_receipt
  WHERE command_id=raci_command_test.key(1)), 'one canonical receipt per task namespace');

BEGIN;
UPDATE event_operation_grant SET scope_code='task.read' WHERE grantee_party_id=2 AND resource_id='301';
SELECT raci_command_test.check_that(raci_command_test.command(301,2,1,4,'responsible',2,3)->>'replayed'='true',
  'read-only downgrade permits historical replay');
SELECT raci_command_test.check_that(raci_command_test.command(301,2,2,6,'responsible',3,2)->>'error'='forbidden',
  'read-only downgrade denies new mutation');
UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='test'
  WHERE grantee_party_id=2 AND resource_id='301';
SELECT raci_command_test.check_that(raci_command_test.command(301,2,1,4,'responsible',2,3)->>'error'='not_found',
  'revocation hides accepted receipt');
ROLLBACK;

DO $$ DECLARE code TEXT; result JSONB; before_rows JSONB; BEGIN
  before_rows := raci_command_test.rows();
  FOREACH code IN ARRAY ARRAY['event.read','event.manage','event.approve','finance.approve','audit.read','task.read','task.manage'] LOOP
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
      VALUES (10,4,code,1);
    PERFORM raci_command_test.check_that(event_operation_actor_can_manage_task(10,300,4,clock_timestamp())
      = (code='task.manage'), 'no ambient task authority: ' || code);
    DELETE FROM event_operation_grant WHERE grantee_party_id=4;
  END LOOP;
  INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES(10,4,'coproducer');
  PERFORM raci_command_test.check_that(NOT event_operation_actor_can_manage_task(10,300,4,clock_timestamp()),
    'coproduction alone cannot manage task');
  DELETE FROM event_operation_relationship WHERE party_id=4;
END $$;

DO $$ DECLARE state TEXT; task_state TEXT; before_revision BIGINT := raci_command_test.rev(302); BEGIN
  FOREACH state IN ARRAY ARRAY['pending_approval','approved','published','staffing','ready','in_progress',
    'completed','settlement_pending','settled','archived','reprogrammed','cancelled'] LOOP
    UPDATE event_operation_event_state SET canonical_state=state WHERE event_id=10;
    PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,before_revision,'responsible',2,3)->>'error'
      ='operation_not_ready', 'unsupported event lifecycle fails closed: ' || state);
  END LOOP;
  UPDATE event_operation_event_state SET canonical_state='planning' WHERE event_id=10;
  FOREACH task_state IN ARRAY ARRAY['in_progress','completed','cancelled'] LOOP
    UPDATE event_logistics_activity SET status=task_state WHERE id=302;
    PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,raci_command_test.rev(302),'responsible',2,3)->>'error'
      ='operation_not_ready', 'unsupported task state: ' || task_state);
  END LOOP;
  UPDATE event_logistics_activity SET status='planned' WHERE id=302;
END $$;

DO $$ DECLARE before_rows JSONB; expected BIGINT := raci_command_test.rev(302); BEGIN
  before_rows := raci_command_test.rows();
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,4)->>'error'
    ='assignee_unavailable', 'existing ineligible recipient is opaque');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,999)->>'error'
    ='assignee_unavailable', 'unknown recipient uses same error');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',3,2)->>'error'
    ='assignment_not_replaceable', 'missing source rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,2)->>'error'
    ='invalid_request', 'self replacement is not a material command');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,NULL,'responsible',2,3)->>'error'
    ='invalid_request', 'null expected revision rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'owner',2,3)->>'error'
    ='invalid_request', 'invented role rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,3,' ')->>'error'
    ='invalid_request', 'blank reason rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,3,
    repeat('x',2001))->>'error'='invalid_request', 'oversized reason rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,3,
    'test',repeat('x',201))->>'error'='invalid_request', 'oversized correlation rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,3,
    'test',NULL)->>'error'='invalid_request', 'null correlation rejected');
  PERFORM raci_command_test.check_that(event_operation_reassign_raci(10,302,1,NULL,expected,
    'responsible',2,3,'test','test')->>'error'='invalid_request', 'null key rejected');
  PERFORM raci_command_test.check_that(raci_command_test.command(302,1,10,expected,'responsible',2,9007199254740992)->>'error'
    ='invalid_request', 'unsafe transport identity rejected');
  PERFORM raci_command_test.check_that(event_operation_reassign_raci(11,302,1,raci_command_test.key(10),expected,
    'responsible',2,3,'test','test')->>'error'='not_found', 'wrong event target is opaque');
  PERFORM raci_command_test.check_that(raci_command_test.rows()=before_rows, 'invalid attempts have no business effects');
END $$;

BEGIN;
UPDATE event_operation_raci_assignment SET valid_until=clock_timestamp()+interval '1 day'
  WHERE activity_id=302 AND raci_role='responsible';
SELECT raci_command_test.check_that(raci_command_test.command(302,1,10,raci_command_test.rev(302),'responsible',2,3)->>'error'
  ='assignment_not_replaceable', 'time-bounded source is not silently rewritten');
ROLLBACK;
BEGIN;
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()+interval '1 day'
  WHERE activity_id=302 AND raci_role='responsible';
SELECT raci_command_test.check_that(raci_command_test.command(302,1,10,raci_command_test.rev(302),'responsible',2,3)->>'error'
  ='assignment_not_replaceable', 'future source is not silently rewritten');
ROLLBACK;
BEGIN;
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',valid_until=clock_timestamp()-interval '1 day'
  WHERE activity_id=302 AND raci_role='accountable';
SELECT raci_command_test.check_that(raci_command_test.command(302,1,10,raci_command_test.rev(302),'responsible',2,3)->>'error'
  ='accountability_not_ready', 'expired required A is an explicit attention state');
ROLLBACK;
BEGIN;
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id) VALUES(302,3,'responsible',1);
SELECT raci_command_test.check_that(raci_command_test.command(302,1,10,raci_command_test.rev(302),'responsible',2,3)->>'error'
  ='assignment_conflict', 'existing target pair is preserved');
ROLLBACK;

-- Generated command histories alternate explicit replacements, including A, C and I.
DO $$ DECLARE n INTEGER; role TEXT; source BIGINT; recipient BIGINT; expected BIGINT; result JSONB; snapshot JSONB; BEGIN
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES(303,2,'consulted',1),(303,2,'informed',1);
  FOREACH role IN ARRAY ARRAY['accountable','responsible','consulted','informed'] LOOP
    FOR n IN 1..10 LOOP
      SELECT party_id INTO source FROM event_operation_raci_assignment
        WHERE activity_id=303 AND raci_role=role AND revoked_at IS NULL;
      recipient := CASE WHEN source=3 THEN 2 ELSE 3 END;
      expected := raci_command_test.rev(303);
      result := raci_command_test.command(303,1,1000+expected::INTEGER,expected,role,source,recipient);
      PERFORM raci_command_test.check_that(result->>'aggregateRevision'=(expected+2)::TEXT
        AND result->>'replayed'='false', 'generated accepted replacement');
      snapshot := raci_command_test.rows();
      PERFORM raci_command_test.check_that(raci_command_test.command(303,1,1000+expected::INTEGER,expected,role,source,recipient)
        =result || '{"replayed":true}'::JSONB, 'generated exact replay');
      PERFORM raci_command_test.check_that(snapshot=raci_command_test.rows(), 'generated replay preserves business state');
      PERFORM event_operation_validate_task_event(10);
    END LOOP;
  END LOOP;
END $$;
