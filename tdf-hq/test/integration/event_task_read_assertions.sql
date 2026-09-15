CREATE SCHEMA task_read_test;
CREATE FUNCTION task_read_test.check_that(ok BOOLEAN, message TEXT) RETURNS VOID
LANGUAGE plpgsql AS $$ BEGIN
  IF ok IS DISTINCT FROM TRUE THEN RAISE EXCEPTION 'Task read assertion: %', message; END IF;
END $$;
CREATE FUNCTION task_read_test.rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'activities',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_activity t),
    'policy',(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_policy t),
    'raci',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_raci_assignment t),
    'grants',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_grant t),
    'owners',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_relationship t),
    'fence',(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_task_write_fence t),
    'state',(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_event_state t),
    'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_audit_event t),
    'receipts',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_command_receipt t));
$$;

SELECT task_read_test.check_that(event_operation_read_task(10,100,1) IS NULL, 'disabled denies owner');
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_by_party_id=1,
  change_reason='disposable task read test' WHERE feature_code='event.operations.api';

DO $$ DECLARE result JSONB; scope TEXT; resource TEXT; before_rows JSONB; BEGIN
  before_rows := task_read_test.rows();
  result := event_operation_read_task(10,100,1);
  PERFORM task_read_test.check_that(result = '{"eventId":10,"activityId":100,"status":"planned",
    "version":1,"raci":[],"accountabilityNeedsAttention":false}'::JSONB, 'owner exact allowlist, no policy');
  PERFORM task_read_test.check_that(event_operation_read_task(10,100,2) IS NULL
    AND event_operation_read_task(11,100,1) IS NULL
    AND event_operation_read_task(10,102,1) IS NULL
    AND event_operation_read_task(10,999,1) IS NULL
    AND event_operation_read_task(999,100,1) IS NULL
    AND event_operation_read_task(10,100,NULL) IS NULL, 'opaque missing/foreign/unauthorized');
  PERFORM task_read_test.check_that(task_read_test.rows() = before_rows, 'reads do not mutate state');

  FOREACH scope IN ARRAY ARRAY['event.read','event.manage','event.publish','event.approve',
      'booking.manage','contract.manage','finance.read','finance.approve','audit.read','task.read','task.manage'] LOOP
    INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
      VALUES (10,2,scope,1);
    PERFORM task_read_test.check_that((event_operation_read_task(10,100,2) IS NOT NULL)
      = (scope IN ('task.read','task.manage')), 'event-level scope: ' || scope);
    DELETE FROM event_operation_grant WHERE grantee_party_id=2;
  END LOOP;
  FOREACH scope IN ARRAY ARRAY['task.read','task.manage'] LOOP
    FOREACH resource IN ARRAY ARRAY['100','101','0100','+100','100 ','not-an-id','9999999999999999999999999'] LOOP
      INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
        VALUES (10,2,scope,'task',resource,1);
      PERFORM task_read_test.check_that((event_operation_read_task(10,100,2) IS NOT NULL)
        = (resource = '100'), 'exact textual scope: ' || resource);
      DELETE FROM event_operation_grant WHERE grantee_party_id=2;
    END LOOP;
  END LOOP;
  INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
    VALUES (11,2,'task.read','task','100',1),(10,2,'task.read','document','100',1),
      (10,3,'task.read','task','100',1);
  PERFORM task_read_test.check_that(event_operation_read_task(10,100,2) IS NULL, 'grant event/kind/actor binding');
  DELETE FROM event_operation_grant;

  INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,
    issued_by_party_id,valid_from,valid_until)
    VALUES (10,2,'task.read','task','100',1,'2026-01-01Z','2026-01-02Z');
  PERFORM task_read_test.check_that(
    NOT event_operation_actor_can_read_task(10,100,2,'2025-12-31 23:59:59.999999Z')
    AND event_operation_actor_can_read_task(10,100,2,'2026-01-01Z')
    AND event_operation_actor_can_read_task(10,100,2,'2026-01-01 23:59:59.999999Z')
    AND NOT event_operation_actor_can_read_task(10,100,2,'2026-01-02Z'), 'half-open grant bounds');
  UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='test';
  PERFORM task_read_test.check_that(NOT event_operation_actor_can_read_task(10,100,2,'2026-01-01Z'), 'revocation denies');
  DELETE FROM event_operation_grant;

  INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
    VALUES (10,2,'coproducer');
  PERFORM task_read_test.check_that(event_operation_read_task(10,100,2) IS NULL, 'coproduction alone denies');
  DELETE FROM event_operation_relationship WHERE party_id=2;
  INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind,valid_from,valid_until)
    VALUES (10,2,'co_owner','2026-01-01Z','2026-01-02Z');
  PERFORM task_read_test.check_that(event_operation_actor_can_read_task(10,100,2,'2026-01-01Z')
    AND NOT event_operation_actor_can_read_task(10,100,2,'2026-01-02Z'), 'owner validity bounds');
  DELETE FROM event_operation_relationship WHERE party_id=2;
END $$;

BEGIN;
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
  VALUES (100,1,'accountable',1),(100,2,'responsible',1);
INSERT INTO event_operation_task_policy(activity_id) VALUES (100);
COMMIT;
DO $$ DECLARE result JSONB; before_rows JSONB; BEGIN
  before_rows := task_read_test.rows();
  result := event_operation_read_task(10,100,1);
  PERFORM task_read_test.check_that(result = '{"eventId":10,"activityId":100,"status":"planned",
    "version":1,"policy":{"requiresAccountability":true,"dependenciesGateCompletion":true,"version":1},
    "raci":[{"partyId":1,"role":"accountable"},{"partyId":2,"role":"responsible"}],
    "accountabilityNeedsAttention":false}'::JSONB, 'exact protected-task projection');
  PERFORM task_read_test.check_that(event_operation_read_task(10,100,2) IS NULL, 'assignment is not authority');
  PERFORM task_read_test.check_that(task_read_test.rows()=before_rows, 'protected reads preserve rows');
END $$;

-- Existing commit cardinality is non-revoked, not time-aware. Expose the gap explicitly.
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()+interval '1 day'
  WHERE activity_id=100 AND raci_role='responsible';
SELECT task_read_test.check_that(event_operation_read_task(10,100,1)->'accountabilityNeedsAttention'='true'::JSONB
  AND jsonb_array_length(event_operation_read_task(10,100,1)->'raci')=1, 'future Responsible reports attention');
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',
  valid_until=clock_timestamp()-interval '1 day' WHERE activity_id=100 AND raci_role='responsible';
SELECT task_read_test.check_that(event_operation_read_task(10,100,1)->'accountabilityNeedsAttention'='true'::JSONB,
  'expired Responsible reports attention');
UPDATE event_operation_raci_assignment SET valid_until=NULL WHERE activity_id=100;
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()+interval '1 day'
  WHERE activity_id=100 AND raci_role='accountable';
SELECT task_read_test.check_that(event_operation_read_task(10,100,1)->'accountabilityNeedsAttention'='true'::JSONB
  AND jsonb_array_length(event_operation_read_task(10,100,1)->'raci')=1, 'future Accountable reports attention');
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '1 day'
  WHERE activity_id=100 AND raci_role='accountable';
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id,
  revoked_at,revoked_by_party_id,revocation_reason)
  VALUES (100,3,'consulted',1,clock_timestamp(),1,'test');
SELECT task_read_test.check_that(jsonb_array_length(event_operation_read_task(10,100,1)->'raci')=2,
  'revoked consulted assignment is not projected');
CREATE TABLE task_read_test.preserved AS SELECT task_read_test.rows() AS snapshot;
