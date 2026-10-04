CREATE SCHEMA raci_command_test;
CREATE FUNCTION raci_command_test.check_that(ok BOOLEAN, message TEXT) RETURNS VOID
LANGUAGE plpgsql AS $$ BEGIN
  IF ok IS DISTINCT FROM TRUE THEN RAISE EXCEPTION 'RACI command assertion: %', message; END IF;
END $$;
CREATE FUNCTION raci_command_test.rev(task BIGINT) RETURNS BIGINT LANGUAGE sql AS $$
  SELECT revision FROM event_operation_task_revision WHERE activity_id=task
$$;
CREATE FUNCTION raci_command_test.key(n INTEGER) RETURNS UUID LANGUAGE sql AS $$
  SELECT ('10000000-0000-4000-8000-' || lpad(n::TEXT,12,'0'))::UUID
$$;
CREATE FUNCTION raci_command_test.command(task BIGINT, actor BIGINT, key INTEGER, expected BIGINT,
  role TEXT, source BIGINT, recipient BIGINT, reason TEXT DEFAULT 'synthetic reassignment',
  correlation TEXT DEFAULT 'raci-command-test') RETURNS JSONB LANGUAGE sql AS $$
  SELECT event_operation_reassign_raci(10,task,actor,raci_command_test.key(key),expected,
    role,source,recipient,reason,correlation)
$$;
CREATE FUNCTION raci_command_test.seed(task BIGINT) RETURNS VOID LANGUAGE plpgsql AS $$ BEGIN
  INSERT INTO event_logistics_activity(id,event_id,status,version) VALUES (task,10,'planned',1);
  INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
    VALUES (task,1,'accountable',1),(task,2,'responsible',1);
  INSERT INTO event_operation_task_policy(activity_id) VALUES (task);
  INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
    VALUES (10,2,'task.manage','task',task::TEXT,1),(10,3,'task.read','task',task::TEXT,1);
END $$;
-- Coordination-fence attempts are intentionally excluded from business-state equality.
CREATE FUNCTION raci_command_test.rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object(
    'activity',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_activity t),
    'raci',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_raci_assignment t),
    'revisions',(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_revision t),
    'audit',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_audit_event t),
    'receipts',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_command_receipt t),
    'grants',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_grant t),
    'relationships',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_relationship t),
    'state',(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_event_state t))
$$;
INSERT INTO party(id) VALUES (4);
SELECT raci_command_test.seed(300);
SELECT raci_command_test.seed(301);
SELECT raci_command_test.seed(302);
SELECT raci_command_test.seed(303);
CREATE ROLE raci_command_untrusted;
SELECT raci_command_test.check_that(NOT has_function_privilege('raci_command_untrusted',
  'event_operation_reassign_raci(bigint,bigint,bigint,uuid,bigint,text,bigint,bigint,text,text)','EXECUTE'),
  'no PUBLIC execution of command');
SELECT raci_command_test.check_that(raci_command_test.command(300,1,1,4,'responsible',2,3)
  = '{"error":"feature_disabled"}'::JSONB, 'disabled command has no success');
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_by_party_id=1,
  change_reason='disposable RACI command test' WHERE feature_code='event.operations.api';
