CREATE FUNCTION raci_command_test.context(actor BIGINT, cursor_id BIGINT DEFAULT 0) RETURNS JSONB
LANGUAGE sql AS $$ SELECT event_operation_read_raci_editor_context(10,300,actor,cursor_id) $$;
SELECT raci_command_test.check_that(NOT EXISTS (
  SELECT 1 FROM pg_proc procedure,
    LATERAL aclexplode(COALESCE(procedure.proacl,acldefault('f',procedure.proowner))) privilege
  WHERE procedure.oid='event_operation_read_raci_editor_context(bigint,bigint,bigint,bigint)'::regprocedure
    AND privilege.grantee=0 AND privilege.privilege_type='EXECUTE'), 'no PUBLIC execution of actor-selecting function');
DO $$ DECLARE before_rows JSONB := raci_command_test.rows(); fence_rows JSONB; result JSONB; BEGIN
  SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) INTO fence_rows FROM event_operation_task_write_fence t;
  result := raci_command_test.context(1);
  PERFORM raci_command_test.check_that(result='{"eventId":10,"activityId":300,"aggregateRevision":"4", "canManage":true,"operationReady":true,"replaceableAssignments":[{"partyId":1,"role":"accountable"},{"partyId":2,"role":"responsible"}],"eligiblePartyIds":[1,2,3]}'::JSONB,
    'exact manager context contains only necessary options');
  PERFORM raci_command_test.check_that(raci_command_test.context(2)=result, 'exact-task manager without parent scope works');
  PERFORM raci_command_test.check_that(raci_command_test.context(3)=result ||
    '{"canManage":false,"operationReady":false,"replaceableAssignments":[],"eligiblePartyIds":[]}'::JSONB,
    'reader sees no manager-only options');
  PERFORM raci_command_test.check_that(raci_command_test.context(4) IS NULL
    AND event_operation_read_raci_editor_context(11,300,1,0) IS NULL
    AND event_operation_read_raci_editor_context(10,999,1,0) IS NULL, 'opaque unauthorized targets');
  PERFORM raci_command_test.check_that(raci_command_test.context(1,1)->'eligiblePartyIds'='[2,3]'::JSONB
    AND raci_command_test.context(1,3)->'eligiblePartyIds'='[]'::JSONB, 'exclusive keyset cursor');
  PERFORM raci_command_test.check_that(raci_command_test.context(1,-1) IS NULL
    AND raci_command_test.context(1,NULL) IS NULL
    AND raci_command_test.context(1,9007199254740992) IS NULL, 'invalid cursor fails closed');
  PERFORM raci_command_test.check_that(before_rows=raci_command_test.rows()
    AND fence_rows=(SELECT jsonb_agg(to_jsonb(t) ORDER BY event_id) FROM event_operation_task_write_fence t),
    'context reads never mutate business or coordination state');
END $$;

BEGIN;
UPDATE event_operation_grant SET scope_code='task.read' WHERE grantee_party_id=2 AND resource_id='300';
SELECT raci_command_test.check_that(raci_command_test.context(2)->>'canManage'='false', 'downgrade hides editor options');
UPDATE event_operation_grant SET revoked_at=clock_timestamp(),revoked_by_party_id=1,revocation_reason='test'
  WHERE grantee_party_id=2 AND resource_id='300';
SELECT raci_command_test.check_that(raci_command_test.context(2) IS NULL
  AND raci_command_test.context(1)->'eligiblePartyIds'='[1,3]'::JSONB, 'revocation removes current authority and eligibility');
ROLLBACK;

BEGIN;
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind) VALUES (10,4,'coproducer');
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id) VALUES (10,4,'event.manage',1);
SELECT raci_command_test.check_that(raci_command_test.context(4) IS NULL
  AND raci_command_test.context(1)->'eligiblePartyIds'='[1,2,3]'::JSONB, 'no ambient authority or eligibility');
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
VALUES (10,4,'task.read','task','301',1);
SELECT raci_command_test.check_that(raci_command_test.context(1)->'eligiblePartyIds'='[1,2,3]'::JSONB,
  'sibling grant cannot enter task roster');
UPDATE event_operation_grant SET resource_id='300' WHERE grantee_party_id=4 AND resource_kind='task';
SELECT raci_command_test.check_that(raci_command_test.context(1)->'eligiblePartyIds'='[1,2,3,4]'::JSONB,
  'exact task read makes a recipient eligible but not a manager');
UPDATE event_operation_grant SET valid_from=clock_timestamp()+interval '1 day' WHERE grantee_party_id=4 AND resource_kind='task';
SELECT raci_command_test.check_that(raci_command_test.context(1)->'eligiblePartyIds'='[1,2,3]'::JSONB, 'future grants excluded');
ROLLBACK;

BEGIN;
UPDATE event_operation_raci_assignment SET valid_until=clock_timestamp()+interval '1 day'
  WHERE activity_id=300 AND raci_role='responsible';
SELECT raci_command_test.check_that(raci_command_test.context(1)->'replaceableAssignments'='[{"partyId":1,"role":"accountable"}]'::JSONB
  AND raci_command_test.context(1)->>'operationReady'='true', 'current timed source is not offered for replacement');
UPDATE event_operation_raci_assignment SET valid_from=clock_timestamp()-interval '2 days',valid_until=clock_timestamp()-interval '1 day'
  WHERE activity_id=300 AND raci_role='responsible';
SELECT raci_command_test.check_that(raci_command_test.context(1)->>'operationReady'='false'
  AND raci_command_test.context(1)->'eligiblePartyIds'='[]'::JSONB
  AND raci_command_test.context(1)->'replaceableAssignments'='[]'::JSONB,
  'expired required responsibility hides actionable options');
ROLLBACK;

BEGIN;
UPDATE event_operation_event_state SET canonical_state='ready' WHERE event_id=10;
SELECT raci_command_test.check_that(raci_command_test.context(1)->>'canManage'='true'
  AND raci_command_test.context(1)->>'operationReady'='false'
  AND raci_command_test.context(1)->'eligiblePartyIds'='[]'::JSONB, 'unsupported lifecycle is not actionable');
ROLLBACK;

BEGIN;
INSERT INTO party(id) SELECT generate_series(1000,1104);
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
SELECT 10,n,'task.read',1 FROM generate_series(1000,1104) n;
DO $$ DECLARE first_page JSONB; second_page JSONB; BEGIN
  first_page := raci_command_test.context(1);
  second_page := raci_command_test.context(1,1096);
  PERFORM raci_command_test.check_that(jsonb_array_length(first_page->'eligiblePartyIds')=100
    AND first_page->>'nextAfterPartyId'='1096', 'bounded first page uses last returned ID as cursor');
  PERFORM raci_command_test.check_that(second_page->'eligiblePartyIds'='[1097,1098,1099,1100,1101,1102,1103,1104]'::JSONB
    AND NOT second_page ? 'nextAfterPartyId', 'last page has no false continuation');
END $$;
ROLLBACK;

BEGIN;
UPDATE event_operation_task_revision SET revision=9223372036854775807 WHERE activity_id=300;
SELECT raci_command_test.check_that(raci_command_test.context(1)->'aggregateRevision'='"9223372036854775807"'::JSONB,
  'maximum revision remains exact text');
ROLLBACK;
BEGIN;
UPDATE event_operation_feature_flag SET enabled=FALSE,updated_by_party_id=1,change_reason='test disabled context';
SELECT raci_command_test.check_that(raci_command_test.context(1) IS NULL, 'disabled context has no data');
ROLLBACK;
