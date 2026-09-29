BEGIN;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Moderation actor '||n,false,now() FROM generate_series(916000001,916000004) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-moderation-'||n,'not-a-login-hash',true FROM generate_series(916000001,916000004) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(916000001,916000001,'Moderation club');
INSERT INTO fan_club_post(id,club_id,fan_party_id,content,created_at) VALUES(916000001,916000001,916000001,'Post',now());
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(916000002,916000001,now()),(916000003,916000001,now());
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
SELECT 916000004,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
UPDATE interaction_runtime SET enabled=true WHERE singleton;
DO $$
DECLARE target uuid; comment_key uuid; result_value jsonb; version_value bigint;
BEGIN
 ASSERT interaction_is_moderator(916000004);
 ASSERT NOT interaction_is_moderator(916000001);
 target:=interaction_register('club_post','916000001',916000001);
 result_value:=interaction_command(916000002,target,gen_random_uuid(),'{"operation":"comment.create","body":"Test report"}');
 comment_key:=(result_value->>'id')::uuid;
 result_value:=interaction_command(916000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.report','commentId',comment_key,'reason','Please review'));
 ASSERT result_value->>'reported'='true';
 ASSERT interaction_report_inbox(916000001,NULL,20)->>'error'='forbidden';
 ASSERT jsonb_array_length(interaction_report_inbox(916000004,NULL,20)->'items')=1;
 ASSERT interaction_report_inbox(916000004,NULL,20)->'items'->0->'reportReasons'->>0='Please review';
 ASSERT interaction_moderation_page(916000004,target,NULL,20)->'items'->0->'reportReasons'->>0='Please review';
 ASSERT interaction_report_reasons(916000001,comment_key)='[]', 'Owners cannot see private report reasons';
 ASSERT interaction_report_reasons(916000003,comment_key)='[]', 'Reporters cannot browse other reports';
 result_value:=interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.report.resolve','commentId',comment_key,'expectedVersion',1,'reason','Owner cannot dismiss safety reports','decision','dismissed'));
 ASSERT result_value->>'error'='forbidden';
 result_value:=interaction_command(916000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.report.resolve','commentId',comment_key,'expectedVersion',1,'reason','Reviewed and allowed','decision','dismissed'));
 ASSERT NOT result_value ? 'error';
 ASSERT jsonb_array_length(interaction_report_inbox(916000004,NULL,20)->'items')=0;
 result_value:=interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_key,'expectedVersion',1,'reason','Owner hides'));
 ASSERT result_value->>'state'='hidden';
 result_value:=interaction_command(916000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.delete','commentId',comment_key,'expectedVersion',2));
 ASSERT result_value->>'state'='deleted', 'Hiding must not prevent author erasure';
 result_value:=interaction_command(916000002,target,gen_random_uuid(),'{"operation":"comment.create","body":"Second comment"}');
 comment_key:=(result_value->>'id')::uuid;
 result_value:=interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_key,'expectedVersion',1,'reason','Owner hides'));
 result_value:=interaction_command(916000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.remove','commentId',comment_key,'expectedVersion',2,'reason','Administrative removal'));
 ASSERT result_value->>'state'='removed', 'Admin can strongly remove already hidden content';
 ASSERT (SELECT body='' FROM interaction_comment WHERE id=comment_key);
 ASSERT (SELECT count(*)=2 FROM interaction_audit WHERE actor_id=916000004);
 result_value:=interaction_block(916000002,916000003,true,0,gen_random_uuid());
 ASSERT interaction_block_list(916000002,NULL,20)->'items'->0->>'partyId'='916000003';
 ASSERT jsonb_array_length(interaction_block_list(916000003,NULL,20)->'items')=0, 'Blocked peer cannot inspect who blocked them';
 version_value:=(result_value->>'version')::bigint;
 PERFORM interaction_block(916000002,916000003,false,version_value,gen_random_uuid());
 ASSERT jsonb_array_length(interaction_block_list(916000002,NULL,20)->'items')=0;
 -- Blocking severs both legacy relationship directions permanently.
 INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(916000001,916000002,now());
 INSERT INTO party_follow(follower_party_id,following_party_id,via_nfc,created_at)
 VALUES(916000001,916000002,false,now()),(916000002,916000001,false,now());
 ASSERT interaction_follows(916000002,916000001);
 result_value:=interaction_block(916000001,916000002,true,0,gen_random_uuid());
 ASSERT NOT result_value ? 'error',result_value::text;
 ASSERT NOT EXISTS(SELECT 1 FROM fan_follow WHERE fan_party_id IN (916000001,916000002) AND artist_party_id IN (916000001,916000002));
 ASSERT NOT EXISTS(SELECT 1 FROM party_follow WHERE follower_party_id IN (916000001,916000002) AND following_party_id IN (916000001,916000002));
 PERFORM interaction_block(916000001,916000002,false,(result_value->>'version')::bigint,gen_random_uuid());
 ASSERT NOT interaction_follows(916000002,916000001);
 ASSERT NOT interaction_follows(916000001,916000002);
 ASSERT NOT interaction_club_access(916000002,916000001), 'Unblock cannot silently restore club membership';
 ASSERT NOT interaction_can_comment(target,916000002), 'Unblock cannot restore member-only discussion writes';
 -- Directory-owned blocks share account projection, revision and ownership.
 INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug)
 VALUES('91600000-0000-4000-8000-000000000002',916000002,'person','Two','interaction-review-two'),
 ('91600000-0000-4000-8000-000000000003',916000003,'person','Three','interaction-review-three');
 UPDATE social_v2_pair SET follow_a=true,follow_b=true WHERE party_a=916000002 AND party_b=916000003;
 version_value:=(interaction_block_state(916000002,916000003)->>'version')::bigint;
 INSERT INTO directory_profile_block(blocker_profile_id,blocked_profile_id,created_by)
 VALUES('91600000-0000-4000-8000-000000000002','91600000-0000-4000-8000-000000000003',916000002);
 ASSERT interaction_block_state(916000002,916000003)->>'blocked'='true';
 ASSERT interaction_block_list(916000002,NULL,20)->'items'->0->>'partyId'='916000003';
 ASSERT jsonb_array_length(interaction_block_list(916000003,NULL,20)->'items')=0, 'Peer cannot see actor-owned directory blocks';
 ASSERT interaction_block(916000002,916000003,false,version_value,gen_random_uuid())->>'error'='revision_conflict';
 INSERT INTO directory_profile_block(blocker_profile_id,blocked_profile_id,created_by)
 VALUES('91600000-0000-4000-8000-000000000003','91600000-0000-4000-8000-000000000002',916000003);
 version_value:=(interaction_block_state(916000002,916000003)->>'version')::bigint;
 result_value:=interaction_block(916000002,916000003,false,version_value,gen_random_uuid());
 ASSERT result_value->>'blocked'='false',result_value::text;
 ASSERT interaction_blocked(916000002,916000003), 'Own unblock must preserve the peer-owned directory block';
 ASSERT NOT interaction_owned_directory_block(916000002,916000003);
 ASSERT interaction_owned_directory_block(916000003,916000002);
 PERFORM interaction_block(916000003,916000002,false,(result_value->>'version')::bigint,gen_random_uuid());
 ASSERT NOT interaction_blocked(916000002,916000003);
 ASSERT NOT interaction_follows(916000002,916000003);
 ASSERT NOT interaction_follows(916000003,916000002);
 -- An author cannot evade scoped enforcement by blocking the owner or moderator.
 INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(916000002,916000001,now());
 result_value:=interaction_command(916000002,target,gen_random_uuid(),'{"operation":"comment.create","body":"Blocked author report"}');
 comment_key:=(result_value->>'id')::uuid;
 ASSERT comment_key IS NOT NULL,result_value::text;
 PERFORM interaction_command(916000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.report','commentId',comment_key,'reason','Blocking must not hide evidence'));
 PERFORM interaction_block(916000002,916000001,true,(interaction_block_state(916000002,916000001)->>'version')::bigint,gen_random_uuid());
 PERFORM interaction_block(916000002,916000004,true,0,gen_random_uuid());
 ASSERT interaction_blocked(916000001,916000002) AND interaction_blocked(916000004,916000002);
 -- The publication owner cannot make the entire target disappear from enforcement.
 PERFORM interaction_block(916000001,916000004,true,0,gen_random_uuid());
 ASSERT interaction_target_context(target,916000004) IS NULL;
 ASSERT interaction_moderation_context(target,916000004) IS NOT NULL;
 ASSERT interaction_moderation_context(target,916000003) IS NULL;
 ASSERT interaction_resolve_scoped('club_post','916000001',916000003,true) IS NULL;
 result_value:=interaction_summary(916000004,'club_post','916000001');
 ASSERT result_value->>'canModerate'='true' AND result_value->>'canReact'='false' AND result_value->>'canComment'='false';
 ASSERT result_value->'reactions'='[]'::jsonb AND result_value->>'commentCount'='0';
 ASSERT interaction_command(916000004,target,gen_random_uuid(),'{"operation":"comment.create","body":"Moderator social write"}')->>'error'='unavailable';
 ASSERT interaction_command(916000004,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}')->>'error'='unavailable';
 ASSERT interaction_comments_page(target,916000004,NULL,'newest',NULL,20)->>'error'='unavailable';

 ASSERT interaction_report_inbox(916000004,NULL,20)->'items'->0->>'moderationBody'='Blocked author report';
 ASSERT interaction_report_inbox(916000004,NULL,20)->'items'->0->>'state'='visible';
 ASSERT interaction_report_reasons(916000004,comment_key)->>0='Blocking must not hide evidence';
 ASSERT EXISTS(SELECT 1 FROM jsonb_array_elements(interaction_moderation_page(916000001,target,NULL,20)->'items') c
   WHERE c->>'id'=comment_key::text AND c->>'moderationBody'='Blocked author report' AND c->>'state'='visible');
 ASSERT NOT EXISTS(SELECT 1 FROM jsonb_array_elements(interaction_comments_page(target,916000001,NULL,'newest',NULL,20)->'items') c WHERE c->>'id'=comment_key::text), 'Normal discussion still honors blocks';
 result_value:=interaction_destination(916000004,'comment',comment_key);
 ASSERT NOT result_value ? 'error', 'Moderator inbox link remains resolvable';
 ASSERT result_value->'context'->'comment'->>'body'='' AND result_value->'context'->'comment'->'author'='null'::jsonb, 'Deep context is a redacted moderation entry point';
 ASSERT interaction_report_reasons(916000001,comment_key)='[]';
 ASSERT interaction_moderation_page(916000003,target,NULL,20)->>'error'='unavailable';
 ASSERT interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.remove','commentId',comment_key,'expectedVersion',1,'reason','Not an admin'))->>'error'='unavailable';
 result_value:=interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_key,'expectedVersion',1,'reason','Owner hides blocked author'));
 ASSERT result_value->>'state'='hidden',result_value::text;
 result_value:=interaction_command(916000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.restore','commentId',comment_key,'expectedVersion',2,'reason','Owner restores'));
 ASSERT NOT result_value ? 'error',result_value::text;
 ASSERT (SELECT state='visible' FROM interaction_comment WHERE id=comment_key);
 result_value:=interaction_command(916000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.report.resolve','commentId',comment_key,'expectedVersion',3,'reason','Reviewed despite block','decision','reviewed'));
 ASSERT NOT result_value ? 'error',result_value::text;
 result_value:=interaction_command(916000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.remove','commentId',comment_key,'expectedVersion',3,'reason','Moderator removal despite block'));
 ASSERT result_value->>'state'='removed',result_value::text;
 ASSERT (SELECT body='' FROM interaction_comment WHERE id=comment_key);
 ASSERT EXISTS(SELECT 1 FROM interaction_audit WHERE comment_id=comment_key AND actor_id=916000004 AND operation='comment.remove');
 INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 916000004,id,'bootstrap',true,now(),1 FROM security_role WHERE code='engineer';
 ASSERT NOT interaction_is_moderator(916000004), 'Mixed privileged staff grants do not satisfy strict admin';
 ASSERT interaction_report_inbox(916000004,NULL,20)->>'error'='forbidden';
END $$;
ROLLBACK;
