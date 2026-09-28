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
 INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 916000004,id,'bootstrap',true,now(),1 FROM security_role WHERE code='engineer';
 ASSERT NOT interaction_is_moderator(916000004), 'Mixed privileged staff grants do not satisfy strict admin';
 ASSERT interaction_report_inbox(916000004,NULL,20)->>'error'='forbidden';
END $$;
ROLLBACK;
