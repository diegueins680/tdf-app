BEGIN;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Interaction synthetic '||n,false,now() FROM generate_series(910000001,910000005) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-synthetic-'||n,'not-a-login-hash',true FROM generate_series(910000001,910000005) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(910000001,910000001,'Interaction synthetic club');
INSERT INTO fan_club_post(id,club_id,fan_party_id,content,created_at)
VALUES(910000001,910000001,910000001,'Synthetic post',now());
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at)
VALUES(910000002,910000001,now()),(910000003,910000001,now());
DO $$
DECLARE target uuid; root uuid:=gen_random_uuid(); child uuid:=gen_random_uuid(); page jsonb;
BEGIN
 ASSERT interaction_resolve('payment','1',910000001) IS NULL;
 ASSERT interaction_resolve('club_post','910000001',NULL) IS NULL;
 ASSERT interaction_resolve('club_post','910000001',910000004) IS NOT NULL, 'Existing authenticated club reading remains supported';
 ASSERT interaction_resolve('club_post','910000001',910000002) IS NOT NULL;
 target:=interaction_register('club_post','910000001',910000001);
 ASSERT target IS NOT NULL;
 ASSERT target=interaction_register('club_post','910000001',910000002);
 ASSERT interaction_can_comment(target,910000002);
 ASSERT NOT interaction_can_comment(target,910000004);
 UPDATE interaction_target SET comment_policy='off' WHERE id=target;
 ASSERT NOT interaction_can_comment(target,910000001), 'Owner cannot bypass comments off';
 UPDATE interaction_target SET comment_policy='mentioned' WHERE id=target;
 ASSERT NOT interaction_can_comment(target,910000002);
 INSERT INTO interaction_target_mention VALUES(target,910000002);
 ASSERT interaction_can_comment(target,910000002);
 ASSERT NOT interaction_can_comment(target,910000003);
 UPDATE interaction_target SET comment_policy='followers' WHERE id=target;
 ASSERT interaction_can_comment(target,910000003);
 INSERT INTO interaction_comment(id,target_id,author_id,root_id,body)
 VALUES(root,target,910000001,root,'Synthetic parent');
 INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body)
 VALUES(child,target,910000002,root,root,1,'Synthetic reply');
 page:=interaction_comments_page(target,910000003,NULL,'relevant',NULL,10);
 ASSERT page->>'sort'='newest';
 ASSERT jsonb_array_length(page->'items')=1;
 ASSERT page->'items'->0->>'replyCount'='1';
 ASSERT interaction_comment_context(target,910000003,child)->'root'->>'id'=root::text;
 UPDATE interaction_comment SET state='deleted',body='' WHERE id=root;
 page:=interaction_comments_page(target,910000003,NULL,'oldest',NULL,10);
 ASSERT page->'items'->0->>'state'='deleted';
 ASSERT page->'items'->0->>'body'='';
 ASSERT page->'items'->0->>'replyCount'='1';
 INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(910000001,910000002,true);
 ASSERT interaction_target_context(target,910000002) IS NULL;
 ASSERT NOT interaction_can_comment(target,910000002);
 ASSERT interaction_comments_page(target,910000002,NULL,'newest',NULL,10)->>'error'='unavailable';
 ASSERT interaction_comment_context(target,910000002,child)->>'error'='unavailable';
 UPDATE social_v2_pair SET block_a=false WHERE party_a=910000001 AND party_b=910000002;
 ASSERT interaction_can_comment(target,910000002);
 UPDATE user_credential SET active=false WHERE party_id=910000002;
 ASSERT NOT interaction_can_comment(target,910000002);
 ASSERT interaction_target_context(target,910000002) IS NULL;
 UPDATE fan_club_post SET is_hidden=true WHERE id=910000001;
 ASSERT interaction_target_context(target,910000001) IS NULL;
END $$;
ROLLBACK;
