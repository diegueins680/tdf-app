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
UPDATE interaction_runtime SET enabled=true;
DO $$
DECLARE target uuid; response jsonb; created jsonb; child jsonb; command jsonb;
 request_id uuid:=gen_random_uuid(); root uuid; reaction uuid;
BEGIN
 target:=interaction_register('club_post','910000001',910000001);
 command:=jsonb_build_object('operation','comment.create','body','Hello @Synthetic','mentions',
   jsonb_build_array(jsonb_build_object('partyId',910000003,'start',6,'end',16)));
 created:=interaction_command(910000002,target,request_id,command);
 ASSERT created->>'state'='visible',created::text;
 root:=(created->>'id')::uuid;
 ASSERT jsonb_array_length(created->'mentions')=1;
 ASSERT interaction_command(910000002,target,request_id,command)->>'replay'='true';
 ASSERT (SELECT count(*) FROM interaction_comment WHERE target_id=target)=1;
 ASSERT interaction_command(910000002,target,request_id,command||'{"body":"Different"}')->>'error'='request_key_conflict';
 ASSERT interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit',
   'commentId',root,'expectedVersion',1,'body','Attack','mentions','[]'::jsonb))->>'error'='forbidden';
 child:=interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.create',
   'parentId',root,'body','A reply','mentions','[]'::jsonb));
 ASSERT child->>'parentId'=root::text,child::text;
 -- Creation policy changes do not revoke an existing author's edit permission.
 UPDATE interaction_target SET comment_policy='off' WHERE id=target;
 response:=interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit',
   'commentId',child->>'id','expectedVersion',1,'body','Correct my reply','mentions','[]'::jsonb));
 ASSERT response->>'body'='Correct my reply',response::text;
 ASSERT interaction_command(910000003,target,gen_random_uuid(),'{"operation":"comment.create","body":"Off"}')->>'error'='comments_not_allowed';
 UPDATE interaction_target SET comment_policy='mentioned' WHERE id=target;
 response:=interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit',
   'commentId',child->>'id','expectedVersion',2,'body','Correct again','mentions','[]'::jsonb));
 ASSERT response->>'version'='3',response::text;
 UPDATE interaction_target SET comment_policy='everyone' WHERE id=target;
 response:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide',
   'commentId',root,'expectedVersion',1,'reason','Owner hide'));
 ASSERT response->>'state'='hidden',response::text;
 ASSERT response->>'body'='';
 ASSERT interaction_command(910000002,target,request_id,command)->>'body'='', 'Replay leaked hidden text';
 response:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.restore',
   'commentId',root,'expectedVersion',2,'reason','Owner restore'));
 ASSERT response->>'state'='visible',response::text;
 ASSERT response->>'body'='Hello @Synthetic';
 ASSERT interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.remove',
   'commentId',root,'expectedVersion',3,'reason','Owner is not an administrator'))->>'error'='forbidden';
 response:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.delete',
   'commentId',root,'expectedVersion',3));
 ASSERT response->>'state'='deleted',response::text;
 ASSERT (SELECT count(*) FROM interaction_comment WHERE root_id=root)=2;
 ASSERT interaction_command(910000002,target,request_id,command)->>'body'='', 'Replay leaked deleted text';
 SELECT r.id INTO reaction FROM content_reaction_type r WHERE NOT EXISTS(
   SELECT 1 FROM interaction_reaction_choice c WHERE c.reaction_type_id=r.id) AND r.active LIMIT 1;
 ASSERT reaction IS NOT NULL;
 response:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 ASSERT response->>'error'='invalid_reaction', 'Only explicitly selectable reactions may be newly chosen';
 SELECT id INTO reaction FROM content_reaction_type WHERE code='like';
 response:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 ASSERT response->>'reactionTypeId'=reaction::text,response::text;
 -- Losing follow eligibility permits only withdrawal, never a new or changed reaction.
 DELETE FROM fan_follow WHERE fan_party_id=910000002 AND artist_party_id=910000001;
 ASSERT NOT interaction_domain_write(target,910000002);
 response:=interaction_summary(910000002,'club_post','910000001');
 ASSERT response->>'canReact'='true' AND response->>'myReactionTypeId'=reaction::text;
 ASSERT NOT EXISTS(SELECT 1 FROM jsonb_array_elements(response->'reactions') r WHERE (r->>'selectable')::boolean);
 ASSERT interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction))->>'error'='invalid';
 ASSERT interaction_command(910000003,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}')->'reactionTypeId'='null'::jsonb;
 ASSERT EXISTS(SELECT 1 FROM interaction_reaction WHERE target_id=target AND actor_id=910000002), 'Another actor cannot withdraw my reaction';
 response:=interaction_command(910000002,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}');
 ASSERT response->'reactionTypeId'='null'::jsonb;
 ASSERT (SELECT count(*) FROM interaction_reaction WHERE target_id=target)=0;
 ASSERT (SELECT coalesce(sum(total),0) FROM interaction_reaction_total WHERE target_id=target)=0;
 response:=interaction_summary(910000002,'club_post','910000001');
 ASSERT response->>'canReact'='false' AND response->'myReactionTypeId'='null'::jsonb;
 ASSERT interaction_command(910000002,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}')->'reactionTypeId'='null'::jsonb;
 INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(910000002,910000001,now());
 ASSERT interaction_summary(910000002,'club_post','910000001')->'myReactionTypeId'='null'::jsonb, 'Restoring eligibility cannot restore a withdrawn reaction';
 ASSERT interaction_summary(910000002,'club_post','910000001')->>'canReact'='true';
 ASSERT interaction_command(910000002,target,gen_random_uuid(),'{"operation":"comment.create","body":"Forged","actorId":910000001}')->>'error'='invalid';
 INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(910000001,910000002,true);
 ASSERT interaction_command(910000002,target,request_id,command)->>'error'='unavailable';
END $$;
ROLLBACK;
