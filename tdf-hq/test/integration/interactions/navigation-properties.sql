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
INSERT INTO social_v2_preference(party_id,discoverable) VALUES(910000002,true),(910000003,true);
UPDATE interaction_runtime SET enabled=true;
DO $$
DECLARE target uuid; comment_key uuid; result_value jsonb; key_value uuid:=gen_random_uuid();
BEGIN
 target:=interaction_register('club_post','910000001',910000001);
 result_value:=interaction_command(910000002,target,key_value,'{"operation":"comment.create","body":"private retry body"}');
 comment_key:=(result_value->>'id')::uuid;
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_request WHERE result::text LIKE '%private retry body%'), 'Receipts must not retain private text';
 ASSERT interaction_destination(910000003,'comment',comment_key)->>'targetId'=target::text;
 ASSERT interaction_destination(NULL,'comment',comment_key)->>'error'='unavailable';
 ASSERT interaction_destination(910000003,'target',target)->>'key'='910000001';
 ASSERT NOT interaction_mentions_valid(target,910000001,'@Person','[{"partyId":null,"start":0,"end":7}]');
 ASSERT NOT interaction_mentions_valid(target,910000001,'@Person','[{"partyId":910000002,"start":null,"end":7}]');
 ASSERT NOT interaction_mentions_valid(target,910000001,'@Person','[null]');
 result_value:=interaction_mention_candidates(910000001,target,'synthetic',NULL,1);
 ASSERT jsonb_array_length(result_value->'items')=1;
 ASSERT result_value->>'nextCursor'='910000002';
 ASSERT interaction_mention_candidates(910000001,target,'synthetic',910000002,10)->'items'->0->>'partyId'='910000003';
 result_value:=interaction_block(910000001,910000002,true,0,gen_random_uuid());
 ASSERT result_value->>'blocked'='true',result_value::text;
 ASSERT interaction_destination(910000002,'comment',comment_key)->>'error'='unavailable';
 ASSERT jsonb_array_length(interaction_mention_candidates(910000001,target,'synthetic',NULL,10)->'items')=1;
 result_value:=interaction_block(910000001,910000002,false,1,gen_random_uuid());
 ASSERT result_value->>'blocked'='false';
 ASSERT NOT EXISTS(SELECT 1 FROM social_v2_pair WHERE party_a=910000001 AND party_b=910000002 AND (consent_a OR consent_b OR follow_a OR follow_b));
 result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_key,'reason','Test','expectedVersion',1));
 ASSERT result_value->>'state'='hidden';
 ASSERT interaction_destination(910000003,'comment',comment_key)->>'error'='unavailable';
 ASSERT interaction_destination(910000002,'comment',comment_key)->'context'->'comment'->>'body'='';
 ASSERT interaction_moderation_page(910000003,target,NULL,10)->>'error'='unavailable';
 ASSERT interaction_moderation_page(910000001,target,NULL,10)->'items'->0->>'moderationBody'='private retry body';
 ASSERT interaction_preferences(910000003,'{"reactions":false,"comments":true,"replies":true,"mentions":false}')->>'mentions'='false';
 ASSERT interaction_preferences(910000003,'{"reactions":"false","comments":true,"replies":true,"mentions":false}')->>'error'='invalid';
END $$;
ROLLBACK;
