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
DECLARE target uuid; comment_key uuid; result_value jsonb; key_value uuid:=gen_random_uuid(); bits integer; eligible boolean; editable uuid; mention_value jsonb; notif bigint; before_count bigint;
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
 -- Autocomplete and authoritative writes share the entire privacy truth table.
 result_value:=interaction_command(910000001,target,gen_random_uuid(),'{"operation":"comment.create","body":"Editable original"}');
 editable:=(result_value->>'id')::uuid;
 mention_value:='[{"partyId":910000003,"start":0,"end":10}]';
 -- Eight unblocked preference/consent states and two blocked states; the DB
 -- correctly forbids consent alongside a block.
 FOR bits IN 0..9 LOOP
   UPDATE social_v2_preference SET discoverable=(bits&1)>0 WHERE party_id=910000003;
   INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b,block_a,block_b)
   VALUES(910000001,910000003,(bits&2)>0,(bits&4)>0,(bits&8)>0,false)
   ON CONFLICT(party_a,party_b) DO UPDATE SET consent_a=excluded.consent_a,consent_b=excluded.consent_b,block_a=excluded.block_a,block_b=false;
   eligible:=(bits&8)=0 AND ((bits&1)>0 OR ((bits&2)>0 AND (bits&4)>0));
   ASSERT interaction_mentions_valid(target,910000001,'@Synthetic',mention_value)=eligible,'Mention privacy truth table';
   ASSERT EXISTS(SELECT 1 FROM jsonb_array_elements(interaction_mention_candidates(910000001,target,'synthetic',NULL,20)->'items') p
     WHERE p->>'partyId'='910000003')=eligible,'Selector and write eligibility diverged';
   SELECT count(*) INTO before_count FROM interaction_comment WHERE target_id=target;
   result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.create','body','@Synthetic','mentions',mention_value));
   IF eligible THEN ASSERT result_value->>'state'='visible',result_value::text;
   ELSE
     ASSERT result_value->>'error'='invalid',result_value::text;
     ASSERT (SELECT count(*)=before_count FROM interaction_comment WHERE target_id=target),'Rejected mention persisted a comment';
   END IF;
 END LOOP;
 UPDATE social_v2_preference SET discoverable=false WHERE party_id=910000003;
 UPDATE social_v2_pair SET consent_a=false,consent_b=false,block_a=false,block_b=false WHERE party_a=910000001 AND party_b=910000003;
 ASSERT NOT interaction_mentions_valid(target,910000001,'@Self','[{"partyId":910000001,"start":0,"end":5}]');
 ASSERT NOT interaction_mentions_valid(target,910000001,'@Absent','[{"partyId":910000005,"start":0,"end":7}]'),'Absent preference is private';
 result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit','commentId',editable,
   'expectedVersion',1,'body','@Synthetic','mentions',mention_value));
 ASSERT result_value->>'error'='invalid',result_value::text;
 ASSERT (SELECT body='Editable original' AND version=1 FROM interaction_comment WHERE id=editable),'Rejected edit changed content';
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_comment_mention WHERE comment_id=editable);
 result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','settings.update','commentPolicy','mentioned',
   'expectedVersion',(SELECT version FROM interaction_target WHERE id=target),'mentionedPartyIds',jsonb_build_array(910000003)));
 ASSERT result_value->>'error'='invalid',result_value::text;
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_target_mention WHERE target_id=target),'Owner policy bypassed mention privacy';
 PERFORM interaction_dispatch_events(50);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_notification WHERE target_id=target AND recipient_id=910000003 AND event_kind='mention'),
   'Queued mentions must recheck privacy at delivery';
 UPDATE social_v2_pair SET consent_a=true,consent_b=true WHERE party_a=910000001 AND party_b=910000003;
 result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit','commentId',editable,
   'expectedVersion',1,'body','@Synthetic','mentions',mention_value));
 ASSERT result_value->>'state'='visible',result_value::text;
 PERFORM interaction_dispatch_events(50);
 SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=editable AND recipient_id=910000003 AND event_kind='mention';
 ASSERT notif IS NOT NULL AND interaction_notification_visible(notif,910000003),'Mutual connection allows private mention';
 UPDATE social_v2_pair SET consent_a=false WHERE party_a=910000001 AND party_b=910000003;
 ASSERT NOT interaction_notification_visible(notif,910000003),'Connection revocation must hide private mention activity';
 result_value:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit','commentId',editable,
   'expectedVersion',2,'body','Remove private mention','mentions','[]'::jsonb));
 ASSERT result_value->>'state'='visible',result_value::text;
 UPDATE social_v2_preference SET discoverable=true WHERE party_id=910000003;
 UPDATE social_v2_pair SET consent_a=false,consent_b=false WHERE party_a=910000001 AND party_b=910000003;
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
