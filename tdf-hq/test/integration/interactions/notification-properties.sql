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
DECLARE target uuid; reply jsonb; parent jsonb; reaction uuid; notif bigint; n integer;
BEGIN
 target:=interaction_register('club_post','910000001',910000001);
 parent:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.create','body','Owner parent'));
 reply:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.create',
   'parentId',parent->>'id','body','Hello @Owner','mentions',jsonb_build_array(jsonb_build_object('partyId',910000001,'start',6,'end',12))));
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT count(*) FROM interaction_notification WHERE comment_id=(reply->>'id')::uuid)=1,
   'Owner/reply/mention must not multiply a notification';
 SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=(reply->>'id')::uuid;
 ASSERT interaction_notification_visible(notif,910000001);
 ASSERT NOT interaction_notification_visible(notif,910000003), 'Foreign notification disclosure';
 ASSERT (SELECT event_kind FROM interaction_notification WHERE notification_id=notif)='mention';
 ASSERT (SELECT target_key FROM notification WHERE id=notif)=reply->>'id';
 ASSERT (SELECT target_type FROM notification WHERE id=notif)='interaction_comment';
 SELECT id INTO reaction FROM content_reaction_type WHERE code='fire';
 PERFORM interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 PERFORM interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT count(*) FROM interaction_notification WHERE target_id=target AND event_kind='reaction')=1;
 SELECT notification_id INTO notif FROM interaction_notification WHERE target_id=target AND event_kind='reaction';
 ASSERT (SELECT count(*) FROM interaction_notification_actor WHERE notification_id=notif)=2;
 PERFORM interaction_command(910000002,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}');
 ASSERT interaction_notification_visible(notif,910000001), 'Other active reaction remains';
 PERFORM interaction_command(910000003,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":null}');
 ASSERT NOT interaction_notification_visible(notif,910000001), 'Removed reactions must not survive as actionable activity';
 INSERT INTO social_v2_pair(party_a,party_b,block_a) VALUES(910000001,910000002,true);
 SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=(reply->>'id')::uuid;
 ASSERT NOT interaction_notification_visible(notif,910000001), 'Block must revoke old mention notification';
END $$;
ROLLBACK;
