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
DECLARE target uuid; reply jsonb; parent jsonb; reaction uuid; notif bigint; n integer; edited jsonb; original_events bigint;
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
 -- Newly added mentions refresh an already-read owner comment notification.
 parent:=interaction_command(910000002,target,gen_random_uuid(),'{"operation":"comment.create","body":"Before mention"}');
 PERFORM interaction_dispatch_events(20);
 SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=(parent->>'id')::uuid AND recipient_id=910000001;
 UPDATE notification SET is_read=true,created_at=now()-interval '1 day' WHERE id=notif;
 edited:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit',
   'commentId',parent->>'id','expectedVersion',1,'body','Hello @Owner',
   'mentions',jsonb_build_array(jsonb_build_object('partyId',910000001,'start',6,'end',12))));
 ASSERT NOT edited ? 'error',edited::text;
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT NOT is_read AND notif_type='interaction.mention' AND created_at>now()-interval '1 minute' FROM notification WHERE id=notif);
 ASSERT (SELECT count(*)=1 FROM interaction_notification WHERE comment_id=(parent->>'id')::uuid AND recipient_id=910000001);
 UPDATE notification SET is_read=true WHERE id=notif;
 edited:=interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.edit',
   'commentId',parent->>'id','expectedVersion',2,'body','Hello @Owner, edited again',
   'mentions',jsonb_build_array(jsonb_build_object('partyId',910000001,'start',6,'end',12))));
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT is_read FROM notification WHERE id=notif), 'Unchanged mention must not notify on every edit';
 -- Preference truth table: choose the strongest enabled applicable reason.
 FOR n IN 0..7 LOOP
   PERFORM interaction_preferences(910000001,jsonb_build_object('reactions',true,
     'comments',(n&1)>0,'replies',(n&2)>0,'mentions',(n&4)>0));
   edited:=interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.create',
     'parentId',(SELECT parent_id FROM interaction_comment WHERE id=(reply->>'id')::uuid),'body','Hello @Owner','mentions',jsonb_build_array(jsonb_build_object('partyId',910000001,'start',6,'end',12))));
   ASSERT NOT edited ? 'error',edited::text;
   PERFORM interaction_dispatch_events(20);
   SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=(edited->>'id')::uuid AND recipient_id=910000001;
   IF n=0 THEN ASSERT notif IS NULL;
   ELSE
     ASSERT notif IS NOT NULL;
     ASSERT (SELECT event_kind=CASE WHEN (n&4)>0 THEN 'mention' WHEN (n&2)>0 THEN 'reply' ELSE 'comment' END
       FROM interaction_notification WHERE notification_id=notif), 'Disabled mention must preserve enabled reply/comment';
     ASSERT interaction_notification_visible(notif,910000001);
   END IF;
 END LOOP;
 PERFORM interaction_preferences(910000001,'{"reactions":true,"comments":true,"replies":true,"mentions":true}');
 SELECT id INTO reaction FROM content_reaction_type WHERE code='fire';
 PERFORM interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 PERFORM interaction_dispatch_events(20);
 SELECT notification_id INTO notif FROM interaction_notification WHERE target_id=target AND event_kind='reaction';
 UPDATE notification SET is_read=true,created_at=now()-interval '1 day' WHERE id=notif;
 SELECT count(*) INTO original_events FROM interaction_event;
 PERFORM interaction_command(910000002,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 ASSERT (SELECT count(*)=original_events FROM interaction_event), 'Fresh-key desired-state retry enqueues no activity';
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT is_read FROM notification WHERE id=notif), 'No-op reaction must leave aggregate read';
 PERFORM interaction_command(910000003,target,gen_random_uuid(),jsonb_build_object('operation','reaction.set','reactionTypeId',reaction));
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT NOT is_read AND created_at>now()-interval '1 minute' FROM notification WHERE id=notif), 'New actor refreshes read aggregate';
 UPDATE notification SET is_read=true WHERE id=notif;
 PERFORM interaction_dispatch_events(20);
 ASSERT (SELECT is_read FROM notification WHERE id=notif), 'Completed-event retry cannot resurrect read activity';
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
 -- Publication-owner blocks cannot suppress a valid moderation event for an
 -- otherwise eligible author. Recipient blocking/preferences remain authoritative.
 INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 910000004,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
 ASSERT interaction_is_moderator(910000004);
 parent:=interaction_command(910000003,target,gen_random_uuid(),'{"operation":"comment.create","body":"Moderation delivery"}');
 ASSERT NOT parent ? 'error',parent::text;
 PERFORM interaction_dispatch_events(50);
 edited:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide',
   'commentId',parent->>'id','expectedVersion',1,'reason','Owner hiding'));
 ASSERT edited->>'state'='hidden',edited::text;
 PERFORM interaction_dispatch_events(50);
 ASSERT EXISTS(SELECT 1 FROM interaction_notification WHERE comment_id=(parent->>'id')::uuid
   AND recipient_id=910000003 AND event_kind='moderation'),'Nonmoderator owner hiding still notifies';
 edited:=interaction_command(910000001,target,gen_random_uuid(),jsonb_build_object('operation','comment.restore',
   'commentId',parent->>'id','expectedVersion',2,'reason','Owner restore'));
 ASSERT edited->>'state'='visible',edited::text;
 DELETE FROM notification WHERE id IN(SELECT notification_id FROM interaction_notification
   WHERE comment_id=(parent->>'id')::uuid AND event_kind='moderation');
 PERFORM interaction_block(910000001,910000004,true,0,gen_random_uuid());
 ASSERT interaction_target_context(target,910000004) IS NULL;
 ASSERT interaction_moderation_context(target,910000004) IS NOT NULL;
 edited:=interaction_command(910000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.remove',
   'commentId',parent->>'id','expectedVersion',3,'reason','Platform enforcement'));
 ASSERT edited->>'state'='removed',edited::text;
 PERFORM interaction_dispatch_events(50);
 SELECT notification_id INTO notif FROM interaction_notification WHERE comment_id=(parent->>'id')::uuid
   AND recipient_id=910000003 AND event_kind='moderation';
 ASSERT notif IS NOT NULL,'Owner block must not silently complete moderation without notifying the eligible author';
 ASSERT interaction_notification_visible(notif,910000003);
 ASSERT interaction_destination(910000003,'comment',(parent->>'id')::uuid)->'context'->'comment'->>'state'='removed';
 UPDATE notification SET is_read=true WHERE id=notif;
 PERFORM interaction_dispatch_events(50);
 ASSERT (SELECT is_read FROM notification WHERE id=notif),'Moderation replay must not refresh read state';
 -- A blocked social event still has no privileged delivery path.
 ASSERT NOT interaction_notification_allowed(target,910000001,910000004,'comment');
 PERFORM interaction_block(910000003,910000004,true,0,gen_random_uuid());
 ASSERT NOT interaction_notification_visible(notif,910000003),'Recipient block still hides notification';
END $$;
ROLLBACK;
