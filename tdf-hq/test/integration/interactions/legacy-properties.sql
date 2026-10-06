-- Rehearses non-empty legacy cutover, activation retry, pause safety and erasure.
BEGIN;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Migration actor '||n,false,now() FROM generate_series(915000001,915000003) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-migration-'||n,'not-a-login-hash',true FROM generate_series(915000001,915000003) n;
INSERT INTO fan_club(id,artist_party_id,name) VALUES(915000001,915000001,'Migration club');
INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at) VALUES(915000002,915000001,now()),(915000003,915000001,now());
INSERT INTO fan_club_post(id,club_id,fan_party_id,parent_id,title,content,media_urls,created_at)
VALUES(915000001,915000001,915000001,NULL,'Root','Root body',NULL,now()),
 (915000002,915000001,915000002,915000001,'Legacy reply',repeat('a',4096),'https://example.test/image.jpg',now()),
 (915000003,915000001,915000003,915000002,NULL,'Nested legacy reply',NULL,now());
INSERT INTO fan_club_member_profile(id,party_id,club_id) VALUES(915000001,915000002,915000001);
INSERT INTO fan_club_memory(id,member_profile_id,title) VALUES(915000001,915000001,'Legacy memory');
INSERT INTO fan_club_post_reaction(id,post_id,reactor_party_id,reaction_type_id,created_at)
VALUES('91500000-0000-4000-8000-000000000001',915000001,915000002,'50900000-0000-4000-8000-000000000001',now()),
 ('91500000-0000-4000-8000-000000000002',915000002,915000003,'50900000-0000-4000-8000-000000000004',now());
INSERT INTO fan_club_memory_reaction(id,memory_id,reactor_party_id,reaction_type_id,created_at)
VALUES('91500000-0000-4000-8000-000000000003',915000001,915000001,'50900000-0000-4000-8000-000000000002',now());
INSERT INTO social_event(id,organizer_party_id,title,start_time,event_type_id,workflow_state_id)
SELECT 915000001,'915000001','Private migration event',now(),id,'00000000-0000-4000-8000-000000000232' FROM event_type WHERE code='concert';
INSERT INTO event_moment(id,event_id,author_party_id,author_name,media_url,media_type)
VALUES(915000001,915000001,'915000001','Migration actor','https://example.test/photo.jpg','image');
INSERT INTO event_moment_comment(id,moment_id,author_party_id,author_name,body)
VALUES(915000001,915000001,'915000001','Migration actor','Original moment comment');
INSERT INTO event_moment_reaction(id,moment_id,reactor_party_id,reaction_type_id)
VALUES('91500000-0000-4000-8000-000000000004',915000001,'915000001','50800000-0000-4000-8000-000000000002');
-- Anonymous historical attribution must never be silently hidden after cutover.
DO $$ BEGIN
 BEGIN
   INSERT INTO event_moment_comment(moment_id,author_name,body) VALUES(915000001,'Historical guest','Preserve this text');
   UPDATE interaction_runtime SET enabled=true WHERE singleton;
   RAISE EXCEPTION 'Anonymous source unexpectedly activated';
 EXCEPTION WHEN raise_exception THEN
   ASSERT SQLERRM='Legacy interactions require identity/parent reconciliation before activation';
 END;
 ASSERT NOT (SELECT activated_once FROM interaction_runtime WHERE singleton);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_legacy_cutover);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_legacy_mapping);
END $$;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
DO $$
DECLARE target uuid; reply uuid; child uuid; moment_comment uuid; result_value jsonb; before_count bigint; new_alias text; legacy_choice uuid; canonical_choice uuid; moment_target uuid;
BEGIN
 ASSERT (SELECT activated_once FROM interaction_runtime WHERE singleton);
 ASSERT (SELECT source_counts=migrated_counts FROM interaction_legacy_cutover);
 ASSERT (SELECT source_counts->>'clubReplies'='2' AND source_counts->>'momentComments'='1' AND source_counts->>'postReactions'='2' FROM interaction_legacy_cutover);
 SELECT id INTO target FROM interaction_target WHERE entity_kind='club_post' AND entity_key='915000001';
 SELECT comment_id INTO reply FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id='915000002';
 SELECT comment_id INTO child FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id='915000003';
 ASSERT (SELECT length(body)=4096 AND root_id=id AND parent_id IS NULL FROM interaction_comment WHERE id=reply);
 ASSERT (SELECT root_id=reply AND parent_id=reply AND depth=1 FROM interaction_comment WHERE id=child);
 ASSERT (SELECT reaction_type_id='50900000-0000-4000-8000-000000000004' FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_post' AND t.entity_key='915000002');
 ASSERT (SELECT reaction_type_id='50900000-0000-4000-8000-000000000002' FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='event_moment' AND t.entity_key='915000001');
 SELECT count(*) INTO before_count FROM interaction_comment;
 PERFORM interaction_migrate_legacy();
 ASSERT (SELECT count(*)=before_count FROM interaction_comment), 'Reapplying conversion duplicates no comments';
 result_value:=interaction_legacy_reaction_summary(915000001,'club_post','915000001');
 ASSERT result_value->>'rsTotal'='1';
 result_value:=interaction_legacy_command(915000003,'club_post','915000001',jsonb_build_object('operation','legacy.reaction','reactionTypeId','50900000-0000-4000-8000-000000000002'));
 ASSERT NOT result_value ? 'error';
 ASSERT interaction_legacy_reaction_summary(915000001,'club_post','915000001')->>'rsTotal'='2';
 result_value:=interaction_legacy_command(915000003,'club_post','915000001',jsonb_build_object('operation','legacy.comment','body','New old-client reply','title','Preserved title','mediaUrls',jsonb_build_array('https://example.test/new.jpg')));
 ASSERT result_value->>'fcpContent'='New old-client reply';
 ASSERT NOT EXISTS(SELECT 1 FROM fan_club_post WHERE id=(result_value->>'fcpId')::bigint), 'Canonical writer does not duplicate legacy bodies';
 new_alias:=result_value->>'fcpId';
 result_value:=interaction_legacy_command(915000003,'club_post',new_alias,jsonb_build_object('operation','legacy.comment','body','Nested new legacy alias','artistId',915000001));
 ASSERT result_value->>'fcpContent'='Nested new legacy alias';
 ASSERT result_value->>'fcpParentId'=new_alias, 'Legacy DTO preserves immediate requested parent';
 ASSERT EXISTS(SELECT 1 FROM interaction_comment WHERE body='Nested new legacy alias' AND parent_id=(SELECT comment_id FROM interaction_legacy_mapping WHERE legacy_id=new_alias AND legacy_kind='club_reply'));
 result_value:=interaction_legacy_command(915000002,'club_post',new_alias,jsonb_build_object('operation','legacy.reaction','artistId',915000001,'reactionTypeId','50900000-0000-4000-8000-000000000001'));
 ASSERT NOT result_value ? 'error',result_value::text;
 ASSERT interaction_legacy_reaction_summary(915000002,'club_post',new_alias)->>'rsTotal'='1';
 ASSERT interaction_legacy_reaction_summary(915000002,'club_post','915000001')->>'rsTotal'='2', 'Reply reactions do not overwrite the parent post slot';
 ASSERT interaction_legacy_command(915000002,'club_post',new_alias,jsonb_build_object('operation','legacy.reaction','artistId',915000003,'reactionTypeId','50900000-0000-4000-8000-000000000001'))->>'error'='unavailable';
 ASSERT interaction_legacy_command(915000001,'club_post',new_alias,'{"operation":"legacy.hide","artistId":915000001}')->>'state'='hidden';
 ASSERT interaction_legacy_command(915000002,'club_post',new_alias,jsonb_build_object('operation','legacy.reaction','artistId',915000001,'reactionTypeId','50900000-0000-4000-8000-000000000001'))->>'error'='unavailable', 'Hidden aliases cannot accept reactions';
 ASSERT interaction_legacy_command(915000001,'club_post',new_alias,'{"operation":"legacy.restore","artistId":915000001}')->>'state'='visible';
 ASSERT interaction_legacy_command(915000001,'club_post',new_alias,'{"operation":"legacy.hide","artistId":915000003}')->>'error'='unavailable';
 BEGIN
   INSERT INTO event_moment_comment(moment_id,author_name,body) VALUES(915000001,'No actor','Bypass');
   RAISE EXCEPTION 'Legacy writer bypassed the fence';
 EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
 result_value:=interaction_command(915000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.delete','commentId',reply,'expectedVersion',1));
 ASSERT result_value->>'state'='deleted';
 ASSERT (SELECT body='' FROM interaction_comment WHERE id=reply);
 ASSERT (SELECT content='' AND media_urls IS NULL AND title IS NULL FROM fan_club_post WHERE id=915000002);
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_post' AND t.entity_key='915000002'), 'Deleted migrated replies retire independent reaction slots';
 ASSERT (SELECT NOT source_value ? 'mediaUrls' AND NOT source_value ? 'title' FROM interaction_legacy_mapping WHERE comment_id=reply);
 ASSERT (SELECT body='Nested legacy reply' AND parent_id=reply FROM interaction_comment WHERE id=child);
 SELECT comment_id INTO moment_comment FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id=new_alias;
 result_value:=interaction_command(915000003,target,gen_random_uuid(),jsonb_build_object('operation','comment.delete','commentId',moment_comment,'expectedVersion',3));
 ASSERT result_value->>'state'='deleted',result_value::text;
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_post' AND t.entity_key=new_alias);
 ASSERT EXISTS(SELECT 1 FROM interaction_target WHERE entity_kind='club_post' AND entity_key=new_alias AND retired_at IS NOT NULL);
 SELECT comment_id INTO moment_comment FROM interaction_legacy_mapping WHERE legacy_kind='moment_comment' AND legacy_id='915000001';
 result_value:=interaction_command(915000001,(SELECT target_id FROM interaction_comment WHERE id=moment_comment),gen_random_uuid(),jsonb_build_object('operation','comment.delete','commentId',moment_comment,'expectedVersion',1));
 ASSERT result_value->>'state'='deleted';
 ASSERT (SELECT body='' AND author_name='' FROM event_moment_comment WHERE id=915000001);
 -- All old-client moment reaction mappings exercise the same canonical slot.
 SELECT id INTO moment_target FROM interaction_target WHERE entity_kind='event_moment' AND entity_key='915000001';
 FOR legacy_choice,canonical_choice IN SELECT r.id,choice.id FROM reaction_type r JOIN content_reaction_type choice
   ON choice.code=CASE r.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE r.code END LOOP
   result_value:=interaction_legacy_command(915000001,'event_moment','915000001',jsonb_build_object('operation','legacy.reaction','reactionTypeId',legacy_choice,'active',true));
   ASSERT NOT result_value ? 'error',result_value::text;
   ASSERT (SELECT reaction_type_id=canonical_choice FROM interaction_reaction WHERE target_id=moment_target AND actor_id=915000001);
   UPDATE catalog_definition SET active=false WHERE id IN
     (SELECT catalog_id FROM reaction_type WHERE id=legacy_choice UNION SELECT catalog_id FROM content_reaction_type WHERE id=canonical_choice);
   result_value:=interaction_legacy_command(915000001,'event_moment','915000001',jsonb_build_object('operation','legacy.reaction','reactionTypeId',legacy_choice,'active',false));
   ASSERT NOT result_value ? 'error',result_value::text;
   ASSERT NOT EXISTS(SELECT 1 FROM interaction_reaction WHERE target_id=moment_target AND actor_id=915000001);
   ASSERT NOT EXISTS(SELECT 1 FROM interaction_reaction_total WHERE target_id=moment_target AND total<>0);
   ASSERT interaction_legacy_command(915000001,'event_moment','915000001',jsonb_build_object('operation','legacy.reaction','reactionTypeId',legacy_choice,'active',true)) ? 'error';
   UPDATE catalog_definition SET active=true WHERE id IN
     (SELECT catalog_id FROM reaction_type WHERE id=legacy_choice UNION SELECT catalog_id FROM content_reaction_type WHERE id=canonical_choice);
 END LOOP;
 UPDATE interaction_runtime SET enabled=false WHERE singleton;
 ASSERT (SELECT activated_once FROM interaction_runtime WHERE singleton);
 ASSERT interaction_legacy_command(915000003,'club_post','915000001','{"operation":"legacy.comment","body":"Paused"}')->>'error'='disabled';
 BEGIN UPDATE interaction_runtime SET activated_once=false WHERE singleton; RAISE EXCEPTION 'Activation history reset';
 EXCEPTION WHEN raise_exception THEN ASSERT SQLERRM='Interaction activation history cannot be reset'; END;
 INSERT INTO interaction_audit(actor_id,operation) VALUES(915000001,'test.audit');
 BEGIN UPDATE interaction_audit SET reason='Tamper'; RAISE EXCEPTION 'Audit updated';
 EXCEPTION WHEN object_not_in_prerequisite_state THEN NULL; END;
END $$;
ROLLBACK;
