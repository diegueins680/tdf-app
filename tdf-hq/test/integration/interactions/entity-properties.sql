BEGIN;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Entity actor '||n,false,now() FROM generate_series(917000001,917000004) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'interaction-entity-'||n,'not-a-login-hash',true FROM generate_series(917000001,917000004) n;
INSERT INTO social_event(id,organizer_party_id,title,start_time,event_type_id,workflow_state_id)
SELECT 917000001,'917000001','Private event',now(),id,'00000000-0000-4000-8000-000000000232' FROM event_type WHERE code='concert';
INSERT INTO event_moment(id,event_id,author_party_id,author_name,media_url,media_type)
VALUES(917000001,917000001,'917000002','Member','https://example.test/photo.jpg','image');
INSERT INTO event_logistics_member(event_id,party_id,member_role) VALUES(917000001,'917000002','viewer');
INSERT INTO event_invitation(event_id,from_party_id,to_party_id,status) VALUES(917000001,'917000003','917000003','accepted');
INSERT INTO artist_profile(artist_party_id,created_at) VALUES(917000001,now());
INSERT INTO social_sync_post(id,platform,external_post_id,artist_party_id,caption,fetched_at,ingest_source,created_at,updated_at)
VALUES(917000001,'instagram','synthetic-private-update',917000001,'Private ingestion caption',now(),'manual',now(),now());
UPDATE interaction_runtime SET enabled=true WHERE singleton;
DO $$
DECLARE target uuid; result_value jsonb; root uuid; reply uuid; k record;
BEGIN
 FOR k IN SELECT code FROM interaction_entity_kind LOOP
   ASSERT interaction_resolve(k.code,'not-an-id',917000001) IS NULL;
 END LOOP;
 ASSERT interaction_resolve('artist_update','917000001',NULL) IS NULL, 'Importing an artist update grants no anonymous publication';
 ASSERT interaction_resolve('artist_update','917000001',917000001) IS NULL;
 ASSERT interaction_register('artist_update','917000001',917000001) IS NULL;
 UPDATE interaction_entity_kind SET enabled=true WHERE code='artist_update';
 ASSERT interaction_resolve('artist_update','917000001',NULL) IS NULL, 'Capability toggles cannot replace publication authority';
 UPDATE interaction_entity_kind SET enabled=false WHERE code='artist_update';
 ASSERT (SELECT caption='Private ingestion caption' FROM social_sync_post WHERE id=917000001), 'Unavailable target retains imported source';
 ASSERT interaction_event_access(917000001,917000001);
 ASSERT interaction_event_access(917000001,917000002);
 ASSERT NOT interaction_event_access(917000001,917000003), 'Legacy self-invitation is not an access grant';
 ASSERT NOT interaction_event_access(917000001,NULL);
 INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
 SELECT 917000004,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
 PERFORM interaction_block(917000001,917000004,true,0,gen_random_uuid());
 ASSERT interaction_resolve_scoped('event','917000001',917000004,true) IS NULL, 'Moderation cannot invent private-event access';
 INSERT INTO event_logistics_member(event_id,party_id,member_role) VALUES(917000001,'917000004','viewer');
 ASSERT NOT interaction_event_access(917000001,917000004);
 ASSERT interaction_resolve_scoped('event','917000001',917000004,true) IS NOT NULL, 'Valid private grant survives owner block for enforcement';
 DELETE FROM event_logistics_member WHERE event_id=917000001 AND party_id='917000004';
 ASSERT interaction_resolve_scoped('event','917000001',917000004,true) IS NULL, 'Revoked private grant also revokes moderation access';
 PERFORM interaction_block(917000001,917000004,false,(interaction_block_state(917000001,917000004)->>'version')::bigint,gen_random_uuid());

 ASSERT interaction_resolve('event_moment','917000001',917000002) IS NOT NULL;
 DELETE FROM event_logistics_member WHERE event_id=917000001 AND party_id='917000002';
 ASSERT interaction_resolve('event_moment','917000001',917000002) IS NULL, 'Moment authors cannot bypass private parent access revocation';
 UPDATE social_event SET metadata='{"isPublic":true}' WHERE id=917000001;
 ASSERT interaction_resolve('event','917000001',NULL) IS NOT NULL;
 ASSERT interaction_resolve('event_moment','917000001',917000003) IS NOT NULL;
 target:=interaction_register('event_moment','917000001',917000003);
 result_value:=interaction_command(917000003,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":"50900000-0000-4000-8000-000000000003"}');
 ASSERT NOT result_value ? 'error';
 ASSERT (SELECT count(*)=1 FROM engagement_event WHERE actor_party_id=917000003 AND event_type='reaction_added');
 PERFORM interaction_command(917000003,target,gen_random_uuid(),'{"operation":"reaction.set","reactionTypeId":"50900000-0000-4000-8000-000000000003"}');
 ASSERT (SELECT count(*)=1 FROM engagement_event WHERE actor_party_id=917000003 AND event_type='reaction_added'), 'No-op reaction duplicates no first-value evidence';
 result_value:=interaction_command(917000003,target,gen_random_uuid(),'{"operation":"comment.create","body":"A root"}');
 root:=(result_value->>'id')::uuid;
 result_value:=interaction_command(917000004,target,gen_random_uuid(),jsonb_build_object('operation','comment.create','body','A reply','parentId',root));
 reply:=(result_value->>'id')::uuid;
 PERFORM interaction_command(917000002,target,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',root,'expectedVersion',1,'reason','Hide root'));
 ASSERT interaction_comments_page(target,917000004,NULL,'newest',NULL,20)->'items'->0->>'state'='hidden';
 ASSERT interaction_comments_page(target,917000004,NULL,'newest',NULL,20)->'items'->0->>'body'='';
 ASSERT interaction_comments_page(target,917000004,root,'oldest',NULL,20)->'items'->0->>'id'=reply::text;
 ASSERT interaction_summary(917000004,'event_moment','917000001')->>'commentCount'='1';
 PERFORM interaction_block(917000001,917000003,true,0,gen_random_uuid());
 ASSERT interaction_target_context(target,917000003) IS NULL, 'Organizer block applies even when moment owner is a different person';
 DELETE FROM event_moment WHERE id=917000001;
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_reaction WHERE target_id=target), 'Deleted source leaves no active reactions';
 ASSERT NOT EXISTS(SELECT 1 FROM interaction_comment WHERE target_id=target AND (state<>'removed' OR body<>'')), 'Source deletion erases bodies';
 ASSERT (SELECT count(*)=2 FROM interaction_comment WHERE target_id=target), 'Source deletion preserves thread identities';
 ASSERT interaction_target_context(target,917000001) IS NULL;
 ASSERT EXISTS(SELECT 1 FROM interaction_audit WHERE target_id=target AND operation='target.deleted');
END $$;
ROLLBACK;
