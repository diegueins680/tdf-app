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
 ASSERT interaction_resolve('event','917000001',917000001)->>'route'='/social/eventos/917000001';
 ASSERT interaction_resolve('event','917000001',917000002)->>'route'='/social/eventos/917000001';
 ASSERT interaction_resolve('event_moment','917000001',917000002)->>'route'='/social/eventos/917000001?moment=917000001';
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
 ASSERT interaction_resolve('event','917000001',NULL)->>'route'='/eventos/917000001';
 ASSERT interaction_resolve('event_moment','917000001',917000003)->>'route'='/eventos/917000001?moment=917000001';
 ASSERT interaction_resolve('event_moment','917000001',917000003) IS NOT NULL;
 -- Cancelled public events are still readable even though sharing is disabled.
 BEGIN
 UPDATE social_event SET workflow_state_id='00000000-0000-4000-8000-000000000239' WHERE id=917000001;
 ASSERT interaction_resolve('event','917000001',NULL)->>'public'='false';
 ASSERT interaction_resolve('event','917000001',NULL)->>'route'='/eventos/917000001';
 ASSERT interaction_resolve('event_moment','917000001',NULL)->>'route'='/eventos/917000001?moment=917000001';
 RAISE EXCEPTION 'Rollback cancelled route fixture';
 EXCEPTION WHEN raise_exception THEN ASSERT SQLERRM='Rollback cancelled route fixture';
 END;
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
-- Label records use current catalog grants; created_by remains audit metadata.
INSERT INTO party_security_role(party_id,role_id,approval_mode,active,created_at,version)
SELECT 917000001,id,'bootstrap',true,now(),1 FROM security_role WHERE code='admin';
DO $$
DECLARE kind_value text; source_id uuid; target_id_value uuid; comment_value jsonb; result_value jsonb; grant_id uuid;
BEGIN
 FOR kind_value IN SELECT unnest(ARRAY['recording','recording_session','record_release']) LOOP
   EXECUTE format('SELECT id FROM %I WHERE active ORDER BY id LIMIT 1',kind_value) INTO source_id;
   ASSERT source_id IS NOT NULL, 'Nonempty production catalog fixture required';
   EXECUTE format('UPDATE %I SET created_by=$1 WHERE id=$2',kind_value) USING 917000001,source_id;
   UPDATE party_security_role SET active=true WHERE party_id=917000001;
   ASSERT interaction_catalog_manager(917000001);
   result_value:=interaction_resolve(kind_value,source_id::text,917000001);
   ASSERT result_value->>'ownerId' IS NULL, 'Creator provenance is not publication ownership';
   ASSERT result_value->>'canManage'='true';
   target_id_value:=interaction_register(kind_value,source_id::text,917000001);
   comment_value:=interaction_command(917000002,target_id_value,gen_random_uuid(),'{"operation":"comment.create","body":"Institutional catalog discussion","mentions":[]}');
   ASSERT NOT comment_value ? 'error',comment_value::text;
   UPDATE party_security_role SET active=false WHERE party_id=917000001;
   ASSERT interaction_resolve(kind_value,source_id::text,917000001)->>'canManage'='false';
   ASSERT interaction_command(917000001,target_id_value,gen_random_uuid(),jsonb_build_object('operation','settings.update','commentPolicy','off','expectedVersion',(SELECT version FROM interaction_target WHERE id=target_id_value),'mentionedPartyIds','[]'::jsonb))->>'error'='forbidden';
   ASSERT interaction_command(917000001,target_id_value,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_value->>'id','expectedVersion',1,'reason','Revoked creator'))->>'error'='forbidden';
   ASSERT interaction_resolve(kind_value,source_id::text,917000004)->>'canManage'='true', 'Another current catalog administrator retains authority';
   result_value:=interaction_command(917000004,target_id_value,gen_random_uuid(),jsonb_build_object('operation','comment.hide','commentId',comment_value->>'id','expectedVersion',1,'reason','Current catalog administrator'));
   ASSERT result_value->>'state'='hidden',result_value::text;
   SELECT rp.id INTO grant_id FROM role_permission rp JOIN security_role r ON r.id=rp.role_id
     JOIN security_permission p ON p.id=rp.permission_id WHERE r.code='admin' AND p.code='catalog.update';
   UPDATE role_permission SET active=false WHERE id=grant_id;
   ASSERT NOT interaction_catalog_manager(917000004), 'Role alone does not replace current catalog capability';
   ASSERT interaction_resolve(kind_value,source_id::text,917000004)->>'canManage'='false';
   UPDATE role_permission SET active=true WHERE id=grant_id;
 END LOOP;
END $$;
ROLLBACK;
