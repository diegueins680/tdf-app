-- Only records admitted to the public editorial feed may expose discussions.
-- Existing engagement stays intact and becomes inaccessible while withdrawn.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE INDEX IF NOT EXISTS interaction_collection_recording_source ON collection_recording(recording_id,collection_id);
CREATE INDEX IF NOT EXISTS interaction_collection_session_source ON collection_session(session_id,collection_id);
CREATE INDEX IF NOT EXISTS interaction_collection_release_source ON collection_release(release_id,collection_id);
CREATE OR REPLACE FUNCTION interaction_resolve_scoped(kind text,entity text,actor bigint,moderation boolean)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE owner_id bigint; title_value text; route_value text; visible boolean:=false;
 public_value boolean:=false; manage boolean:=false; club bigint; profile uuid;
 parent_context jsonb; k interaction_entity_kind%ROWTYPE; legacy_reply interaction_comment%ROWTYPE;
BEGIN
 moderation:=coalesce(moderation,false);
 IF moderation AND NOT interaction_is_moderator(actor) THEN RETURN NULL; END IF;
 SELECT * INTO k FROM interaction_entity_kind WHERE code=kind AND enabled;
 IF NOT FOUND OR entity IS NULL OR length(entity)>128 THEN RETURN NULL; END IF;
 entity:=interaction_normalize_key(kind,entity); IF entity IS NULL THEN RETURN NULL; END IF;
 CASE kind
 WHEN 'club_post' THEN
   -- New legacy IDs are stable aliases of canonical replies, never duplicate posts.
   SELECT c.* INTO legacy_reply FROM interaction_legacy_mapping m JOIN interaction_comment c ON c.id=m.comment_id
     WHERE m.legacy_kind='club_reply' AND m.legacy_id=entity;
   IF FOUND THEN
     parent_context:=CASE WHEN moderation THEN interaction_moderation_context(legacy_reply.target_id,actor)
       ELSE interaction_target_context(legacy_reply.target_id,actor) END;
     IF parent_context IS NULL OR legacy_reply.state<>'visible' OR NOT interaction_actor_live(legacy_reply.author_id)
       OR (NOT moderation AND interaction_blocked(actor,legacy_reply.author_id)) THEN RETURN NULL; END IF;
     RETURN parent_context||jsonb_build_object('kind',kind,'key',entity,'ownerId',legacy_reply.author_id,
       'title',left(legacy_reply.body,160),'route','/conversacion/comment/'||legacy_reply.id,
       'reactable',k.reactable,'commentable',false,'shareable',k.shareable);
   END IF;
   SELECT p.fan_party_id,coalesce(p.title,'Publicación'),c.id,
     '/fans/clubs/'||c.artist_party_id||'?post='||p.id
   INTO owner_id,title_value,club,route_value
   FROM fan_club_post p JOIN fan_club c ON c.id=p.club_id
   WHERE p.id=entity::bigint AND NOT p.is_hidden;
   visible:=FOUND AND interaction_actor_live(actor);
   IF EXISTS(SELECT 1 FROM fan_club_post WHERE id=entity::bigint AND parent_id IS NOT NULL)
     AND EXISTS(SELECT 1 FROM interaction_runtime WHERE activated_once) THEN
     SELECT c.* INTO legacy_reply FROM interaction_legacy_mapping m JOIN interaction_comment c ON c.id=m.comment_id
       WHERE m.legacy_kind='club_reply' AND m.legacy_id=entity;
     visible:=visible AND FOUND AND legacy_reply.state='visible'
       AND CASE WHEN moderation THEN interaction_moderation_context(legacy_reply.target_id,actor) ELSE interaction_target_context(legacy_reply.target_id,actor) END IS NOT NULL;
     k.commentable:=false;
     route_value:='/conversacion/comment/'||legacy_reply.id;
   END IF;
 WHEN 'club_memory' THEN
   SELECT p.party_id,m.title,p.club_id,'/fans/clubs/'||c.artist_party_id||'?memory='||m.id
   INTO owner_id,title_value,club,route_value FROM fan_club_memory m
   JOIN fan_club_member_profile p ON p.id=m.member_profile_id JOIN fan_club c ON c.id=p.club_id
   WHERE m.id=entity::bigint AND NOT m.is_hidden AND NOT m.is_deleted;
   visible:=FOUND AND interaction_actor_live(actor);
 WHEN 'event' THEN
   SELECT CASE WHEN e.organizer_party_id ~ '^[1-9][0-9]{0,17}$' THEN e.organizer_party_id::bigint END,
     e.title,(CASE WHEN v.id IS NULL THEN '/social/eventos/' ELSE '/eventos/' END)||e.id,coalesce(v.public_share_eligible,false)
   INTO owner_id,title_value,route_value,public_value FROM social_event e
   LEFT JOIN directory_public_rsvp_event v ON v.id=e.id WHERE e.id=entity::bigint;
   visible:=FOUND AND EXISTS(SELECT 1 FROM social_event e WHERE e.id=entity::bigint AND interaction_event_access_scoped(e.id,actor,moderation));
 WHEN 'event_moment' THEN
   SELECT CASE WHEN m.author_party_id ~ '^[1-9][0-9]{0,17}$' THEN m.author_party_id::bigint END,
     coalesce(m.caption,'Momento'),(CASE WHEN v.id IS NULL THEN '/social/eventos/' ELSE '/eventos/' END)||e.id||'?moment='||m.id,
     coalesce(v.public_share_eligible,false)
   INTO owner_id,title_value,route_value,public_value FROM event_moment m
   JOIN social_event e ON e.id=m.event_id LEFT JOIN directory_public_rsvp_event v ON v.id=e.id
   WHERE m.id=entity::bigint;
   -- A moment's author does not gain access to its private parent event.
   visible:=FOUND AND EXISTS(SELECT 1 FROM event_moment m WHERE m.id=entity::bigint AND interaction_event_access_scoped(m.event_id,actor,moderation));
 WHEN 'recording' THEN
   SELECT NULL::bigint,r.title_es,'/records?recording='||r.id INTO owner_id,title_value,route_value
   FROM recording r JOIN workflow_state s ON s.id=r.workflow_state_id
   JOIN workflow_definition w ON w.id=s.workflow_id AND w.code='catalog-publication'
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published'
     AND EXISTS(SELECT 1 FROM collection_recording m JOIN editorial_collection c ON c.id=m.collection_id
       WHERE m.recording_id=r.id AND c.active AND c.collection_type='recording' AND c.workflow_state_id=s.id);
   public_value:=FOUND; visible:=public_value;
 WHEN 'recording_session' THEN
   SELECT NULL::bigint,r.title_es,'/records?session='||r.id INTO owner_id,title_value,route_value
   FROM recording_session r JOIN workflow_state s ON s.id=r.workflow_state_id
   JOIN workflow_definition w ON w.id=s.workflow_id AND w.code='catalog-publication'
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published'
     AND EXISTS(SELECT 1 FROM collection_session m JOIN editorial_collection c ON c.id=m.collection_id
       WHERE m.session_id=r.id AND c.active AND c.collection_type='session' AND c.workflow_state_id=s.id);
   public_value:=FOUND; visible:=public_value;
 WHEN 'record_release' THEN
   SELECT NULL::bigint,r.title_es,'/records?release='||r.id INTO owner_id,title_value,route_value
   FROM record_release r JOIN workflow_state s ON s.id=r.workflow_state_id
   JOIN workflow_definition w ON w.id=s.workflow_id AND w.code='catalog-publication'
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published'
     AND EXISTS(SELECT 1 FROM collection_release m JOIN editorial_collection c ON c.id=m.collection_id
       WHERE m.release_id=r.id AND c.active AND c.collection_type='release' AND c.workflow_state_id=s.id);
   public_value:=FOUND; visible:=public_value;
 WHEN 'artist_release' THEN
   SELECT r.artist_party_id,r.title,'/artista/'||r.artist_party_id||'?release='||r.id
   INTO owner_id,title_value,route_value FROM artist_release r
   JOIN artist_profile a ON a.artist_party_id=r.artist_party_id WHERE r.id=entity::bigint;
   public_value:=FOUND; visible:=public_value;
 WHEN 'artist_update' THEN
   -- Imported social-sync records have no publication/review authority. Artist
   -- association, a permalink, or successful ingestion cannot make them public.
   -- A future public update model requires its own explicit source adapter.
   RETURN NULL;
 WHEN 'directory_profile' THEN
   SELECT p.subject_party_id,p.public_name,'/directorio/'||p.slug,p.id,
     EXISTS(SELECT 1 FROM directory_public_profile v WHERE v.id=p.id)
   INTO owner_id,title_value,route_value,profile,public_value
   FROM directory_profile p WHERE p.id=entity::uuid AND p.canonical_profile_id IS NULL
     AND p.archived_at IS NULL AND p.suspended_at IS NULL AND p.moderation_status='allowed';
   visible:=FOUND AND public_value;
 WHEN 'classified' THEN
   SELECT p.subject_party_id,c.title,'/clasificados/'||c.slug,p.id,
     c.status='published' AND c.moderation_status='allowed'
       AND (c.expires_at IS NULL OR c.expires_at>now())
       AND EXISTS(SELECT 1 FROM directory_public_profile v WHERE v.id=p.id)
   INTO owner_id,title_value,route_value,profile,public_value
   FROM classified c JOIN directory_profile p ON p.id=c.author_profile_id WHERE c.id=entity::uuid;
   visible:=FOUND AND public_value;
 ELSE RETURN NULL;
 END CASE;
 IF club IS NOT NULL AND EXISTS(SELECT 1 FROM fan_club c WHERE c.id=club
   AND ((NOT moderation AND interaction_blocked(actor,c.artist_party_id)) OR NOT interaction_owner_live(c.artist_party_id))) THEN RETURN NULL; END IF;
 IF visible IS DISTINCT FROM true OR NOT interaction_owner_live(owner_id)
   OR (NOT moderation AND interaction_blocked(actor,owner_id)) THEN RETURN NULL; END IF;
 IF actor IS NOT NULL AND NOT interaction_actor_live(actor) THEN RETURN NULL; END IF;
 manage:=actor IS NOT NULL AND (actor=owner_id OR (club IS NOT NULL AND EXISTS(SELECT 1 FROM fan_club c WHERE c.id=club AND (c.artist_party_id=actor OR EXISTS(SELECT 1 FROM fan_club_officer o WHERE o.club_id=club AND o.fan_party_id=actor AND (o.term_ends_at IS NULL OR o.term_ends_at>now()))))) OR (profile IS NOT NULL AND EXISTS(
   SELECT 1 FROM directory_profile_manager m WHERE m.profile_id=profile
     AND m.account_party_id=actor AND m.active AND m.can_manage)));
 IF kind IN ('recording','recording_session','record_release') THEN manage:=interaction_catalog_manager(actor); END IF;
 RETURN jsonb_build_object('kind',kind,'key',entity,'ownerId',owner_id,
   'title',title_value,'route',route_value,'public',public_value,'canManage',coalesce(manage,false),
   'reactable',k.reactable,'commentable',k.commentable,'shareable',k.shareable);
END $$;
CREATE OR REPLACE FUNCTION interaction_lock_source(target uuid) RETURNS void LANGUAGE plpgsql AS $$
DECLARE t interaction_target%ROWTYPE; table_name text; parent_event bigint; state_key uuid; key_type text; membership_table text; membership_key text; reply interaction_comment%ROWTYPE;
BEGIN
 SELECT * INTO t FROM interaction_target WHERE id=target;
 IF t.entity_kind='club_post' THEN
   SELECT c.* INTO reply FROM interaction_legacy_mapping m JOIN interaction_comment c ON c.id=m.comment_id
     WHERE m.legacy_kind='club_reply' AND m.legacy_id=t.entity_key;
   IF FOUND THEN
     -- Same source/target order as parent deletion; serialize reaction and erasure.
     PERFORM interaction_lock_source(reply.target_id);
     PERFORM id FROM interaction_target WHERE id=reply.target_id FOR UPDATE;
     PERFORM id FROM interaction_comment WHERE id=reply.id FOR SHARE;
   END IF;
 END IF;
 table_name:=CASE t.entity_kind WHEN 'club_post' THEN 'fan_club_post' WHEN 'club_memory' THEN 'fan_club_memory'
   WHEN 'event' THEN 'social_event' WHEN 'event_moment' THEN 'event_moment'
   WHEN 'recording' THEN 'recording' WHEN 'recording_session' THEN 'recording_session'
   WHEN 'record_release' THEN 'record_release' WHEN 'artist_release' THEN 'artist_release'
   WHEN 'artist_update' THEN 'social_sync_post' WHEN 'classified' THEN 'classified'
   WHEN 'directory_profile' THEN 'directory_profile' END;
 IF table_name IS NULL THEN RETURN; END IF;
 key_type:=CASE WHEN t.entity_kind IN ('recording','recording_session','record_release','classified','directory_profile') THEN 'uuid' ELSE 'bigint' END;
 EXECUTE format('SELECT id FROM %I WHERE id=$1::%s FOR SHARE',table_name,key_type) USING t.entity_key;
 IF t.entity_kind IN ('recording','recording_session','record_release') THEN
   EXECUTE format('SELECT workflow_state_id FROM %I WHERE id=$1::uuid',table_name) INTO state_key USING t.entity_key;
   PERFORM id FROM workflow_state WHERE id=state_key FOR SHARE;
   PERFORM w.id FROM workflow_definition w JOIN workflow_state s ON s.workflow_id=w.id WHERE s.id=state_key FOR SHARE OF w;
   membership_table:=CASE t.entity_kind WHEN 'recording' THEN 'collection_recording' WHEN 'recording_session' THEN 'collection_session' ELSE 'collection_release' END;
   membership_key:=CASE t.entity_kind WHEN 'recording' THEN 'recording_id' WHEN 'recording_session' THEN 'session_id' ELSE 'release_id' END;
   -- Membership deletion and collection withdrawal serialize with admitted writes.
   -- Lock every existing membership, then re-evaluate publication under these locks.
   EXECUTE format('SELECT m.id FROM %I m JOIN editorial_collection c ON c.id=m.collection_id
     WHERE m.%I=$1::uuid ORDER BY c.id,m.id FOR SHARE OF c,m',membership_table,membership_key) USING t.entity_key;

 END IF;
 IF t.entity_kind IN ('artist_release','artist_update') THEN
   PERFORM a.id FROM artist_profile a WHERE a.artist_party_id IN (
     SELECT artist_party_id FROM artist_release WHERE id::text=t.entity_key AND t.entity_kind='artist_release'
     UNION SELECT artist_party_id FROM social_sync_post WHERE id::text=t.entity_key AND t.entity_kind='artist_update') FOR SHARE;
 END IF;
 IF t.entity_kind IN ('club_post','club_memory') THEN
   PERFORM c.id FROM fan_club c WHERE c.id IN (
     SELECT club_id FROM fan_club_post WHERE id::text=t.entity_key AND t.entity_kind='club_post'
     UNION SELECT p.club_id FROM fan_club_memory m JOIN fan_club_member_profile p ON p.id=m.member_profile_id
       WHERE m.id::text=t.entity_key AND t.entity_kind='club_memory') ORDER BY c.id FOR SHARE;
 END IF;
 IF t.entity_kind='event' THEN SELECT id INTO parent_event FROM social_event WHERE id::text=t.entity_key; END IF;
 IF t.entity_kind='event_moment' THEN
   SELECT event_id INTO parent_event FROM event_moment WHERE id::text=t.entity_key;
   PERFORM id FROM social_event WHERE id=parent_event FOR SHARE;
 END IF;
 IF parent_event IS NOT NULL THEN
   PERFORM id FROM event_logistics_member WHERE event_id=parent_event ORDER BY id FOR SHARE;
   PERFORM id FROM workflow_state WHERE id IN (SELECT workflow_state_id FROM social_event WHERE id=parent_event) FOR SHARE;
   PERFORM state_id FROM workflow_state_capability WHERE state_id IN (SELECT workflow_state_id FROM social_event WHERE id=parent_event) FOR SHARE;
   PERFORM id FROM external_event_ref WHERE event_id=parent_event ORDER BY id FOR SHARE;
 END IF;
 IF t.entity_kind IN ('classified','directory_profile') THEN
   PERFORM p.id FROM directory_profile p WHERE p.id::text=t.entity_key OR p.id IN (
     SELECT author_profile_id FROM classified WHERE id::text=t.entity_key) ORDER BY p.id FOR SHARE;
   -- Directory block commands have normal row locks only. Pair-independent
   -- insertion is fenced by locking the referenced profile on both paths.
 END IF;
END $$;
COMMIT;
