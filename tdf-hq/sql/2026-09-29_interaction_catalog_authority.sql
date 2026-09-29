-- Institutional publications have current catalog authority, not creator ownership.
BEGIN;
CREATE OR REPLACE FUNCTION interaction_catalog_manager(actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT interaction_actor_live(actor) AND count(DISTINCT permission_kind)=2 FROM (
   SELECT CASE WHEN p.resource_scope='module' AND a.code='access' AND m.code='catalog' THEN 'access'
     WHEN p.code='catalog.update' THEN 'update' END AS permission_kind
   FROM party_security_role psr JOIN security_role r ON r.id=psr.role_id
   JOIN role_permission rp ON rp.role_id=r.id JOIN security_permission p ON p.id=rp.permission_id
   JOIN security_action a ON a.id=p.action_id JOIN security_module m ON m.id=p.module_id
   WHERE psr.party_id=actor AND psr.active AND r.active AND rp.active AND p.active AND a.active AND m.active
 ) grants
$$;
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
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published';
   public_value:=FOUND; visible:=public_value;
 WHEN 'recording_session' THEN
   SELECT NULL::bigint,r.title_es,'/records?session='||r.id INTO owner_id,title_value,route_value
   FROM recording_session r JOIN workflow_state s ON s.id=r.workflow_state_id
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published';
   public_value:=FOUND; visible:=public_value;
 WHEN 'record_release' THEN
   SELECT NULL::bigint,r.title_es,'/records?release='||r.id INTO owner_id,title_value,route_value
   FROM record_release r JOIN workflow_state s ON s.id=r.workflow_state_id
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published';
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
CREATE OR REPLACE FUNCTION interaction_lock_permissions(target uuid,actor bigint) RETURNS void LANGUAGE plpgsql AS $$
DECLARE context jsonb; owner_id bigint; club_id_value bigint; parent_target uuid;
BEGIN
 SELECT c.target_id INTO parent_target FROM interaction_target t JOIN interaction_legacy_mapping m
   ON m.legacy_kind='club_reply' AND m.legacy_id=t.entity_key JOIN interaction_comment c ON c.id=m.comment_id
   WHERE t.id=target AND t.entity_kind='club_post';
 IF parent_target IS NOT NULL THEN PERFORM interaction_lock_permissions(parent_target,actor); END IF;
 context:=interaction_target_context(target,actor); owner_id:=(context->>'ownerId')::bigint;
 SELECT p.club_id INTO club_id_value FROM interaction_target t JOIN fan_club_post p ON p.id::text=t.entity_key
   WHERE t.id=target AND t.entity_kind='club_post';
 IF club_id_value IS NULL THEN
   SELECT p.club_id INTO club_id_value FROM interaction_target t JOIN fan_club_memory m ON m.id::text=t.entity_key
     JOIN fan_club_member_profile p ON p.id=m.member_profile_id WHERE t.id=target AND t.entity_kind='club_memory';
 END IF;
 PERFORM id FROM fan_follow WHERE fan_party_id=actor AND (artist_party_id=owner_id OR artist_party_id IN (
   SELECT artist_party_id FROM fan_club WHERE id=club_id_value)) ORDER BY id FOR SHARE;
 PERFORM id FROM party_follow WHERE follower_party_id=actor AND following_party_id=owner_id ORDER BY id FOR SHARE;
 PERFORM id FROM fan_club_officer WHERE club_id=club_id_value AND fan_party_id=actor ORDER BY id FOR SHARE;
 PERFORM profile_id FROM directory_profile_manager WHERE account_party_id=actor ORDER BY profile_id FOR SHARE;
 PERFORM id FROM party_security_role WHERE party_id=actor ORDER BY id FOR SHARE;
 IF EXISTS(SELECT 1 FROM interaction_target WHERE id=target AND entity_kind IN ('recording','recording_session','record_release')) THEN
   PERFORM r.id FROM security_role r JOIN party_security_role p ON p.role_id=r.id
     WHERE p.party_id=actor ORDER BY r.id FOR SHARE OF r;
   PERFORM rp.id FROM role_permission rp JOIN party_security_role p ON p.role_id=rp.role_id
     WHERE p.party_id=actor ORDER BY rp.id FOR SHARE OF rp;
   PERFORM permission.id FROM security_permission permission WHERE permission.id IN (
     SELECT rp.permission_id FROM role_permission rp JOIN party_security_role p ON p.role_id=rp.role_id WHERE p.party_id=actor)
     ORDER BY permission.id FOR SHARE;
   PERFORM a.id FROM security_action a WHERE a.id IN (SELECT permission.action_id FROM security_permission permission
     JOIN role_permission rp ON rp.permission_id=permission.id JOIN party_security_role p ON p.role_id=rp.role_id WHERE p.party_id=actor)
     ORDER BY a.id FOR SHARE;
   PERFORM m.id FROM security_module m WHERE m.id IN (SELECT permission.module_id FROM security_permission permission
     JOIN role_permission rp ON rp.permission_id=permission.id JOIN party_security_role p ON p.role_id=rp.role_id WHERE p.party_id=actor)
     ORDER BY m.id FOR SHARE;
 END IF;
END $$;
COMMIT;
