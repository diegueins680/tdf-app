-- Preserve legacy reply identity and independent canonical reaction slots.
BEGIN;
SET LOCAL lock_timeout='5s';
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
     e.title,'/eventos/'||e.id,coalesce(v.public_share_eligible,false)
   INTO owner_id,title_value,route_value,public_value FROM social_event e
   LEFT JOIN directory_public_rsvp_event v ON v.id=e.id WHERE e.id=entity::bigint;
   visible:=FOUND AND EXISTS(SELECT 1 FROM social_event e WHERE e.id=entity::bigint AND interaction_event_access_scoped(e.id,actor,moderation));
 WHEN 'event_moment' THEN
   SELECT CASE WHEN m.author_party_id ~ '^[1-9][0-9]{0,17}$' THEN m.author_party_id::bigint END,
     coalesce(m.caption,'Momento'),'/eventos/'||e.id||'?moment='||m.id,
     coalesce(v.public_share_eligible,false)
   INTO owner_id,title_value,route_value,public_value FROM event_moment m
   JOIN social_event e ON e.id=m.event_id LEFT JOIN directory_public_rsvp_event v ON v.id=e.id
   WHERE m.id=entity::bigint;
   -- A moment's author does not gain access to its private parent event.
   visible:=FOUND AND EXISTS(SELECT 1 FROM event_moment m WHERE m.id=entity::bigint AND interaction_event_access_scoped(m.event_id,actor,moderation));
 WHEN 'recording' THEN
   SELECT r.created_by,r.title_es,'/records?recording='||r.id INTO owner_id,title_value,route_value
   FROM recording r JOIN workflow_state s ON s.id=r.workflow_state_id
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published';
   public_value:=FOUND; visible:=public_value;
 WHEN 'recording_session' THEN
   SELECT r.created_by,r.title_es,'/records?session='||r.id INTO owner_id,title_value,route_value
   FROM recording_session r JOIN workflow_state s ON s.id=r.workflow_state_id
   WHERE r.id=entity::uuid AND r.active AND s.active AND s.code='published';
   public_value:=FOUND; visible:=public_value;
 WHEN 'record_release' THEN
   SELECT r.created_by,r.title_es,'/records?release='||r.id INTO owner_id,title_value,route_value
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
 RETURN jsonb_build_object('kind',kind,'key',entity,'ownerId',owner_id,
   'title',title_value,'route',route_value,'public',public_value,'canManage',coalesce(manage,false),
   'reactable',k.reactable,'commentable',k.commentable,'shareable',k.shareable);
END $$;
CREATE OR REPLACE FUNCTION interaction_domain_write(target uuid,actor bigint) RETURNS boolean
LANGUAGE plpgsql STABLE AS $$
DECLARE t interaction_target%ROWTYPE; parent_target uuid;
BEGIN
 SELECT * INTO t FROM interaction_target WHERE id=target;
 IF NOT FOUND THEN RETURN false; END IF;
 IF t.entity_kind='club_post' THEN
   SELECT c.target_id INTO parent_target FROM interaction_legacy_mapping m JOIN interaction_comment c ON c.id=m.comment_id
     WHERE m.legacy_kind='club_reply' AND m.legacy_id=t.entity_key;
   IF parent_target IS NOT NULL THEN RETURN interaction_domain_write(parent_target,actor); END IF;
   RETURN EXISTS(SELECT 1 FROM fan_club_post p WHERE p.id=t.entity_key::bigint AND interaction_club_access(actor,p.club_id));
 ELSIF t.entity_kind='club_memory' THEN
   RETURN EXISTS(SELECT 1 FROM fan_club_memory m JOIN fan_club_member_profile p ON p.id=m.member_profile_id
     WHERE m.id=t.entity_key::bigint AND interaction_club_access(actor,p.club_id));
 END IF;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION interaction_lock_source(target uuid) RETURNS void LANGUAGE plpgsql AS $$
DECLARE t interaction_target%ROWTYPE; table_name text; parent_event bigint; state_key uuid; key_type text; reply interaction_comment%ROWTYPE;
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
END $$;
CREATE OR REPLACE FUNCTION interaction_legacy_command(actor bigint,kind text,entity text,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; context jsonb; requested uuid; current_reaction uuid; mapped uuid; desired boolean;
 command jsonb; result_value jsonb; alias_value bigint; c interaction_comment%ROWTYPE; reply_to uuid; requested_entity text:=entity; source_entity text:=entity;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF kind='club_post' AND payload->>'operation' IN ('legacy.comment','legacy.hide','legacy.restore','legacy.reaction') THEN
   SELECT comment_id INTO reply_to FROM interaction_legacy_mapping WHERE legacy_kind='club_reply' AND legacy_id=entity;
   IF reply_to IS NOT NULL THEN
     SELECT * INTO c FROM interaction_comment WHERE id=reply_to;
     SELECT entity_key INTO source_entity FROM interaction_target WHERE id=c.target_id;
     IF payload->>'operation'<>'legacy.reaction' THEN entity:=source_entity; END IF;
   END IF;
   IF payload ? 'artistId' AND NOT EXISTS(SELECT 1 FROM fan_club_post p JOIN fan_club club ON club.id=p.club_id
       WHERE p.id=source_entity::bigint AND club.artist_party_id=(payload->>'artistId')::bigint) THEN RETURN '{"error":"unavailable"}'; END IF;
 END IF;
 target:=interaction_register(kind,entity,actor);
 IF target IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 context:=interaction_target_context(target,actor);
 PERFORM id FROM party WHERE id IN(actor,(context->>'ownerId')::bigint) ORDER BY id FOR SHARE;
 PERFORM interaction_lock_source(target); PERFORM interaction_lock_permissions(target,actor);
 PERFORM pg_advisory_xact_lock(hashtextextended('interaction-actor:'||actor,0));
 PERFORM id FROM interaction_target WHERE id=target FOR UPDATE;
 IF payload->>'operation'='legacy.reaction' THEN
   BEGIN requested:=(payload->>'reactionTypeId')::uuid; EXCEPTION WHEN invalid_text_representation THEN RETURN '{"error":"invalid"}'; END;
   mapped:=requested;
   IF kind='event_moment' THEN
     SELECT c.id INTO mapped FROM reaction_type r JOIN content_reaction_type c
       ON c.code=CASE r.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE r.code END WHERE r.id=requested;
   END IF;
   IF mapped IS NULL THEN RETURN '{"error":"invalid_reaction"}'; END IF;
   SELECT reaction_type_id INTO current_reaction FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
   desired:=coalesce((payload->>'active')::boolean,current_reaction IS DISTINCT FROM mapped);
   command:=jsonb_build_object('operation','reaction.set','reactionTypeId',CASE WHEN desired THEN mapped
     WHEN current_reaction=mapped THEN NULL ELSE current_reaction END);
   RETURN interaction_command(actor,target,gen_random_uuid(),command);
 ELSIF payload->>'operation' IN ('legacy.hide','legacy.restore') THEN
   IF reply_to IS NOT NULL THEN
     PERFORM id FROM interaction_comment WHERE id=reply_to FOR UPDATE;
     SELECT * INTO c FROM interaction_comment WHERE id=reply_to;
     RETURN interaction_command(actor,target,gen_random_uuid(),jsonb_build_object('operation',
       CASE payload->>'operation' WHEN 'legacy.hide' THEN 'comment.hide' ELSE 'comment.restore' END,
       'commentId',reply_to,'expectedVersion',c.version,'reason','Content owner moderation'));
   END IF;
   IF NOT (context->>'canManage')::boolean THEN RETURN '{"error":"forbidden"}'; END IF;
   UPDATE fan_club_post SET is_hidden=(payload->>'operation'='legacy.hide') WHERE id=entity::bigint;
   RETURN jsonb_build_object('ok',true);
 ELSIF payload->>'operation'='legacy.comment' THEN
   result_value:=interaction_command(actor,target,gen_random_uuid(),jsonb_build_object('operation','comment.create',
     'body',payload->>'body','mentions','[]'::jsonb)||CASE WHEN reply_to IS NULL THEN '{}'::jsonb ELSE jsonb_build_object('parentId',reply_to) END);
   IF result_value ? 'error' THEN RETURN result_value; END IF;
   SELECT * INTO c FROM interaction_comment WHERE id=(result_value->>'id')::uuid;
   IF kind='club_post' THEN
     alias_value:=nextval(pg_get_serial_sequence('fan_club_post','id'));
     INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
     VALUES('club_reply',alias_value::text,target,c.id,jsonb_build_object('title',payload->'title','mediaUrls',payload->'mediaUrls'));
     RETURN jsonb_build_object('fcpId',alias_value,'fcpParentId',requested_entity::bigint,'fcpTitle',payload->'title',
       'fcpContent',c.body,'fcpMediaUrls',coalesce(payload->'mediaUrls','[]'::jsonb),'fcpAuthorId',actor,
       'fcpAuthorName',result_value->'author'->>'displayName','fcpAvatarUrl',result_value->'author'->'avatarUrl',
       'fcpIsPinned',false,'fcpIsHidden',false,'fcpReplies',0,
       'fcpReactions',jsonb_build_object('rsItems','[]'::jsonb,'rsTotal',0,'rsMyReactionTypeId',NULL),
       'fcpCreatedAt',c.created_at,'fcpUpdatedAt',NULL);
   ELSIF kind='event_moment' THEN
     alias_value:=nextval(pg_get_serial_sequence('event_moment_comment','id'));
     INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
     VALUES('moment_comment',alias_value::text,target,c.id,'{}');
     RETURN jsonb_build_object('emcId',alias_value::text,'emcMomentId',entity,'emcAuthorPartyId',actor::text,
       'emcAuthorName',result_value->'author'->>'displayName','emcBody',c.body,'emcCreatedAt',c.created_at,'emcUpdatedAt',c.updated_at);
   END IF;
 END IF;
 RETURN '{"error":"invalid"}';
END $$;
CREATE OR REPLACE FUNCTION interaction_retire_reply_reactions() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE reply_target uuid;
BEGIN
 IF NEW.state NOT IN ('deleted','removed') OR OLD.state=NEW.state THEN RETURN NULL; END IF;
 FOR reply_target IN SELECT t.id FROM interaction_legacy_mapping m JOIN interaction_target t
   ON t.entity_kind='club_post' AND t.entity_key=m.legacy_id
   WHERE m.legacy_kind='club_reply' AND m.comment_id=NEW.id ORDER BY t.id FOR UPDATE OF t LOOP
   DELETE FROM interaction_reaction WHERE target_id=reply_target;
   UPDATE interaction_target SET retired_at=coalesce(retired_at,now()),version=version+1,updated_at=now() WHERE id=reply_target;
   INSERT INTO interaction_audit(target_id,comment_id,operation,reason)
     VALUES(reply_target,NEW.id,'target.retired','Canonical reply was erased');
 END LOOP;
 RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS interaction_reply_reaction_retirement ON interaction_comment;
CREATE TRIGGER interaction_reply_reaction_retirement AFTER UPDATE OF state ON interaction_comment
FOR EACH ROW EXECUTE FUNCTION interaction_retire_reply_reactions();
COMMIT;
