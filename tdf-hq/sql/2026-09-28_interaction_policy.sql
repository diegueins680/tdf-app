-- Live domain adapters; references never grant authority independently of source.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE OR REPLACE FUNCTION interaction_normalize_key(kind text,entity text) RETURNS text
LANGUAGE plpgsql IMMUTABLE AS $$
BEGIN
 IF kind IN ('recording','recording_session','record_release','classified','directory_profile') THEN RETURN entity::uuid::text; END IF;
 IF kind IN ('club_post','club_memory','event','event_moment','artist_release','artist_update')
   AND entity ~ '^[1-9][0-9]{0,17}$' THEN RETURN entity::bigint::text; END IF;
 RETURN NULL;
EXCEPTION WHEN invalid_text_representation OR numeric_value_out_of_range THEN RETURN NULL;
END $$;
CREATE OR REPLACE FUNCTION interaction_actor_live(actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT social_v2_live(actor)
   AND NOT EXISTS(SELECT 1 FROM identity_party_archive a WHERE a.party_id=actor)
$$;
CREATE OR REPLACE FUNCTION interaction_blocked(a bigint,b bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT a IS NOT NULL AND b IS NOT NULL AND a<>b AND (
   EXISTS(SELECT 1 FROM social_v2_pair p WHERE p.party_a=least(a,b) AND p.party_b=greatest(a,b)
     AND (p.block_a OR p.block_b))
   OR EXISTS(SELECT 1 FROM directory_profile_block x
     JOIN directory_profile p ON p.id=x.blocker_profile_id
     JOIN directory_profile q ON q.id=x.blocked_profile_id
     WHERE (p.subject_party_id=a AND q.subject_party_id=b)
        OR (p.subject_party_id=b AND q.subject_party_id=a)))
$$;
CREATE OR REPLACE FUNCTION interaction_owner_live(owner_id bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT owner_id IS NULL OR (EXISTS(SELECT 1 FROM party WHERE id=owner_id)
   AND NOT EXISTS(SELECT 1 FROM identity_party_archive WHERE party_id=owner_id)
   AND NOT EXISTS(SELECT 1 FROM social_v2_preference WHERE party_id=owner_id AND closed))
$$;
CREATE OR REPLACE FUNCTION interaction_club_access(actor bigint,club bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT EXISTS(SELECT 1 FROM fan_club c WHERE c.id=club AND (
   c.artist_party_id=actor OR EXISTS(SELECT 1 FROM fan_follow f
     WHERE f.fan_party_id=actor AND f.artist_party_id=c.artist_party_id)
   OR EXISTS(SELECT 1 FROM fan_club_officer o WHERE o.club_id=c.id AND o.fan_party_id=actor
     AND (o.term_ends_at IS NULL OR o.term_ends_at>now()))))
$$;

-- A legacy invitation/RSVP is not an access grant: older invitation writers
-- allow attendees to invite peers. Only the organizer-managed logistics roster
-- grants access to a non-public event until the secure invitation flow is live.
CREATE OR REPLACE FUNCTION interaction_event_access(event_key bigint,actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT EXISTS(SELECT 1 FROM social_event e WHERE e.id=event_key
   AND (EXISTS(SELECT 1 FROM directory_public_rsvp_event v WHERE v.id=e.id)
     OR (interaction_actor_live(actor) AND (e.organizer_party_id=actor::text
       OR EXISTS(SELECT 1 FROM event_logistics_member m WHERE m.event_id=e.id AND m.party_id=actor::text))))
   AND NOT EXISTS(SELECT 1 FROM external_event_ref r WHERE r.event_id=e.id AND lower(btrim(r.source_status))='suppressed')
   AND interaction_owner_live(CASE WHEN e.organizer_party_id ~ '^[1-9][0-9]{0,17}$' THEN e.organizer_party_id::bigint END)
   AND NOT interaction_blocked(actor,CASE WHEN e.organizer_party_id ~ '^[1-9][0-9]{0,17}$' THEN e.organizer_party_id::bigint END))
$$;

-- The closed dispatch chooses source tables. Entity keys are compared as text,
-- avoiding casts of untrusted input and preserving legacy bigint/UUID identities.
CREATE OR REPLACE FUNCTION interaction_resolve(kind text,entity text,actor bigint)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE owner_id bigint; title_value text; route_value text; visible boolean:=false;
 public_value boolean:=false; manage boolean:=false; club bigint; profile uuid;
 k interaction_entity_kind%ROWTYPE; legacy_reply interaction_comment%ROWTYPE;
BEGIN
 SELECT * INTO k FROM interaction_entity_kind WHERE code=kind AND enabled;
 IF NOT FOUND OR entity IS NULL OR length(entity)>128 THEN RETURN NULL; END IF;
 entity:=interaction_normalize_key(kind,entity); IF entity IS NULL THEN RETURN NULL; END IF;
 CASE kind
 WHEN 'club_post' THEN
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
       AND interaction_target_context(legacy_reply.target_id,actor) IS NOT NULL;
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
   visible:=FOUND AND EXISTS(SELECT 1 FROM social_event e WHERE e.id=entity::bigint AND interaction_event_access(e.id,actor));
 WHEN 'event_moment' THEN
   SELECT CASE WHEN m.author_party_id ~ '^[1-9][0-9]{0,17}$' THEN m.author_party_id::bigint END,
     coalesce(m.caption,'Momento'),'/eventos/'||e.id||'?moment='||m.id,
     coalesce(v.public_share_eligible,false)
   INTO owner_id,title_value,route_value,public_value FROM event_moment m
   JOIN social_event e ON e.id=m.event_id LEFT JOIN directory_public_rsvp_event v ON v.id=e.id
   WHERE m.id=entity::bigint;
   -- A moment's author does not gain access to its private parent event.
   visible:=FOUND AND EXISTS(SELECT 1 FROM event_moment m WHERE m.id=entity::bigint AND interaction_event_access(m.event_id,actor));
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
   SELECT p.artist_party_id,left(coalesce(p.caption,'Actualización'),160),
     '/artista/'||p.artist_party_id||'?update='||p.id
   INTO owner_id,title_value,route_value FROM social_sync_post p
   JOIN artist_profile a ON a.artist_party_id=p.artist_party_id WHERE p.id=entity::bigint;
   public_value:=FOUND; visible:=public_value;
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
   AND (interaction_blocked(actor,c.artist_party_id) OR NOT interaction_owner_live(c.artist_party_id))) THEN RETURN NULL; END IF;
 IF visible IS DISTINCT FROM true OR NOT interaction_owner_live(owner_id)
   OR interaction_blocked(actor,owner_id) THEN RETURN NULL; END IF;
 IF actor IS NOT NULL AND NOT interaction_actor_live(actor) THEN RETURN NULL; END IF;
 manage:=actor IS NOT NULL AND (actor=owner_id OR (club IS NOT NULL AND EXISTS(SELECT 1 FROM fan_club c WHERE c.id=club AND (c.artist_party_id=actor OR EXISTS(SELECT 1 FROM fan_club_officer o WHERE o.club_id=club AND o.fan_party_id=actor AND (o.term_ends_at IS NULL OR o.term_ends_at>now()))))) OR (profile IS NOT NULL AND EXISTS(
   SELECT 1 FROM directory_profile_manager m WHERE m.profile_id=profile
     AND m.account_party_id=actor AND m.active AND m.can_manage)));
 RETURN jsonb_build_object('kind',kind,'key',entity,'ownerId',owner_id,
   'title',title_value,'route',route_value,'public',public_value,'canManage',coalesce(manage,false),
   'reactable',k.reactable,'commentable',k.commentable,'shareable',k.shareable);
END $$;

CREATE OR REPLACE FUNCTION interaction_target_context(target uuid,actor bigint)
RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT interaction_resolve(t.entity_kind,t.entity_key,actor)
 FROM interaction_target t WHERE t.id=target AND t.retired_at IS NULL
$$;
CREATE OR REPLACE FUNCTION interaction_follows(actor bigint,owner_id bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT actor=owner_id OR EXISTS(SELECT 1 FROM social_v2_pair p
   WHERE p.party_a=least(actor,owner_id) AND p.party_b=greatest(actor,owner_id)
     AND CASE WHEN actor=p.party_a THEN p.follow_a ELSE p.follow_b END
     AND NOT (p.block_a OR p.block_b))
   OR EXISTS(SELECT 1 FROM fan_follow WHERE fan_party_id=actor AND artist_party_id=owner_id)
   OR EXISTS(SELECT 1 FROM party_follow WHERE follower_party_id=actor AND following_party_id=owner_id)
$$;
-- Club reading is available to authenticated users in the existing API. Its
-- posting/reaction boundary requires following the artist (or artist/officer).
CREATE OR REPLACE FUNCTION interaction_domain_write(target uuid,actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT CASE t.entity_kind
 WHEN 'club_post' THEN EXISTS(SELECT 1 FROM fan_club_post p WHERE p.id=t.entity_key::bigint AND interaction_club_access(actor,p.club_id))
 WHEN 'club_memory' THEN EXISTS(SELECT 1 FROM fan_club_memory m JOIN fan_club_member_profile p ON p.id=m.member_profile_id
   WHERE m.id=t.entity_key::bigint AND interaction_club_access(actor,p.club_id))
 ELSE true END FROM interaction_target t WHERE t.id=target
$$;
CREATE OR REPLACE FUNCTION interaction_can_comment(target uuid,actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 WITH context AS MATERIALIZED (SELECT interaction_target_context(target,actor) AS value)
 SELECT coalesce(interaction_actor_live(actor) AND c.value IS NOT NULL
   AND interaction_domain_write(target,actor) AND (c.value->>'commentable')::boolean AND CASE t.comment_policy
     WHEN 'off' THEN false
     WHEN 'everyone' THEN true
     WHEN 'followers' THEN interaction_follows(actor,(c.value->>'ownerId')::bigint)
     WHEN 'mentioned' THEN EXISTS(SELECT 1 FROM interaction_target_mention m
       WHERE m.target_id=t.id AND m.party_id=actor)
     ELSE false END,false)
 FROM interaction_target t CROSS JOIN context c WHERE t.id=target
$$;
CREATE OR REPLACE FUNCTION interaction_register(kind text,entity text,actor bigint)
RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE target uuid; context jsonb;
BEGIN
 context:=interaction_resolve(kind,entity,actor);
 IF context IS NULL THEN RETURN NULL; END IF;
 entity:=context->>'key';
 INSERT INTO interaction_target(entity_kind,entity_key) VALUES(kind,entity)
   ON CONFLICT(entity_kind,entity_key) DO NOTHING;
 SELECT id INTO target FROM interaction_target WHERE entity_kind=kind AND entity_key=entity
   AND retired_at IS NULL;
 RETURN target;
END $$;
COMMIT;
