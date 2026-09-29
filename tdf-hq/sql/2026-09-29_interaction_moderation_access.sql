-- Social blocks cannot revoke platform enforcement authority.
-- Share one source adapter; only scoped moderation ignores blocks, never publication or private-event grants.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE OR REPLACE FUNCTION interaction_event_access_scoped(event_key bigint,actor bigint,moderation boolean) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT EXISTS(SELECT 1 FROM social_event e WHERE e.id=event_key
   AND (EXISTS(SELECT 1 FROM directory_public_rsvp_event v WHERE v.id=e.id)
     OR (interaction_actor_live(actor) AND (e.organizer_party_id=actor::text
       OR EXISTS(SELECT 1 FROM event_logistics_member m WHERE m.event_id=e.id AND m.party_id=actor::text))))
   AND NOT EXISTS(SELECT 1 FROM external_event_ref r WHERE r.event_id=e.id AND lower(btrim(r.source_status))='suppressed')
   AND interaction_owner_live(CASE WHEN e.organizer_party_id ~ '^[1-9][0-9]{0,17}$' THEN e.organizer_party_id::bigint END)
   AND (coalesce(moderation AND interaction_is_moderator(actor),false) OR NOT interaction_blocked(actor,CASE WHEN e.organizer_party_id ~ '^[1-9][0-9]{0,17}$' THEN e.organizer_party_id::bigint END)))
$$;
CREATE OR REPLACE FUNCTION interaction_event_access(event_key bigint,actor bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$ SELECT interaction_event_access_scoped(event_key,actor,false) $$;

CREATE OR REPLACE FUNCTION interaction_resolve_scoped(kind text,entity text,actor bigint,moderation boolean)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE owner_id bigint; title_value text; route_value text; visible boolean:=false;
 public_value boolean:=false; manage boolean:=false; club bigint; profile uuid;
 k interaction_entity_kind%ROWTYPE; legacy_reply interaction_comment%ROWTYPE;
BEGIN
 moderation:=coalesce(moderation,false);
 IF moderation AND NOT interaction_is_moderator(actor) THEN RETURN NULL; END IF;
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
CREATE OR REPLACE FUNCTION interaction_resolve(kind text,entity text,actor bigint)
RETURNS jsonb LANGUAGE sql STABLE AS $$ SELECT interaction_resolve_scoped(kind,entity,actor,false) $$;
CREATE OR REPLACE FUNCTION interaction_moderation_context(target uuid,actor bigint)
RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT interaction_resolve_scoped(t.entity_kind,t.entity_key,actor,true)
 FROM interaction_target t WHERE t.id=target AND t.retired_at IS NULL AND interaction_is_moderator(actor)
$$;
CREATE OR REPLACE FUNCTION interaction_command_context(target uuid,actor bigint,operation text)
RETURNS jsonb LANGUAGE sql STABLE AS $$
 SELECT CASE WHEN operation IN ('comment.remove','comment.restore','comment.report.resolve') AND interaction_is_moderator(actor)
   THEN interaction_moderation_context(target,actor) ELSE interaction_target_context(target,actor) END
$$;

CREATE OR REPLACE FUNCTION interaction_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE context jsonb; op text; fingerprint text; prior interaction_request%ROWTYPE;
 t interaction_target%ROWTYPE; c interaction_comment%ROWTYPE; parent interaction_comment%ROWTYPE;
 result_value jsonb; body_value text; mentions jsonb; comment_key uuid; parent_key uuid;
 initial_context jsonb; moderating boolean:=false; reaction_changed boolean:=false; new_mentions bigint[];
 reaction_key uuid; next_state text; reason_value text; expected bigint; allowed_keys text[];
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF actor IS NULL OR request_id IS NULL OR jsonb_typeof(payload) IS DISTINCT FROM 'object'
   OR pg_column_size(payload)>32768 OR NOT interaction_actor_live(actor) THEN RETURN '{"error":"invalid"}'; END IF;
 op:=payload->>'operation';
 allowed_keys:=CASE op
   WHEN 'reaction.set' THEN ARRAY['operation','reactionTypeId']
   WHEN 'comment.create' THEN ARRAY['operation','body','parentId','mentions']
   WHEN 'comment.edit' THEN ARRAY['operation','commentId','body','mentions','expectedVersion']
   WHEN 'comment.delete' THEN ARRAY['operation','commentId','expectedVersion']
   WHEN 'comment.hide' THEN ARRAY['operation','commentId','expectedVersion','reason']
   WHEN 'comment.remove' THEN ARRAY['operation','commentId','expectedVersion','reason']
   WHEN 'comment.restore' THEN ARRAY['operation','commentId','expectedVersion','reason']
   WHEN 'comment.report' THEN ARRAY['operation','commentId','reason']
   WHEN 'comment.report.resolve' THEN ARRAY['operation','commentId','expectedVersion','reason','decision']
   WHEN 'subscription.set' THEN ARRAY['operation','mode']
   WHEN 'settings.update' THEN ARRAY['operation','commentPolicy','expectedVersion','mentionedPartyIds']
   ELSE NULL END;
 IF allowed_keys IS NULL OR payload-allowed_keys<>'{}'::jsonb THEN RETURN '{"error":"invalid"}'; END IF;
 initial_context:=interaction_command_context(target,actor,op);
 IF initial_context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 -- Shared account locks compose with canonical block/closure writers. Include
 -- reply authors and mention recipients; all IDs are validated before casts.
 PERFORM id FROM party WHERE id IN (
   SELECT actor UNION SELECT (initial_context->>'ownerId')::bigint
   UNION SELECT author_id FROM interaction_comment WHERE target_id=target
     AND id::text IN (payload->>'parentId',payload->>'commentId')
   UNION SELECT CASE WHEN value->>'partyId' ~ '^[1-9][0-9]{0,17}$' THEN (value->>'partyId')::bigint END
     FROM jsonb_array_elements(CASE WHEN jsonb_typeof(payload->'mentions')='array' THEN payload->'mentions' ELSE '[]'::jsonb END)
 ) ORDER BY id FOR SHARE;
 PERFORM interaction_lock_source(target);
 PERFORM interaction_lock_permissions(target,actor);
 IF interaction_command_context(target,actor,op)->>'ownerId' IS DISTINCT FROM initial_context->>'ownerId'
 THEN RETURN '{"error":"revision_conflict"}'; END IF;
 -- Serialize an actor's command budget and idempotency keys across targets.
 PERFORM pg_advisory_xact_lock(hashtextextended('interaction-actor:'||actor,0));
 -- The API also holds its current session lock. Target serialization protects
 -- absent reactions, idempotent creation and settings changes in one transaction.
 SELECT * INTO t FROM interaction_target WHERE id=target FOR UPDATE;
 context:=interaction_command_context(target,actor,op);
 IF NOT FOUND OR context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 -- Blocking constrains social contact, never the existing scoped enforcement role.
 moderating:=CASE op
   WHEN 'comment.hide' THEN coalesce((context->>'canManage')::boolean,false)
   WHEN 'comment.restore' THEN coalesce((context->>'canManage')::boolean,false) OR interaction_is_moderator(actor)
   WHEN 'comment.remove' THEN interaction_is_moderator(actor)
   WHEN 'comment.report.resolve' THEN interaction_is_moderator(actor)
   ELSE false END;
 fingerprint:=encode(digest(payload::text,'sha256'),'hex');
 SELECT * INTO prior FROM interaction_request WHERE actor_id=actor AND request_key=request_id;
 IF FOUND THEN
   IF prior.target_id<>target OR prior.payload_hash<>fingerprint THEN RETURN '{"error":"request_key_conflict"}'; END IF;
   -- Reconcile a replay against current state; stored responses must never
   -- disclose a subsequently removed body or resurrect an old reaction.
   IF op LIKE 'comment.%' AND op<>'comment.report' THEN
     SELECT * INTO c FROM interaction_comment WHERE id=(prior.result->>'id')::uuid AND target_id=target;
     IF NOT FOUND OR (interaction_blocked(actor,c.author_id) AND NOT moderating) THEN RETURN '{"error":"unavailable"}'; END IF;
     RETURN interaction_comment_json(c,actor)||jsonb_build_object('replay',true);
   ELSIF op='reaction.set' THEN
     SELECT reaction_type_id INTO reaction_key FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
     RETURN jsonb_build_object('reactionTypeId',reaction_key,'replay',true);
   ELSIF op='settings.update' THEN
     RETURN jsonb_build_object('commentPolicy',t.comment_policy,'version',t.version,'replay',true);
   ELSIF op='subscription.set' THEN
     RETURN (SELECT jsonb_build_object('mode',mode,'replay',true) FROM interaction_subscription WHERE target_id=target AND party_id=actor);
   END IF;
   RETURN prior.result||jsonb_build_object('replay',true);
 END IF;
 IF (SELECT count(*) FROM interaction_request WHERE actor_id=actor AND created_at>now()-interval '1 minute')>=30
   THEN RETURN '{"error":"rate_limited"}'; END IF;
 IF op LIKE 'comment.%' AND op<>'comment.create' THEN
   IF coalesce(payload->>'commentId','') !~ '^[0-9a-fA-F-]{36}$' THEN RETURN '{"error":"invalid"}'; END IF;
   BEGIN comment_key:=(payload->>'commentId')::uuid; EXCEPTION WHEN invalid_text_representation THEN RETURN '{"error":"invalid"}'; END;
   SELECT * INTO c FROM interaction_comment WHERE id=comment_key AND target_id=target FOR UPDATE;
   IF NOT FOUND OR (interaction_blocked(actor,c.author_id) AND NOT moderating) THEN RETURN '{"error":"unavailable"}'; END IF;
   IF op<>'comment.report' THEN
     IF coalesce(payload->>'expectedVersion','') !~ '^[1-9][0-9]{0,17}$' THEN RETURN '{"error":"invalid"}'; END IF;
     expected:=(payload->>'expectedVersion')::bigint;
     IF expected<>c.version THEN RETURN '{"error":"revision_conflict"}'; END IF;
   END IF;
 END IF;
 IF op IN ('comment.create','comment.edit') THEN
   body_value:=payload->>'body'; mentions:=coalesce(payload->'mentions','[]'::jsonb);
   IF jsonb_typeof(payload->'body') IS DISTINCT FROM 'string' OR length(body_value)>4096 OR length(btrim(body_value))<1
     OR body_value ~ '[\x01-\x08\x0B\x0C\x0E-\x1F\x7F]'
     OR NOT interaction_mentions_valid(target,actor,body_value,mentions) THEN RETURN '{"error":"invalid"}'; END IF;
   IF op='comment.create' AND NOT interaction_can_comment(target,actor) THEN RETURN '{"error":"comments_not_allowed"}'; END IF;
 END IF;
 CASE op
 WHEN 'reaction.set' THEN
   IF NOT (payload ? 'reactionTypeId') THEN RETURN '{"error":"invalid"}'; END IF;
   IF payload->'reactionTypeId'<>'null'::jsonb THEN
     IF NOT interaction_domain_write(target,actor) OR NOT (context->>'reactable')::boolean THEN RETURN '{"error":"invalid"}'; END IF;
     BEGIN reaction_key:=(payload->>'reactionTypeId')::uuid; EXCEPTION WHEN invalid_text_representation THEN RETURN '{"error":"invalid"}'; END;
     IF NOT EXISTS(SELECT 1 FROM content_reaction_type r
       JOIN interaction_reaction_choice choice ON choice.reaction_type_id=r.id
       JOIN catalog_definition catalog ON catalog.id=r.catalog_id AND catalog.code='content-reaction-types' AND catalog.active
       JOIN workflow_state w ON w.id=r.workflow_state_id AND w.workflow_id=catalog.workflow_id WHERE r.id=reaction_key AND r.active
         AND r.deprecated_at IS NULL AND w.active AND w.code='published') THEN RETURN '{"error":"invalid_reaction"}'; END IF;
     INSERT INTO interaction_reaction(target_id,actor_id,reaction_type_id) VALUES(target,actor,reaction_key)
       ON CONFLICT(target_id,actor_id) DO UPDATE SET reaction_type_id=excluded.reaction_type_id,updated_at=now()
       WHERE interaction_reaction.reaction_type_id<>excluded.reaction_type_id;
     reaction_changed:=FOUND;
   ELSE DELETE FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
   END IF;
   IF reaction_changed AND reaction_key IS NOT NULL AND t.entity_kind='event_moment' THEN
     INSERT INTO engagement_event(actor_party_id,entity_type,entity_id,event_type,metadata,created_at)
     SELECT actor,'event_moment',t.entity_key::bigint,'reaction_added',reaction_key::text,r.updated_at
     FROM interaction_reaction r WHERE r.target_id=target AND r.actor_id=actor AND NOT EXISTS(
       SELECT 1 FROM engagement_event e WHERE e.actor_party_id=actor AND e.entity_type='event_moment'
         AND e.entity_id=t.entity_key::bigint AND e.event_type='reaction_added'
         AND e.created_at=r.updated_at AND e.metadata=reaction_key::text);
   END IF;
   result_value:=jsonb_build_object('reactionTypeId',reaction_key);
 WHEN 'comment.create' THEN
   IF payload->>'parentId' IS NOT NULL THEN
     BEGIN parent_key:=(payload->>'parentId')::uuid; EXCEPTION WHEN invalid_text_representation THEN RETURN '{"error":"invalid"}'; END;
     SELECT * INTO parent FROM interaction_comment WHERE id=parent_key AND target_id=target FOR SHARE;
     IF NOT FOUND OR parent.state IN ('hidden','removed') OR parent.depth>=1000
       OR interaction_blocked(actor,parent.author_id) THEN RETURN '{"error":"unavailable"}'; END IF;
   END IF;
   comment_key:=gen_random_uuid();
   INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body)
   VALUES(comment_key,target,actor,parent_key,coalesce(parent.root_id,comment_key),coalesce(parent.depth+1,0),body_value)
   RETURNING * INTO c;
   INSERT INTO interaction_subscription(target_id,party_id) VALUES(target,actor) ON CONFLICT DO NOTHING;
   result_value:=interaction_comment_json(c,actor);
 WHEN 'comment.edit' THEN
   IF c.author_id IS DISTINCT FROM actor OR c.state<>'visible' THEN RETURN '{"error":"forbidden"}'; END IF;
   UPDATE interaction_comment SET body=body_value,version=version+1,updated_at=now(),edited_at=now()
     WHERE id=c.id RETURNING * INTO c;
   result_value:=interaction_comment_json(c,actor);
 WHEN 'comment.delete' THEN
   IF c.author_id IS DISTINCT FROM actor OR c.state NOT IN ('visible','hidden') THEN RETURN '{"error":"forbidden"}'; END IF;
   UPDATE interaction_comment SET body='',state='deleted',version=version+1,updated_at=now()
     WHERE id=c.id RETURNING * INTO c;
   DELETE FROM interaction_comment_mention WHERE comment_id=c.id;
   result_value:=interaction_comment_json(c,actor);
 WHEN 'comment.hide','comment.remove','comment.restore' THEN
   reason_value:=payload->>'reason';
   IF reason_value IS NULL OR length(btrim(reason_value)) NOT BETWEEN 1 AND 1000 THEN RETURN '{"error":"invalid"}'; END IF;
   IF op='comment.hide' AND NOT (context->>'canManage')::boolean THEN RETURN '{"error":"forbidden"}'; END IF;
   IF op='comment.remove' AND NOT interaction_is_moderator(actor) THEN RETURN '{"error":"forbidden"}'; END IF;
   IF op='comment.restore' AND NOT ((context->>'canManage')::boolean OR interaction_is_moderator(actor))
     THEN RETURN '{"error":"forbidden"}'; END IF;
   IF (op='comment.restore' AND c.state<>'hidden') OR (op='comment.hide' AND c.state<>'visible') OR (op='comment.remove' AND c.state NOT IN ('visible','hidden'))
     THEN RETURN '{"error":"revision_conflict"}'; END IF;
   next_state:=CASE WHEN op='comment.hide' THEN 'hidden' WHEN op='comment.restore' THEN 'visible' ELSE 'removed' END;
   INSERT INTO interaction_audit(target_id,comment_id,actor_id,operation,reason,previous_state,new_state)
   VALUES(target,c.id,actor,op,reason_value,c.state,next_state);
   UPDATE interaction_comment SET body=CASE WHEN next_state='removed' THEN '' ELSE body END,state=next_state,version=version+1,updated_at=now()
     WHERE id=c.id RETURNING * INTO c;
   IF next_state='removed' THEN
     DELETE FROM interaction_comment_mention WHERE comment_id=c.id;
     UPDATE interaction_report SET state='reviewed' WHERE comment_id=c.id AND state='open';
   END IF;
   result_value:=interaction_comment_json(c,actor);
 WHEN 'comment.report' THEN
   reason_value:=payload->>'reason';
   IF reason_value IS NULL OR length(btrim(reason_value)) NOT BETWEEN 1 AND 1000 OR c.state<>'visible'
     THEN RETURN '{"error":"invalid"}'; END IF;
   INSERT INTO interaction_report(target_id,comment_id,reporter_id,reason) VALUES(target,c.id,actor,reason_value)
     ON CONFLICT(comment_id,reporter_id) DO NOTHING;
   result_value:='{"reported":true}';
 WHEN 'comment.report.resolve' THEN
   IF NOT interaction_is_moderator(actor) THEN RETURN '{"error":"forbidden"}'; END IF;
   reason_value:=payload->>'reason';
   IF reason_value IS NULL OR length(btrim(reason_value)) NOT BETWEEN 1 AND 1000
     OR coalesce(payload->>'decision','') NOT IN ('reviewed','dismissed') THEN RETURN '{"error":"invalid"}'; END IF;
   UPDATE interaction_report SET state=payload->>'decision' WHERE comment_id=c.id AND state='open';
   IF NOT FOUND THEN RETURN '{"error":"revision_conflict"}'; END IF;
   INSERT INTO interaction_audit(target_id,comment_id,actor_id,operation,reason,previous_state,new_state)
   VALUES(target,c.id,actor,op,reason_value,'open',payload->>'decision');
   result_value:=interaction_comment_json(c,actor);
 WHEN 'subscription.set' THEN
   IF coalesce(payload->>'mode','') NOT IN ('all','participating','muted') THEN RETURN '{"error":"invalid"}'; END IF;
   INSERT INTO interaction_subscription(target_id,party_id,mode) VALUES(target,actor,payload->>'mode')
     ON CONFLICT(target_id,party_id) DO UPDATE SET mode=excluded.mode,updated_at=now();
   result_value:=jsonb_build_object('mode',payload->>'mode');
 WHEN 'settings.update' THEN
   IF NOT (context->>'canManage')::boolean THEN RETURN '{"error":"forbidden"}'; END IF;
   IF coalesce(payload->>'commentPolicy','') NOT IN ('everyone','followers','mentioned','off')
     OR coalesce(payload->>'expectedVersion','') !~ '^[1-9][0-9]{0,17}$'
     OR jsonb_typeof(payload->'mentionedPartyIds') IS DISTINCT FROM 'array'
     OR jsonb_array_length(payload->'mentionedPartyIds')>100 THEN RETURN '{"error":"invalid"}'; END IF;
   IF (payload->>'expectedVersion')::bigint<>t.version THEN RETURN '{"error":"revision_conflict"}'; END IF;
   IF EXISTS(SELECT 1 FROM jsonb_array_elements_text(payload->'mentionedPartyIds') x(id)
      WHERE id !~ '^[1-9][0-9]{0,17}$') THEN RETURN '{"error":"invalid"}'; END IF;
   IF EXISTS(SELECT 1 FROM jsonb_array_elements_text(payload->'mentionedPartyIds') x(id)
      WHERE NOT interaction_actor_live(id::bigint) OR interaction_blocked(actor,id::bigint)
         OR interaction_target_context(target,id::bigint) IS NULL) THEN RETURN '{"error":"invalid"}'; END IF;
   UPDATE interaction_target SET comment_policy=payload->>'commentPolicy',version=version+1,updated_at=now()
     WHERE id=target RETURNING * INTO t;
   DELETE FROM interaction_target_mention WHERE target_id=target;
   INSERT INTO interaction_target_mention(target_id,party_id)
     SELECT target,id::bigint FROM jsonb_array_elements_text(payload->'mentionedPartyIds') x(id) ON CONFLICT DO NOTHING;
   INSERT INTO interaction_audit(target_id,actor_id,operation,new_state) VALUES(target,actor,op,t.comment_policy);
   result_value:=jsonb_build_object('commentPolicy',t.comment_policy,'version',t.version);
 ELSE RETURN '{"error":"invalid"}';
 END CASE;
 IF op IN ('comment.create','comment.edit') THEN
   SELECT coalesce(array_agg(DISTINCT (x->>'partyId')::bigint),'{}'::bigint[]) INTO new_mentions
   FROM jsonb_array_elements(mentions) x WHERE NOT EXISTS(SELECT 1 FROM interaction_comment_mention m
     WHERE m.comment_id=c.id AND m.party_id=(x->>'partyId')::bigint);
   DELETE FROM interaction_comment_mention WHERE comment_id=c.id;
   INSERT INTO interaction_comment_mention(comment_id,party_id,start_offset,end_offset)
   SELECT c.id,(x->>'partyId')::bigint,(x->>'start')::integer,(x->>'end')::integer FROM jsonb_array_elements(mentions) x;
   result_value:=interaction_comment_json(c,actor);
 END IF;
 result_value:=result_value||jsonb_build_object('eventKind',CASE op
   WHEN 'reaction.set' THEN CASE WHEN reaction_changed AND reaction_key IS NOT NULL THEN 'reaction' END
   WHEN 'comment.create' THEN 'comment' WHEN 'comment.edit' THEN 'edit'
   WHEN 'comment.hide' THEN 'moderation' WHEN 'comment.remove' THEN 'moderation' END,
   'commentId',CASE WHEN op LIKE 'comment.%' THEN c.id END);
 INSERT INTO interaction_request(actor_id,request_key,target_id,payload_hash,result)
   VALUES(actor,request_id,target,fingerprint,jsonb_strip_nulls(jsonb_build_object(
     'id',result_value->'id','eventKind',result_value->'eventKind','commentId',result_value->'commentId',
     'reported',result_value->'reported','mentionedPartyIds',to_jsonb(new_mentions))));
 RETURN result_value;
END $$;
CREATE OR REPLACE FUNCTION interaction_report_reasons(actor bigint,comment_key uuid) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 SELECT coalesce(jsonb_agg(reason ORDER BY created_at DESC,id DESC),'[]'::jsonb) FROM (
   SELECT r.reason,r.created_at,r.id FROM interaction_report r JOIN interaction_comment c ON c.id=r.comment_id
   WHERE r.comment_id=comment_key AND r.state='open' AND interaction_is_moderator(actor)
     AND interaction_moderation_context(c.target_id,actor) IS NOT NULL
   ORDER BY r.created_at DESC,r.id DESC LIMIT 20
 ) recent
$$;
CREATE OR REPLACE FUNCTION interaction_moderation_page(actor bigint,target uuid,before_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE context jsonb; pivot interaction_comment%ROWTYPE; items jsonb; cursor_value uuid;
BEGIN
 context:=coalesce(interaction_target_context(target,actor),interaction_moderation_context(target,actor));
 IF context IS NULL OR NOT ((context->>'canManage')::boolean OR interaction_is_moderator(actor)) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE target_id=target AND id=before_id;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 WITH page AS MATERIALIZED (
   SELECT c.* FROM interaction_comment c WHERE c.target_id=target
     AND (c.state='hidden' OR (c.state='visible' AND interaction_blocked(actor,c.author_id)) OR (interaction_is_moderator(actor) AND EXISTS(
       SELECT 1 FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open')))
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ), numbered AS (SELECT p.*,row_number() OVER(ORDER BY p.created_at DESC,p.id DESC) ordinal FROM page p)
 SELECT coalesce(jsonb_agg(interaction_comment_json(c,actor)||jsonb_build_object('state',c.state,'moderationBody',c.body,
   'openReports',(SELECT count(*) FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open'),
   'reportReasons',interaction_report_reasons(actor,c.id))
   ORDER BY n.ordinal) FILTER(WHERE n.ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(n.id ORDER BY n.ordinal))[page_size] END
 INTO items,cursor_value FROM numbered n JOIN interaction_comment c ON c.id=n.id;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
CREATE OR REPLACE FUNCTION interaction_report_inbox(actor bigint,before_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE pivot interaction_comment%ROWTYPE; keys uuid[]; items jsonb; cursor_value uuid;
BEGIN
 IF NOT interaction_is_moderator(actor) THEN RETURN '{"error":"forbidden"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE id=before_id;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 SELECT array_agg(id) INTO keys FROM (
   SELECT c.id FROM interaction_comment c WHERE EXISTS(SELECT 1 FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open')
     AND interaction_moderation_context(c.target_id,actor) IS NOT NULL
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ) page;
 IF array_length(keys,1)>page_size THEN cursor_value:=keys[page_size]; END IF;
 SELECT coalesce(jsonb_agg(j.value||jsonb_build_object('state',c.state,'moderationBody',CASE WHEN c.state IN ('visible','hidden') THEN c.body ELSE '' END,
   'openReports',(SELECT count(*) FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open'),
   'reportReasons',interaction_report_reasons(actor,c.id))
   ORDER BY array_position(keys,c.id)),'[]'::jsonb) INTO items FROM interaction_comments_json(keys[1:page_size],actor) j
 JOIN interaction_comment c ON c.id=j.comment_id;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
CREATE OR REPLACE FUNCTION interaction_comment_context(target uuid,actor bigint,comment_key uuid)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE c interaction_comment%ROWTYPE; context jsonb; keys uuid[]; authors bigint[]; mapped jsonb;
BEGIN
 context:=coalesce(interaction_target_context(target,actor),interaction_moderation_context(target,actor));
 IF context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO c FROM interaction_comment WHERE id=comment_key AND target_id=target;
 IF NOT FOUND OR (interaction_blocked(actor,c.author_id) AND NOT
   (coalesce((context->>'canManage')::boolean,false) OR interaction_is_moderator(actor)))
   OR (c.state IN ('hidden','removed') AND actor IS DISTINCT FROM c.author_id
     AND NOT coalesce((context->>'canManage')::boolean,false) AND NOT interaction_is_moderator(actor))
 THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT array_agg(party_id) INTO authors FROM interaction_author_batch(actor,ARRAY(
   SELECT author_id FROM interaction_comment_total WHERE target_id=target AND root_id=c.root_id AND comments>0));
 SELECT array_agg(id) INTO keys FROM (SELECT x.id FROM interaction_comment x
   WHERE x.target_id=target AND x.root_id=c.root_id AND x.state='visible' AND x.author_id=ANY(authors)
     AND (x.created_at,x.id)>=(c.created_at,c.id) ORDER BY x.created_at,x.id LIMIT 11) page;
 SELECT jsonb_object_agg(comment_id::text,value) INTO mapped
 FROM interaction_comments_json(coalesce(keys,'{}'::uuid[])||ARRAY[c.id,c.root_id,c.parent_id],actor);
 RETURN jsonb_build_object('target',context,'root',mapped->c.root_id::text,'comment',mapped->c.id::text,
   'parent',mapped->c.parent_id::text,'surrounding',coalesce((SELECT jsonb_agg(mapped->key::text ORDER BY ordinal)
     FROM unnest(keys) WITH ORDINALITY k(key,ordinal)),'[]'::jsonb));
END $$;
CREATE OR REPLACE FUNCTION interaction_destination(actor bigint,destination_kind text,destination uuid)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE t interaction_target%ROWTYPE; context jsonb; comment_value jsonb;
BEGIN
 IF destination_kind='comment' THEN
   SELECT target.* INTO t FROM interaction_comment c JOIN interaction_target target ON target.id=c.target_id
     WHERE c.id=destination;
 ELSIF destination_kind='target' THEN SELECT * INTO t FROM interaction_target WHERE id=destination;
 ELSE RETURN '{"error":"invalid"}'; END IF;
 IF NOT FOUND THEN RETURN '{"error":"unavailable"}'; END IF;
 context:=coalesce(interaction_target_context(t.id,actor),interaction_moderation_context(t.id,actor));
 IF context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF destination_kind='comment' THEN
   comment_value:=interaction_comment_context(t.id,actor,destination);
   IF comment_value ? 'error' THEN RETURN comment_value; END IF;
 END IF;
 RETURN context||jsonb_build_object('targetId',t.id,'commentId',CASE WHEN destination_kind='comment' THEN destination END,
   'context',comment_value);
END $$;
CREATE OR REPLACE FUNCTION interaction_summary(actor bigint,kind text,entity text)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE target uuid; t interaction_target%ROWTYPE; context jsonb; choices jsonb;
 total_comments bigint; total_roots bigint; selected uuid; mode_value text; author_ids bigint[]; can_select boolean;
BEGIN
 target:=interaction_register(kind,entity,actor);
 IF target IS NULL AND interaction_is_moderator(actor) THEN
   SELECT * INTO t FROM interaction_target WHERE entity_kind=kind AND entity_key=interaction_normalize_key(kind,entity) AND retired_at IS NULL;
   context:=interaction_moderation_context(t.id,actor);
   IF context IS NOT NULL THEN
     -- A moderation entry point does not grant social writes or reveal counters.
     RETURN context||jsonb_build_object('id',t.id,'version',t.version,'commentPolicy',t.comment_policy,
       'canManage',false,'canReact',false,'canComment',false,'canModerate',true,
       'reactable',false,'commentable',true,'shareable',false,'mentionedPeople','[]'::jsonb,
       'reactions','[]'::jsonb,'myReactionTypeId',NULL,'commentCount',0,'rootCount',0,
       'subscription','muted','defaultSort','newest');
   END IF;
 END IF;
 IF target IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 SELECT * INTO t FROM interaction_target WHERE id=target;
 context:=interaction_target_context(target,actor);
 can_select:=coalesce(actor IS NOT NULL AND interaction_domain_write(target,actor) AND (context->>'reactable')::boolean,false);
 SELECT array_agg(party_id) INTO author_ids FROM interaction_author_batch(actor,ARRAY(
   SELECT author_id FROM interaction_target_comment_total WHERE target_id=target AND comments>0
   UNION SELECT actor_id FROM interaction_reaction WHERE target_id=target));
 SELECT coalesce(sum(c.comments),0),coalesce(sum(c.roots),0) INTO total_comments,total_roots
 FROM interaction_target_comment_total c WHERE c.target_id=target AND c.author_id=ANY(author_ids);
 WITH counts AS (
   SELECT reaction_type_id,count(*) total FROM interaction_reaction WHERE target_id=target AND actor_id=ANY(author_ids) GROUP BY reaction_type_id
 ) SELECT coalesce(jsonb_agg(jsonb_build_object('id',r.id,'code',r.code,'emoji',r.emoji,'label',r.name_es,'count',coalesce(c.total,0),
   'selectable',can_select AND choice.reaction_type_id IS NOT NULL AND catalog.active AND catalog.code='content-reaction-types' AND w.workflow_id=catalog.workflow_id AND r.active AND r.deprecated_at IS NULL AND w.active AND w.code='published')
     ORDER BY choice.default_order NULLS LAST,r.sort_order),'[]'::jsonb)
 INTO choices FROM content_reaction_type r JOIN workflow_state w ON w.id=r.workflow_state_id
 JOIN catalog_definition catalog ON catalog.id=r.catalog_id
 LEFT JOIN interaction_reaction_choice choice ON choice.reaction_type_id=r.id LEFT JOIN counts c ON c.reaction_type_id=r.id
 WHERE (choice.reaction_type_id IS NOT NULL AND catalog.active AND catalog.code='content-reaction-types' AND w.workflow_id=catalog.workflow_id AND r.active AND r.deprecated_at IS NULL AND w.active AND w.code='published') OR c.total>0;
 SELECT reaction_type_id INTO selected FROM interaction_reaction WHERE target_id=target AND actor_id=actor;
 SELECT mode INTO mode_value FROM interaction_subscription WHERE target_id=target AND party_id=actor;
 RETURN context||jsonb_build_object('id',target,'version',t.version,'commentPolicy',t.comment_policy,
   'canReact',can_select OR selected IS NOT NULL,'canComment',coalesce(interaction_can_comment(target,actor),false),
   'mentionedPeople',CASE WHEN (context->>'canManage')::boolean THEN coalesce((SELECT jsonb_agg(a.author ORDER BY a.party_id)
     FROM interaction_author_batch(actor,ARRAY(SELECT party_id FROM interaction_target_mention WHERE target_id=target)) a),'[]'::jsonb) ELSE '[]'::jsonb END,
   'canModerate',coalesce(interaction_is_moderator(actor),false),'commentCount',total_comments,'rootCount',total_roots,
   'reactions',choices,'myReactionTypeId',selected,'subscription',coalesce(mode_value,'participating'),'defaultSort','newest');
END $$;
COMMIT;
