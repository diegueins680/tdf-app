-- One mention authority for selection, writes, owner allowlists and delivery.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE OR REPLACE FUNCTION interaction_mention_eligible(target uuid,actor bigint,peer bigint)
RETURNS boolean LANGUAGE sql STABLE AS $$
 SELECT actor<>peer AND interaction_actor_live(actor) AND interaction_actor_live(peer)
   AND EXISTS(SELECT 1 FROM party p WHERE p.id=peer AND NOT p.is_org)
   AND NOT interaction_blocked(actor,peer)
   AND interaction_target_context(target,actor) IS NOT NULL
   AND interaction_target_context(target,peer) IS NOT NULL
   AND (EXISTS(SELECT 1 FROM social_v2_preference p WHERE p.party_id=peer AND p.discoverable)
     OR EXISTS(SELECT 1 FROM social_v2_pair p WHERE p.party_a=least(actor,peer) AND p.party_b=greatest(actor,peer)
       AND p.consent_a AND p.consent_b))
$$;
CREATE OR REPLACE FUNCTION interaction_mentions_valid(target uuid,actor bigint,body text,mentions jsonb)
RETURNS boolean LANGUAGE plpgsql STABLE AS $$
DECLARE mention jsonb; start_at integer; end_at integer; previous_end integer:=0; peer bigint;
BEGIN
 IF mentions IS NULL OR jsonb_typeof(mentions)<>'array' OR jsonb_array_length(mentions)>20 THEN RETURN false; END IF;
 FOR mention IN SELECT value FROM jsonb_array_elements(mentions) LOOP
   IF jsonb_typeof(mention) IS DISTINCT FROM 'object' OR (mention-'partyId'-'start'-'end')<>'{}'::jsonb
     OR coalesce(mention->>'partyId','') !~ '^[1-9][0-9]{0,17}$'
     OR coalesce(mention->>'start','') !~ '^[0-9]{1,4}$' OR coalesce(mention->>'end','') !~ '^[0-9]{1,4}$'
     OR NOT (mention ?& ARRAY['partyId','start','end']) THEN RETURN false; END IF;
   peer:=(mention->>'partyId')::bigint; start_at:=(mention->>'start')::integer; end_at:=(mention->>'end')::integer;
   IF start_at<previous_end OR end_at<=start_at OR end_at>length(body) OR end_at-start_at>120
     OR substring(body FROM start_at+1 FOR 1)<>'@' OR NOT interaction_mention_eligible(target,actor,peer)
     THEN RETURN false; END IF;
   previous_end:=end_at;
 END LOOP;
 RETURN true;
END $$;
CREATE OR REPLACE FUNCTION interaction_mention_candidates(actor bigint,target uuid,query_text text,after_party bigint,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE result_value jsonb; cursor_value bigint; term text;
BEGIN
 IF NOT interaction_actor_live(actor) OR interaction_target_context(target,actor) IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 IF length(btrim(query_text)) NOT BETWEEN 2 AND 120 OR page_size NOT BETWEEN 1 AND 50
   OR after_party<0 THEN RETURN '{"error":"invalid"}'; END IF;
 term:='%'||replace(replace(replace(lower(btrim(query_text)),chr(92),chr(92)||chr(92)),'%',chr(92)||'%'),'_',chr(92)||'_')||'%';
 WITH page AS MATERIALIZED (
   SELECT p.id,p.display_name,f.avatar_url,u.username FROM party p
   LEFT JOIN fan_profile f ON f.fan_party_id=p.id
   JOIN LATERAL (SELECT c.username FROM user_credential c WHERE c.party_id=p.id AND c.active ORDER BY c.id LIMIT 1) u ON true
   WHERE p.id>coalesce(after_party,0) AND p.id<>actor AND NOT p.is_org
     AND (lower(p.display_name) LIKE term OR lower(u.username) LIKE term)
     AND interaction_mention_eligible(target,actor,p.id)
   ORDER BY p.id LIMIT page_size+1
 ), numbered AS (SELECT *,row_number() OVER(ORDER BY id) ordinal FROM page)
 SELECT coalesce(jsonb_agg(jsonb_build_object('partyId',id,'partyType','person','displayName',display_name,
   'username',username,'avatarUrl',avatar_url,'accountStatus','active','secondaryLabel',NULL) ORDER BY id)
     FILTER(WHERE ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(id ORDER BY id))[page_size] END
 INTO result_value,cursor_value FROM numbered;
 RETURN jsonb_build_object('items',result_value,'nextCursor',cursor_value);
END $$;
CREATE OR REPLACE FUNCTION interaction_notification_allowed(target uuid,recipient bigint,actor bigint,kind text)
RETURNS boolean LANGUAGE sql STABLE AS $$
 SELECT recipient<>actor AND interaction_actor_live(recipient) AND interaction_actor_live(actor)
   AND NOT interaction_blocked(recipient,actor) AND interaction_target_context(target,recipient) IS NOT NULL
   AND (kind<>'mention' OR interaction_mention_eligible(target,actor,recipient))
   AND NOT EXISTS(SELECT 1 FROM interaction_subscription s WHERE s.target_id=target AND s.party_id=recipient AND s.mode='muted')
   AND coalesce((SELECT CASE kind WHEN 'reaction' THEN p.reactions WHEN 'comment' THEN p.comments
      WHEN 'reply' THEN p.replies WHEN 'mention' THEN p.mentions ELSE true END
     FROM interaction_notification_preference p WHERE p.party_id=recipient),true)
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
   UNION SELECT CASE WHEN id ~ '^[1-9][0-9]{0,17}$' THEN id::bigint END
     FROM jsonb_array_elements_text(CASE WHEN jsonb_typeof(payload->'mentionedPartyIds')='array' THEN payload->'mentionedPartyIds' ELSE '[]'::jsonb END) p(id)
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
      WHERE NOT interaction_mention_eligible(target,actor,id::bigint)) THEN RETURN '{"error":"invalid"}'; END IF;
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
COMMIT;
