-- Forward repair: preserve every previously registered migration checksum.
BEGIN;
SET LOCAL lock_timeout='5s';
DO $$ BEGIN
 IF NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public'
   AND table_name='interaction_event' AND column_name='mention_party_ids') THEN
   ALTER TABLE interaction_event ADD COLUMN mention_party_ids bigint[] NOT NULL DEFAULT '{}';
   -- Only pending pre-upgrade events retain their current mention recipients.
   -- Never repeat this backfill on newly generated delta-only edit events.
   UPDATE interaction_event e SET mention_party_ids=ARRAY(SELECT DISTINCT party_id
     FROM interaction_comment_mention WHERE comment_id=e.comment_id ORDER BY party_id)
   WHERE e.completed_at IS NULL AND e.kind IN ('comment','edit');
 END IF;
END $$;
ALTER TABLE interaction_notification ADD COLUMN IF NOT EXISTS last_event_id bigint NOT NULL DEFAULT 0;
CREATE INDEX IF NOT EXISTS interaction_report_comment_open ON interaction_report(comment_id,created_at DESC,id DESC) WHERE state='open';


CREATE OR REPLACE FUNCTION interaction_command(actor bigint,target uuid,request_id uuid,payload jsonb)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE context jsonb; op text; fingerprint text; prior interaction_request%ROWTYPE;
 t interaction_target%ROWTYPE; c interaction_comment%ROWTYPE; parent interaction_comment%ROWTYPE;
 result_value jsonb; body_value text; mentions jsonb; comment_key uuid; parent_key uuid;
 initial_context jsonb; reaction_changed boolean:=false; new_mentions bigint[];
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
 initial_context:=interaction_target_context(target,actor);
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
 IF interaction_target_context(target,actor)->>'ownerId' IS DISTINCT FROM initial_context->>'ownerId'
 THEN RETURN '{"error":"revision_conflict"}'; END IF;
 -- Serialize an actor's command budget and idempotency keys across targets.
 PERFORM pg_advisory_xact_lock(hashtextextended('interaction-actor:'||actor,0));
 -- The API also holds its current session lock. Target serialization protects
 -- absent reactions, idempotent creation and settings changes in one transaction.
 SELECT * INTO t FROM interaction_target WHERE id=target FOR UPDATE;
 context:=interaction_target_context(target,actor);
 IF NOT FOUND OR context IS NULL THEN RETURN '{"error":"unavailable"}'; END IF;
 fingerprint:=encode(digest(payload::text,'sha256'),'hex');
 SELECT * INTO prior FROM interaction_request WHERE actor_id=actor AND request_key=request_id;
 IF FOUND THEN
   IF prior.target_id<>target OR prior.payload_hash<>fingerprint THEN RETURN '{"error":"request_key_conflict"}'; END IF;
   -- Reconcile a replay against current state; stored responses must never
   -- disclose a subsequently removed body or resurrect an old reaction.
   IF op LIKE 'comment.%' AND op<>'comment.report' THEN
     SELECT * INTO c FROM interaction_comment WHERE id=(prior.result->>'id')::uuid AND target_id=target;
     IF NOT FOUND OR interaction_blocked(actor,c.author_id) THEN RETURN '{"error":"unavailable"}'; END IF;
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
   IF NOT FOUND OR interaction_blocked(actor,c.author_id) THEN RETURN '{"error":"unavailable"}'; END IF;
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
   IF NOT interaction_domain_write(target,actor) OR NOT (context->>'reactable')::boolean OR NOT (payload ? 'reactionTypeId') THEN RETURN '{"error":"invalid"}'; END IF;
   IF payload->'reactionTypeId'<>'null'::jsonb THEN
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

-- Directory workflows and account controls expose the same effective block.
-- Removing one's own block never removes a block owned by the other party.
CREATE OR REPLACE FUNCTION interaction_owned_directory_block(actor bigint,peer bigint)
RETURNS boolean LANGUAGE sql STABLE AS $$
 SELECT EXISTS(SELECT 1 FROM directory_profile_block b
   JOIN directory_profile p ON p.id=b.blocker_profile_id
   JOIN directory_profile q ON q.id=b.blocked_profile_id
   WHERE p.subject_party_id=actor AND q.subject_party_id=peer)
$$;
CREATE OR REPLACE FUNCTION interaction_block_state(actor bigint,peer bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 SELECT jsonb_build_object('partyId',peer,'blocked',
   (CASE WHEN p.party_a=actor THEN coalesce(p.block_a,false) ELSE coalesce(p.block_b,false) END)
     OR interaction_owned_directory_block(actor,peer),'version',coalesce(p.revision,0))
 FROM (VALUES(1)) seed(n) LEFT JOIN social_v2_pair p
 ON p.party_a=least(actor,peer) AND p.party_b=greatest(actor,peer)
$$;
CREATE OR REPLACE FUNCTION interaction_sever_follows(actor bigint,peer bigint) RETURNS void
LANGUAGE sql AS $$
 UPDATE social_v2_pair SET consent_a=false,consent_b=false,follow_a=false,follow_b=false
 WHERE party_a=least(actor,peer) AND party_b=greatest(actor,peer);
 DELETE FROM fan_follow WHERE (fan_party_id=actor AND artist_party_id=peer)
   OR (fan_party_id=peer AND artist_party_id=actor);
 DELETE FROM party_follow WHERE (follower_party_id=actor AND following_party_id=peer)
   OR (follower_party_id=peer AND following_party_id=actor)
$$;
-- Legacy directory writes share the account lock and revision boundary after
-- activation. A stale account-controls screen must not undo a newer block.
CREATE OR REPLACE FUNCTION interaction_directory_block_revision() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE actor bigint; peer bigint;
BEGIN
 IF EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND activated_once) THEN
   SELECT subject_party_id INTO actor FROM directory_profile
     WHERE id=CASE WHEN TG_OP='DELETE' THEN OLD.blocker_profile_id ELSE NEW.blocker_profile_id END;
   SELECT subject_party_id INTO peer FROM directory_profile
     WHERE id=CASE WHEN TG_OP='DELETE' THEN OLD.blocked_profile_id ELSE NEW.blocked_profile_id END;
   IF actor IS NOT NULL AND peer IS NOT NULL AND actor<>peer THEN
     PERFORM id FROM party WHERE id IN (actor,peer) ORDER BY id FOR UPDATE;
     INSERT INTO social_v2_pair(party_a,party_b,revision) VALUES(least(actor,peer),greatest(actor,peer),1)
       ON CONFLICT(party_a,party_b) DO UPDATE SET revision=social_v2_pair.revision+1,updated_at=now();
     IF TG_OP='INSERT' THEN PERFORM interaction_sever_follows(actor,peer); END IF;
   END IF;
 END IF;
 RETURN CASE WHEN TG_OP='DELETE' THEN OLD ELSE NEW END;
END $$;
DROP TRIGGER IF EXISTS interaction_directory_block_revision ON directory_profile_block;
CREATE TRIGGER interaction_directory_block_revision BEFORE INSERT OR DELETE ON directory_profile_block
 FOR EACH ROW EXECUTE FUNCTION interaction_directory_block_revision();

CREATE OR REPLACE FUNCTION interaction_block(actor bigint,peer bigint,blocked boolean,
 expected bigint,request_id uuid) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE p social_v2_pair%ROWTYPE; old social_v2_command%ROWTYPE; op text;
BEGIN
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled) THEN RETURN '{"error":"disabled"}'; END IF;
 IF actor IS NULL OR peer IS NULL OR actor=peer OR peer<=0 OR blocked IS NULL OR expected IS NULL OR expected<0 OR request_id IS NULL
 THEN RETURN '{"error":"invalid"}'; END IF;
 PERFORM id FROM party WHERE id IN (actor,peer) ORDER BY id FOR UPDATE;
 PERFORM id FROM user_credential WHERE party_id IN (actor,peer) ORDER BY id FOR UPDATE;
 IF NOT interaction_actor_live(actor) OR NOT EXISTS(SELECT 1 FROM party WHERE id=peer) THEN RETURN '{"error":"unavailable"}'; END IF;
 INSERT INTO social_v2_pair(party_a,party_b) VALUES(least(actor,peer),greatest(actor,peer)) ON CONFLICT DO NOTHING;
 SELECT * INTO STRICT p FROM social_v2_pair WHERE party_a=least(actor,peer) AND party_b=greatest(actor,peer) FOR UPDATE;
 op:=CASE WHEN blocked THEN 'block' ELSE 'unblock' END;
 SELECT * INTO old FROM social_v2_command c WHERE c.actor=interaction_block.actor
   AND c.request_key='interaction:'||request_id;
 IF FOUND THEN
   IF old.target<>peer OR old.operation<>op OR old.expected_revision<>expected THEN RETURN '{"error":"request_key_conflict"}'; END IF;
   RETURN interaction_block_state(actor,peer)||'{"replay":true}'::jsonb;
 END IF;
 IF p.revision<>expected THEN RETURN '{"error":"revision_conflict"}'; END IF;
 IF (SELECT count(*) FROM social_v2_command c WHERE c.actor=interaction_block.actor
   AND c.created_at>now()-interval '1 minute')>=30 THEN RETURN '{"error":"rate_limited"}'; END IF;
 UPDATE social_v2_pair SET
   block_a=CASE WHEN actor=party_a THEN blocked ELSE block_a END,
   block_b=CASE WHEN actor=party_b THEN blocked ELSE block_b END,
   consent_a=CASE WHEN blocked THEN false ELSE consent_a END,
   consent_b=CASE WHEN blocked THEN false ELSE consent_b END,
   follow_a=CASE WHEN blocked THEN false ELSE follow_a END,
   follow_b=CASE WHEN blocked THEN false ELSE follow_b END,
   revision=revision+1,updated_at=now()
 WHERE party_a=p.party_a AND party_b=p.party_b;
 -- Both relationship stores are still read during incremental consolidation.
 -- Account locks above serialize this revocation with admitted follow writers.
 -- Unblocking clears only the block; it never recreates either direction.
 IF blocked OR interaction_owned_directory_block(actor,peer) THEN
   PERFORM interaction_sever_follows(actor,peer);
 END IF;
 IF NOT blocked THEN
   DELETE FROM directory_profile_block b USING directory_profile blocker_profile,directory_profile blocked_profile
   WHERE blocker_profile.id=b.blocker_profile_id AND blocked_profile.id=b.blocked_profile_id
     AND blocker_profile.subject_party_id=actor AND blocked_profile.subject_party_id=peer;
 END IF;
 INSERT INTO social_v2_command(actor,request_key,target,operation,expected_revision,result)
 VALUES(actor,'interaction:'||request_id,peer,op,expected,interaction_block_state(actor,peer));
 INSERT INTO interaction_audit(actor_id,operation,reason) VALUES(actor,'user.'||op,'Account block updated');
 RETURN interaction_block_state(actor,peer);
END $$;

CREATE OR REPLACE FUNCTION interaction_enqueue_event() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE operation_code text;
BEGIN
 -- Command payloads are not retained: only operation/result identities. The
 -- fingerprint and event type are passed via the result's bounded audit metadata.
 operation_code:=NEW.result->>'eventKind';
 IF operation_code IN ('reaction','comment','edit','moderation') THEN
   INSERT INTO interaction_event(target_id,actor_id,comment_id,kind,request_key,mention_party_ids)
   VALUES(NEW.target_id,NEW.actor_id,(NEW.result->>'commentId')::uuid,operation_code,NEW.request_key,ARRAY(SELECT value::bigint FROM jsonb_array_elements_text(coalesce(NEW.result->'mentionedPartyIds','[]'::jsonb))))
   ON CONFLICT(actor_id,request_key) DO NOTHING;
 END IF;
 RETURN NULL;
END $$;

CREATE OR REPLACE FUNCTION interaction_dispatch_events(batch_size integer DEFAULT 20) RETURNS integer
LANGUAGE plpgsql AS $$
DECLARE e interaction_event%ROWTYPE; context jsonb; comment_row interaction_comment%ROWTYPE;
 recipient record; kind_value text; dedupe text; notif bigint; processed integer:=0; recipients_seen integer; activity_changed boolean;
BEGIN
 IF batch_size NOT BETWEEN 1 AND 50 THEN RAISE EXCEPTION 'Invalid dispatch size'; END IF;
 IF NOT EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled) THEN RETURN 0; END IF;
 FOR e IN SELECT * FROM interaction_event WHERE completed_at IS NULL ORDER BY id
   LIMIT batch_size FOR UPDATE SKIP LOCKED LOOP
   comment_row:=NULL;
   context:=interaction_target_context(e.target_id,e.actor_id);
   IF context IS NULL THEN UPDATE interaction_event SET completed_at=now() WHERE id=e.id; CONTINUE; END IF;
   IF e.comment_id IS NOT NULL THEN
     SELECT * INTO comment_row FROM interaction_comment WHERE id=e.comment_id AND target_id=e.target_id;
     IF NOT FOUND OR (e.kind<>'moderation' AND comment_row.state<>'visible') THEN
       UPDATE interaction_event SET completed_at=now() WHERE id=e.id; CONTINUE;
     END IF;
   END IF;
   recipients_seen:=0;
   -- Bound fanout work per event/transaction. A durable recipient cursor resumes
   -- large opt-in discussions; recipient choice never depends on display text.
   FOR recipient IN
     WITH candidates AS (
       SELECT (context->>'ownerId')::bigint party_id,1 priority
         WHERE e.kind IN ('reaction','comment')
       UNION ALL SELECT c.author_id,2 FROM interaction_comment c
         WHERE c.id=comment_row.parent_id AND e.kind='comment'
       UNION ALL SELECT m.party_id,3 FROM interaction_comment_mention m
         WHERE m.comment_id=e.comment_id AND m.party_id=ANY(e.mention_party_ids) AND e.kind IN ('comment','edit')
       UNION ALL SELECT s.party_id,1 FROM interaction_subscription s
         WHERE s.target_id=e.target_id AND s.mode='all' AND e.kind='comment'
       UNION ALL SELECT comment_row.author_id,4 WHERE e.kind='moderation'
     ) SELECT party_id,array_agg(DISTINCT priority ORDER BY priority DESC) priorities FROM candidates
       WHERE party_id>e.recipient_cursor AND party_id<>e.actor_id GROUP BY party_id ORDER BY party_id LIMIT 50
   LOOP
     recipients_seen:=recipients_seen+1;
     UPDATE interaction_event SET recipient_cursor=recipient.party_id WHERE id=e.id;
     PERFORM id FROM party WHERE id IN (recipient.party_id,e.actor_id,(context->>'ownerId')::bigint) ORDER BY id FOR SHARE;
     PERFORM id FROM interaction_target WHERE id=e.target_id FOR SHARE;
     -- Pick the strongest enabled reason, retaining reply/comment fallbacks
     -- when the same activity is also a mention that this recipient disabled.
     SELECT kind INTO kind_value FROM (SELECT priority,CASE priority
       WHEN 4 THEN 'moderation' WHEN 3 THEN 'mention' WHEN 2 THEN 'reply'
       ELSE CASE WHEN e.kind='reaction' THEN 'reaction' ELSE 'comment' END END kind
       FROM unnest(recipient.priorities) priority) reasons
     WHERE interaction_notification_allowed(e.target_id,recipient.party_id,e.actor_id,kind)
     ORDER BY priority DESC LIMIT 1;
     IF kind_value IS NULL THEN CONTINUE; END IF;
     IF e.kind='reaction' AND NOT EXISTS(SELECT 1 FROM interaction_reaction r
       WHERE r.target_id=e.target_id AND r.actor_id=e.actor_id) THEN CONTINUE; END IF;
     dedupe:=CASE WHEN e.kind='reaction' THEN e.target_id::text||':'||floor(extract(epoch FROM e.created_at)/600)::bigint
       ELSE e.comment_id::text END;
     -- Serialize recipient+target buckets across workers before the absent insert.
     PERFORM pg_advisory_xact_lock(hashtextextended('interaction-notification:'||recipient.party_id||':'||e.target_id,0));
     SELECT notification_id INTO notif FROM interaction_notification n
       WHERE n.recipient_id=recipient.party_id AND n.dedupe_key=dedupe
         AND (n.event_kind=kind_value OR (n.event_kind IN ('comment','reply','mention') AND kind_value IN ('comment','reply','mention')));
     IF NOT FOUND THEN
       INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at)
       VALUES(recipient.party_id,'interaction.'||kind_value,
         CASE kind_value WHEN 'reaction' THEN 'Nuevas reacciones' WHEN 'reply' THEN 'Nueva respuesta'
           WHEN 'mention' THEN 'Te mencionaron' WHEN 'moderation' THEN 'Actualización de moderación' ELSE 'Nuevo comentario' END,
         'Abre la conversación para ver la actividad.',
         CASE WHEN e.comment_id IS NULL THEN 'interaction_target' ELSE 'interaction_comment' END,
         coalesce(e.comment_id,e.target_id)::text,false,now()) RETURNING id INTO notif;
       INSERT INTO interaction_notification(notification_id,target_id,comment_id,recipient_id,event_kind,dedupe_key)
       VALUES(notif,e.target_id,e.comment_id,recipient.party_id,kind_value,dedupe);
     END IF;
     -- Refresh an aggregate once for genuinely new activity, including a newly
     -- added mention. Event identity prevents replay/out-of-order delivery from
     -- resurrecting read state; command no-ops never enqueue events.
     UPDATE interaction_notification SET last_event_id=e.id,event_kind=kind_value
       WHERE notification_id=notif AND last_event_id<e.id;
     activity_changed:=FOUND;
     IF activity_changed THEN
       UPDATE notification SET is_read=false,created_at=greatest(created_at,e.created_at),
         notif_type='interaction.'||kind_value,title=CASE kind_value
           WHEN 'reaction' THEN 'Nuevas reacciones' WHEN 'reply' THEN 'Nueva respuesta'
           WHEN 'mention' THEN 'Te mencionaron' WHEN 'moderation' THEN 'Actualización de moderación' ELSE 'Nuevo comentario' END
       WHERE id=notif;
     END IF;
     INSERT INTO interaction_notification_actor(notification_id,actor_id) VALUES(notif,e.actor_id) ON CONFLICT DO NOTHING;
   END LOOP;
   IF recipients_seen<50 THEN UPDATE interaction_event SET completed_at=now() WHERE id=e.id; END IF;
   processed:=processed+1;
 END LOOP;
 RETURN processed;
END $$;

CREATE OR REPLACE FUNCTION interaction_migrate_legacy() RETURNS void LANGUAGE plpgsql AS $$
DECLARE row_value record; target uuid; comment_key uuid; parent_key uuid; root_key uuid;
 parent_depth integer; source_counts jsonb; migrated_counts jsonb;
BEGIN
 IF EXISTS(SELECT 1 FROM interaction_legacy_cutover) THEN RETURN; END IF;
 LOCK TABLE fan_club_post,fan_club_post_reaction,fan_club_memory,fan_club_memory_reaction,
   event_moment,event_moment_comment,event_moment_reaction IN SHARE ROW EXCLUSIVE MODE;
 -- Fail the complete transaction on ambiguous identities; never discard a row
 -- by choosing an arbitrary winner or inventing an author account.
 IF EXISTS(SELECT 1 FROM fan_club_post p JOIN fan_club_post q ON q.id=p.parent_id WHERE p.club_id<>q.club_id)
   OR EXISTS(SELECT 1 FROM fan_club_post p LEFT JOIN fan_club_post q ON q.id=p.parent_id WHERE p.parent_id IS NOT NULL AND q.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_reaction r LEFT JOIN party p ON p.id::text=r.reactor_party_id WHERE p.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_comment c LEFT JOIN party p ON p.id::text=c.author_party_id WHERE p.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_reaction GROUP BY moment_id,reactor_party_id HAVING count(*)>1)
 THEN RAISE EXCEPTION 'Legacy interactions require identity/parent reconciliation before activation'; END IF;
 WITH RECURSIVE reachable AS (
   SELECT id,0 AS depth FROM fan_club_post WHERE parent_id IS NULL
   UNION ALL SELECT p.id,r.depth+1 FROM fan_club_post p JOIN reachable r ON p.parent_id=r.id WHERE r.depth<1000
 ) SELECT jsonb_build_object('reachable',count(*),'source',(SELECT count(*) FROM fan_club_post)) INTO source_counts FROM reachable;
 IF source_counts->>'reachable'<>source_counts->>'source' THEN RAISE EXCEPTION 'Legacy reply cycle or depth exceeds supported structure'; END IF;
 SELECT jsonb_build_object('clubReplies',(SELECT count(*) FROM fan_club_post WHERE parent_id IS NOT NULL),
   'momentComments',(SELECT count(*) FROM event_moment_comment),
   'postReactions',(SELECT count(*) FROM fan_club_post_reaction),
   'memoryReactions',(SELECT count(*) FROM fan_club_memory_reaction),
   'momentReactions',(SELECT count(*) FROM event_moment_reaction)) INTO source_counts;
 INSERT INTO interaction_target(entity_kind,entity_key)
 SELECT 'club_post',id::text FROM fan_club_post
 UNION ALL SELECT 'club_memory',id::text FROM fan_club_memory
 UNION ALL SELECT 'event_moment',id::text FROM event_moment
 ON CONFLICT(entity_kind,entity_key) DO NOTHING;
 FOR row_value IN WITH RECURSIVE posts AS (
   SELECT p.*,p.id AS top_id,0 AS nesting FROM fan_club_post p WHERE parent_id IS NULL
   UNION ALL SELECT p.*,r.top_id,r.nesting+1 FROM fan_club_post p JOIN posts r ON p.parent_id=r.id
 ) SELECT * FROM posts WHERE nesting>0 ORDER BY nesting,id LOOP
   SELECT id INTO target FROM interaction_target WHERE entity_kind='club_post' AND entity_key=row_value.top_id::text;
   comment_key:=gen_random_uuid(); parent_key:=NULL; root_key:=comment_key; parent_depth:=-1;
   IF row_value.nesting>1 THEN
     SELECT c.id,c.root_id,c.depth INTO parent_key,root_key,parent_depth FROM interaction_legacy_mapping m
       JOIN interaction_comment c ON c.id=m.comment_id WHERE m.legacy_kind='club_reply' AND m.legacy_id=row_value.parent_id::text;
     IF parent_key IS NULL THEN RAISE EXCEPTION 'Legacy reply parent was not converted'; END IF;
   END IF;
   INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body,state,created_at,updated_at,edited_at,legacy_kind,legacy_key)
   VALUES(comment_key,target,row_value.fan_party_id,parent_key,root_key,parent_depth+1,row_value.content,
     CASE WHEN row_value.is_hidden THEN 'hidden' ELSE 'visible' END,row_value.created_at,
     coalesce(row_value.updated_at,row_value.created_at),row_value.updated_at,'club_reply',row_value.id::text);
   INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
   VALUES('club_reply',row_value.id::text,target,comment_key,jsonb_build_object('title',row_value.title,'mediaUrls',row_value.media_urls,
     'sourceHash',encode(digest(to_jsonb(row_value)::text,'sha256'),'hex')));
 END LOOP;
 FOR row_value IN SELECT c.*,p.id AS author_id FROM event_moment_comment c LEFT JOIN party p ON p.id::text=c.author_party_id ORDER BY c.id LOOP
   SELECT id INTO target FROM interaction_target WHERE entity_kind='event_moment' AND entity_key=row_value.moment_id::text;
   comment_key:=gen_random_uuid();
   INSERT INTO interaction_comment(id,target_id,author_id,root_id,body,created_at,updated_at,edited_at,legacy_kind,legacy_key)
   VALUES(comment_key,target,row_value.author_id,comment_key,row_value.body,row_value.created_at,row_value.updated_at,
     CASE WHEN row_value.updated_at>row_value.created_at THEN row_value.updated_at END,'moment_comment',row_value.id::text);
   INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
   VALUES('moment_comment',row_value.id::text,target,comment_key,jsonb_build_object('sourceHash',encode(digest(to_jsonb(row_value)::text,'sha256'),'hex')));
 END LOOP;
 INSERT INTO interaction_reaction(target_id,actor_id,reaction_type_id,created_at,updated_at)
 SELECT t.id,r.reactor_party_id,r.reaction_type_id,r.created_at,r.created_at FROM fan_club_post_reaction r
   JOIN interaction_target t ON t.entity_kind='club_post' AND t.entity_key=r.post_id::text
 UNION ALL SELECT t.id,r.reactor_party_id,r.reaction_type_id,r.created_at,r.created_at FROM fan_club_memory_reaction r
   JOIN interaction_target t ON t.entity_kind='club_memory' AND t.entity_key=r.memory_id::text
 UNION ALL SELECT t.id,p.id,c.id,r.created_at,r.created_at FROM event_moment_reaction r
   JOIN party p ON p.id::text=r.reactor_party_id
   JOIN interaction_target t ON t.entity_kind='event_moment' AND t.entity_key=r.moment_id::text
   JOIN reaction_type old ON old.id=r.reaction_type_id
   JOIN content_reaction_type c ON c.code=CASE old.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE old.code END;
 INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,source_value)
 SELECT 'post_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM fan_club_post_reaction r JOIN interaction_target t ON t.entity_kind='club_post' AND t.entity_key=r.post_id::text
 UNION ALL SELECT 'memory_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM fan_club_memory_reaction r JOIN interaction_target t ON t.entity_kind='club_memory' AND t.entity_key=r.memory_id::text
 UNION ALL SELECT 'moment_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM event_moment_reaction r JOIN interaction_target t ON t.entity_kind='event_moment' AND t.entity_key=r.moment_id::text;
 SELECT jsonb_build_object('clubReplies',(SELECT count(*) FROM interaction_comment WHERE legacy_kind='club_reply'),
   'momentComments',(SELECT count(*) FROM interaction_comment WHERE legacy_kind='moment_comment'),
   'postReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_post'),
   'memoryReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_memory'),
   'momentReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='event_moment')) INTO migrated_counts;
 IF source_counts<>migrated_counts THEN RAISE EXCEPTION 'Legacy interaction reconciliation mismatch'; END IF;
 INSERT INTO interaction_legacy_cutover(singleton,completed_at,source_counts,migrated_counts) VALUES(true,now(),source_counts,migrated_counts);
END $$;

-- Reasons are sensitive moderation input. Owners without the moderator role
-- receive no report text or reporter identity. Bound output to the latest 20.
CREATE OR REPLACE FUNCTION interaction_report_reasons(actor bigint,comment_key uuid) RETURNS jsonb
LANGUAGE sql STABLE AS $$
 SELECT coalesce(jsonb_agg(reason ORDER BY created_at DESC,id DESC),'[]'::jsonb) FROM (
   SELECT r.reason,r.created_at,r.id FROM interaction_report r JOIN interaction_comment c ON c.id=r.comment_id
   WHERE r.comment_id=comment_key AND r.state='open' AND interaction_is_moderator(actor)
     AND interaction_target_context(c.target_id,actor) IS NOT NULL AND NOT interaction_blocked(actor,c.author_id)
   ORDER BY r.created_at DESC,r.id DESC LIMIT 20
 ) recent
$$;

CREATE OR REPLACE FUNCTION interaction_moderation_page(actor bigint,target uuid,before_id uuid,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE context jsonb; pivot interaction_comment%ROWTYPE; items jsonb; cursor_value uuid;
BEGIN
 context:=interaction_target_context(target,actor);
 IF context IS NULL OR NOT ((context->>'canManage')::boolean OR interaction_is_moderator(actor)) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 THEN RETURN '{"error":"invalid"}'; END IF;
 IF before_id IS NOT NULL THEN
   SELECT * INTO pivot FROM interaction_comment WHERE target_id=target AND id=before_id;
   IF NOT FOUND THEN RETURN '{"error":"invalid_cursor"}'; END IF;
 END IF;
 WITH page AS MATERIALIZED (
   SELECT c.* FROM interaction_comment c WHERE c.target_id=target
     AND NOT interaction_blocked(actor,c.author_id)
     AND (c.state='hidden' OR (interaction_is_moderator(actor) AND EXISTS(
       SELECT 1 FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open')))
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ), numbered AS (SELECT p.*,row_number() OVER(ORDER BY p.created_at DESC,p.id DESC) ordinal FROM page p)
 SELECT coalesce(jsonb_agg(interaction_comment_json(c,actor)||jsonb_build_object('moderationBody',c.body,
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
     AND interaction_target_context(c.target_id,actor) IS NOT NULL AND NOT interaction_blocked(actor,c.author_id)
     AND (before_id IS NULL OR (c.created_at,c.id)<(pivot.created_at,pivot.id))
   ORDER BY c.created_at DESC,c.id DESC LIMIT page_size+1
 ) page;
 IF array_length(keys,1)>page_size THEN cursor_value:=keys[page_size]; END IF;
 SELECT coalesce(jsonb_agg(j.value||jsonb_build_object('moderationBody',CASE WHEN c.state IN ('visible','hidden') THEN c.body ELSE '' END,
   'openReports',(SELECT count(*) FROM interaction_report r WHERE r.comment_id=c.id AND r.state='open'),
   'reportReasons',interaction_report_reasons(actor,c.id))
   ORDER BY array_position(keys,c.id)),'[]'::jsonb) INTO items FROM interaction_comments_json(keys[1:page_size],actor) j
 JOIN interaction_comment c ON c.id=j.comment_id;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;

CREATE OR REPLACE FUNCTION interaction_block_list(actor bigint,after_party bigint,page_size integer)
RETURNS jsonb LANGUAGE plpgsql STABLE AS $$
DECLARE items jsonb; cursor_value bigint;
BEGIN
 IF NOT interaction_actor_live(actor) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF page_size NOT BETWEEN 1 AND 50 OR after_party<0 THEN RETURN '{"error":"invalid"}'; END IF;
 WITH blocked AS (
   SELECT CASE WHEN party_a=actor THEN party_b ELSE party_a END peer FROM social_v2_pair
   WHERE (party_a=actor AND block_a) OR (party_b=actor AND block_b)
   UNION SELECT q.subject_party_id FROM directory_profile_block b
     JOIN directory_profile p ON p.id=b.blocker_profile_id
     JOIN directory_profile q ON q.id=b.blocked_profile_id
     WHERE p.subject_party_id=actor AND q.subject_party_id<>actor
 ), projected AS (
   SELECT b.peer,coalesce(p.revision,0) revision FROM blocked b LEFT JOIN social_v2_pair p
     ON p.party_a=least(actor,b.peer) AND p.party_b=greatest(actor,b.peer)
 ), page AS MATERIALIZED (
   SELECT b.peer,b.revision,p.display_name FROM projected b JOIN party p ON p.id=b.peer
   WHERE b.peer>coalesce(after_party,0) ORDER BY b.peer LIMIT page_size+1
 ), numbered AS (SELECT *,row_number() OVER(ORDER BY peer) ordinal FROM page)
 SELECT coalesce(jsonb_agg(jsonb_build_object('partyId',peer,'displayName',display_name,'version',revision,'blocked',true)
   ORDER BY peer) FILTER(WHERE ordinal<=page_size),'[]'::jsonb),
   CASE WHEN count(*)>page_size THEN (array_agg(peer ORDER BY peer))[page_size] END INTO items,cursor_value FROM numbered;
 RETURN jsonb_build_object('items',items,'nextCursor',cursor_value);
END $$;
COMMIT;
