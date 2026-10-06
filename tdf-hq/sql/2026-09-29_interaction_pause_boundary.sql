-- Freeze serializes with registration, commands, preferences, blocks and delivery.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE OR REPLACE FUNCTION interaction_writes_enabled() RETURNS boolean
LANGUAGE plpgsql AS $$
DECLARE permitted boolean;
BEGIN
 SELECT enabled INTO permitted FROM interaction_runtime WHERE singleton FOR SHARE;
 RETURN coalesce(permitted,false);
END $$;
CREATE OR REPLACE FUNCTION interaction_register(kind text,entity text,actor bigint)
RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE target uuid; context jsonb;
BEGIN
 context:=interaction_resolve(kind,entity,actor);
 IF context IS NULL THEN RETURN NULL; END IF;
 entity:=context->>'key';
 -- Existing discussions need no registration write or global runtime row lock.
 SELECT id INTO target FROM interaction_target WHERE entity_kind=kind AND entity_key=entity AND retired_at IS NULL;
 IF target IS NOT NULL THEN RETURN target; END IF;
 IF interaction_writes_enabled() THEN
   INSERT INTO interaction_target(entity_kind,entity_key) VALUES(kind,entity)
     ON CONFLICT(entity_kind,entity_key) DO NOTHING;
 END IF;
 SELECT id INTO target FROM interaction_target WHERE entity_kind=kind AND entity_key=entity
   AND retired_at IS NULL;
 RETURN target;
END $$;
CREATE OR REPLACE FUNCTION interaction_dispatch_events(batch_size integer DEFAULT 20) RETURNS integer
LANGUAGE plpgsql AS $$
DECLARE e interaction_event%ROWTYPE; context jsonb; comment_row interaction_comment%ROWTYPE;
 recipient record; kind_value text; dedupe text; notif bigint; processed integer:=0; recipients_seen integer; activity_changed boolean;
BEGIN
 IF batch_size NOT BETWEEN 1 AND 50 THEN RAISE EXCEPTION 'Invalid dispatch size'; END IF;
 IF NOT interaction_writes_enabled() THEN RETURN 0; END IF;
 FOR e IN SELECT * FROM interaction_event WHERE completed_at IS NULL ORDER BY id
   LIMIT batch_size FOR UPDATE SKIP LOCKED LOOP
   comment_row:=NULL;
   context:=interaction_target_context(e.target_id,e.actor_id);
   -- Owner hiding uses ordinary authority; only platform enforcement may need
   -- the strict moderation adapter after a publication-owner social block.
   IF context IS NULL AND e.kind='moderation' THEN
     context:=interaction_moderation_context(e.target_id,e.actor_id);
   END IF;
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
     WHERE CASE WHEN kind='moderation' THEN interaction_moderation_notice_allowed(e.target_id,recipient.party_id,e.actor_id,e.comment_id)
       ELSE interaction_notification_allowed(e.target_id,recipient.party_id,e.actor_id,kind) END
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
CREATE OR REPLACE FUNCTION interaction_block(actor bigint,peer bigint,blocked boolean,
 expected bigint,request_id uuid) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE p social_v2_pair%ROWTYPE; old social_v2_command%ROWTYPE; op text;
BEGIN
 IF NOT interaction_writes_enabled() THEN RETURN '{"error":"disabled"}'; END IF;
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
CREATE OR REPLACE FUNCTION interaction_preferences(actor bigint,settings jsonb DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE result_value jsonb;
BEGIN
 IF NOT interaction_actor_live(actor) THEN RETURN '{"error":"unavailable"}'; END IF;
 IF settings IS NOT NULL THEN
   IF NOT interaction_writes_enabled() THEN RETURN '{"error":"disabled"}'; END IF;
   IF jsonb_typeof(settings)<>'object' OR settings-ARRAY['reactions','comments','replies','mentions']<>'{}'::jsonb
     OR NOT (settings ?& ARRAY['reactions','comments','replies','mentions'])
     OR EXISTS(SELECT 1 FROM jsonb_each(settings) s WHERE jsonb_typeof(s.value)<>'boolean')
   THEN RETURN '{"error":"invalid"}'; END IF;
   INSERT INTO interaction_notification_preference(party_id,reactions,comments,replies,mentions)
   VALUES(actor,(settings->>'reactions')::boolean,(settings->>'comments')::boolean,
     (settings->>'replies')::boolean,(settings->>'mentions')::boolean)
   ON CONFLICT(party_id) DO UPDATE SET reactions=excluded.reactions,comments=excluded.comments,
     replies=excluded.replies,mentions=excluded.mentions,updated_at=now();
 END IF;
 SELECT jsonb_build_object('reactions',p.reactions,'comments',p.comments,'replies',p.replies,'mentions',p.mentions)
 INTO result_value FROM interaction_notification_preference p WHERE p.party_id=actor;
 RETURN coalesce(result_value,'{"reactions":true,"comments":true,"replies":true,"mentions":true}'::jsonb);
END $$;
COMMIT;
