-- Audited notices about the recipient's own content are system activity, not
-- social contact. Their generic text discloses neither actor nor private body.
-- A blocked/unavailable publication still fails closed when the link is opened.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE INDEX IF NOT EXISTS interaction_audit_moderation_notice
 ON interaction_audit(comment_id,actor_id,target_id)
 WHERE operation IN ('comment.hide','comment.remove');
CREATE OR REPLACE FUNCTION interaction_moderation_notice_allowed(target uuid,recipient bigint,actor bigint,comment_key uuid)
RETURNS boolean LANGUAGE sql STABLE AS $$
 SELECT recipient<>actor AND interaction_actor_live(recipient) AND interaction_actor_live(actor)
   AND EXISTS(SELECT 1 FROM interaction_comment c JOIN interaction_target t ON t.id=c.target_id
     WHERE c.id=comment_key AND c.target_id=target AND c.author_id=recipient AND t.retired_at IS NULL
       AND EXISTS(SELECT 1 FROM interaction_audit a WHERE a.comment_id=c.id AND a.target_id=t.id
         AND a.actor_id=actor AND a.operation IN ('comment.hide','comment.remove')))
   AND NOT EXISTS(SELECT 1 FROM interaction_subscription s
     WHERE s.target_id=target AND s.party_id=recipient AND s.mode='muted')
$$;
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
CREATE OR REPLACE FUNCTION interaction_notification_visible(notification_key bigint,recipient bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT NOT EXISTS(SELECT 1 FROM interaction_notification WHERE notification_id=notification_key)
   OR EXISTS(SELECT 1 FROM interaction_notification n JOIN interaction_notification_actor a USING(notification_id)
     WHERE n.notification_id=notification_key AND n.recipient_id=recipient
       AND CASE WHEN n.event_kind='moderation' THEN interaction_moderation_notice_allowed(n.target_id,recipient,a.actor_id,n.comment_id)
         ELSE interaction_notification_allowed(n.target_id,recipient,a.actor_id,n.event_kind) END
       AND (n.comment_id IS NULL OR EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=n.comment_id
         AND (c.state IN ('visible','deleted') OR n.event_kind='moderation')))
       AND (n.event_kind<>'reaction' OR EXISTS(SELECT 1 FROM interaction_reaction r
         WHERE r.target_id=n.target_id AND r.actor_id=a.actor_id)))
$$;
COMMIT;
