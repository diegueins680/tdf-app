BEGIN;
SET LOCAL lock_timeout='5s';
CREATE TABLE IF NOT EXISTS interaction_migration_state (
 key text PRIMARY KEY,value jsonb NOT NULL
);
DO $$
DECLARE previous text;
BEGIN
 SELECT pg_get_expr(conbin,conrelid) INTO previous FROM pg_constraint
 WHERE conrelid='notification'::regclass AND conname='notification_notif_type_check' AND contype='c';
 IF previous IS NOT NULL AND NOT EXISTS(SELECT 1 FROM interaction_migration_state WHERE key='notification_constraint') THEN
   INSERT INTO interaction_migration_state VALUES('notification_constraint',to_jsonb(previous));
   ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
   EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''interaction.reaction'',''interaction.comment'',''interaction.reply'',''interaction.mention'',''interaction.moderation''))',previous);
 END IF;
END $$;
CREATE TABLE IF NOT EXISTS interaction_event (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 target_id uuid NOT NULL REFERENCES interaction_target(id),
 actor_id bigint NOT NULL REFERENCES party(id),
 comment_id uuid REFERENCES interaction_comment(id),
 kind text NOT NULL CHECK(kind IN ('reaction','comment','edit','moderation')),
 request_key uuid NOT NULL,
 recipient_cursor bigint NOT NULL DEFAULT 0,
 completed_at timestamptz,
 created_at timestamptz NOT NULL DEFAULT now(),
 UNIQUE(actor_id,request_key)
);
CREATE INDEX IF NOT EXISTS interaction_event_pending ON interaction_event(id) WHERE completed_at IS NULL;
CREATE OR REPLACE FUNCTION interaction_enqueue_event() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE operation_code text;
BEGIN
 -- Command payloads are not retained: only operation/result identities. The
 -- fingerprint and event type are passed via the result's bounded audit metadata.
 operation_code:=NEW.result->>'eventKind';
 IF operation_code IN ('reaction','comment','edit','moderation') THEN
   INSERT INTO interaction_event(target_id,actor_id,comment_id,kind,request_key)
   VALUES(NEW.target_id,NEW.actor_id,(NEW.result->>'commentId')::uuid,operation_code,NEW.request_key)
   ON CONFLICT(actor_id,request_key) DO NOTHING;
 END IF;
 RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS interaction_command_event ON interaction_request;
CREATE TRIGGER interaction_command_event AFTER INSERT ON interaction_request
FOR EACH ROW EXECUTE FUNCTION interaction_enqueue_event();

CREATE OR REPLACE FUNCTION interaction_notification_allowed(target uuid,recipient bigint,actor bigint,kind text)
RETURNS boolean LANGUAGE sql STABLE AS $$
 SELECT recipient<>actor AND interaction_actor_live(recipient) AND interaction_actor_live(actor)
   AND NOT interaction_blocked(recipient,actor) AND interaction_target_context(target,recipient) IS NOT NULL
   AND NOT EXISTS(SELECT 1 FROM interaction_subscription s WHERE s.target_id=target AND s.party_id=recipient AND s.mode='muted')
   AND coalesce((SELECT CASE kind WHEN 'reaction' THEN p.reactions WHEN 'comment' THEN p.comments
      WHEN 'reply' THEN p.replies WHEN 'mention' THEN p.mentions ELSE true END
     FROM interaction_notification_preference p WHERE p.party_id=recipient),true)
$$;
CREATE OR REPLACE FUNCTION interaction_dispatch_events(batch_size integer DEFAULT 20) RETURNS integer
LANGUAGE plpgsql AS $$
DECLARE e interaction_event%ROWTYPE; context jsonb; comment_row interaction_comment%ROWTYPE;
 recipient record; kind_value text; dedupe text; notif bigint; processed integer:=0; recipients_seen integer;
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
         WHERE m.comment_id=e.comment_id AND e.kind IN ('comment','edit')
       UNION ALL SELECT s.party_id,1 FROM interaction_subscription s
         WHERE s.target_id=e.target_id AND s.mode='all' AND e.kind='comment'
       UNION ALL SELECT comment_row.author_id,4 WHERE e.kind='moderation'
     ) SELECT party_id,max(priority) priority FROM candidates
       WHERE party_id>e.recipient_cursor AND party_id<>e.actor_id GROUP BY party_id ORDER BY party_id LIMIT 50
   LOOP
     recipients_seen:=recipients_seen+1;
     UPDATE interaction_event SET recipient_cursor=recipient.party_id WHERE id=e.id;
     kind_value:=CASE recipient.priority WHEN 4 THEN 'moderation' WHEN 3 THEN 'mention' WHEN 2 THEN 'reply'
       ELSE CASE WHEN e.kind='reaction' THEN 'reaction' ELSE 'comment' END END;
     PERFORM id FROM party WHERE id IN (recipient.party_id,e.actor_id,(context->>'ownerId')::bigint) ORDER BY id FOR SHARE;
     PERFORM id FROM interaction_target WHERE id=e.target_id FOR SHARE;
     IF NOT interaction_notification_allowed(e.target_id,recipient.party_id,e.actor_id,kind_value) THEN CONTINUE; END IF;
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
       AND interaction_notification_allowed(n.target_id,recipient,a.actor_id,n.event_kind)
       AND (n.comment_id IS NULL OR EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=n.comment_id
         AND (c.state IN ('visible','deleted') OR n.event_kind='moderation')))
       AND (n.event_kind<>'reaction' OR EXISTS(SELECT 1 FROM interaction_reaction r
         WHERE r.target_id=n.target_id AND r.actor_id=a.actor_id)))
$$;
COMMIT;
