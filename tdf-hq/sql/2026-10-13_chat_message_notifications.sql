-- MSG-NOTIFY-001: a committed message and its recipient inbox notice are atomic.
-- No historical replay or external delivery is performed by this migration.
BEGIN;
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '30s';

-- Persistent's capitalized IndexChatMessageThread declaration was a UNIQUE
-- constraint, not an index. It accidentally allowed only one message per thread.
DO $$ DECLARE columns smallint[]; thread_column smallint; BEGIN
  SELECT attnum INTO thread_column FROM pg_attribute
    WHERE attrelid='chat_message'::regclass AND attname='thread_id' AND NOT attisdropped;
  SELECT conkey INTO columns FROM pg_constraint
    WHERE conrelid='chat_message'::regclass AND conname='index_chat_message_thread' AND contype='u';
  IF columns IS NOT NULL THEN
    IF columns<>ARRAY[thread_column] THEN
      RAISE EXCEPTION 'Unexpected index_chat_message_thread constraint';
    END IF;
    ALTER TABLE chat_message DROP CONSTRAINT index_chat_message_thread;
  END IF;
END $$;
-- Preserve efficient history reads on every supported baseline.
CREATE INDEX IF NOT EXISTS social_v2_chat_last_message ON chat_message(thread_id,id DESC);

CREATE TABLE IF NOT EXISTS chat_notification_migration_state (
  id integer PRIMARY KEY CHECK (id=1), original_expression text NOT NULL
);
DO $$ DECLARE previous text; BEGIN
  SELECT pg_get_expr(conbin,conrelid) INTO previous FROM pg_constraint
    WHERE conrelid='notification'::regclass
      AND conname='notification_notif_type_check' AND contype='c';
  IF previous IS NOT NULL AND NOT EXISTS (SELECT 1 FROM chat_notification_migration_state) THEN
    INSERT INTO chat_notification_migration_state VALUES (1,previous);
    ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
    EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type = ''chat.message'')',previous);
  END IF;
END $$;

CREATE TABLE IF NOT EXISTS chat_message_notification (
  message_id bigint PRIMARY KEY REFERENCES chat_message(id) ON DELETE CASCADE,
  notification_id bigint NOT NULL UNIQUE REFERENCES notification(id) ON DELETE CASCADE
);

CREATE OR REPLACE FUNCTION chat_message_notify_recipient() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE recipient bigint; notice bigint;
BEGIN
  SELECT CASE WHEN NEW.sender_party_id=t.dm_party_a THEN t.dm_party_b ELSE t.dm_party_a END
    INTO recipient FROM chat_thread t
    WHERE t.id=NEW.thread_id AND NEW.sender_party_id IN (t.dm_party_a,t.dm_party_b)
      AND t.dm_party_a<>t.dm_party_b;
  IF recipient IS NULL THEN RAISE EXCEPTION 'chat_notification_invalid_participant'; END IF;
  -- Minimized content: no private message text, sender name or email in the inbox.
  INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_key,is_read,created_at)
    VALUES(recipient,'chat.message','Nuevo mensaje','Tienes un mensaje nuevo en TDF.',
      'chat_thread',NEW.thread_id::text,false,NEW.created_at) RETURNING id INTO notice;
  INSERT INTO chat_message_notification(message_id,notification_id) VALUES(NEW.id,notice);
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS chat_message_recipient_notification ON chat_message;
CREATE TRIGGER chat_message_recipient_notification AFTER INSERT ON chat_message
  FOR EACH ROW EXECUTE FUNCTION chat_message_notify_recipient();

-- This is the existing inbox visibility entrypoint used by list, detail and count.
-- Preserve the interaction predicate while applying the chat read authority to DM
-- notices. Missing/deleted message relationships fail closed, even if the inbox row
-- survives deletion. Legacy, never-governed pairs keep the existing chat read rule.
CREATE OR REPLACE FUNCTION interaction_notification_visible(notification_key bigint,recipient bigint)
RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT CASE WHEN EXISTS (SELECT 1 FROM notification WHERE id=notification_key
    AND (notif_type='chat.message' OR target_type='chat_thread')) THEN
    EXISTS (SELECT 1 FROM chat_message_notification c
      JOIN notification n ON n.id=c.notification_id
      JOIN chat_message m ON m.id=c.message_id
      JOIN chat_thread t ON t.id=m.thread_id
      WHERE n.id=notification_key AND n.recipient_party_id=recipient
        AND recipient IN (t.dm_party_a,t.dm_party_b) AND m.sender_party_id<>recipient
        AND (NOT social_v2_dm_required(t.dm_party_a,t.dm_party_b)
          OR social_v2_dm_allowed(t.dm_party_a,t.dm_party_b)))
  ELSE
    NOT EXISTS(SELECT 1 FROM interaction_notification WHERE notification_id=notification_key)
    OR EXISTS(SELECT 1 FROM interaction_notification n JOIN interaction_notification_actor a USING(notification_id)
      WHERE n.notification_id=notification_key AND n.recipient_id=recipient
        AND interaction_notification_allowed(n.target_id,recipient,a.actor_id,n.event_kind)
        AND (n.comment_id IS NULL OR EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=n.comment_id
          AND (c.state IN ('visible','deleted') OR n.event_kind='moderation')))
        AND (n.event_kind<>'reaction' OR EXISTS(SELECT 1 FROM interaction_reaction r
          WHERE r.target_id=n.target_id AND r.actor_id=a.actor_id)))
  END
$$;
COMMIT;
