-- MSG-DELIVERY-001: recipient-controlled email and device push for chat notices.
-- Additive and repeatable. Delivery intent is written in the same transaction as
-- the message and its in-app notice. No historical message is enqueued, and no
-- provider is contacted by this migration; the API worker only sends when its
-- channel is explicitly enabled through the environment.
BEGIN;
SET LOCAL lock_timeout = '5s';
SET LOCAL statement_timeout = '60s';
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Recipient-authenticated read state (the web chat previously kept it only in
-- the browser). A receipt never moves backwards.
CREATE TABLE IF NOT EXISTS chat_thread_read_receipt (
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  thread_id bigint NOT NULL REFERENCES chat_thread(id) ON DELETE CASCADE,
  last_read_message_id bigint NOT NULL CHECK (last_read_message_id > 0),
  read_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (party_id, thread_id)
);

-- Persisted opt-out. A missing row means the channel is enabled.
CREATE TABLE IF NOT EXISTS notification_channel_preference (
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  category text NOT NULL CHECK (category IN ('chat')),
  channel text NOT NULL CHECK (channel IN ('email', 'push')),
  enabled boolean NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (party_id, category, channel)
);

-- Device subscriptions. One device token belongs to the party that registered it
-- last, so a shared phone never keeps receiving a previous account's notices.
CREATE TABLE IF NOT EXISTS notification_push_subscription (
  id bigserial PRIMARY KEY,
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  provider text NOT NULL CHECK (provider IN ('expo')),
  platform text NOT NULL CHECK (platform IN ('ios', 'android')),
  token_digest text NOT NULL CHECK (token_digest ~ '^[0-9a-f]{64}$'),
  encrypted_token bytea NOT NULL,
  active boolean NOT NULL DEFAULT true,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  last_success_at timestamptz,
  last_failure_code text CHECK (last_failure_code IS NULL OR length(last_failure_code) <= 64),
  UNIQUE (provider, token_digest)
);
CREATE INDEX IF NOT EXISTS notification_push_subscription_party_active
  ON notification_push_subscription(party_id) WHERE active;

-- One delivery intent per notice and channel. Outcomes are minimized codes; no
-- message text, address or token is stored here.
CREATE TABLE IF NOT EXISTS chat_delivery_outbox (
  id bigserial PRIMARY KEY,
  notification_id bigint NOT NULL REFERENCES notification(id) ON DELETE CASCADE,
  message_id bigint NOT NULL REFERENCES chat_message(id) ON DELETE CASCADE,
  recipient_party_id bigint NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  thread_id bigint NOT NULL REFERENCES chat_thread(id) ON DELETE CASCADE,
  channel text NOT NULL CHECK (channel IN ('email', 'push')),
  state text NOT NULL DEFAULT 'pending'
    CHECK (state IN ('pending', 'leased', 'accepted', 'skipped', 'dead')),
  due_at timestamptz NOT NULL,
  attempts integer NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  lease_token uuid,
  lease_expires_at timestamptz,
  outcome text CHECK (outcome IS NULL OR outcome ~ '^[a-z_]{1,40}$'),
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (notification_id, channel),
  CHECK ((state = 'leased') = (lease_token IS NOT NULL AND lease_expires_at IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS chat_delivery_outbox_due
  ON chat_delivery_outbox(due_at, id) WHERE state IN ('pending', 'leased');
CREATE INDEX IF NOT EXISTS chat_delivery_outbox_recent_accepted
  ON chat_delivery_outbox(recipient_party_id, thread_id, channel, updated_at DESC)
  WHERE state = 'accepted';

-- Same producer as 2026-10-13_chat_message_notifications plus the delivery intent.
-- Push is due immediately; email waits so that a recipient who is online reads the
-- message first and receives no email (the offline reminder window).
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
  INSERT INTO chat_delivery_outbox(notification_id,message_id,recipient_party_id,thread_id,channel,due_at)
    VALUES (notice,NEW.id,recipient,NEW.thread_id,'push',now()),
           (notice,NEW.id,recipient,NEW.thread_id,'email',now() + interval '10 minutes');
  RETURN NEW;
END $$;

-- Lease due work for the enabled channels. An expired lease is retried until the
-- attempt budget is spent, then dead-lettered. SKIP LOCKED lets several API
-- replicas share the queue; every completion is fenced by the lease token.
CREATE OR REPLACE FUNCTION chat_delivery_lease(channels text[], batch integer, lease_seconds integer)
RETURNS TABLE(outbox_id bigint, outbox_channel text, outbox_lease uuid, outbox_attempts integer)
LANGUAGE plpgsql AS $$
BEGIN
  IF batch < 1 OR batch > 100 OR lease_seconds < 10 OR lease_seconds > 900 THEN
    RAISE EXCEPTION 'chat_delivery_lease_invalid_bounds';
  END IF;
  UPDATE chat_delivery_outbox SET state='dead', outcome='lease_expired', lease_token=NULL,
      lease_expires_at=NULL, updated_at=now()
    WHERE state='leased' AND lease_expires_at < now() AND attempts >= 5 AND channel = ANY(channels);
  RETURN QUERY
  UPDATE chat_delivery_outbox o SET state='leased', lease_token=gen_random_uuid(),
      lease_expires_at=now() + make_interval(secs => lease_seconds),
      attempts=o.attempts + 1, updated_at=now()
    WHERE o.id IN (
      SELECT c.id FROM chat_delivery_outbox c
      WHERE c.channel = ANY(channels)
        AND ((c.state='pending' AND c.due_at <= now())
          OR (c.state='leased' AND c.lease_expires_at < now()))
      ORDER BY c.due_at, c.id
      LIMIT batch
      FOR UPDATE SKIP LOCKED)
    RETURNING o.id, o.channel, o.lease_token, o.attempts;
END $$;

-- Rechecks every condition at dispatch time (read state, current chat consent and
-- block policy through the inbox visibility predicate, account closure, opt-out,
-- freshness and coalescing). Returns 'send' or a minimized skip reason.
CREATE OR REPLACE FUNCTION chat_delivery_decision(target bigint, lease uuid)
RETURNS text LANGUAGE plpgsql STABLE AS $$
DECLARE item chat_delivery_outbox%ROWTYPE; notice notification%ROWTYPE;
BEGIN
  SELECT * INTO item FROM chat_delivery_outbox WHERE id=target AND lease_token=lease AND state='leased';
  IF NOT FOUND THEN RETURN 'lease_lost'; END IF;
  SELECT * INTO notice FROM notification WHERE id=item.notification_id;
  IF NOT FOUND THEN RETURN 'not_visible'; END IF;
  IF notice.is_read THEN RETURN 'read'; END IF;
  IF EXISTS (SELECT 1 FROM chat_thread_read_receipt r WHERE r.party_id=item.recipient_party_id
      AND r.thread_id=item.thread_id AND r.last_read_message_id >= item.message_id) THEN
    RETURN 'read';
  END IF;
  IF NOT interaction_notification_visible(item.notification_id, item.recipient_party_id) THEN
    RETURN 'not_visible';
  END IF;
  IF NOT EXISTS (SELECT 1 FROM user_credential u WHERE u.party_id=item.recipient_party_id AND u.active) THEN
    RETURN 'inactive_recipient';
  END IF;
  IF EXISTS (SELECT 1 FROM notification_channel_preference p WHERE p.party_id=item.recipient_party_id
      AND p.category='chat' AND p.channel=item.channel AND NOT p.enabled) THEN
    RETURN 'opted_out';
  END IF;
  -- Never send retrospective reminders (for example after enabling a channel).
  IF notice.created_at < now() - interval '24 hours' THEN RETURN 'expired'; END IF;
  IF item.channel = 'email' THEN
    IF NOT EXISTS (SELECT 1 FROM party p WHERE p.id=item.recipient_party_id
        AND p.primary_email IS NOT NULL AND btrim(p.primary_email) <> '') THEN
      RETURN 'no_address';
    END IF;
    -- At most one reminder per conversation every six hours.
    IF EXISTS (SELECT 1 FROM chat_delivery_outbox a WHERE a.recipient_party_id=item.recipient_party_id
        AND a.thread_id=item.thread_id AND a.channel='email' AND a.state='accepted'
        AND a.updated_at > now() - interval '6 hours') THEN
      RETURN 'coalesced';
    END IF;
  ELSE
    IF NOT EXISTS (SELECT 1 FROM notification_push_subscription s
        WHERE s.party_id=item.recipient_party_id AND s.active) THEN
      RETURN 'no_subscription';
    END IF;
    -- A burst of messages in one conversation produces one push per minute.
    IF EXISTS (SELECT 1 FROM chat_delivery_outbox a WHERE a.recipient_party_id=item.recipient_party_id
        AND a.thread_id=item.thread_id AND a.channel='push' AND a.state='accepted'
        AND a.updated_at > now() - interval '1 minute') THEN
      RETURN 'coalesced';
    END IF;
  END IF;
  RETURN 'send';
END $$;

-- Fenced completion. 'retry' reschedules with the caller's backoff until the
-- attempt budget is spent, then dead-letters the item.
CREATE OR REPLACE FUNCTION chat_delivery_complete(target bigint, lease uuid, result text,
  reason text, retry_after_seconds integer)
RETURNS boolean LANGUAGE plpgsql AS $$
DECLARE changed integer;
BEGIN
  IF result NOT IN ('accepted', 'skipped', 'retry', 'dead') THEN
    RAISE EXCEPTION 'chat_delivery_complete_invalid_result';
  END IF;
  UPDATE chat_delivery_outbox SET
      state = CASE
        WHEN result = 'retry' AND attempts >= 5 THEN 'dead'
        WHEN result = 'retry' THEN 'pending'
        ELSE result END,
      due_at = CASE WHEN result = 'retry'
        THEN now() + make_interval(secs => greatest(1, least(retry_after_seconds, 3600)))
        ELSE due_at END,
      outcome = reason, lease_token = NULL, lease_expires_at = NULL, updated_at = now()
    WHERE id = target AND lease_token = lease AND state = 'leased';
  GET DIAGNOSTICS changed = ROW_COUNT;
  RETURN changed = 1;
END $$;

-- Marks the caller's notices for a conversation read up to a message and records
-- the receipt. Only a participant may write a receipt.
CREATE OR REPLACE FUNCTION chat_mark_thread_read(reader bigint, target_thread bigint, up_to bigint)
RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
  IF up_to IS NULL OR up_to < 1 THEN RAISE EXCEPTION 'chat_read_invalid_message'; END IF;
  IF NOT EXISTS (SELECT 1 FROM chat_thread t WHERE t.id=target_thread
      AND reader IN (t.dm_party_a, t.dm_party_b)) THEN
    RETURN false;
  END IF;
  -- Clamp to messages that exist in this conversation.
  SELECT max(m.id) INTO up_to FROM chat_message m WHERE m.thread_id=target_thread AND m.id <= up_to;
  IF up_to IS NULL THEN RETURN true; END IF;
  INSERT INTO chat_thread_read_receipt(party_id, thread_id, last_read_message_id, read_at)
    VALUES (reader, target_thread, up_to, now())
    ON CONFLICT (party_id, thread_id) DO UPDATE
      SET last_read_message_id = greatest(chat_thread_read_receipt.last_read_message_id, EXCLUDED.last_read_message_id),
          read_at = now();
  UPDATE notification n SET is_read = true
    FROM chat_message_notification c JOIN chat_message m ON m.id = c.message_id
    WHERE n.id = c.notification_id AND n.recipient_party_id = reader AND NOT n.is_read
      AND m.thread_id = target_thread AND m.id <= up_to;
  RETURN true;
END $$;
COMMIT;
