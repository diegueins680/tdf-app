-- Additive read/compatibility API; retain during pause. Requires DM write boundary.
BEGIN;
CREATE INDEX IF NOT EXISTS social_v2_chat_last_message ON chat_message(thread_id,id DESC);
CREATE INDEX IF NOT EXISTS social_v2_chat_actor_a ON chat_thread(dm_party_a,updated_at DESC,id DESC);
CREATE INDEX IF NOT EXISTS social_v2_chat_actor_b ON chat_thread(dm_party_b,updated_at DESC,id DESC);

CREATE OR REPLACE FUNCTION social_v2_chat_threads(actor bigint) RETURNS jsonb
LANGUAGE sql STABLE AS $$
  SELECT jsonb_build_object('result',coalesce(jsonb_agg(dto ORDER BY updated_at DESC,id DESC),'[]'::jsonb))
  FROM (
    SELECT t.id,t.updated_at,jsonb_build_object('ctThreadId',t.id,
      'ctOtherPartyId',p.id,'ctOtherDisplayName',p.display_name,
      'ctLastMessage',m.body,'ctLastMessageAt',m.created_at,'ctUpdatedAt',t.updated_at) dto
    FROM chat_thread t JOIN party p ON p.id=CASE WHEN t.dm_party_a=actor THEN t.dm_party_b ELSE t.dm_party_a END
    LEFT JOIN LATERAL (SELECT body,created_at FROM chat_message WHERE thread_id=t.id ORDER BY id DESC LIMIT 1) m ON true
    WHERE actor IN (t.dm_party_a,t.dm_party_b)
      AND (NOT social_v2_dm_required(t.dm_party_a,t.dm_party_b) OR social_v2_dm_allowed(t.dm_party_a,t.dm_party_b))
  ) permitted
$$;

CREATE OR REPLACE FUNCTION social_v2_chat_messages(actor bigint,thread bigint,
  before_id bigint,after_id bigint,page_size integer) RETURNS jsonb
LANGUAGE plpgsql STABLE AS $$
DECLARE a bigint; b bigint; result jsonb;
BEGIN
  IF thread IS NULL OR thread<=0 OR page_size IS NULL OR page_size<1 OR page_size>200
    OR before_id<=0 OR after_id<=0 OR (before_id IS NOT NULL AND after_id IS NOT NULL) THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  SELECT dm_party_a,dm_party_b INTO a,b FROM chat_thread WHERE id=thread AND actor IN (dm_party_a,dm_party_b);
  -- Policy precedes all cursor lookups, counts, names and message fields.
  IF a IS NULL OR (social_v2_dm_required(a,b) AND NOT social_v2_dm_allowed(a,b)) THEN
    RETURN '{"error":"unavailable"}'::jsonb;
  END IF;
  IF before_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM chat_message WHERE id=before_id AND thread_id=thread) THEN
    RETURN '{"error":"before_not_found"}'::jsonb;
  END IF;
  IF after_id IS NOT NULL AND NOT EXISTS(SELECT 1 FROM chat_message WHERE id=after_id AND thread_id=thread) THEN
    RETURN '{"error":"after_not_found"}'::jsonb;
  END IF;
  SELECT coalesce(jsonb_agg(jsonb_build_object('cmId',id,'cmThreadId',thread_id,
      'cmSenderPartyId',sender_party_id,'cmBody',body,'cmCreatedAt',created_at) ORDER BY id),'[]'::jsonb)
    INTO result FROM (
      SELECT id,thread_id,sender_party_id,body,created_at FROM chat_message WHERE thread_id=thread
        AND (before_id IS NULL OR id<before_id) AND (after_id IS NULL OR id>after_id)
      ORDER BY CASE WHEN after_id IS NULL THEN id END DESC,
               CASE WHEN after_id IS NOT NULL THEN id END ASC LIMIT page_size
    ) page;
  RETURN jsonb_build_object('result',result);
END $$;

-- Preserve the never-activated legacy writer contract, without manufacturing consent.
-- The boolean is trusted server context; it is not accepted from request JSON.
CREATE OR REPLACE FUNCTION social_v2_chat_can_send(actor bigint,other bigint,legacy_admin boolean)
RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT CASE WHEN social_v2_dm_required(actor,other) THEN social_v2_dm_allowed(actor,other)
    ELSE coalesce(legacy_admin,false) OR (
      EXISTS(SELECT 1 FROM party_follow WHERE follower_party_id=actor AND following_party_id=other)
      AND EXISTS(SELECT 1 FROM party_follow WHERE follower_party_id=other AND following_party_id=actor)) END
$$;
CREATE OR REPLACE FUNCTION social_v2_chat_open(actor bigint,other bigint,legacy_admin boolean)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE a bigint := least(actor,other); b bigint := greatest(actor,other);
  thread chat_thread%ROWTYPE; last_message chat_message%ROWTYPE; name text;
BEGIN
  IF actor IS NULL OR other IS NULL OR actor<=0 OR other<=0 OR actor=other THEN
    RETURN '{"error":"invalid"}'::jsonb;
  END IF;
  PERFORM id FROM party WHERE id IN (a,b) ORDER BY id FOR UPDATE;
  PERFORM id FROM user_credential WHERE party_id IN (a,b) ORDER BY id FOR UPDATE;
  SELECT display_name INTO name FROM party WHERE id=other;
  IF name IS NULL THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  IF NOT social_v2_chat_can_send(actor,other,legacy_admin) THEN RETURN '{"error":"forbidden"}'::jsonb; END IF;
  INSERT INTO chat_thread(dm_party_a,dm_party_b,created_at,updated_at)
    VALUES(a,b,clock_timestamp(),clock_timestamp()) ON CONFLICT(dm_party_a,dm_party_b) DO NOTHING;
  SELECT * INTO thread FROM chat_thread WHERE dm_party_a=a AND dm_party_b=b;
  SELECT * INTO last_message FROM chat_message WHERE thread_id=thread.id ORDER BY id DESC LIMIT 1;
  RETURN jsonb_build_object('result',jsonb_build_object('ctThreadId',thread.id,'ctOtherPartyId',other,
    'ctOtherDisplayName',name,'ctLastMessage',last_message.body,'ctLastMessageAt',last_message.created_at,
    'ctUpdatedAt',thread.updated_at));
END $$;
CREATE OR REPLACE FUNCTION social_v2_chat_send(actor bigint,thread bigint,message text,legacy_admin boolean)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE a bigint; b bigint; other bigint; written chat_message%ROWTYPE;
BEGIN
  IF message IS NULL OR length(message)=0 OR length(message)>5000 THEN RETURN '{"error":"invalid"}'::jsonb; END IF;
  SELECT dm_party_a,dm_party_b INTO a,b FROM chat_thread WHERE id=thread AND actor IN (dm_party_a,dm_party_b);
  IF a IS NULL THEN RETURN '{"error":"unavailable"}'::jsonb; END IF;
  PERFORM id FROM party WHERE id IN (a,b) ORDER BY id FOR UPDATE;
  PERFORM id FROM user_credential WHERE party_id IN (a,b) ORDER BY id FOR UPDATE;
  other := CASE WHEN actor=a THEN b ELSE a END;
  IF NOT social_v2_chat_can_send(actor,other,legacy_admin) THEN RETURN '{"error":"forbidden"}'::jsonb; END IF;
  INSERT INTO chat_message(thread_id,sender_party_id,body,created_at)
    VALUES(thread,actor,message,clock_timestamp()) RETURNING * INTO written;
  UPDATE chat_thread SET updated_at=written.created_at WHERE id=thread;
  RETURN jsonb_build_object('result',jsonb_build_object('cmId',written.id,'cmThreadId',thread,
    'cmSenderPartyId',actor,'cmBody',written.body,'cmCreatedAt',written.created_at));
END $$;
REVOKE ALL ON FUNCTION social_v2_chat_open(bigint,bigint,boolean) FROM PUBLIC;
REVOKE ALL ON FUNCTION social_v2_chat_send(bigint,bigint,text,boolean) FROM PUBLIC;
COMMIT;
