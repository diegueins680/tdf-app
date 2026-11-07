-- Fully migrated synthetic database only; all data changes roll back.
BEGIN;
SET LOCAL statement_timeout='15s';
DO $$
DECLARE sender bigint; recipient bigint; outsider bigint; thread bigint;
  message bigint; notice bigint; before_count bigint; result jsonb;
BEGIN
  INSERT INTO party(display_name,is_org,created_at) VALUES('Synthetic sender',false,now()) RETURNING id INTO sender;
  INSERT INTO party(display_name,is_org,created_at) VALUES('Synthetic recipient',false,now()) RETURNING id INTO recipient;
  INSERT INTO party(display_name,is_org,created_at) VALUES('Synthetic outsider',false,now()) RETURNING id INTO outsider;
  INSERT INTO user_credential(party_id,username,password_hash,active) VALUES
    (sender,'chat-notify-sender-'||sender,'not-a-login',true),
    (recipient,'chat-notify-recipient-'||recipient,'not-a-login',true);
  INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b)
    VALUES(least(sender,recipient),greatest(sender,recipient),true,true);
  INSERT INTO chat_thread(dm_party_a,dm_party_b,created_at,updated_at)
    VALUES(least(sender,recipient),greatest(sender,recipient),now(),now()) RETURNING id INTO thread;
  result := social_v2_chat_send(sender,thread,'Synthetic private text',false);
  ASSERT result ? 'result', 'accepted sender denied';
  message := (result->'result'->>'cmId')::bigint;
  SELECT notification_id INTO notice FROM chat_message_notification WHERE message_id=message;
  ASSERT notice IS NOT NULL, 'accepted message did not notify recipient';
  ASSERT (SELECT count(*)=1 FROM notification WHERE id=notice AND recipient_party_id=recipient
    AND notif_type='chat.message' AND target_type='chat_thread' AND target_key=thread::text
    AND NOT is_read AND body NOT LIKE '%Synthetic private text%'), 'recipient, target, or privacy mismatch';
  ASSERT interaction_notification_visible(notice,recipient), 'recipient cannot read notice';
  ASSERT NOT interaction_notification_visible(notice,sender), 'sender sees recipient notice';
  ASSERT NOT interaction_notification_visible(notice,outsider), 'outsider sees recipient notice';

  result := social_v2_chat_send(recipient,thread,'Synthetic reply',false);
  ASSERT result ? 'result', 'recipient could not reply in the same conversation';
  ASSERT EXISTS(SELECT 1 FROM chat_message_notification c JOIN notification n ON n.id=c.notification_id
    WHERE c.message_id=(result->'result'->>'cmId')::bigint AND n.recipient_party_id=sender),
    'reply did not notify original sender';

  -- Transaction rollback must discard both effects (models HTTP decoder rejection).
  SELECT count(*) INTO before_count FROM chat_message_notification;
  BEGIN
    PERFORM social_v2_chat_send(sender,thread,'Rolled back text',false);
    RAISE EXCEPTION USING ERRCODE='ZZ001',MESSAGE='synthetic rejected response';
  EXCEPTION WHEN SQLSTATE 'ZZ001' THEN NULL; END;
  ASSERT (SELECT count(*) FROM chat_message_notification)=before_count, 'rejected mutation left a notice';
  ASSERT (SELECT count(*) FROM chat_message WHERE thread_id=thread)=2, 'rejected mutation left a message';

  -- Failure to store the notice must also prevent an accepted message.
  ALTER TABLE notification ADD CONSTRAINT synthetic_chat_notice_failure
    CHECK (notif_type<>'chat.message') NOT VALID;
  BEGIN
    PERFORM social_v2_chat_send(sender,thread,'Must not commit',false);
    RAISE EXCEPTION 'notification failure was ignored';
  EXCEPTION WHEN check_violation THEN NULL; END;
  ALTER TABLE notification DROP CONSTRAINT synthetic_chat_notice_failure;
  ASSERT (SELECT count(*) FROM chat_message WHERE thread_id=thread)=2, 'notification failure left a message';
  ASSERT (SELECT count(*) FROM chat_message_notification)=before_count, 'notification failure left mapping';

  -- Blocked notices disappear from the shared detail/list/count predicate, even paused.
  UPDATE social_v2_pair SET block_b=true,consent_a=false,consent_b=false WHERE party_a=least(sender,recipient) AND party_b=greatest(sender,recipient);
  ASSERT NOT interaction_notification_visible(notice,recipient), 'block leaked notice';
  result := social_v2_chat_send(sender,thread,'Forbidden',true);
  ASSERT result->>'error'='forbidden', 'admin bypassed current consent';
  ASSERT (SELECT count(*) FROM chat_message_notification)=before_count, 'denied send generated notice';
  UPDATE social_v2_pair SET block_b=false,consent_a=true,consent_b=false WHERE party_a=least(sender,recipient) AND party_b=greatest(sender,recipient);
  ASSERT NOT interaction_notification_visible(notice,recipient), 'revoked consent leaked notice';
  UPDATE social_v2_pair SET consent_b=true WHERE party_a=least(sender,recipient) AND party_b=greatest(sender,recipient);
  UPDATE user_credential SET active=false WHERE party_id=recipient;
  ASSERT NOT interaction_notification_visible(notice,recipient), 'inactive recipient sees notice';
  UPDATE user_credential SET active=true WHERE party_id=recipient;
  ASSERT interaction_notification_visible(notice,recipient), 'valid recipient did not recover';
  UPDATE notification SET is_read=true WHERE id=notice;
  ASSERT (SELECT is_read FROM notification WHERE id=notice), 'read state not durable';
  DELETE FROM chat_message WHERE id=message;
  ASSERT NOT interaction_notification_visible(notice,recipient), 'deleted message leaked notice';
  ASSERT NOT EXISTS(SELECT 1 FROM chat_message_notification WHERE message_id=message), 'deleted message retained mapping';
  ASSERT (SELECT is_read FROM notification WHERE id=notice), 'deleted message changed inbox read history';
  RAISE NOTICE 'PASS: recipient-only atomic notice, minimized content, rollback, deny, block, consent, inactive account, read state, deletion';
END $$;
ROLLBACK;
