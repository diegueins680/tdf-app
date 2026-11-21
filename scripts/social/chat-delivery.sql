-- MSG-DELIVERY-001. Fully migrated synthetic database only; everything rolls back.
BEGIN;
SET LOCAL statement_timeout='15s';
DO $$
DECLARE sender bigint; recipient bigint; thread bigint; message bigint; notice bigint;
  result jsonb; push_row bigint; email_row bigint; lease uuid; leased record;
  second_message bigint;
BEGIN
  INSERT INTO party(display_name,is_org,primary_email,created_at)
    VALUES('Synthetic delivery sender',false,'sender@example.invalid',now()) RETURNING id INTO sender;
  INSERT INTO party(display_name,is_org,primary_email,created_at)
    VALUES('Synthetic delivery recipient',false,'recipient@example.invalid',now()) RETURNING id INTO recipient;
  INSERT INTO user_credential(party_id,username,password_hash,active) VALUES
    (sender,'chat-delivery-sender-'||sender,'not-a-login',true),
    (recipient,'chat-delivery-recipient-'||recipient,'not-a-login',true);
  INSERT INTO social_v2_pair(party_a,party_b,consent_a,consent_b)
    VALUES(least(sender,recipient),greatest(sender,recipient),true,true);
  INSERT INTO chat_thread(dm_party_a,dm_party_b,created_at,updated_at)
    VALUES(least(sender,recipient),greatest(sender,recipient),now(),now()) RETURNING id INTO thread;

  -- 1. The send writes exactly one intent per channel, atomically with the notice.
  result := social_v2_chat_send(sender,thread,'Synthetic private text',false);
  message := (result->'result'->>'cmId')::bigint;
  SELECT notification_id INTO notice FROM chat_message_notification WHERE message_id=message;
  ASSERT (SELECT count(*) FROM chat_delivery_outbox WHERE notification_id=notice)=2, 'expected push and email intents';
  SELECT id INTO push_row FROM chat_delivery_outbox WHERE notification_id=notice AND channel='push';
  SELECT id INTO email_row FROM chat_delivery_outbox WHERE notification_id=notice AND channel='email';
  ASSERT (SELECT recipient_party_id=recipient AND state='pending' AND due_at<=now() FROM chat_delivery_outbox WHERE id=push_row),
    'push intent must be due now for the recipient';
  ASSERT (SELECT due_at>=now()+interval '9 minutes' FROM chat_delivery_outbox WHERE id=email_row),
    'email must wait for the offline window';

  -- 2. Disabled channels are never leased; email is not due yet.
  ASSERT NOT EXISTS (SELECT 1 FROM chat_delivery_lease(ARRAY['email'],10,60)), 'email leased before its window';
  SELECT * INTO leased FROM chat_delivery_lease(ARRAY['push'],10,60) WHERE outbox_id=push_row;
  ASSERT leased.outbox_id IS NOT NULL AND leased.outbox_attempts=1, 'push not leased';
  lease := leased.outbox_lease;

  -- 3. No device registered: skip with a minimized reason.
  ASSERT chat_delivery_decision(push_row,lease)='no_subscription', 'missing subscription not detected';
  INSERT INTO notification_push_subscription(party_id,provider,platform,token_digest,encrypted_token)
    VALUES(recipient,'expo','ios',encode(digest('ExponentPushToken[synthetic]','sha256'),'hex'),
      pgp_sym_encrypt('ExponentPushToken[synthetic]','synthetic-key-synthetic-key-0000'));
  ASSERT chat_delivery_decision(push_row,lease)='send', 'eligible push not sendable';
  ASSERT chat_delivery_decision(push_row,gen_random_uuid())='lease_lost', 'foreign lease accepted';

  -- 4. Opt-out, read receipt and block are rechecked at dispatch.
  INSERT INTO notification_channel_preference(party_id,category,channel,enabled) VALUES(recipient,'chat','push',false);
  ASSERT chat_delivery_decision(push_row,lease)='opted_out', 'opt-out ignored';
  UPDATE notification_channel_preference SET enabled=true WHERE party_id=recipient;
  UPDATE social_v2_pair SET block_b=true,consent_a=false,consent_b=false
    WHERE party_a=least(sender,recipient) AND party_b=greatest(sender,recipient);
  ASSERT chat_delivery_decision(push_row,lease)='not_visible', 'block ignored at dispatch';
  UPDATE social_v2_pair SET block_b=false,consent_a=true,consent_b=true
    WHERE party_a=least(sender,recipient) AND party_b=greatest(sender,recipient);
  ASSERT chat_delivery_decision(push_row,lease)='send', 'unblock did not restore eligibility';
  UPDATE user_credential SET active=false WHERE party_id=recipient;
  ASSERT chat_delivery_decision(push_row,lease) IN ('inactive_recipient','not_visible'), 'closed account still notified';
  UPDATE user_credential SET active=true WHERE party_id=recipient;

  -- 5. Fenced completion: a foreign token cannot complete; the holder can.
  ASSERT NOT chat_delivery_complete(push_row,gen_random_uuid(),'accepted','provider_accepted',60), 'unfenced completion';
  ASSERT chat_delivery_complete(push_row,lease,'accepted','provider_accepted',60), 'lease holder could not complete';
  ASSERT (SELECT state='accepted' AND lease_token IS NULL FROM chat_delivery_outbox WHERE id=push_row), 'accepted state not stored';
  ASSERT NOT chat_delivery_complete(push_row,lease,'accepted','provider_accepted',60), 'double completion';

  -- 6. Retry backoff and dead letter after the attempt budget.
  UPDATE chat_delivery_outbox SET due_at=now()-interval '1 second' WHERE id=email_row;
  FOR i IN 1..5 LOOP
    SELECT outbox_lease INTO lease FROM chat_delivery_lease(ARRAY['email'],10,60) WHERE outbox_id=email_row;
    ASSERT lease IS NOT NULL, 'email not leased on attempt '||i;
    ASSERT chat_delivery_complete(email_row,lease,'retry','provider_error',1), 'retry not recorded';
    UPDATE chat_delivery_outbox SET due_at=now()-interval '1 second' WHERE id=email_row AND state='pending';
  END LOOP;
  ASSERT (SELECT state='dead' AND attempts=5 AND outcome='provider_error' FROM chat_delivery_outbox WHERE id=email_row),
    'retry budget did not dead-letter';

  -- 7. Reading the conversation clears the notice and stops later reminders.
  result := social_v2_chat_send(sender,thread,'Second synthetic text',false);
  second_message := (result->'result'->>'cmId')::bigint;
  ASSERT NOT chat_mark_thread_read(sender+recipient+1000000,thread,second_message), 'outsider wrote a receipt';
  ASSERT chat_mark_thread_read(recipient,thread,second_message), 'participant receipt rejected';
  ASSERT (SELECT bool_and(n.is_read) FROM chat_message_notification c JOIN notification n ON n.id=c.notification_id
    WHERE c.message_id IN (message,second_message)), 'read receipt left notices unread';
  ASSERT chat_mark_thread_read(recipient,thread,message), 'older receipt rejected';
  ASSERT (SELECT last_read_message_id=second_message FROM chat_thread_read_receipt
    WHERE party_id=recipient AND thread_id=thread), 'receipt moved backwards';
  UPDATE chat_delivery_outbox SET due_at=now()-interval '1 second'
    WHERE message_id=second_message AND channel='email';
  SELECT outbox_id, outbox_lease INTO push_row, lease FROM chat_delivery_lease(ARRAY['email'],10,60)
    WHERE outbox_id=(SELECT id FROM chat_delivery_outbox WHERE message_id=second_message AND channel='email');
  ASSERT chat_delivery_decision(push_row,lease)='read', 'read conversation still reminded';

  -- 8. Email coalescing: one reminder per conversation every six hours.
  UPDATE notification SET is_read=false WHERE id=(SELECT notification_id FROM chat_message_notification WHERE message_id=second_message);
  DELETE FROM chat_thread_read_receipt WHERE party_id=recipient;
  ASSERT chat_delivery_decision(push_row,lease)='send', 'unread second message not sendable';
  UPDATE chat_delivery_outbox SET state='accepted',lease_token=NULL,lease_expires_at=NULL,updated_at=now()
    WHERE id=email_row;
  ASSERT chat_delivery_decision(push_row,lease)='coalesced', 'second email inside six hours not coalesced';

  -- 9. Stale intents are never sent retrospectively.
  UPDATE chat_delivery_outbox SET state='dead',lease_token=NULL,lease_expires_at=NULL WHERE id=email_row;
  UPDATE notification SET created_at=now()-interval '25 hours'
    WHERE id=(SELECT notification_id FROM chat_message_notification WHERE message_id=second_message);
  ASSERT chat_delivery_decision(push_row,lease)='expired', 'day-old notice was still sendable';
  RAISE NOTICE 'PASS: transactional intents, channel gating, dispatch rechecks, fencing, retry/dead letter, read receipts, coalescing, freshness';
END $$;
ROLLBACK;
