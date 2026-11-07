-- Nine constraints were created as UNIQUE from model lines that were written as
-- indexes: a conversation could hold one message, a WhatsApp reply could not be
-- stored at the instant of the message it answers, and two course follow-ups
-- could not share a reminder time. Each becomes a plain index with the same
-- name and columns. No row is changed. Safe to apply more than once.

BEGIN;

SET LOCAL lock_timeout = '10s';

ALTER TABLE chat_message DROP CONSTRAINT IF EXISTS index_chat_message_thread;
CREATE INDEX IF NOT EXISTS index_chat_message_thread ON chat_message (thread_id);

ALTER TABLE course_registration DROP CONSTRAINT IF EXISTS index_course_registration_party;
CREATE INDEX IF NOT EXISTS index_course_registration_party ON course_registration (party_id, created_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_next;
CREATE INDEX IF NOT EXISTS index_course_registration_follow_up_next ON course_registration_follow_up (next_follow_up_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_party;
CREATE INDEX IF NOT EXISTS index_course_registration_follow_up_party ON course_registration_follow_up (party_id, created_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_registration;
CREATE INDEX IF NOT EXISTS index_course_registration_follow_up_registration ON course_registration_follow_up (registration_id, created_at);

ALTER TABLE course_registration_receipt DROP CONSTRAINT IF EXISTS index_course_registration_receipt_party;
CREATE INDEX IF NOT EXISTS index_course_registration_receipt_party ON course_registration_receipt (party_id, created_at);

ALTER TABLE course_registration_receipt DROP CONSTRAINT IF EXISTS index_course_registration_receipt_registration;
CREATE INDEX IF NOT EXISTS index_course_registration_receipt_registration ON course_registration_receipt (registration_id, created_at);

ALTER TABLE whats_app_message DROP CONSTRAINT IF EXISTS index_whats_app_message_party;
CREATE INDEX IF NOT EXISTS index_whats_app_message_party ON whats_app_message (party_id, created_at);

ALTER TABLE whats_app_message DROP CONSTRAINT IF EXISTS index_whats_app_message_phone;
CREATE INDEX IF NOT EXISTS index_whats_app_message_phone ON whats_app_message (phone_e164, created_at);

-- A unique index of one of these names that is not a constraint would survive
-- the statements above; stop rather than leave it in place.
DO $$
DECLARE remaining text;
BEGIN
  SELECT string_agg(c.relname, ', ') INTO remaining
  FROM pg_index i JOIN pg_class c ON c.oid = i.indexrelid
  WHERE c.relnamespace = 'public'::regnamespace AND i.indisunique AND c.relname IN (
    'index_chat_message_thread',
    'index_course_registration_party',
    'index_course_registration_follow_up_next',
    'index_course_registration_follow_up_party',
    'index_course_registration_follow_up_registration',
    'index_course_registration_receipt_party',
    'index_course_registration_receipt_registration',
    'index_whats_app_message_party',
    'index_whats_app_message_phone');
  IF remaining IS NOT NULL THEN
    RAISE EXCEPTION 'still unique after the index declaration migration: %', remaining;
  END IF;
END $$;

COMMIT;
