-- Restores the nine uniqueness constraints removed by
-- 2026-10-11_index_declarations_not_unique.sql. It fails, changing nothing, once
-- any table holds rows the constraints forbid (for example two messages in one
-- conversation); those rows are legitimate and must not be deleted to make it pass.
-- Safe to apply more than once.

BEGIN;

SET LOCAL lock_timeout = '10s';

ALTER TABLE chat_message DROP CONSTRAINT IF EXISTS index_chat_message_thread;
DROP INDEX IF EXISTS index_chat_message_thread;
ALTER TABLE chat_message ADD CONSTRAINT index_chat_message_thread UNIQUE (thread_id);

ALTER TABLE course_registration DROP CONSTRAINT IF EXISTS index_course_registration_party;
DROP INDEX IF EXISTS index_course_registration_party;
ALTER TABLE course_registration ADD CONSTRAINT index_course_registration_party UNIQUE (party_id, created_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_next;
DROP INDEX IF EXISTS index_course_registration_follow_up_next;
ALTER TABLE course_registration_follow_up ADD CONSTRAINT index_course_registration_follow_up_next UNIQUE (next_follow_up_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_party;
DROP INDEX IF EXISTS index_course_registration_follow_up_party;
ALTER TABLE course_registration_follow_up ADD CONSTRAINT index_course_registration_follow_up_party UNIQUE (party_id, created_at);

ALTER TABLE course_registration_follow_up DROP CONSTRAINT IF EXISTS index_course_registration_follow_up_registration;
DROP INDEX IF EXISTS index_course_registration_follow_up_registration;
ALTER TABLE course_registration_follow_up ADD CONSTRAINT index_course_registration_follow_up_registration UNIQUE (registration_id, created_at);

ALTER TABLE course_registration_receipt DROP CONSTRAINT IF EXISTS index_course_registration_receipt_party;
DROP INDEX IF EXISTS index_course_registration_receipt_party;
ALTER TABLE course_registration_receipt ADD CONSTRAINT index_course_registration_receipt_party UNIQUE (party_id, created_at);

ALTER TABLE course_registration_receipt DROP CONSTRAINT IF EXISTS index_course_registration_receipt_registration;
DROP INDEX IF EXISTS index_course_registration_receipt_registration;
ALTER TABLE course_registration_receipt ADD CONSTRAINT index_course_registration_receipt_registration UNIQUE (registration_id, created_at);

ALTER TABLE whats_app_message DROP CONSTRAINT IF EXISTS index_whats_app_message_party;
DROP INDEX IF EXISTS index_whats_app_message_party;
ALTER TABLE whats_app_message ADD CONSTRAINT index_whats_app_message_party UNIQUE (party_id, created_at);

ALTER TABLE whats_app_message DROP CONSTRAINT IF EXISTS index_whats_app_message_phone;
DROP INDEX IF EXISTS index_whats_app_message_phone;
ALTER TABLE whats_app_message ADD CONSTRAINT index_whats_app_message_phone UNIQUE (phone_e164, created_at);

COMMIT;
