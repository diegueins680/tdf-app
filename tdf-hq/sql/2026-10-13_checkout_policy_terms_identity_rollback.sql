-- Rollback for 2026-10-13_checkout_policy_terms_identity: removes the
-- terms-version identity triggers; existing rows are untouched.
DROP TRIGGER IF EXISTS trg_course_checkout_policy_terms_identity ON course_checkout_policy;
DROP FUNCTION IF EXISTS course_checkout_policy_terms_identity();
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_policy_terms_identity ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_checkout_policy_terms_identity();
