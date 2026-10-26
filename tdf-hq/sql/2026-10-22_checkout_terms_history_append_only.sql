-- LEGAL-CONSENT-001: preserve the archive used to bind terms-version identity.
-- Retain across application rollback. No historical row is repaired or erased.
-- Ordinary DML and TRUNCATE are guarded; owner/DDL bypass is outside this boundary.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE course_checkout_policy_history, event_ticket_checkout_policy_history
  IN SHARE ROW EXCLUSIVE MODE;

CREATE OR REPLACE FUNCTION checkout_terms_history_append_only()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'Checkout policy history is append-only'
    USING ERRCODE = 'integrity_constraint_violation';
END;
$$;

DROP TRIGGER IF EXISTS trg_course_terms_history_append_only ON course_checkout_policy_history;
CREATE TRIGGER trg_course_terms_history_append_only
  BEFORE UPDATE OR DELETE ON course_checkout_policy_history
  FOR EACH ROW EXECUTE FUNCTION checkout_terms_history_append_only();
DROP TRIGGER IF EXISTS trg_course_terms_history_no_truncate ON course_checkout_policy_history;
CREATE TRIGGER trg_course_terms_history_no_truncate
  BEFORE TRUNCATE ON course_checkout_policy_history
  FOR EACH STATEMENT EXECUTE FUNCTION checkout_terms_history_append_only();

DROP TRIGGER IF EXISTS trg_event_terms_history_append_only ON event_ticket_checkout_policy_history;
CREATE TRIGGER trg_event_terms_history_append_only
  BEFORE UPDATE OR DELETE ON event_ticket_checkout_policy_history
  FOR EACH ROW EXECUTE FUNCTION checkout_terms_history_append_only();
DROP TRIGGER IF EXISTS trg_event_terms_history_no_truncate ON event_ticket_checkout_policy_history;
CREATE TRIGGER trg_event_terms_history_no_truncate
  BEFORE TRUNCATE ON event_ticket_checkout_policy_history
  FOR EACH STATEMENT EXECUTE FUNCTION checkout_terms_history_append_only();
COMMIT;
