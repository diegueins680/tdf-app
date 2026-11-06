-- LEGAL-CONSENT-001: a deleted or edited draft cannot erase a terms identity.
-- Existing append-only policy snapshots remain the historical authority. This
-- additive guard is retained across application rollback; do not remove it to
-- permit reuse of a previously disclosed terms version. Conflicting historical
-- text requires explicit reviewed reconciliation, never a rewritten snapshot.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE course_checkout_policy, event_ticket_checkout_policy,
  course_checkout_policy_history, event_ticket_checkout_policy_history
  IN SHARE ROW EXCLUSIVE MODE;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM course_checkout_policy_history
    WHERE snapshot->>'terms_version' IS NULL OR snapshot->>'terms_summary' IS NULL
      OR snapshot->>'cancellation_policy' IS NULL) THEN
    RAISE EXCEPTION 'course_checkout_policy contains incomplete historical terms snapshots';
  END IF;
  IF EXISTS (
    SELECT 1 FROM (
      SELECT course_id AS scope_id, terms_version, terms_summary, cancellation_policy AS policy_text
        FROM course_checkout_policy
      UNION ALL
      SELECT course_id, snapshot->>'terms_version', snapshot->>'terms_summary',
        snapshot->>'cancellation_policy' FROM course_checkout_policy_history
    ) versions GROUP BY scope_id, terms_version
      HAVING count(DISTINCT (terms_summary, policy_text)) > 1
  ) THEN
    RAISE EXCEPTION 'course_checkout_policy has conflicting historical terms identities; reviewed reconciliation required';
  END IF;
END $$;
CREATE INDEX IF NOT EXISTS course_checkout_policy_history_terms_idx
  ON course_checkout_policy_history (course_id, (snapshot->>'terms_version'));
CREATE OR REPLACE FUNCTION course_checkout_policy_terms_identity()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM pg_advisory_xact_lock(hashtext('course_checkout_policy.terms_version'),
    hashtext(NEW.course_id::text || ':' || NEW.terms_version));
  IF EXISTS (
    SELECT 1 FROM course_checkout_policy existing
    WHERE existing.course_id=NEW.course_id AND existing.terms_version=NEW.terms_version
      AND existing.id<>NEW.id
      AND (existing.terms_summary, existing.cancellation_policy)
        IS DISTINCT FROM (NEW.terms_summary, NEW.cancellation_policy)
  ) OR EXISTS (
    SELECT 1 FROM course_checkout_policy_history history
    WHERE history.course_id=NEW.course_id
      AND history.snapshot->>'terms_version'=NEW.terms_version
      AND (history.snapshot->>'terms_summary', history.snapshot->>'cancellation_policy')
        IS DISTINCT FROM (NEW.terms_summary, NEW.cancellation_policy)
  ) THEN
    RAISE EXCEPTION 'Course terms version % already names different historical terms; assign a new terms_version',
      NEW.terms_version USING ERRCODE = 'integrity_constraint_violation';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_policy_history
    WHERE snapshot->>'terms_version' IS NULL OR snapshot->>'terms_summary' IS NULL
      OR snapshot->>'refund_policy' IS NULL) THEN
    RAISE EXCEPTION 'event_ticket_checkout_policy contains incomplete historical terms snapshots';
  END IF;
  IF EXISTS (
    SELECT 1 FROM (
      SELECT event_id AS scope_id, terms_version, terms_summary, refund_policy AS policy_text
        FROM event_ticket_checkout_policy
      UNION ALL
      SELECT event_id, snapshot->>'terms_version', snapshot->>'terms_summary',
        snapshot->>'refund_policy' FROM event_ticket_checkout_policy_history
    ) versions GROUP BY scope_id, terms_version
      HAVING count(DISTINCT (terms_summary, policy_text)) > 1
  ) THEN
    RAISE EXCEPTION 'event_ticket_checkout_policy has conflicting historical terms identities; reviewed reconciliation required';
  END IF;
END $$;
CREATE INDEX IF NOT EXISTS event_ticket_checkout_policy_history_terms_idx
  ON event_ticket_checkout_policy_history (event_id, (snapshot->>'terms_version'));
CREATE OR REPLACE FUNCTION event_ticket_checkout_policy_terms_identity()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM pg_advisory_xact_lock(hashtext('event_ticket_checkout_policy.terms_version'),
    hashtext(NEW.event_id::text || ':' || NEW.terms_version));
  IF EXISTS (
    SELECT 1 FROM event_ticket_checkout_policy existing
    WHERE existing.event_id=NEW.event_id AND existing.terms_version=NEW.terms_version
      AND existing.id<>NEW.id
      AND (existing.terms_summary, existing.refund_policy)
        IS DISTINCT FROM (NEW.terms_summary, NEW.refund_policy)
  ) OR EXISTS (
    SELECT 1 FROM event_ticket_checkout_policy_history history
    WHERE history.event_id=NEW.event_id
      AND history.snapshot->>'terms_version'=NEW.terms_version
      AND (history.snapshot->>'terms_summary', history.snapshot->>'refund_policy')
        IS DISTINCT FROM (NEW.terms_summary, NEW.refund_policy)
  ) THEN
    RAISE EXCEPTION 'Ticket terms version % already names different historical terms; assign a new terms_version',
      NEW.terms_version USING ERRCODE = 'integrity_constraint_violation';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_course_checkout_policy_terms_identity ON course_checkout_policy;
CREATE TRIGGER trg_course_checkout_policy_terms_identity
  BEFORE INSERT OR UPDATE OF course_id, terms_version, terms_summary, cancellation_policy
  ON course_checkout_policy FOR EACH ROW EXECUTE FUNCTION course_checkout_policy_terms_identity();
DROP TRIGGER IF EXISTS trg_event_ticket_checkout_policy_terms_identity ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_checkout_policy_terms_identity
  BEFORE INSERT OR UPDATE OF event_id, terms_version, terms_summary, refund_policy
  ON event_ticket_checkout_policy FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_policy_terms_identity();
COMMIT;
