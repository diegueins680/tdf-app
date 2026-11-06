-- A terms_version names exactly one terms text within its course or event.
--
-- Course and ticket checkout bind the buyer's acceptance to the active policy's
-- terms_version (LEGAL-CONSENT-001). Policies are immutable once approved and
-- only (scope, policy_version) is unique, so a replacement policy row could
-- reuse an earlier terms_version with different terms_summary / cancellation
-- or refund text; a buyer still holding the old text would pass the version
-- comparison and the order would record the new text as accepted (#564).
--
-- Rows may share a terms_version only when their terms text is identical
-- (for example a price-only policy change). Any other reuse is rejected at
-- write time; the transaction-scoped advisory lock serialises concurrent
-- writers of the same (scope, terms_version) so two differing rows cannot
-- both pass the check under READ COMMITTED.
--
-- The migration fails closed if existing rows already violate the invariant,
-- so an operator reconciles them (assign a new terms_version) before the
-- trigger is installed.

BEGIN;
SET LOCAL lock_timeout = '10s';
-- Keep the preflight and both trigger installations in one writer-fenced
-- transaction. Otherwise a conflicting writer can commit after the preflight
-- and before CREATE TRIGGER, leaving invalid history under an installed guard.
LOCK TABLE course_checkout_policy, event_ticket_checkout_policy
  IN SHARE ROW EXCLUSIVE MODE;

DO $$
DECLARE
  offending TEXT;
BEGIN
  SELECT string_agg(format('course %s terms_version %L', course_id, terms_version), ', ')
    INTO offending
  FROM (
    SELECT course_id, terms_version
    FROM course_checkout_policy
    GROUP BY course_id, terms_version
    HAVING count(DISTINCT (terms_summary, cancellation_policy)) > 1
  ) duplicates;
  IF offending IS NOT NULL THEN
    RAISE EXCEPTION 'course_checkout_policy reuses a terms_version for different terms text: %; assign a new terms_version before applying 2026-10-13_checkout_policy_terms_identity', offending;
  END IF;

  SELECT string_agg(format('event %s terms_version %L', event_id, terms_version), ', ')
    INTO offending
  FROM (
    SELECT event_id, terms_version
    FROM event_ticket_checkout_policy
    GROUP BY event_id, terms_version
    HAVING count(DISTINCT (terms_summary, refund_policy)) > 1
  ) duplicates;
  IF offending IS NOT NULL THEN
    RAISE EXCEPTION 'event_ticket_checkout_policy reuses a terms_version for different terms text: %; assign a new terms_version before applying 2026-10-13_checkout_policy_terms_identity', offending;
  END IF;
END $$;

CREATE OR REPLACE FUNCTION course_checkout_policy_terms_identity()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM pg_advisory_xact_lock(
    hashtext('course_checkout_policy.terms_version'),
    hashtext(NEW.course_id::text || ':' || NEW.terms_version));
  IF EXISTS (
    SELECT 1
    FROM course_checkout_policy existing
    WHERE existing.course_id = NEW.course_id
      AND existing.terms_version = NEW.terms_version
      AND existing.id <> NEW.id
      AND (existing.terms_summary, existing.cancellation_policy)
          IS DISTINCT FROM (NEW.terms_summary, NEW.cancellation_policy)
  ) THEN
    RAISE EXCEPTION 'Course terms version % already names different terms for course %; assign a new terms_version',
      NEW.terms_version, NEW.course_id
      USING ERRCODE = 'integrity_constraint_violation';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_course_checkout_policy_terms_identity ON course_checkout_policy;
CREATE TRIGGER trg_course_checkout_policy_terms_identity
  BEFORE INSERT OR UPDATE OF course_id, terms_version, terms_summary, cancellation_policy
  ON course_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION course_checkout_policy_terms_identity();

CREATE OR REPLACE FUNCTION event_ticket_checkout_policy_terms_identity()
RETURNS TRIGGER AS $$
BEGIN
  PERFORM pg_advisory_xact_lock(
    hashtext('event_ticket_checkout_policy.terms_version'),
    hashtext(NEW.event_id::text || ':' || NEW.terms_version));
  IF EXISTS (
    SELECT 1
    FROM event_ticket_checkout_policy existing
    WHERE existing.event_id = NEW.event_id
      AND existing.terms_version = NEW.terms_version
      AND existing.id <> NEW.id
      AND (existing.terms_summary, existing.refund_policy)
          IS DISTINCT FROM (NEW.terms_summary, NEW.refund_policy)
  ) THEN
    RAISE EXCEPTION 'Ticket terms version % already names different terms for event %; assign a new terms_version',
      NEW.terms_version, NEW.event_id
      USING ERRCODE = 'integrity_constraint_violation';
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_policy_terms_identity ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_checkout_policy_terms_identity
  BEFORE INSERT OR UPDATE OF event_id, terms_version, terms_summary, refund_policy
  ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_policy_terms_identity();

COMMIT;
