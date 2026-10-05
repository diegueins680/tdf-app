-- Preserve the immutable commercial-policy lifecycle and annotate legacy audit
-- entries without rewriting the original evidence or its author/timestamp.
BEGIN;
CREATE OR REPLACE FUNCTION event_ticket_policy_lifecycle_guard()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.approval_status = 'approved' AND NEW.approval_status = 'draft'
     OR OLD.approval_status = 'retired' AND NEW.approval_status <> 'retired' THEN
    RAISE EXCEPTION 'Published ticket policies cannot return to an earlier approval state';
  END IF;
  IF OLD.approval_status IN ('approved','retired') AND
     (to_jsonb(NEW) - 'active' - 'approval_status' - 'updated_at') IS DISTINCT FROM
     (to_jsonb(OLD) - 'active' - 'approval_status' - 'updated_at') THEN
    RAISE EXCEPTION 'Published ticket policy is immutable; create a new version';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_policy_lifecycle_guard ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_policy_lifecycle_guard
  BEFORE UPDATE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_policy_lifecycle_guard();

-- Serialize with concurrent history writers. Corrective snapshots explicitly
-- identify the source historical row; they are not new policy approvals.
LOCK TABLE event_ticket_checkout_policy_history IN SHARE ROW EXCLUSIVE MODE;
INSERT INTO event_ticket_checkout_policy_history
  (policy_id,event_id,policy_version,snapshot,changed_by)
SELECT h.policy_id,h.event_id,h.policy_version,
       h.snapshot || jsonb_build_object(
         'max_tickets_per_order',100,
         'migration_origin_history_id',h.id,
         'migration_origin_changed_at',h.changed_at),
       'migration:2026-10-05_ticket_policy_lifecycle_history'
FROM event_ticket_checkout_policy_history h
WHERE NOT (h.snapshot ? 'max_tickets_per_order')
  AND NOT EXISTS (
    SELECT 1 FROM event_ticket_checkout_policy_history correction
    WHERE correction.changed_by='migration:2026-10-05_ticket_policy_lifecycle_history'
      AND correction.snapshot->>'migration_origin_history_id'=h.id::text
  );
COMMIT;
