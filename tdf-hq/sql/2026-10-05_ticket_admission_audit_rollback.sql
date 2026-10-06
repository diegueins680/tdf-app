-- Roll back only before admission has begun. Never discard attendance evidence.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_admission_audit) THEN
    RAISE EXCEPTION 'Admission audit is populated; retain it and recover forward';
  END IF;
END $$;
DROP TABLE event_ticket_admission_audit;
COMMIT;
