-- Stop discovery before rollback. Retain approvals, candidates, references and audits.
BEGIN;
UPDATE event_discovery_source SET enabled=false WHERE source_type<>'web';
DROP TRIGGER IF EXISTS event_discovery_pilot_limit_trigger ON external_event_ref;
DROP FUNCTION IF EXISTS tdf_enforce_discovery_pilot_limit();
DROP FUNCTION IF EXISTS tdf_event_pilot_keys(bigint,bigint);
CREATE OR REPLACE FUNCTION enforce_event_research_pilot_limit()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  pilot_approved BOOLEAN;
  pilot_limit INTEGER;
  active_candidates INTEGER;
BEGIN
  -- Let an INSERT ... ON CONFLICT retry reach the unique key. Any transition
  -- from discarded back to active is checked again by the UPDATE trigger.
  IF TG_OP = 'INSERT' AND EXISTS (
    SELECT 1
      FROM event_research_candidate
     WHERE provider = NEW.provider
       AND external_id = NEW.external_id
  ) THEN
    RETURN NEW;
  END IF;

  SELECT approved, max_active_candidates
    INTO pilot_approved, pilot_limit
    FROM event_research_pilot_control
   WHERE control_key = 'default'
   FOR UPDATE;

  IF pilot_approved IS NULL THEN
    RAISE EXCEPTION 'event research pilot control is not initialized';
  END IF;

  IF NOT pilot_approved AND NEW.is_pilot AND NEW.review_state <> 'discarded' THEN
    SELECT count(*)
      INTO active_candidates
      FROM event_research_candidate
     WHERE is_pilot
       AND review_state <> 'discarded'
       AND id <> COALESCE(NEW.id, -1);

    IF active_candidates >= pilot_limit THEN
      RAISE EXCEPTION 'event research pilot candidate limit reached';
    END IF;
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS event_research_pilot_limit_trigger ON event_research_candidate;
CREATE TRIGGER event_research_pilot_limit_trigger
BEFORE INSERT OR UPDATE OF review_state, is_pilot ON event_research_candidate
FOR EACH ROW EXECUTE FUNCTION enforce_event_research_pilot_limit();

COMMIT;
