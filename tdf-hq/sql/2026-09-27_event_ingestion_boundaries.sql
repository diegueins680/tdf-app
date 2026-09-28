-- One cumulative pilot for research candidates and structured discovery.
-- No research approval grants automatic publication authority.
BEGIN;
SET LOCAL lock_timeout='5s';

CREATE TABLE IF NOT EXISTS event_discovery_publication_approval (
  id bigserial PRIMARY KEY,
  source_id bigint NOT NULL REFERENCES event_discovery_source(id),
  approval_reference text NOT NULL CHECK(length(trim(approval_reference)) BETWEEN 1 AND 500),
  approved_by_party_id bigint NOT NULL REFERENCES party(id),
  approved_at timestamptz NOT NULL DEFAULT now(),
  revoked_at timestamptz,
  CHECK(revoked_at IS NULL OR revoked_at>=approved_at)
);
CREATE UNIQUE INDEX IF NOT EXISTS event_discovery_publication_approval_active
  ON event_discovery_publication_approval(source_id) WHERE revoked_at IS NULL;

CREATE OR REPLACE FUNCTION tdf_lock_event_publication_approval()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM 1 FROM event_research_pilot_control WHERE control_key='default' FOR UPDATE;
  IF TG_OP='DELETE' THEN RAISE EXCEPTION 'publication approvals must be retained; revoke instead'; END IF;
  IF TG_OP='INSERT' AND NOT EXISTS(SELECT 1 FROM event_discovery_source
    WHERE id=NEW.source_id AND source_type IN ('ticketmaster','buenplan','ical','json')) THEN
    RAISE EXCEPTION 'manual research sources cannot receive automatic publication approval';
  END IF;
  IF TG_OP='UPDATE' AND (NEW.source_id IS DISTINCT FROM OLD.source_id
    OR NEW.approval_reference IS DISTINCT FROM OLD.approval_reference
    OR NEW.approved_by_party_id IS DISTINCT FROM OLD.approved_by_party_id
    OR NEW.approved_at IS DISTINCT FROM OLD.approved_at
    OR (OLD.revoked_at IS NOT NULL AND NEW.revoked_at IS DISTINCT FROM OLD.revoked_at)) THEN
    RAISE EXCEPTION 'publication approval evidence is immutable';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_publication_approval_lock ON event_discovery_publication_approval;
CREATE TRIGGER event_publication_approval_lock BEFORE INSERT OR UPDATE OR DELETE
  ON event_discovery_publication_approval FOR EACH ROW EXECUTE FUNCTION tdf_lock_event_publication_approval();

-- Approval is scoped to the source that was reviewed, not a future replacement
-- URL or identity stored under the same numeric key.
CREATE OR REPLACE FUNCTION tdf_revoke_changed_event_source_approval()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.source_key IS DISTINCT FROM OLD.source_key OR NEW.source_type IS DISTINCT FROM OLD.source_type
    OR NEW.feed_url IS DISTINCT FROM OLD.feed_url OR NEW.city_id IS DISTINCT FROM OLD.city_id
    OR (OLD.enabled AND NOT NEW.enabled) THEN
    UPDATE event_discovery_publication_approval SET revoked_at=now()
      WHERE source_id=OLD.id AND revoked_at IS NULL;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_source_publication_scope ON event_discovery_source;
CREATE TRIGGER event_source_publication_scope BEFORE UPDATE ON event_discovery_source
  FOR EACH ROW EXECUTE FUNCTION tdf_revoke_changed_event_source_approval();

CREATE OR REPLACE FUNCTION tdf_event_pilot_keys(excluded_candidate bigint DEFAULT -1,
  excluded_reference bigint DEFAULT -1)
RETURNS TABLE(identity text) LANGUAGE sql STABLE AS $$
  SELECT 'event:'||r.event_id::text FROM external_event_ref r
    WHERE r.id<>excluded_reference AND lower(trim(r.source_status))<>'suppressed'
  UNION
  SELECT CASE WHEN coalesce(c.event_id,r.event_id) IS NOT NULL
    THEN 'event:'||coalesce(c.event_id,r.event_id)::text ELSE 'candidate:'||c.id::text END
    FROM event_research_candidate c LEFT JOIN external_event_ref r
      ON r.provider=c.provider AND r.external_id=c.external_id
      AND lower(trim(r.source_status))<>'suppressed'
    WHERE c.id<>excluded_candidate AND c.is_pilot AND c.review_state<>'discarded'
$$;

CREATE OR REPLACE FUNCTION enforce_event_research_pilot_limit()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE approved boolean; maximum integer; target text; active_count integer;
BEGIN
  SELECT c.approved,c.max_active_candidates INTO approved,maximum
    FROM event_research_pilot_control c WHERE c.control_key='default' FOR UPDATE;
  IF approved IS NULL THEN RAISE EXCEPTION 'event research pilot control is not initialized'; END IF;
  IF approved THEN RETURN NEW; END IF;
  -- An API caller cannot evade the unapproved pilot by clearing this flag.
  NEW.is_pilot:=true;
  IF NEW.review_state='discarded' THEN RETURN NEW; END IF;
  IF TG_OP='INSERT' AND EXISTS(SELECT 1 FROM event_research_candidate
    WHERE provider=NEW.provider AND external_id=NEW.external_id) THEN RETURN NEW; END IF;
  SELECT 'event:'||coalesce(NEW.event_id,r.event_id)::text INTO target
    FROM (SELECT 1) seed LEFT JOIN external_event_ref r
      ON r.provider=NEW.provider AND r.external_id=NEW.external_id
      AND lower(trim(r.source_status))<>'suppressed';
  target:=coalesce(target,'candidate:'||NEW.id::text);
  SELECT count(*) INTO active_count FROM tdf_event_pilot_keys(coalesce(NEW.id,-1),-1);
  IF NOT EXISTS(SELECT 1 FROM tdf_event_pilot_keys(coalesce(NEW.id,-1),-1) WHERE identity=target)
    AND active_count>=maximum THEN
    -- Existing active records can still refresh if historical data exceeds cap.
    IF TG_OP='UPDATE' AND OLD.is_pilot AND OLD.review_state<>'discarded'
      AND OLD.event_id IS NOT DISTINCT FROM NEW.event_id THEN RETURN NEW; END IF;
    RAISE EXCEPTION 'event research pilot candidate limit reached';
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_research_pilot_limit_trigger ON event_research_candidate;
CREATE TRIGGER event_research_pilot_limit_trigger
  BEFORE INSERT OR UPDATE OF review_state,is_pilot,event_id,provider,external_id
  ON event_research_candidate FOR EACH ROW EXECUTE FUNCTION enforce_event_research_pilot_limit();

CREATE OR REPLACE FUNCTION tdf_enforce_discovery_pilot_limit()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE approved boolean; maximum integer; active_count integer; target text;
BEGIN
  SELECT c.approved,c.max_active_candidates INTO approved,maximum
    FROM event_research_pilot_control c WHERE c.control_key='default' FOR UPDATE;
  IF approved IS NULL THEN RAISE EXCEPTION 'event research pilot control is not initialized'; END IF;
  IF approved OR lower(trim(NEW.source_status))='suppressed' THEN RETURN NEW; END IF;
  IF TG_OP='INSERT' AND EXISTS(SELECT 1 FROM external_event_ref
    WHERE provider=NEW.provider AND external_id=NEW.external_id) THEN RETURN NEW; END IF;
  IF TG_OP='UPDATE' AND OLD.event_id=NEW.event_id AND lower(trim(OLD.source_status))<>'suppressed'
    THEN RETURN NEW; END IF;
  target:='event:'||NEW.event_id::text;
  -- A matching unlinked candidate is replaced by the canonical event identity,
  -- so attaching its verified provider reference consumes no second slot.
  IF EXISTS(SELECT 1 FROM event_research_candidate WHERE provider=NEW.provider
    AND external_id=NEW.external_id AND is_pilot AND review_state<>'discarded') THEN RETURN NEW; END IF;
  SELECT count(*) INTO active_count FROM tdf_event_pilot_keys(-1,coalesce(NEW.id,-1));
  IF NOT EXISTS(SELECT 1 FROM tdf_event_pilot_keys(-1,coalesce(NEW.id,-1)) WHERE identity=target)
    AND active_count>=maximum THEN RAISE EXCEPTION 'event research pilot candidate limit reached'; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_discovery_pilot_limit_trigger ON external_event_ref;
CREATE TRIGGER event_discovery_pilot_limit_trigger
  BEFORE INSERT OR UPDATE OF event_id,source_status ON external_event_ref
  FOR EACH ROW EXECUTE FUNCTION tdf_enforce_discovery_pilot_limit();
COMMIT;
