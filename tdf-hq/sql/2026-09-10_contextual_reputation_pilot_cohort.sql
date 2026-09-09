-- Server-enforced membership for the limited contextual-reputation pilot.
--
-- A deployment flag may make the feature available, but it must never grant
-- access by itself. This additive migration keeps the cohort small, explicit,
-- reversible, and auditable without placing membership data in client state.
\set ON_ERROR_STOP on
BEGIN;

CREATE TABLE IF NOT EXISTS reputation_pilot_cohort_membership (
  party_id BIGINT PRIMARY KEY REFERENCES party(id) ON DELETE RESTRICT,
  status TEXT NOT NULL DEFAULT 'active'
    CHECK (status IN ('active', 'withdrawn', 'suspended')),
  enrolled_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  expires_at TIMESTAMPTZ,
  enrolled_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  changed_by_party_id BIGINT REFERENCES party(id) ON DELETE RESTRICT,
  change_reason TEXT NOT NULL CHECK (length(btrim(change_reason)) BETWEEN 3 AND 500),
  changed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (expires_at IS NULL OR expires_at > enrolled_at)
);

CREATE INDEX IF NOT EXISTS reputation_pilot_cohort_active_idx
  ON reputation_pilot_cohort_membership (party_id, expires_at)
  WHERE status = 'active';

CREATE OR REPLACE FUNCTION reputation_pilot_cohort_membership_audit()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  action_name TEXT;
  actor_party BIGINT;
BEGIN
  IF TG_OP = 'INSERT' THEN
    action_name := 'reputation.pilot-cohort.enrolled';
    actor_party := COALESCE(NEW.changed_by_party_id, NEW.enrolled_by_party_id);
  ELSIF OLD.status IS DISTINCT FROM NEW.status THEN
    action_name := 'reputation.pilot-cohort.status-changed';
    actor_party := NEW.changed_by_party_id;
  ELSE
    action_name := 'reputation.pilot-cohort.updated';
    actor_party := NEW.changed_by_party_id;
  END IF;

  IF TG_OP = 'UPDATE' THEN
    NEW.changed_at := now();
  END IF;

  INSERT INTO reputation_audit_log(
    actor_party_id, action, resource_kind, resource_id, reason, metadata
  ) VALUES (
    actor_party,
    action_name,
    'reputation_pilot_cohort_membership',
    NEW.party_id::text,
    NEW.change_reason,
    jsonb_build_object(
      'status', NEW.status,
      'expiresAt', NEW.expires_at,
      'previousStatus', CASE WHEN TG_OP = 'UPDATE' THEN OLD.status ELSE NULL END
    )
  );
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_reputation_pilot_cohort_membership_audit
  ON reputation_pilot_cohort_membership;
CREATE TRIGGER trg_reputation_pilot_cohort_membership_audit
  BEFORE INSERT OR UPDATE ON reputation_pilot_cohort_membership
  FOR EACH ROW EXECUTE FUNCTION reputation_pilot_cohort_membership_audit();

COMMIT;
