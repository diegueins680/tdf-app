-- Safe RSVP identity, consent, concurrency, feed, and public-event projection.
-- Historical RSVPs remain private because show_on_profile defaults to FALSE
-- and visibility_decided_at remains NULL until a fresh authenticated choice.
\set ON_ERROR_STOP on
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

DO $$
BEGIN
  IF to_regclass('public.event_rsvp') IS NULL THEN
    RAISE EXCEPTION 'event_rsvp is required';
  END IF;
  IF to_regclass('public.social_event') IS NULL OR to_regclass('public.party') IS NULL THEN
    RAISE EXCEPTION 'social_event and party are required';
  END IF;
END
$$;

ALTER TABLE event_rsvp ADD COLUMN IF NOT EXISTS show_on_profile BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE event_rsvp ADD COLUMN IF NOT EXISTS visibility_decided_at TIMESTAMPTZ;
ALTER TABLE user_locale_preferences ADD COLUMN IF NOT EXISTS show_event_rsvps_on_profile BOOLEAN NOT NULL DEFAULT TRUE;

CREATE TABLE IF NOT EXISTS event_rsvp_migration_evidence (
  id BIGSERIAL PRIMARY KEY,
  source_rsvp_id BIGINT NOT NULL,
  kept_rsvp_id BIGINT,
  event_id BIGINT NOT NULL,
  original_party_id TEXT NOT NULL,
  normalized_party_id TEXT,
  original_status TEXT NOT NULL,
  reason TEXT NOT NULL CHECK (reason IN ('invalid_party','invalid_status','duplicate')),
  row_snapshot JSONB NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (source_rsvp_id, reason)
);

-- Quarantine malformed legacy records rather than coercing them into a real
-- account or an invented status. The complete original row remains available
-- to authorized operators in the evidence table.
INSERT INTO event_rsvp_migration_evidence(
  source_rsvp_id,event_id,original_party_id,normalized_party_id,original_status,reason,row_snapshot
)
SELECT rsvp.id,rsvp.event_id,rsvp.party_id,NULL,rsvp.status,'invalid_party',to_jsonb(rsvp)
FROM event_rsvp rsvp
WHERE CASE
  WHEN trim(rsvp.party_id) !~ '^[0-9]+$' THEN TRUE
  WHEN nullif(trim(leading '0' FROM trim(rsvp.party_id)),'') IS NULL THEN TRUE
  WHEN length(trim(leading '0' FROM trim(rsvp.party_id)))>19 THEN TRUE
  WHEN length(trim(leading '0' FROM trim(rsvp.party_id)))=19
    AND trim(leading '0' FROM trim(rsvp.party_id))>'9223372036854775807' THEN TRUE
  ELSE NOT EXISTS (
    SELECT 1 FROM party
    WHERE party.id=(trim(leading '0' FROM trim(rsvp.party_id)))::bigint
  )
END
ON CONFLICT (source_rsvp_id,reason) DO NOTHING;

INSERT INTO event_rsvp_migration_evidence(
  source_rsvp_id,event_id,original_party_id,normalized_party_id,original_status,reason,row_snapshot
)
SELECT
  rsvp.id,rsvp.event_id,rsvp.party_id,
  trim(leading '0' FROM trim(rsvp.party_id)),rsvp.status,'invalid_status',to_jsonb(rsvp)
FROM event_rsvp rsvp
WHERE lower(trim(rsvp.status)) NOT IN ('accepted','maybe','declined')
ON CONFLICT (source_rsvp_id,reason) DO NOTHING;

DELETE FROM event_rsvp rsvp
USING event_rsvp_migration_evidence evidence
WHERE evidence.source_rsvp_id=rsvp.id
  AND evidence.reason IN ('invalid_party','invalid_status');

UPDATE event_rsvp
SET party_id=trim(leading '0' FROM trim(party_id)),
    status=lower(trim(status));

WITH ranked AS (
  SELECT
    rsvp.*,
    first_value(id) OVER (
      PARTITION BY event_id,party_id
      ORDER BY updated_at DESC,created_at DESC,id DESC
    ) AS kept_id,
    row_number() OVER (
      PARTITION BY event_id,party_id
      ORDER BY updated_at DESC,created_at DESC,id DESC
    ) AS duplicate_rank
  FROM event_rsvp rsvp
)
INSERT INTO event_rsvp_migration_evidence(
  source_rsvp_id,kept_rsvp_id,event_id,original_party_id,normalized_party_id,
  original_status,reason,row_snapshot
)
SELECT id,kept_id,event_id,party_id,party_id,status,'duplicate',to_jsonb(ranked)-'duplicate_rank'-'kept_id'
FROM ranked
WHERE duplicate_rank>1
ON CONFLICT (source_rsvp_id,reason) DO NOTHING;

WITH ranked AS (
  SELECT id,row_number() OVER (
    PARTITION BY event_id,party_id
    ORDER BY updated_at DESC,created_at DESC,id DESC
  ) AS duplicate_rank
  FROM event_rsvp
)
DELETE FROM event_rsvp rsvp
USING ranked
WHERE ranked.id=rsvp.id AND ranked.duplicate_rank>1;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname='event_rsvp_status_check' AND conrelid='event_rsvp'::regclass
  ) THEN
    ALTER TABLE event_rsvp
      ADD CONSTRAINT event_rsvp_status_check
      CHECK (status IN ('accepted','maybe','declined')) NOT VALID;
  END IF;
END
$$;
ALTER TABLE event_rsvp VALIDATE CONSTRAINT event_rsvp_status_check;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname='event_rsvp_event_party_unique' AND conrelid='event_rsvp'::regclass
  ) THEN
    CREATE UNIQUE INDEX IF NOT EXISTS ux_event_rsvp_event_party ON event_rsvp(event_id,party_id);
    ALTER TABLE event_rsvp
      ADD CONSTRAINT event_rsvp_event_party_unique
      UNIQUE USING INDEX ux_event_rsvp_event_party;
  END IF;
END
$$;

CREATE INDEX IF NOT EXISTS ix_event_rsvp_event_status
  ON event_rsvp(event_id,status);
CREATE INDEX IF NOT EXISTS ix_event_rsvp_profile_feed
  ON event_rsvp(party_id,updated_at DESC,event_id DESC)
  WHERE show_on_profile AND visibility_decided_at IS NOT NULL AND status IN ('accepted','maybe');

CREATE TABLE IF NOT EXISTS event_rsvp_mutation_rate_limit (
  id BIGSERIAL PRIMARY KEY,
  party_id TEXT NOT NULL,
  window_start TIMESTAMPTZ NOT NULL,
  mutation_count INTEGER NOT NULL DEFAULT 1 CHECK (mutation_count > 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (party_id,window_start)
);
CREATE INDEX IF NOT EXISTS ix_event_rsvp_rate_window
  ON event_rsvp_mutation_rate_limit(window_start);

-- event_rsvp.party_id is a legacy text reference, so it cannot use a normal
-- foreign-key cascade. Ensure hard account deletion removes the associated
-- RSVP and mutation-throttle data instead of leaving a public-feed candidate.
CREATE OR REPLACE FUNCTION delete_event_rsvp_for_party()
RETURNS TRIGGER LANGUAGE plpgsql AS $$
BEGIN
  DELETE FROM event_rsvp WHERE party_id=OLD.id::text;
  DELETE FROM event_rsvp_mutation_rate_limit WHERE party_id=OLD.id::text;
  RETURN OLD;
END
$$;

DROP TRIGGER IF EXISTS trg_delete_event_rsvp_for_party ON party;
CREATE TRIGGER trg_delete_event_rsvp_for_party
BEFORE DELETE ON party
FOR EACH ROW EXECUTE FUNCTION delete_event_rsvp_for_party();

INSERT INTO workflow_state_capability(
  id,state_id,capability_code,enabled,created_at,updated_at,version
)
SELECT gen_random_uuid(),state.id,'rsvp',TRUE,now(),now(),1
FROM workflow_state state
JOIN workflow_definition workflow ON workflow.id=state.workflow_id
WHERE workflow.code='social-event-lifecycle'
  AND workflow.active AND state.active
  AND state.code IN ('announced','on_sale','live','postponed')
ON CONFLICT (state_id,capability_code)
DO UPDATE SET enabled=TRUE,updated_at=excluded.updated_at,version=workflow_state_capability.version+1;

-- Additive columns are appended to the existing view so dependent views stay
-- valid. Explicit event privacy is now part of the anonymous boundary.
CREATE OR REPLACE VIEW directory_public_event AS
SELECT
  event.id,
  event.title,
  event.description,
  event.start_time,
  event.end_time,
  event.timezone,
  event.price_cents,
  event.currency_id,
  event.capacity,
  event.venue_id,
  venue.name AS venue_name,
  city.id AS city_id,
  coalesce(city.name_es,venue.city) AS city_name,
  country.alpha2 AS country_code,
  city.latitude AS public_latitude,
  city.longitude AS public_longitude,
  event.updated_at,
  CASE WHEN event.metadata IS JSON OBJECT
    AND strpos(event.metadata::jsonb->>'imageUrl',chr(92))=0
    AND (
      (
        event.metadata::jsonb->>'imageUrl' ~* '^https://[^[:space:][:cntrl:]]+$'
        AND split_part(event.metadata::jsonb->>'imageUrl','/',3) NOT LIKE '%@%'
      )
      OR event.metadata::jsonb->>'imageUrl' ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'
    )
    THEN event.metadata::jsonb->>'imageUrl'
    ELSE NULL
  END AS image_url,
  TRUE AS is_public,
  state.code AS workflow_state_code,
  EXISTS (
    SELECT 1 FROM workflow_state_capability rsvp_capability
    WHERE rsvp_capability.state_id=state.id
      AND rsvp_capability.capability_code='rsvp'
      AND rsvp_capability.enabled
  ) AS rsvp_eligible,
  (
    state.code<>'cancelled'
    AND EXISTS (
      SELECT 1 FROM workflow_state_capability share_capability
      WHERE share_capability.state_id=state.id
        AND share_capability.capability_code='rsvp'
        AND share_capability.enabled
    )
  ) AS public_share_eligible
FROM social_event event
JOIN workflow_state state ON state.id=event.workflow_state_id
LEFT JOIN venue ON venue.id=event.venue_id
LEFT JOIN city_reference city ON city.id=venue.city_id
LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id,city.country_id)
WHERE state.active
  AND coalesce(
    CASE WHEN event.metadata IS JSON OBJECT
      AND event.metadata::jsonb->>'isPublic' IN ('true','false')
      THEN (event.metadata::jsonb->>'isPublic')::boolean
      ELSE FALSE
    END,
    FALSE
  )
  AND (
    state.code='cancelled'
    OR EXISTS (
      SELECT 1 FROM workflow_state_capability public_capability
      WHERE public_capability.state_id=state.id
        AND public_capability.capability_code='public-listable'
        AND public_capability.enabled
    )
  );

DO $$
DECLARE duplicate_count BIGINT;
DECLARE historical_visibility_count BIGINT;
BEGIN
  SELECT count(*) INTO duplicate_count
  FROM (
    SELECT 1 FROM event_rsvp GROUP BY event_id,party_id HAVING count(*)>1
  ) duplicate_groups;
  IF duplicate_count<>0 THEN
    RAISE EXCEPTION 'RSVP deduplication verification failed: % duplicate groups remain',duplicate_count;
  END IF;

  SELECT count(*) INTO historical_visibility_count
  FROM event_rsvp
  WHERE visibility_decided_at IS NULL AND show_on_profile;
  IF historical_visibility_count<>0 THEN
    RAISE EXCEPTION 'Historical RSVP privacy verification failed';
  END IF;
END
$$;

COMMIT;
