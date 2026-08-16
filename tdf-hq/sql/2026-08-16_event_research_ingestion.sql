BEGIN;

CREATE TABLE IF NOT EXISTS event_research_pilot_control (
  id BIGSERIAL PRIMARY KEY,
  control_key TEXT NOT NULL UNIQUE,
  approved BOOLEAN NOT NULL DEFAULT FALSE,
  approved_at TIMESTAMPTZ,
  approved_by_party_id TEXT,
  approval_reference TEXT,
  max_active_candidates INTEGER NOT NULL DEFAULT 20 CHECK (max_active_candidates = 20),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CHECK (NOT approved OR (approved_at IS NOT NULL AND approved_by_party_id IS NOT NULL AND approval_reference IS NOT NULL))
);

INSERT INTO event_research_pilot_control (control_key, approved, max_active_candidates)
VALUES ('default', FALSE, 20)
ON CONFLICT (control_key) DO NOTHING;

CREATE TABLE IF NOT EXISTS event_research_pilot_audit (
  id BIGSERIAL PRIMARY KEY,
  control_id BIGINT NOT NULL REFERENCES event_research_pilot_control(id),
  approved BOOLEAN NOT NULL,
  approved_by_party_id TEXT NOT NULL,
  approval_reference TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS event_research_run (
  id BIGSERIAL PRIMARY KEY,
  run_key TEXT NOT NULL UNIQUE,
  status TEXT NOT NULL CHECK (status IN ('running', 'completed', 'failed')),
  reconciliation BOOLEAN NOT NULL DEFAULT FALSE,
  checkpoint TEXT,
  counters TEXT NOT NULL DEFAULT '{}',
  error_summary TEXT,
  started_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  finished_at TIMESTAMPTZ,
  created_by_party_id TEXT NOT NULL,
  CHECK ((status = 'running' AND finished_at IS NULL) OR (status IN ('completed', 'failed') AND finished_at IS NOT NULL)),
  CHECK (counters::jsonb IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS event_research_candidate (
  id BIGSERIAL PRIMARY KEY,
  provider TEXT NOT NULL,
  external_id TEXT NOT NULL,
  run_id BIGINT NOT NULL REFERENCES event_research_run(id),
  source_id BIGINT REFERENCES event_discovery_source(id),
  event_id BIGINT REFERENCES social_event(id),
  review_state TEXT NOT NULL CHECK (review_state IN ('draft', 'review', 'discarded')),
  title TEXT NOT NULL,
  start_time TIMESTAMPTZ,
  end_time TIMESTAMPTZ,
  timezone TEXT NOT NULL,
  venue_name TEXT,
  city TEXT,
  province TEXT,
  country_code TEXT NOT NULL CHECK (country_code ~ '^[A-Z]{2}$'),
  source_url TEXT NOT NULL,
  info_url TEXT,
  purchase_url TEXT,
  payload TEXT NOT NULL,
  evidence TEXT NOT NULL,
  confidence TEXT NOT NULL CHECK (confidence IN ('high', 'medium', 'low')),
  managed_fields TEXT NOT NULL DEFAULT '[]',
  content_hash TEXT NOT NULL CHECK (content_hash ~ '^[0-9a-f]{64}$'),
  verified_at TIMESTAMPTZ NOT NULL,
  is_pilot BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  UNIQUE (provider, external_id),
  CHECK (start_time IS NULL OR end_time IS NULL OR start_time < end_time),
  CHECK (payload::jsonb IS NOT NULL),
  CHECK (jsonb_typeof(evidence::jsonb) = 'array'),
  CHECK (jsonb_array_length(evidence::jsonb) > 0),
  CHECK (jsonb_typeof(managed_fields::jsonb) = 'array')
);

CREATE INDEX IF NOT EXISTS event_research_candidate_run_idx
  ON event_research_candidate(run_id, verified_at DESC);
CREATE INDEX IF NOT EXISTS event_research_candidate_review_idx
  ON event_research_candidate(review_state, verified_at DESC);
CREATE INDEX IF NOT EXISTS event_research_candidate_event_idx
  ON event_research_candidate(event_id) WHERE event_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS event_research_change (
  id BIGSERIAL PRIMARY KEY,
  run_id BIGINT NOT NULL REFERENCES event_research_run(id),
  candidate_id BIGINT REFERENCES event_research_candidate(id),
  event_id BIGINT REFERENCES social_event(id),
  action TEXT NOT NULL CHECK (action IN ('created', 'updated', 'verified', 'discarded', 'materialized')),
  before_value TEXT,
  after_value TEXT,
  source_url TEXT NOT NULL,
  confidence TEXT NOT NULL CHECK (confidence IN ('high', 'medium', 'low')),
  consulted_at TIMESTAMPTZ NOT NULL,
  external_id TEXT NOT NULL,
  result TEXT NOT NULL,
  dedupe_key TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL,
  CHECK (before_value IS NULL OR before_value::jsonb IS NOT NULL),
  CHECK (after_value IS NULL OR after_value::jsonb IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS event_research_change_run_idx
  ON event_research_change(run_id, created_at DESC);
CREATE INDEX IF NOT EXISTS event_research_change_candidate_idx
  ON event_research_change(candidate_id, created_at DESC);

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

-- Web sources are maintained for manual research only. The API and database keep
-- them disabled so the structured-feed cron never attempts to scrape HTML.
INSERT INTO event_discovery_source
  (source_key, name, source_type, feed_url, city_id, enabled, priority, configuration,
   etag, last_modified, consecutive_failures, last_success_at, last_error, created_at, updated_at)
VALUES
  ('meet2go-web', 'Meet2Go', 'web', 'https://meet2go.com/', NULL, FALSE, 250, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('passline-ec-web', 'Passline Ecuador', 'web', 'https://www.instagram.com/passline.ec/', NULL, FALSE, 240, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('buenplan-social-web', 'BuenPlan Tickets (social)', 'web', 'https://www.instagram.com/buenplantickets/', NULL, FALSE, 230, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('feelthetickets-web', 'Feel The Tickets', 'web', 'https://www.feelthetickets.com/', NULL, FALSE, 220, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('ticketshow-web', 'TicketShow', 'web', 'https://www.ticketshow.com.ec/', NULL, FALSE, 210, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('ontime-tickets-web', 'On Time Tickets', 'web', 'https://www.ontimetickets.com/', NULL, FALSE, 205, NULL, NULL, NULL, 0, NULL, NULL, now(), now()),
  ('output-concerts-web', 'Output Concerts', 'web', 'https://conciertos.output.ec/', NULL, FALSE, 190, NULL, NULL, NULL, 0, NULL, NULL, now(), now())
ON CONFLICT (source_key) DO NOTHING;

COMMIT;
