-- Provider identities make the city event importer retry-safe and allow it to
-- refresh venue, artist, and event records without name-based duplication.
-- Apply before enabling EVENT_DISCOVERY_ENABLED when RUN_MIGRATIONS=false.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS external_venue_ref (
    id BIGSERIAL PRIMARY KEY,
    provider TEXT NOT NULL,
    external_id TEXT NOT NULL,
    venue_id BIGINT NOT NULL REFERENCES venue(id),
    last_seen_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT unique_external_venue_ref UNIQUE (provider, external_id)
);

CREATE TABLE IF NOT EXISTS external_artist_ref (
    id BIGSERIAL PRIMARY KEY,
    provider TEXT NOT NULL,
    external_id TEXT NOT NULL,
    artist_id BIGINT NOT NULL REFERENCES social_artist_profile(id),
    last_seen_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT unique_external_artist_ref UNIQUE (provider, external_id)
);

CREATE TABLE IF NOT EXISTS external_event_ref (
    id BIGSERIAL PRIMARY KEY,
    provider TEXT NOT NULL,
    external_id TEXT NOT NULL,
    event_id BIGINT NOT NULL REFERENCES social_event(id),
    city TEXT NOT NULL,
    source_url TEXT,
    last_seen_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT unique_external_event_ref UNIQUE (provider, external_id),
    CONSTRAINT unique_external_event_local UNIQUE (event_id)
);

CREATE INDEX IF NOT EXISTS idx_external_event_ref_city
    ON external_event_ref (lower(city));

CREATE TABLE IF NOT EXISTS external_event_discovery_run (
    id BIGSERIAL PRIMARY KEY,
    provider TEXT NOT NULL,
    run_date DATE NOT NULL,
    status TEXT NOT NULL,
    cities_count INTEGER NOT NULL DEFAULT 0,
    events_seen INTEGER NOT NULL DEFAULT 0,
    events_created INTEGER NOT NULL DEFAULT 0,
    events_updated INTEGER NOT NULL DEFAULT 0,
    venues_created INTEGER NOT NULL DEFAULT 0,
    artists_created INTEGER NOT NULL DEFAULT 0,
    error_message TEXT,
    started_at TIMESTAMPTZ NOT NULL,
    finished_at TIMESTAMPTZ,
    CONSTRAINT unique_external_event_discovery_run UNIQUE (provider, run_date)
);

COMMIT;
