-- Global event-city subscriptions, provider registry, six-hour run slots,
-- and multi-provider references for canonical imported events.
-- Apply with EVENT_DISCOVERY_ENABLED=false when RUN_MIGRATIONS=false.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS event_city (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    normalized_name TEXT NOT NULL,
    country_code TEXT NOT NULL,
    time_zone TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_event_city UNIQUE (normalized_name, country_code),
    CONSTRAINT event_city_country_code_check
        CHECK (country_code ~ '^[A-Z]{2}$')
);

CREATE TABLE IF NOT EXISTS event_city_subscription (
    id BIGSERIAL PRIMARY KEY,
    party_id TEXT NOT NULL,
    city_id BIGINT NOT NULL REFERENCES event_city(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_event_city_subscription UNIQUE (party_id, city_id)
);

CREATE INDEX IF NOT EXISTS idx_event_city_subscription_city
    ON event_city_subscription (city_id);

CREATE TABLE IF NOT EXISTS event_discovery_source (
    id BIGSERIAL PRIMARY KEY,
    source_key TEXT NOT NULL,
    name TEXT NOT NULL,
    source_type TEXT NOT NULL,
    feed_url TEXT,
    city_id BIGINT REFERENCES event_city(id),
    enabled BOOLEAN NOT NULL DEFAULT TRUE,
    priority INTEGER NOT NULL DEFAULT 100,
    configuration TEXT,
    etag TEXT,
    last_modified TEXT,
    consecutive_failures INTEGER NOT NULL DEFAULT 0,
    last_success_at TIMESTAMPTZ,
    last_error TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT unique_event_discovery_source UNIQUE (source_key),
    CONSTRAINT event_discovery_source_type_check
        CHECK (source_type IN ('ticketmaster', 'buenplan', 'ical', 'json')),
    CONSTRAINT event_discovery_source_priority_check
        CHECK (priority BETWEEN 0 AND 10000)
);

INSERT INTO event_discovery_source
    (source_key, name, source_type, enabled, priority)
VALUES
    ('ticketmaster', 'Ticketmaster', 'ticketmaster', TRUE, 300),
    ('buenplan', 'Buen Plan', 'buenplan', TRUE, 200)
ON CONFLICT (source_key) DO NOTHING;

ALTER TABLE external_event_ref
    ADD COLUMN IF NOT EXISTS country_code TEXT,
    ADD COLUMN IF NOT EXISTS price_cents INTEGER,
    ADD COLUMN IF NOT EXISTS currency TEXT,
    ADD COLUMN IF NOT EXISTS missing_runs INTEGER NOT NULL DEFAULT 0,
    ADD COLUMN IF NOT EXISTS source_status TEXT NOT NULL DEFAULT 'active';

ALTER TABLE external_event_discovery_run
    ADD COLUMN IF NOT EXISTS scheduled_for TIMESTAMPTZ;

ALTER TABLE external_event_ref
    DROP CONSTRAINT IF EXISTS unique_external_event_local;

CREATE INDEX IF NOT EXISTS idx_external_event_ref_event_id
    ON external_event_ref (event_id);

CREATE INDEX IF NOT EXISTS idx_external_event_ref_city_country
    ON external_event_ref (lower(city), country_code);

ALTER TABLE external_event_discovery_run
    DROP CONSTRAINT IF EXISTS unique_external_event_discovery_run;

CREATE UNIQUE INDEX IF NOT EXISTS unique_external_event_discovery_slot
    ON external_event_discovery_run (provider, scheduled_for)
    WHERE scheduled_for IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_event_discovery_source_enabled_priority
    ON event_discovery_source (enabled, priority);

-- Existing profiles predate country-aware city selection. The product's
-- historical default is Ecuador, so migrate those values once as EC. Older
-- production baselines may not contain both profile tables (or all current
-- columns), so each optional backfill is guarded independently.
DO $$
BEGIN
    IF to_regclass('public.fan_profile') IS NOT NULL
       AND EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'fan_profile'
             AND column_name = 'city'
       ) THEN
        INSERT INTO event_city
            (name, normalized_name, country_code, created_at, updated_at)
        SELECT DISTINCT
            trim(city),
            lower(regexp_replace(trim(city), '\s+', ' ', 'g')),
            'EC',
            now(),
            now()
        FROM fan_profile
        WHERE city IS NOT NULL
          AND trim(city) <> ''
          AND length(trim(city)) <= 120
        ON CONFLICT (normalized_name, country_code) DO NOTHING;
    END IF;

    IF to_regclass('public.artist_profile') IS NOT NULL
       AND EXISTS (
           SELECT 1
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'artist_profile'
             AND column_name = 'city'
       ) THEN
        INSERT INTO event_city
            (name, normalized_name, country_code, created_at, updated_at)
        SELECT DISTINCT
            trim(city),
            lower(regexp_replace(trim(city), '\s+', ' ', 'g')),
            'EC',
            now(),
            now()
        FROM artist_profile
        WHERE city IS NOT NULL
          AND trim(city) <> ''
          AND length(trim(city)) <= 120
        ON CONFLICT (normalized_name, country_code) DO NOTHING;
    END IF;
END
$$;

DO $$
BEGIN
    IF to_regclass('public.user_credential') IS NOT NULL
       AND to_regclass('public.fan_profile') IS NOT NULL
       AND (
           SELECT count(*) = 2
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'fan_profile'
             AND column_name IN ('fan_party_id', 'city')
       )
       AND (
           SELECT count(*) = 2
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'user_credential'
             AND column_name IN ('party_id', 'active')
       ) THEN
        INSERT INTO event_city_subscription (party_id, city_id, created_at)
        SELECT DISTINCT fp.fan_party_id::text, city.id, now()
        FROM fan_profile fp
        INNER JOIN user_credential uc ON uc.party_id = fp.fan_party_id
        INNER JOIN event_city city
            ON city.country_code = 'EC'
           AND city.normalized_name =
               lower(regexp_replace(trim(fp.city), '\s+', ' ', 'g'))
        WHERE uc.active = TRUE
          AND fp.city IS NOT NULL
          AND trim(fp.city) <> ''
        ON CONFLICT (party_id, city_id) DO NOTHING;
    END IF;

    IF to_regclass('public.user_credential') IS NOT NULL
       AND to_regclass('public.artist_profile') IS NOT NULL
       AND (
           SELECT count(*) = 2
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'artist_profile'
             AND column_name IN ('artist_party_id', 'city')
       )
       AND (
           SELECT count(*) = 2
           FROM information_schema.columns
           WHERE table_schema = 'public'
             AND table_name = 'user_credential'
             AND column_name IN ('party_id', 'active')
       ) THEN
        INSERT INTO event_city_subscription (party_id, city_id, created_at)
        SELECT DISTINCT ap.artist_party_id::text, city.id, now()
        FROM artist_profile ap
        INNER JOIN user_credential uc ON uc.party_id = ap.artist_party_id
        INNER JOIN event_city city
            ON city.country_code = 'EC'
           AND city.normalized_name =
               lower(regexp_replace(trim(ap.city), '\s+', ' ', 'g'))
        WHERE uc.active = TRUE
          AND ap.city IS NOT NULL
          AND trim(ap.city) <> ''
        ON CONFLICT (party_id, city_id) DO NOTHING;
    END IF;
END
$$;

COMMIT;
