-- Install the social-sync tables used by the ingestion, discovery, and Meta
-- account flows. These were previously created only by Persistent's optional
-- startup migration, while production deliberately runs with migrations off.
-- Apply before 2026-07-15_social_discovery_reviews.sql.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

DO $preflight$
BEGIN
    IF to_regclass('public.party') IS NULL
       OR to_regclass('public.artist_profile') IS NULL THEN
        RAISE EXCEPTION 'Cannot install social-sync schema: party or artist_profile is missing';
    END IF;

    IF (
        SELECT COUNT(*)
        FROM pg_catalog.pg_class AS c
        JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace
        WHERE n.nspname = 'public'
          AND c.relkind IN ('r', 'p')
          AND c.relname IN (
              'social_sync_account',
              'social_sync_post',
              'social_sync_run'
          )
    ) NOT IN (0, 3) THEN
        RAISE EXCEPTION 'Refusing social-sync migration: runtime tables are partially present';
    END IF;
END
$preflight$;

CREATE TABLE IF NOT EXISTS public.social_sync_account (
    id BIGSERIAL PRIMARY KEY,
    party_id BIGINT REFERENCES public.party(id),
    artist_profile_id BIGINT REFERENCES public.artist_profile(id),
    platform VARCHAR NOT NULL,
    external_user_id VARCHAR NOT NULL,
    handle VARCHAR,
    access_token VARCHAR,
    token_expires_at TIMESTAMPTZ,
    status VARCHAR NOT NULL,
    last_synced_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ,
    CONSTRAINT unique_social_sync_account UNIQUE (platform, external_user_id)
);

CREATE TABLE IF NOT EXISTS public.social_sync_post (
    id BIGSERIAL PRIMARY KEY,
    account_id BIGINT REFERENCES public.social_sync_account(id),
    platform VARCHAR NOT NULL,
    external_post_id VARCHAR NOT NULL,
    artist_party_id BIGINT REFERENCES public.party(id),
    artist_profile_id BIGINT REFERENCES public.artist_profile(id),
    caption VARCHAR,
    permalink VARCHAR,
    media_urls VARCHAR,
    posted_at TIMESTAMPTZ,
    fetched_at TIMESTAMPTZ NOT NULL,
    tags VARCHAR,
    summary VARCHAR,
    ingest_source VARCHAR NOT NULL,
    like_count BIGINT,
    comment_count BIGINT,
    share_count BIGINT,
    view_count BIGINT,
    created_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL,
    CONSTRAINT unique_social_sync_post UNIQUE (platform, external_post_id)
);

CREATE TABLE IF NOT EXISTS public.social_sync_run (
    id BIGSERIAL PRIMARY KEY,
    platform VARCHAR NOT NULL,
    ingest_source VARCHAR NOT NULL,
    started_at TIMESTAMPTZ NOT NULL,
    ended_at TIMESTAMPTZ,
    status VARCHAR NOT NULL,
    new_posts BIGINT NOT NULL,
    updated_posts BIGINT NOT NULL,
    error_message VARCHAR
);

CREATE INDEX IF NOT EXISTS idx_social_sync_account_party
    ON public.social_sync_account (party_id) WHERE party_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_social_sync_account_artist_profile
    ON public.social_sync_account (artist_profile_id) WHERE artist_profile_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_social_sync_post_account
    ON public.social_sync_post (account_id) WHERE account_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_social_sync_post_artist_party
    ON public.social_sync_post (artist_party_id) WHERE artist_party_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_social_sync_post_artist_profile
    ON public.social_sync_post (artist_profile_id) WHERE artist_profile_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_social_sync_post_platform_posted
    ON public.social_sync_post (platform, posted_at DESC, fetched_at DESC);
CREATE INDEX IF NOT EXISTS idx_social_sync_run_platform_started
    ON public.social_sync_run (platform, started_at DESC);

DO $verification$
BEGIN
    IF (
        SELECT COUNT(*)
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name IN (
              'social_sync_account',
              'social_sync_post',
              'social_sync_run'
          )
    ) <> 3 THEN
        RAISE EXCEPTION 'Social-sync runtime tables are incomplete';
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.social_sync_account'::regclass
          AND conname = 'unique_social_sync_account'
          AND contype = 'u'
          AND convalidated
    ) OR NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.social_sync_post'::regclass
          AND conname = 'unique_social_sync_post'
          AND contype = 'u'
          AND convalidated
    ) THEN
        RAISE EXCEPTION 'A social-sync uniqueness constraint is missing or invalid';
    END IF;
END
$verification$;

COMMIT;
