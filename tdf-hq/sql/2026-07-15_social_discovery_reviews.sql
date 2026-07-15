-- Persistent reviewer decisions for event candidates detected in synced social media.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

CREATE TABLE IF NOT EXISTS public.social_discovery_review (
  id BIGSERIAL PRIMARY KEY,
  social_sync_post_id BIGINT NOT NULL REFERENCES public.social_sync_post(id) ON DELETE CASCADE,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'approved', 'dismissed')),
  review_notes TEXT NULL CHECK (char_length(review_notes) <= 2000),
  reviewed_by_party_id BIGINT NULL REFERENCES public.party(id) ON DELETE SET NULL,
  reviewed_at TIMESTAMPTZ NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_social_discovery_review UNIQUE (social_sync_post_id)
);

CREATE INDEX IF NOT EXISTS idx_social_discovery_review_status_updated
  ON public.social_discovery_review (status, updated_at DESC);

COMMIT;
