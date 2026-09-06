-- Persist onboarding state against the authenticated party so completion and
-- new-account eligibility survive refreshes, reinstalls and device changes.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE TABLE IF NOT EXISTS user_onboarding_progress (
  id BIGSERIAL PRIMARY KEY,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  signup_completed_at TIMESTAMPTZ NULL,
  intent TEXT NULL,
  completed_at TIMESTAMPTZ NULL,
  first_value TEXT NULL,
  first_value_completed_at TIMESTAMPTZ NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT user_onboarding_progress_party_unique UNIQUE (party_id),
  CONSTRAINT user_onboarding_progress_intent_check CHECK (
    intent IS NULL OR intent IN (
      'events',
      'follow_artists',
      'artist_profile',
      'internships',
      'learning',
      'professional_tools'
    )
  ),
  CONSTRAINT user_onboarding_progress_first_value_check CHECK (
    first_value IS NULL OR first_value IN (
      'artist_followed',
      'access_requested',
      'event_saved',
      'moment_reaction'
    )
  ),
  CONSTRAINT user_onboarding_progress_first_value_pair_check CHECK (
    (first_value IS NULL) = (first_value_completed_at IS NULL)
  ),
  CONSTRAINT user_onboarding_progress_completion_order_check CHECK (
    completed_at IS NULL
      OR signup_completed_at IS NULL
      OR completed_at >= signup_completed_at
  )
);

CREATE INDEX IF NOT EXISTS user_onboarding_progress_eligible_idx
  ON user_onboarding_progress (signup_completed_at)
  WHERE signup_completed_at IS NOT NULL AND completed_at IS NULL;

COMMIT;
