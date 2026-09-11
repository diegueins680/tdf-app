-- Persist versioned experiment assignment and one-shot exposure against the
-- authenticated Party. The rollout remains disabled by default in application
-- configuration; this table only supplies the durable measurement boundary.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE TABLE IF NOT EXISTS user_experiment_assignment (
  id BIGSERIAL PRIMARY KEY,
  party_id BIGINT NOT NULL REFERENCES party(id) ON DELETE CASCADE,
  experiment_id TEXT NOT NULL,
  experiment_version INTEGER NOT NULL,
  variant TEXT NOT NULL,
  assigned_at TIMESTAMPTZ NOT NULL,
  eligible_until TIMESTAMPTZ NOT NULL,
  exposed_at TIMESTAMPTZ NULL,
  CONSTRAINT user_experiment_assignment_identity_unique
    UNIQUE (party_id, experiment_id, experiment_version),
  CONSTRAINT user_experiment_assignment_id_check
    CHECK (experiment_id = 'single-feature-onboarding-v1'),
  CONSTRAINT user_experiment_assignment_version_check
    CHECK (experiment_version = 1),
  CONSTRAINT user_experiment_assignment_variant_check
    CHECK (variant IN ('control', 'treatment_singlefeature')),
  CONSTRAINT user_experiment_assignment_window_check
    CHECK (eligible_until >= assigned_at),
  CONSTRAINT user_experiment_assignment_exposure_check
    CHECK (exposed_at IS NULL OR (exposed_at >= assigned_at AND exposed_at <= eligible_until))
);

CREATE INDEX IF NOT EXISTS user_experiment_assignment_pending_exposure_idx
  ON user_experiment_assignment (experiment_id, experiment_version, eligible_until)
  WHERE exposed_at IS NULL;

COMMIT;
