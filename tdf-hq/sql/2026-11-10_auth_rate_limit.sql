-- Per-account throttling of login failures, signup and password-reset
-- requests (AUTH-CREDENTIAL-INTAKE-001, AUTH-PASSWORD-RECOVERY-001).
-- subject_hash is a SHA-256 of scope and normalized identifier; no address
-- or credential is stored.
BEGIN;

CREATE TABLE IF NOT EXISTS auth_rate_limit (
  scope TEXT NOT NULL CHECK (scope IN ('login-failure', 'signup', 'password-reset')),
  subject_hash TEXT NOT NULL CHECK (subject_hash ~ '^[0-9a-f]{64}$'),
  window_started_at TIMESTAMPTZ NOT NULL,
  request_count INT NOT NULL CHECK (request_count > 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (scope, subject_hash, window_started_at)
);

CREATE INDEX IF NOT EXISTS auth_rate_limit_window_idx
  ON auth_rate_limit (window_started_at);

COMMIT;
