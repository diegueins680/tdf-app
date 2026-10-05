-- ID-SESSION-003: additive, fail-closed password recovery expiry.
-- Existing tokens intentionally receive no metadata and cannot recover accounts
-- on the enforcing binary. Never backfill a new validity window for old tokens.
-- Unit: UTC Unix seconds from the database clock. Validity is [issued, expires).
CREATE TABLE IF NOT EXISTS auth_recovery_challenge (
  api_token_id BIGINT PRIMARY KEY REFERENCES api_token(id) ON DELETE CASCADE,
  credential_id BIGINT NOT NULL REFERENCES user_credential(id) ON DELETE RESTRICT,
  issued_at_epoch BIGINT NOT NULL,
  expires_at_epoch BIGINT NOT NULL,
  CONSTRAINT auth_recovery_time_window CHECK (
    issued_at_epoch >= 0 AND expires_at_epoch > issued_at_epoch
    AND expires_at_epoch - issued_at_epoch = 900
  )
);
CREATE INDEX IF NOT EXISTS auth_recovery_credential_idx
  ON auth_recovery_challenge(credential_id);
-- No down migration: retaining this table is compatible and preserves binding
-- evidence. Old binaries ignore expiry; recovery must retain the enforcing code.
