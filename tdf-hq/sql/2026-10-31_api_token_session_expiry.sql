-- Session expiry (companion to ID-SESSION-003): additive, fail-closed server-side expiry for interactive sessions.
-- Unit: UTC Unix seconds from the database clock. Validity is [issued, expires).
-- Interactive tokens (label 'password-login:%' / 'google-login:%') issued before
-- enforcement intentionally receive no window: the enforcing binary rejects them
-- and the user signs in once more. Never backfill a validity window for tokens
-- issued before enforcement. Service and custom-label tokens keep NULL and stay
-- valid until revoked, exactly as before.
BEGIN;
SET LOCAL lock_timeout='5s';
SET LOCAL search_path=public,pg_catalog;
ALTER TABLE api_token ADD COLUMN IF NOT EXISTS issued_at_epoch BIGINT;
ALTER TABLE api_token ADD COLUMN IF NOT EXISTS expires_at_epoch BIGINT;
DO $guard$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname='api_token_validity_window' AND conrelid='public.api_token'::regclass
  ) THEN
    -- Both columns NULL (unbounded) or both set to a well-formed window. The
    -- explicit NOT NULL terms matter: a half window would otherwise evaluate
    -- the comparison to NULL and slip past the CHECK.
    ALTER TABLE api_token ADD CONSTRAINT api_token_validity_window CHECK (
      (issued_at_epoch IS NULL AND expires_at_epoch IS NULL)
      OR (issued_at_epoch IS NOT NULL AND expires_at_epoch IS NOT NULL
          AND issued_at_epoch >= 0 AND expires_at_epoch > issued_at_epoch)
    );
  END IF;
END $guard$;
-- Supports future sweeps of expired rows; partial so unbounded service tokens
-- never enter it.
CREATE INDEX IF NOT EXISTS idx_api_token_expiry
  ON api_token(expires_at_epoch) WHERE expires_at_epoch IS NOT NULL;
COMMIT;
-- No down migration: both columns are nullable and ignored by earlier binaries,
-- so retaining them is compatible. Rolling back to a binary that predates
-- enforcement re-enables unbounded sessions; the release floor guards that.
