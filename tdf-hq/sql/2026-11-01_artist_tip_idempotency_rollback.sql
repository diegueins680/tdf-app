-- Rollback of 2026-11-01_artist_tip_idempotency. Dropping the columns discards
-- replay keys only; tips, amounts and PaymentIntent bindings are untouched.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP INDEX IF EXISTS uq_artist_tip_idempotency;
ALTER TABLE artist_tip DROP CONSTRAINT IF EXISTS artist_tip_idempotency_shape;
ALTER TABLE artist_tip
  DROP COLUMN IF EXISTS idempotency_key,
  DROP COLUMN IF EXISTS request_sha256;
COMMIT;
