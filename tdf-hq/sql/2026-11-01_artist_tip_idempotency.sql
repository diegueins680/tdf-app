-- PAY-TIP-001: request-level idempotency for public artist tips. A retried
-- POST /fans/artists/{artistId}/tips with the same Idempotency-Key and the same
-- request replays the existing tip instead of creating another pending row and
-- PaymentIntent; the same key with a different request is a conflict. Rows
-- created without a key keep today's behaviour (idempotency_key IS NULL).
BEGIN;
SET LOCAL lock_timeout = '10s';

ALTER TABLE artist_tip
  ADD COLUMN IF NOT EXISTS idempotency_key TEXT,
  ADD COLUMN IF NOT EXISTS request_sha256 TEXT;

ALTER TABLE artist_tip DROP CONSTRAINT IF EXISTS artist_tip_idempotency_shape;
ALTER TABLE artist_tip ADD CONSTRAINT artist_tip_idempotency_shape CHECK (
  (idempotency_key IS NULL AND request_sha256 IS NULL)
  OR (idempotency_key IS NOT NULL AND request_sha256 IS NOT NULL
      AND idempotency_key ~ '^[A-Za-z0-9_-]{1,128}$' AND request_sha256 ~ '^[0-9a-f]{64}$')
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_tip_idempotency
  ON artist_tip(artist_profile_id, idempotency_key)
  WHERE idempotency_key IS NOT NULL;

COMMIT;
