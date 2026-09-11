-- Bind new provider attempts to the canonical payment intent while preserving
-- every historical attempt. Legacy rows remain nullable until a separately
-- reviewed backfill can prove a unique intent.
\set ON_ERROR_STOP on
BEGIN;

ALTER TABLE commerce_payment_attempt
  ADD COLUMN IF NOT EXISTS payment_intent_id UUID;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conname = 'fk_commerce_payment_attempt_intent'
      AND conrelid = 'commerce_payment_attempt'::regclass
  ) THEN
    ALTER TABLE commerce_payment_attempt
      ADD CONSTRAINT fk_commerce_payment_attempt_intent
      FOREIGN KEY (payment_intent_id)
      REFERENCES commerce_payment_intent(id)
      ON DELETE RESTRICT
      NOT VALID;
  END IF;
END
$$;

ALTER TABLE commerce_payment_attempt
  VALIDATE CONSTRAINT fk_commerce_payment_attempt_intent;

CREATE INDEX IF NOT EXISTS idx_commerce_payment_attempt_intent
  ON commerce_payment_attempt(payment_intent_id, created_at, id)
  WHERE payment_intent_id IS NOT NULL;

COMMIT;
