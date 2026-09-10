\set ON_ERROR_STOP on
BEGIN;

DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM commerce_payment_attempt
    WHERE payment_intent_id IS NOT NULL
  ) THEN
    RAISE EXCEPTION
      'Refusing payment-attempt binding rollback: canonical intent evidence exists';
  END IF;
END
$$;

DROP INDEX IF EXISTS idx_commerce_payment_attempt_intent;
ALTER TABLE commerce_payment_attempt
  DROP CONSTRAINT IF EXISTS fk_commerce_payment_attempt_intent;
ALTER TABLE commerce_payment_attempt
  DROP COLUMN IF EXISTS payment_intent_id;

COMMIT;
