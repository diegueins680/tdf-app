-- Migration: Persist Stripe PaymentIntent ID on Course Registration
-- Date: 2026-05-28
-- Description: Adds course_registration.stripe_payment_intent_id so the
--              `payment_intent.succeeded` webhook can flip the registration
--              to "paid" by looking up the PI without scanning by metadata.
--              Unique partial index so each PI maps to at most one registration.

BEGIN;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'course_registration'
          AND column_name = 'stripe_payment_intent_id'
    ) THEN
        ALTER TABLE course_registration ADD COLUMN stripe_payment_intent_id TEXT;
    END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_course_registration_stripe_payment_intent
    ON course_registration(stripe_payment_intent_id)
    WHERE stripe_payment_intent_id IS NOT NULL;

COMMIT;
