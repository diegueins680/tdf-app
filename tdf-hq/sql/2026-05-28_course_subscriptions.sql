-- Migration: Course subscription columns
-- Date: 2026-05-28
-- Description: Adds the columns Phase 4 needs to model recurring courses:
--                * course.stripe_subscription_price_id — when set, this course
--                  is sold as a Stripe Subscription against the named Price.
--                * course_registration.stripe_subscription_id — the
--                  customer.subscription id we hold once a checkout completes.
--                * course_registration.subscription_status — mirror of Stripe's
--                  subscription status (active, past_due, canceled, ...).
--              Idempotent. Indexes match the lookups the webhook handler does.

BEGIN;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'course'
          AND column_name = 'stripe_subscription_price_id'
    ) THEN
        ALTER TABLE course ADD COLUMN stripe_subscription_price_id TEXT;
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'course_registration'
          AND column_name = 'stripe_subscription_id'
    ) THEN
        ALTER TABLE course_registration ADD COLUMN stripe_subscription_id TEXT;
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'course_registration'
          AND column_name = 'subscription_status'
    ) THEN
        ALTER TABLE course_registration ADD COLUMN subscription_status TEXT;
    END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_course_registration_stripe_subscription
    ON course_registration(stripe_subscription_id)
    WHERE stripe_subscription_id IS NOT NULL;

COMMIT;
