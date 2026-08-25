-- Migration: Persist Stripe Customer ID on Party
-- Date: 2026-05-28
-- Description: Adds party.stripe_customer_id so the same Stripe Customer can be
--              reused across PaymentSheet sessions, saved cards, and future
--              subscription flows. Idempotent.

BEGIN;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'party' AND column_name = 'stripe_customer_id'
    ) THEN
        ALTER TABLE party ADD COLUMN stripe_customer_id TEXT;
    END IF;
END $$;

CREATE INDEX IF NOT EXISTS idx_party_stripe_customer_id
    ON party(stripe_customer_id)
    WHERE stripe_customer_id IS NOT NULL;

COMMIT;
