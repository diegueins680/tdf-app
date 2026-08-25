-- Migration: Persist Stripe PaymentIntent ID on marketplace orders
-- Date: 2026-07-07
-- Description: Adds marketplace_order.stripe_payment_intent_id so the shared
--              Stripe webhook can mark marketplace orders paid/failed without
--              relying on untrusted redirect state.

BEGIN;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'marketplace_order'
          AND column_name = 'stripe_payment_intent_id'
    ) THEN
        ALTER TABLE marketplace_order ADD COLUMN stripe_payment_intent_id TEXT;
    END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_order_stripe_payment_intent
    ON marketplace_order(stripe_payment_intent_id)
    WHERE stripe_payment_intent_id IS NOT NULL;

COMMIT;
