-- Migration: Allow only one active Stripe payment attempt per marketplace cart
-- Date: 2026-07-12
--
-- Apply this migration before deploying the matching application code. If the
-- guard below fails, use this preflight to find affected carts, then reconcile
-- them against Stripe before changing statuses; rows may represent live intents:
--
-- SELECT cart_id, COUNT(*), ARRAY_AGG(id ORDER BY created_at)
-- FROM marketplace_order
-- WHERE cart_id IS NOT NULL AND status = 'stripe_pending'
-- GROUP BY cart_id
-- HAVING COUNT(*) > 1;
--
-- Pre-idempotency rows also require reconciliation before rollout:
-- SELECT id, cart_id, created_at
-- FROM marketplace_order
-- WHERE status = 'stripe_pending'
--   AND stripe_payment_intent_id IS NULL;

BEGIN;

ALTER TABLE marketplace_order
    ADD COLUMN IF NOT EXISTS stripe_idempotency_key TEXT;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM marketplace_order
        WHERE status = 'stripe_pending'
          AND payment_provider IS DISTINCT FROM 'stripe'
    ) THEN
        RAISE EXCEPTION
            'Cannot enforce active Stripe payments: stripe_pending orders have a non-Stripe provider';
    END IF;
END $$;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM marketplace_order
        WHERE cart_id IS NOT NULL
          AND status = 'stripe_pending'
        GROUP BY cart_id
        HAVING COUNT(*) > 1
    ) THEN
        RAISE EXCEPTION
            'Cannot enforce one active Stripe payment per cart: duplicate stripe_pending orders exist';
    END IF;
END $$;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM marketplace_order
        WHERE status = 'stripe_pending'
          AND stripe_payment_intent_id IS NULL
          AND stripe_idempotency_key IS NULL
    ) THEN
        RAISE EXCEPTION
            'Cannot enable idempotent Stripe retries: reconcile unbound stripe_pending orders first';
    END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_cart_active_stripe_payment
    ON marketplace_order(cart_id)
    WHERE cart_id IS NOT NULL AND status = 'stripe_pending';

COMMIT;
