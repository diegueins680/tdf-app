-- Migration: Artist tipping via Stripe Connect
-- Date: 2026-05-28
-- Description: Adds the schema Phase 5 needs to route tips from fans to
--              artists via Stripe Connect destination charges.
--
--              * artist_profile.stripe_account_id — the connected Stripe
--                account (acct_*) we transfer destination funds into.
--              * artist_tip — one row per attempted/completed tip, with the
--                originating PaymentIntent id so the webhook can flip status.
--                platform_fee_cents records the amount the platform retains
--                via application_fee_amount on the PaymentIntent.
--
--              Idempotent.

BEGIN;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name = 'artist_profile'
          AND column_name = 'stripe_account_id'
    ) THEN
        ALTER TABLE artist_profile ADD COLUMN stripe_account_id TEXT;
    END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_profile_stripe_account
    ON artist_profile(stripe_account_id)
    WHERE stripe_account_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS artist_tip (
    id BIGSERIAL PRIMARY KEY,
    artist_profile_id BIGINT NOT NULL REFERENCES artist_profile(id) ON DELETE CASCADE,
    tipper_party_id BIGINT REFERENCES party(id) ON DELETE SET NULL,
    tipper_email TEXT,
    tipper_name TEXT,
    amount_cents INTEGER NOT NULL CHECK (amount_cents > 0),
    currency TEXT NOT NULL,
    platform_fee_cents INTEGER NOT NULL CHECK (platform_fee_cents >= 0),
    stripe_payment_intent_id TEXT,
    status TEXT NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending', 'paid', 'failed', 'refunded')),
    message TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_artist_tip_artist_created
    ON artist_tip(artist_profile_id, created_at DESC);

CREATE UNIQUE INDEX IF NOT EXISTS uq_artist_tip_stripe_payment_intent
    ON artist_tip(stripe_payment_intent_id)
    WHERE stripe_payment_intent_id IS NOT NULL;

COMMIT;
