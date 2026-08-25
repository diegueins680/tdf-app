-- Complete the production ticketing schema required by the current Persistent
-- models. The older 2026-05-24 enhancement script is intentionally not used:
-- it has non-idempotent indexes, timestamp/type drift, and status checks that
-- reject states used by the current handlers.
--
-- Apply before 2026-07-12_ticket_checkout_idempotency.sql while production
-- keeps RUN_MIGRATIONS=false.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

DO $preflight$
BEGIN
    IF to_regclass('public.social_event') IS NULL
       OR to_regclass('public.event_ticket_tier') IS NULL
       OR to_regclass('public.event_ticket_order') IS NULL
       OR to_regclass('public.event_ticket') IS NULL THEN
        RAISE EXCEPTION 'Cannot complete ticketing schema: base ticket tables are missing';
    END IF;

    IF (
        SELECT COUNT(*)
        FROM pg_catalog.pg_class AS c
        JOIN pg_catalog.pg_namespace AS n ON n.oid = c.relnamespace
        WHERE n.nspname = 'public'
          AND c.relkind IN ('r', 'p')
          AND c.relname IN (
              'promo_code',
              'promo_code_redemption',
              'ticket_refund_request',
              'ticket_transfer',
              'event_waitlist',
              'stripe_payment_intent',
              'stripe_webhook_event',
              'ticket_qr_code'
          )
    ) NOT IN (0, 8) THEN
        RAISE EXCEPTION 'Refusing ticketing migration: enhancement tables are partially present';
    END IF;
END
$preflight$;

CREATE TABLE IF NOT EXISTS public.promo_code (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT REFERENCES public.social_event(id),
    code VARCHAR NOT NULL,
    description VARCHAR,
    discount_type VARCHAR NOT NULL,
    discount_value BIGINT NOT NULL,
    currency VARCHAR NOT NULL DEFAULT 'USD',
    max_redemptions BIGINT,
    current_redemptions BIGINT NOT NULL DEFAULT 0,
    valid_from TIMESTAMPTZ,
    valid_until TIMESTAMPTZ,
    tier_ids VARCHAR,
    min_purchase_amount_cents BIGINT,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_by_party_id VARCHAR,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_promo_code UNIQUE (code)
);

CREATE TABLE IF NOT EXISTS public.promo_code_redemption (
    id BIGSERIAL PRIMARY KEY,
    promo_code_id BIGINT NOT NULL REFERENCES public.promo_code(id),
    order_id BIGINT NOT NULL REFERENCES public.event_ticket_order(id),
    discount_amount_cents BIGINT NOT NULL,
    redeemed_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS public.ticket_refund_request (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES public.event_ticket_order(id),
    requested_by_party_id VARCHAR,
    reason VARCHAR,
    amount_cents BIGINT NOT NULL,
    status VARCHAR NOT NULL DEFAULT 'pending',
    approved_by_party_id VARCHAR,
    approved_at TIMESTAMPTZ,
    rejection_reason VARCHAR,
    stripe_refund_id VARCHAR,
    processed_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS public.ticket_transfer (
    id BIGSERIAL PRIMARY KEY,
    ticket_id BIGINT NOT NULL REFERENCES public.event_ticket(id),
    from_party_id VARCHAR,
    to_party_id VARCHAR,
    to_email VARCHAR,
    to_name VARCHAR,
    status VARCHAR NOT NULL DEFAULT 'pending',
    transfer_code VARCHAR NOT NULL,
    message VARCHAR,
    expires_at TIMESTAMPTZ,
    accepted_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_ticket_transfer_code UNIQUE (transfer_code)
);

CREATE TABLE IF NOT EXISTS public.event_waitlist (
    id BIGSERIAL PRIMARY KEY,
    event_id BIGINT NOT NULL REFERENCES public.social_event(id),
    tier_id BIGINT REFERENCES public.event_ticket_tier(id),
    party_id VARCHAR,
    email VARCHAR NOT NULL,
    name VARCHAR,
    quantity BIGINT NOT NULL DEFAULT 1,
    status VARCHAR NOT NULL DEFAULT 'active',
    priority BIGINT NOT NULL DEFAULT 0,
    notified_at TIMESTAMPTZ,
    expires_at TIMESTAMPTZ,
    converted_order_id BIGINT REFERENCES public.event_ticket_order(id),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS public.stripe_payment_intent (
    id BIGSERIAL PRIMARY KEY,
    order_id BIGINT NOT NULL REFERENCES public.event_ticket_order(id),
    stripe_payment_intent_id VARCHAR NOT NULL,
    stripe_client_secret VARCHAR NOT NULL,
    amount_cents BIGINT NOT NULL,
    currency VARCHAR NOT NULL DEFAULT 'USD',
    status VARCHAR NOT NULL,
    metadata VARCHAR,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_stripe_payment_intent UNIQUE (stripe_payment_intent_id)
);

CREATE TABLE IF NOT EXISTS public.stripe_webhook_event (
    id BIGSERIAL PRIMARY KEY,
    stripe_event_id VARCHAR NOT NULL,
    event_type VARCHAR NOT NULL,
    payload VARCHAR NOT NULL,
    processed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_stripe_webhook_event UNIQUE (stripe_event_id)
);

CREATE TABLE IF NOT EXISTS public.ticket_qr_code (
    id BIGSERIAL PRIMARY KEY,
    ticket_id BIGINT NOT NULL REFERENCES public.event_ticket(id),
    qr_data VARCHAR NOT NULL,
    qr_image_url VARCHAR,
    generated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT unique_ticket_qr_code UNIQUE (ticket_id)
);

ALTER TABLE public.event_ticket_order
    ADD COLUMN IF NOT EXISTS stripe_payment_intent_id VARCHAR,
    ADD COLUMN IF NOT EXISTS promo_code_id BIGINT REFERENCES public.promo_code(id),
    ADD COLUMN IF NOT EXISTS original_amount_cents BIGINT,
    ADD COLUMN IF NOT EXISTS payment_method VARCHAR;

ALTER TABLE public.event_ticket
    ADD COLUMN IF NOT EXISTS current_holder_party_id VARCHAR,
    ADD COLUMN IF NOT EXISTS current_holder_email VARCHAR,
    ADD COLUMN IF NOT EXISTS current_holder_name VARCHAR,
    ADD COLUMN IF NOT EXISTS original_holder_party_id VARCHAR,
    ADD COLUMN IF NOT EXISTS transfer_history VARCHAR;

ALTER TABLE public.event_ticket_tier
    ADD COLUMN IF NOT EXISTS enable_waitlist BOOLEAN NOT NULL DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS allow_transfers BOOLEAN NOT NULL DEFAULT TRUE,
    ADD COLUMN IF NOT EXISTS refund_policy VARCHAR NOT NULL DEFAULT 'full',
    ADD COLUMN IF NOT EXISTS refund_deadline TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS idx_promo_code_event
    ON public.promo_code(event_id) WHERE event_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_promo_code_lookup
    ON public.promo_code(code, is_active);
CREATE INDEX IF NOT EXISTS idx_promo_redemption_order
    ON public.promo_code_redemption(order_id);
CREATE INDEX IF NOT EXISTS idx_refund_order
    ON public.ticket_refund_request(order_id);
CREATE INDEX IF NOT EXISTS idx_refund_status
    ON public.ticket_refund_request(status, created_at);
CREATE INDEX IF NOT EXISTS idx_transfer_ticket
    ON public.ticket_transfer(ticket_id);
CREATE INDEX IF NOT EXISTS idx_waitlist_event
    ON public.event_waitlist(event_id, status);
CREATE INDEX IF NOT EXISTS idx_ticket_order_promo
    ON public.event_ticket_order(promo_code_id) WHERE promo_code_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_ticket_current_holder
    ON public.event_ticket(current_holder_party_id) WHERE current_holder_party_id IS NOT NULL;

DO $verification$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM (
            VALUES
                ('event_ticket_order', 'stripe_payment_intent_id', 'character varying', 'YES'),
                ('event_ticket_order', 'promo_code_id', 'bigint', 'YES'),
                ('event_ticket_order', 'original_amount_cents', 'bigint', 'YES'),
                ('event_ticket_order', 'payment_method', 'character varying', 'YES'),
                ('event_ticket', 'current_holder_party_id', 'character varying', 'YES'),
                ('event_ticket', 'current_holder_email', 'character varying', 'YES'),
                ('event_ticket', 'current_holder_name', 'character varying', 'YES'),
                ('event_ticket', 'original_holder_party_id', 'character varying', 'YES'),
                ('event_ticket', 'transfer_history', 'character varying', 'YES'),
                ('event_ticket_tier', 'enable_waitlist', 'boolean', 'NO'),
                ('event_ticket_tier', 'allow_transfers', 'boolean', 'NO'),
                ('event_ticket_tier', 'refund_policy', 'character varying', 'NO'),
                ('event_ticket_tier', 'refund_deadline', 'timestamp with time zone', 'YES')
        ) AS expected(table_name, column_name, data_type, is_nullable)
        LEFT JOIN information_schema.columns AS actual
          ON actual.table_schema = 'public'
         AND actual.table_name = expected.table_name
         AND actual.column_name = expected.column_name
        WHERE actual.column_name IS NULL
           OR actual.data_type <> expected.data_type
           OR actual.is_nullable <> expected.is_nullable
    ) THEN
        RAISE EXCEPTION 'Ticketing base-table extensions do not match the runtime model';
    END IF;

    IF (
        SELECT COUNT(*) FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name IN (
              'promo_code',
              'promo_code_redemption',
              'ticket_refund_request',
              'ticket_transfer',
              'event_waitlist',
              'stripe_payment_intent',
              'stripe_webhook_event',
              'ticket_qr_code'
          )
    ) <> 8 THEN
        RAISE EXCEPTION 'Ticketing runtime tables are incomplete';
    END IF;

    IF NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.promo_code'::regclass
          AND conname = 'unique_promo_code' AND contype = 'u'
    ) OR NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.ticket_transfer'::regclass
          AND conname = 'unique_ticket_transfer_code' AND contype = 'u'
    ) OR NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.stripe_payment_intent'::regclass
          AND conname = 'unique_stripe_payment_intent' AND contype = 'u'
    ) OR NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.stripe_webhook_event'::regclass
          AND conname = 'unique_stripe_webhook_event' AND contype = 'u'
    ) OR NOT EXISTS (
        SELECT 1 FROM pg_catalog.pg_constraint
        WHERE conrelid = 'public.ticket_qr_code'::regclass
          AND conname = 'unique_ticket_qr_code' AND contype = 'u'
    ) THEN
        RAISE EXCEPTION 'A ticketing runtime uniqueness constraint is missing';
    END IF;
END
$verification$;

COMMIT;
