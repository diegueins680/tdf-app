-- Persist the mobile checkout key so network retries cannot reserve or charge twice.
-- Apply before the corresponding API release while RUN_MIGRATIONS=false.

BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '5min';

ALTER TABLE event_ticket_order
    ADD COLUMN IF NOT EXISTS checkout_idempotency_key VARCHAR;

DO $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM event_ticket_order
        WHERE checkout_idempotency_key IS NOT NULL
          AND buyer_party_id IS NULL
    ) THEN
        RAISE EXCEPTION
            'Cannot enforce ticket checkout idempotency: keyed orders without a buyer exist';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM event_ticket_order
        WHERE checkout_idempotency_key IS NOT NULL
        GROUP BY buyer_party_id, checkout_idempotency_key
        HAVING COUNT(*) > 1
    ) THEN
        RAISE EXCEPTION
            'Cannot enforce ticket checkout idempotency: duplicate buyer/key pairs exist';
    END IF;

    IF EXISTS (
        SELECT 1
        FROM event_ticket_order
        WHERE stripe_payment_intent_id IS NOT NULL
        GROUP BY stripe_payment_intent_id
        HAVING COUNT(*) > 1
    ) THEN
        RAISE EXCEPTION
            'Cannot enforce ticket PaymentIntent ownership: duplicate intent ids exist';
    END IF;
END $$;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conname = 'unique_event_ticket_checkout'
          AND conrelid = 'event_ticket_order'::regclass
    ) THEN
        ALTER TABLE event_ticket_order
            ADD CONSTRAINT unique_event_ticket_checkout
            UNIQUE (buyer_party_id, checkout_idempotency_key);
    END IF;
END
$$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_event_ticket_order_stripe_payment_intent
    ON event_ticket_order(stripe_payment_intent_id)
    WHERE stripe_payment_intent_id IS NOT NULL;

COMMIT;
