BEGIN;

DO $$
BEGIN
  IF to_regclass('public.marketplace_rental_order_runtime') IS NOT NULL
     AND EXISTS (SELECT 1 FROM marketplace_rental_order_runtime) THEN
    RAISE EXCEPTION 'Cannot roll back marketplace rental runtime while rental orders exist';
  END IF;
END $$;

UPDATE revenue_feature_flag
SET enabled = FALSE,
    reason = 'Disabled by marketplace rental runtime rollback',
    updated_at = NOW()
WHERE flag_key IN ('commerce.marketplace_sales','commerce.marketplace_rentals')
  AND environment = 'production';

CREATE OR REPLACE FUNCTION marketplace_sync_verified_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime_order UUID;
BEGIN
  IF NEW.domain_type <> 'marketplace_sale' OR OLD.status = NEW.status THEN RETURN NEW; END IF;
  SELECT order_id INTO runtime_order FROM marketplace_sale_order_runtime WHERE checkout_id = NEW.id;
  IF runtime_order IS NULL THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    UPDATE commerce_reservation_hold SET status = 'consumed'
      WHERE checkout_id = NEW.id AND status = 'active';
    UPDATE marketplace_order
      SET status = 'paid', paid_at = COALESCE(paid_at, NEW.paid_at), updated_at = NOW()
      WHERE id = runtime_order;
    UPDATE marketplace_sale_order_runtime SET fulfillment_status = 'ready_to_fulfill'
      WHERE order_id = runtime_order AND fulfillment_status = 'on_hold';
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE marketplace_order SET status = NEW.status, updated_at = NOW()
      WHERE id = runtime_order AND paid_at IS NULL;
    UPDATE marketplace_sale_order_runtime SET fulfillment_status = NEW.status
      WHERE order_id = runtime_order AND fulfillment_status = 'on_hold';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION marketplace_protect_canonical_payment_state()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE canonical_status TEXT;
BEGIN
  IF NEW.status = 'paid' AND OLD.status <> 'paid' AND EXISTS (
    SELECT 1 FROM marketplace_sale_order_runtime WHERE order_id = NEW.id
  ) THEN
    SELECT checkout.status INTO canonical_status
      FROM marketplace_sale_order_runtime runtime
      JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id
      WHERE runtime.order_id = NEW.id;
    IF canonical_status <> 'paid' THEN
      RAISE EXCEPTION 'Canonical marketplace orders can only become paid through verified checkout evidence';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION marketplace_expire_sale_holds(at_time TIMESTAMPTZ DEFAULT NOW())
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE expired_count INTEGER;
BEGIN
  WITH expired AS (
    UPDATE commerce_checkout_session
      SET status = 'expired', updated_at = at_time
      WHERE domain_type = 'marketplace_sale'
        AND status IN ('holding','awaiting_payment','processing','failed')
        AND expires_at <= at_time
      RETURNING id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

DROP VIEW IF EXISTS marketplace_order_checkout_runtime;
DROP TRIGGER IF EXISTS trg_marketplace_record_rental_terms_history ON marketplace_rental_listing_terms;
DROP TRIGGER IF EXISTS trg_marketplace_record_rental_transition ON marketplace_rental_order_runtime;
DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_transition ON marketplace_rental_order_runtime;
DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_runtime ON marketplace_rental_order_runtime;
DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_terms ON marketplace_rental_listing_terms;
DROP FUNCTION IF EXISTS marketplace_record_rental_transition();
DROP FUNCTION IF EXISTS marketplace_validate_rental_transition();
DROP FUNCTION IF EXISTS marketplace_rental_transition_allowed(TEXT, TEXT);
DROP FUNCTION IF EXISTS marketplace_validate_rental_runtime();
DROP FUNCTION IF EXISTS marketplace_validate_rental_terms();
DROP FUNCTION IF EXISTS marketplace_record_rental_terms_history();
DROP TABLE IF EXISTS marketplace_rental_event;
DROP TABLE IF EXISTS marketplace_rental_order_runtime;
DROP TABLE IF EXISTS marketplace_rental_cart_selection;
DROP TABLE IF EXISTS marketplace_rental_listing_terms_history;
DROP TABLE IF EXISTS marketplace_rental_listing_terms;

COMMIT;
