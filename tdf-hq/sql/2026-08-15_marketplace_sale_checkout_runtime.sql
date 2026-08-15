BEGIN;

CREATE TABLE IF NOT EXISTS marketplace_sale_order_runtime (
  order_id UUID PRIMARY KEY REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  lookup_token_hash TEXT NOT NULL UNIQUE CHECK (lookup_token_hash ~ '^[0-9a-f]{64}$'),
  create_idempotency_key TEXT NOT NULL UNIQUE CHECK (length(create_idempotency_key) BETWEEN 16 AND 128),
  create_request_sha256 TEXT NOT NULL CHECK (create_request_sha256 ~ '^[0-9a-f]{64}$'),
  fulfillment_method TEXT NOT NULL CHECK (fulfillment_method IN ('pickup','local_delivery','shipping')),
  fulfillment_status TEXT NOT NULL CHECK (fulfillment_status IN (
    'on_hold','ready_to_fulfill','picking','ready_for_pickup','shipped','delivered',
    'cancellation_requested','cancelled','return_requested','return_authorized',
    'return_in_transit','returned','closed','expired'
  )),
  recipient_name TEXT NOT NULL CHECK (length(btrim(recipient_name)) BETWEEN 1 AND 160),
  recipient_phone TEXT CHECK (recipient_phone IS NULL OR recipient_phone ~ '^\+[0-9]{8,15}$'),
  address_line_1 TEXT CHECK (address_line_1 IS NULL OR length(btrim(address_line_1)) BETWEEN 1 AND 200),
  address_line_2 TEXT CHECK (address_line_2 IS NULL OR length(btrim(address_line_2)) BETWEEN 1 AND 200),
  city TEXT CHECK (city IS NULL OR length(btrim(city)) BETWEEN 1 AND 200),
  province TEXT CHECK (province IS NULL OR length(btrim(province)) BETWEEN 1 AND 200),
  postal_code TEXT CHECK (postal_code IS NULL OR length(btrim(postal_code)) BETWEEN 1 AND 40),
  country_code TEXT CHECK (country_code IS NULL OR country_code ~ '^[A-Z]{2}$'),
  carrier TEXT CHECK (carrier IS NULL OR length(btrim(carrier)) BETWEEN 1 AND 120),
  tracking_reference TEXT CHECK (tracking_reference IS NULL OR length(btrim(tracking_reference)) BETWEEN 1 AND 200),
  hold_expires_at TIMESTAMPTZ NOT NULL,
  delivered_at TIMESTAMPTZ,
  returned_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (
    fulfillment_method = 'pickup'
    OR (address_line_1 IS NOT NULL AND city IS NOT NULL AND province IS NOT NULL AND country_code IS NOT NULL)
  ),
  CHECK (fulfillment_method <> 'shipping' OR country_code IS NOT NULL),
  CHECK (fulfillment_status <> 'delivered' OR delivered_at IS NOT NULL),
  CHECK (fulfillment_status <> 'returned' OR returned_at IS NOT NULL),
  CHECK (hold_expires_at > created_at),
  CHECK (updated_at >= created_at)
);

CREATE TABLE IF NOT EXISTS marketplace_sale_fulfillment_event (
  id BIGSERIAL PRIMARY KEY,
  order_id UUID NOT NULL REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  from_status TEXT CHECK (from_status IS NULL OR from_status IN (
    'on_hold','ready_to_fulfill','picking','ready_for_pickup','shipped','delivered',
    'cancellation_requested','cancelled','return_requested','return_authorized',
    'return_in_transit','returned','closed','expired'
  )),
  to_status TEXT NOT NULL CHECK (to_status IN (
    'on_hold','ready_to_fulfill','picking','ready_for_pickup','shipped','delivered',
    'cancellation_requested','cancelled','return_requested','return_authorized',
    'return_in_transit','returned','closed','expired'
  )),
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','operator','provider','customer')),
  actor_id TEXT CHECK (actor_id IS NULL OR length(btrim(actor_id)) BETWEEN 1 AND 160),
  reason_code TEXT CHECK (reason_code IS NULL OR length(btrim(reason_code)) BETWEEN 1 AND 80),
  notes TEXT CHECK (notes IS NULL OR length(btrim(notes)) BETWEEN 1 AND 1000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_marketplace_sale_runtime_status
  ON marketplace_sale_order_runtime(fulfillment_status, updated_at);
CREATE INDEX IF NOT EXISTS idx_marketplace_sale_fulfillment_event_order
  ON marketplace_sale_fulfillment_event(order_id, created_at, id);

CREATE OR REPLACE FUNCTION marketplace_validate_sale_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  sale_order marketplace_order%ROWTYPE;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  IF checkout.id IS NULL THEN
    RAISE EXCEPTION 'Marketplace checkout % does not exist', NEW.checkout_id;
  END IF;
  SELECT * INTO sale_order FROM marketplace_order WHERE id = NEW.order_id;
  IF sale_order.id IS NULL THEN
    RAISE EXCEPTION 'Marketplace sale order % does not exist', NEW.order_id;
  END IF;
  IF checkout.domain_type <> 'marketplace_sale'
     OR checkout.domain_order_id <> NEW.order_id::text
     OR checkout.total_minor <> sale_order.total_usd_cents
     OR checkout.currency <> sale_order.currency THEN
    RAISE EXCEPTION 'Marketplace runtime does not match its immutable checkout and order';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_sale_runtime ON marketplace_sale_order_runtime;
CREATE TRIGGER trg_marketplace_validate_sale_runtime
  BEFORE INSERT OR UPDATE OF order_id, checkout_id
  ON marketplace_sale_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_sale_runtime();

CREATE OR REPLACE FUNCTION marketplace_fulfillment_transition_allowed(
  method TEXT, from_status TEXT, to_status TEXT
) RETURNS BOOLEAN LANGUAGE sql IMMUTABLE AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('on_hold','ready_to_fulfill'), ('on_hold','cancelled'), ('on_hold','expired'),
    ('ready_to_fulfill','picking'), ('ready_to_fulfill','cancellation_requested'),
    ('picking','cancellation_requested'), ('ready_for_pickup','delivered'),
    ('ready_for_pickup','cancellation_requested'), ('shipped','delivered'),
    ('cancellation_requested','cancelled'), ('delivered','return_requested'),
    ('delivered','closed'), ('return_requested','return_authorized'),
    ('return_requested','closed'), ('return_authorized','return_in_transit'),
    ('return_authorized','returned'), ('return_in_transit','returned'),
    ('returned','closed')
  ) OR (from_status = 'picking' AND to_status = 'ready_for_pickup' AND method = 'pickup')
    OR (from_status = 'picking' AND to_status = 'shipped' AND method IN ('local_delivery','shipping'));
$$;

CREATE OR REPLACE FUNCTION marketplace_validate_fulfillment_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout_status TEXT;
BEGIN
  IF NOT marketplace_fulfillment_transition_allowed(
    OLD.fulfillment_method, OLD.fulfillment_status, NEW.fulfillment_status
  ) THEN
    RAISE EXCEPTION 'Invalid marketplace fulfillment transition: % -> %',
      OLD.fulfillment_status, NEW.fulfillment_status;
  END IF;
  IF NEW.fulfillment_status NOT IN ('on_hold','cancelled','expired') THEN
    SELECT status INTO checkout_status FROM commerce_checkout_session WHERE id = NEW.checkout_id;
    IF NEW.fulfillment_status IN (
         'ready_to_fulfill','picking','ready_for_pickup','shipped','delivered'
       ) AND checkout_status NOT IN ('paid','partially_refunded') THEN
      RAISE EXCEPTION 'Marketplace outbound fulfillment requires a paid checkout without a full refund or dispute';
    ELSIF checkout_status NOT IN ('paid','partially_refunded','refunded','disputed','chargeback') THEN
      RAISE EXCEPTION 'Marketplace fulfillment cannot advance without a verified paid checkout';
    END IF;
  END IF;
  IF NEW.fulfillment_status = 'delivered' AND NEW.delivered_at IS NULL THEN
    NEW.delivered_at := NOW();
  END IF;
  IF NEW.fulfillment_status = 'returned' AND NEW.returned_at IS NULL THEN
    NEW.returned_at := NOW();
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_fulfillment_transition ON marketplace_sale_order_runtime;
CREATE TRIGGER trg_marketplace_validate_fulfillment_transition
  BEFORE UPDATE OF fulfillment_status ON marketplace_sale_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_fulfillment_transition();

CREATE OR REPLACE FUNCTION marketplace_record_fulfillment_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.fulfillment_status IS DISTINCT FROM NEW.fulfillment_status THEN
    INSERT INTO marketplace_sale_fulfillment_event(
      order_id, from_status, to_status, actor_type, actor_id, reason_code, notes
    ) VALUES (
      NEW.order_id, OLD.fulfillment_status, NEW.fulfillment_status,
      COALESCE(NULLIF(current_setting('tdf.actor_type', TRUE), ''), 'system'),
      NULLIF(current_setting('tdf.actor_id', TRUE), ''),
      NULLIF(current_setting('tdf.reason_code', TRUE), ''),
      NULLIF(current_setting('tdf.notes', TRUE), '')
    );
  END IF;
  IF NEW.fulfillment_status IN ('cancelled','expired') THEN
    UPDATE commerce_reservation_hold
      SET status = 'released'
      WHERE checkout_id = NEW.checkout_id AND status = 'active';
  ELSIF NEW.fulfillment_status = 'delivered' THEN
    UPDATE asset SET status = 'Sold'
      WHERE id IN (
        SELECT listing.asset_id
        FROM marketplace_order_item item
        JOIN marketplace_listing listing ON listing.id = item.listing_id
        WHERE item.order_id = NEW.order_id
      );
    UPDATE marketplace_listing SET active = FALSE, updated_at = NOW()
      WHERE asset_id IN (
        SELECT listing.asset_id
        FROM marketplace_order_item item
        JOIN marketplace_listing listing ON listing.id = item.listing_id
        WHERE item.order_id = NEW.order_id
      );
  ELSIF NEW.fulfillment_status = 'returned' THEN
    UPDATE asset SET status = 'Active'
      WHERE id IN (
        SELECT listing.asset_id
        FROM marketplace_order_item item
        JOIN marketplace_listing listing ON listing.id = item.listing_id
        WHERE item.order_id = NEW.order_id
      );
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_record_fulfillment_transition ON marketplace_sale_order_runtime;
CREATE TRIGGER trg_marketplace_record_fulfillment_transition
  AFTER UPDATE OF fulfillment_status ON marketplace_sale_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_record_fulfillment_transition();

CREATE OR REPLACE FUNCTION marketplace_sync_verified_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime_order UUID;
BEGIN
  IF NEW.domain_type <> 'marketplace_sale' OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT order_id INTO runtime_order
    FROM marketplace_sale_order_runtime WHERE checkout_id = NEW.id;
  IF runtime_order IS NULL THEN
    RETURN NEW;
  END IF;
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

DROP TRIGGER IF EXISTS trg_marketplace_sync_verified_checkout ON commerce_checkout_session;
CREATE TRIGGER trg_marketplace_sync_verified_checkout
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION marketplace_sync_verified_checkout();

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

DROP TRIGGER IF EXISTS trg_marketplace_protect_canonical_payment_state ON marketplace_order;
CREATE TRIGGER trg_marketplace_protect_canonical_payment_state
  BEFORE UPDATE OF status ON marketplace_order
  FOR EACH ROW EXECUTE FUNCTION marketplace_protect_canonical_payment_state();

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

CREATE OR REPLACE VIEW marketplace_sale_checkout_backfill_report AS
SELECT
  orders.id AS order_id,
  orders.status AS legacy_status,
  orders.payment_provider,
  orders.total_usd_cents,
  orders.currency,
  CASE
    WHEN runtime.order_id IS NOT NULL THEN 'linked'
    WHEN orders.paid_at IS NOT NULL OR orders.status = 'paid' THEN 'requires_payment_reconciliation'
    WHEN orders.status IN ('pending','contact','datafast_init','datafast_pending','paypal_pending','stripe_pending')
      THEN 'eligible_unpaid_manual_review'
    ELSE 'historical_terminal_manual_review'
  END AS backfill_disposition
FROM marketplace_order orders
LEFT JOIN marketplace_sale_order_runtime runtime ON runtime.order_id = orders.id;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.marketplace_sales', FALSE, 'production',
   'Requires marketplace sale migration, provider sandbox verification, reconciliation ownership, and staged rollout'),
  ('commerce.marketplace_rentals', FALSE, 'production',
   'Requires dated availability, deposits, custody, return, damage, and dispute operations')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
