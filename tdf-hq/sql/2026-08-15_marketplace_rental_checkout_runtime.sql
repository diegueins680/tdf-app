BEGIN;

CREATE EXTENSION IF NOT EXISTS btree_gist;

CREATE TABLE IF NOT EXISTS marketplace_rental_listing_terms (
  listing_id UUID PRIMARY KEY REFERENCES marketplace_listing(id) ON DELETE RESTRICT,
  daily_rate_usd_cents BIGINT NOT NULL CHECK (daily_rate_usd_cents BETWEEN 1 AND 2147483647),
  weekly_rate_usd_cents BIGINT CHECK (
    weekly_rate_usd_cents IS NULL OR
    (weekly_rate_usd_cents BETWEEN 1 AND 2147483647 AND weekly_rate_usd_cents <= daily_rate_usd_cents * 7)
  ),
  security_deposit_usd_cents BIGINT NOT NULL DEFAULT 0 CHECK (security_deposit_usd_cents BETWEEN 0 AND 2147483647),
  late_fee_usd_cents BIGINT NOT NULL DEFAULT 0 CHECK (late_fee_usd_cents BETWEEN 0 AND 2147483647),
  min_days INTEGER NOT NULL DEFAULT 1 CHECK (min_days >= 1),
  max_days INTEGER NOT NULL DEFAULT 30 CHECK (max_days >= min_days AND max_days <= 366),
  cancellation_window_hours INTEGER NOT NULL DEFAULT 24 CHECK (cancellation_window_hours BETWEEN 0 AND 8760),
  timezone TEXT NOT NULL DEFAULT 'America/Guayaquil'
    CHECK (timezone = 'America/Guayaquil'),
  terms_version TEXT NOT NULL CHECK (length(btrim(terms_version)) BETWEEN 1 AND 80),
  terms_summary TEXT NOT NULL CHECK (length(btrim(terms_summary)) BETWEEN 1 AND 1000),
  active BOOLEAN NOT NULL DEFAULT FALSE,
  approved_at TIMESTAMPTZ,
  approved_by TEXT CHECK (approved_by IS NULL OR length(btrim(approved_by)) BETWEEN 1 AND 160),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (NOT active OR approved_at IS NOT NULL),
  CHECK (updated_at >= created_at)
);

CREATE TABLE IF NOT EXISTS marketplace_rental_cart_selection (
  cart_item_id UUID PRIMARY KEY REFERENCES marketplace_cart_item(id) ON DELETE CASCADE,
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  duration_days INTEGER NOT NULL CHECK (duration_days >= 1 AND duration_days <= 366),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (end_date >= start_date),
  CHECK (duration_days = (end_date - start_date) + 1),
  CHECK (updated_at >= created_at)
);

CREATE TABLE IF NOT EXISTS marketplace_rental_listing_terms_history (
  id BIGSERIAL PRIMARY KEY,
  listing_id UUID NOT NULL REFERENCES marketplace_listing(id) ON DELETE RESTRICT,
  daily_rate_usd_cents BIGINT NOT NULL,
  weekly_rate_usd_cents BIGINT,
  security_deposit_usd_cents BIGINT NOT NULL,
  late_fee_usd_cents BIGINT NOT NULL,
  min_days INTEGER NOT NULL,
  max_days INTEGER NOT NULL,
  cancellation_window_hours INTEGER NOT NULL,
  timezone TEXT NOT NULL,
  terms_version TEXT NOT NULL,
  terms_summary TEXT NOT NULL,
  active BOOLEAN NOT NULL,
  approved_at TIMESTAMPTZ,
  approved_by TEXT,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  changed_by TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS marketplace_rental_order_runtime (
  order_id UUID PRIMARY KEY REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  listing_id UUID NOT NULL REFERENCES marketplace_listing(id) ON DELETE RESTRICT,
  asset_id UUID NOT NULL REFERENCES asset(id) ON DELETE RESTRICT,
  lookup_token_hash TEXT NOT NULL UNIQUE CHECK (lookup_token_hash ~ '^[0-9a-f]{64}$'),
  create_idempotency_key TEXT NOT NULL UNIQUE CHECK (length(create_idempotency_key) BETWEEN 16 AND 128),
  create_request_sha256 TEXT NOT NULL CHECK (create_request_sha256 ~ '^[0-9a-f]{64}$'),
  fulfillment_method TEXT NOT NULL CHECK (fulfillment_method IN ('pickup','local_delivery','shipping')),
  rental_status TEXT NOT NULL CHECK (rental_status IN (
    'on_hold','confirmed','ready_for_handoff','checked_out','return_due',
    'returned_pending_inspection','damage_review','deposit_refund_due','closed',
    'cancellation_requested','cancelled','no_show','lost','disputed','expired'
  )),
  deposit_status TEXT NOT NULL CHECK (deposit_status IN (
    'awaiting_payment','collected','inspection_pending','deduction_proposed',
    'refund_due','partial_refund_due','refunded','partially_refunded','forfeited','disputed'
  )),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  duration_days INTEGER NOT NULL CHECK (duration_days >= 1 AND duration_days <= 366),
  timezone TEXT NOT NULL CHECK (timezone = 'America/Guayaquil'),
  daily_rate_usd_cents BIGINT NOT NULL CHECK (daily_rate_usd_cents > 0),
  weekly_rate_usd_cents BIGINT CHECK (weekly_rate_usd_cents IS NULL OR weekly_rate_usd_cents > 0),
  rental_charge_usd_cents BIGINT NOT NULL CHECK (rental_charge_usd_cents > 0),
  security_deposit_usd_cents BIGINT NOT NULL CHECK (security_deposit_usd_cents >= 0),
  deposit_deduction_usd_cents BIGINT NOT NULL DEFAULT 0
    CHECK (deposit_deduction_usd_cents >= 0 AND deposit_deduction_usd_cents <= security_deposit_usd_cents),
  late_fee_usd_cents BIGINT NOT NULL CHECK (late_fee_usd_cents >= 0),
  terms_version TEXT NOT NULL CHECK (length(btrim(terms_version)) BETWEEN 1 AND 80),
  terms_accepted_at TIMESTAMPTZ NOT NULL,
  identity_document_type TEXT NOT NULL CHECK (identity_document_type IN ('cedula','passport','ruc')),
  identity_document_last4 TEXT NOT NULL CHECK (identity_document_last4 ~ '^[A-Za-z0-9]{2,4}$'),
  recipient_name TEXT NOT NULL CHECK (length(btrim(recipient_name)) BETWEEN 1 AND 160),
  recipient_phone TEXT CHECK (recipient_phone IS NULL OR recipient_phone ~ '^\+[0-9]{8,15}$'),
  address_line_1 TEXT CHECK (address_line_1 IS NULL OR length(btrim(address_line_1)) BETWEEN 1 AND 200),
  address_line_2 TEXT CHECK (address_line_2 IS NULL OR length(btrim(address_line_2)) BETWEEN 1 AND 200),
  city TEXT CHECK (city IS NULL OR length(btrim(city)) BETWEEN 1 AND 200),
  province TEXT CHECK (province IS NULL OR length(btrim(province)) BETWEEN 1 AND 200),
  postal_code TEXT CHECK (postal_code IS NULL OR length(btrim(postal_code)) BETWEEN 1 AND 40),
  country_code TEXT CHECK (country_code IS NULL OR country_code ~ '^[A-Z]{2}$'),
  condition_out TEXT CHECK (condition_out IS NULL OR length(btrim(condition_out)) BETWEEN 1 AND 1000),
  condition_in TEXT CHECK (condition_in IS NULL OR length(btrim(condition_in)) BETWEEN 1 AND 1000),
  evidence_url TEXT CHECK (evidence_url IS NULL OR length(btrim(evidence_url)) BETWEEN 1 AND 2048),
  hold_expires_at TIMESTAMPTZ NOT NULL,
  checked_out_at TIMESTAMPTZ,
  returned_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (end_date >= start_date),
  CHECK (duration_days = (end_date - start_date) + 1),
  CHECK (
    fulfillment_method = 'pickup'
    OR (address_line_1 IS NOT NULL AND city IS NOT NULL AND province IS NOT NULL AND country_code IS NOT NULL)
  ),
  CHECK (rental_status <> 'checked_out' OR (checked_out_at IS NOT NULL AND condition_out IS NOT NULL)),
  CHECK (rental_status <> 'returned_pending_inspection' OR (returned_at IS NOT NULL AND condition_in IS NOT NULL)),
  CHECK (hold_expires_at > created_at),
  CHECK (updated_at >= created_at),
  EXCLUDE USING gist (
    asset_id WITH =,
    daterange(start_date, end_date, '[]') WITH &&
  ) WHERE (rental_status IN (
    'on_hold','confirmed','ready_for_handoff','checked_out','return_due',
    'returned_pending_inspection','damage_review','deposit_refund_due','lost','disputed'
  ))
);

CREATE TABLE IF NOT EXISTS marketplace_rental_event (
  id BIGSERIAL PRIMARY KEY,
  order_id UUID NOT NULL REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','operator','provider','customer')),
  actor_id TEXT CHECK (actor_id IS NULL OR length(btrim(actor_id)) BETWEEN 1 AND 160),
  reason_code TEXT CHECK (reason_code IS NULL OR length(btrim(reason_code)) BETWEEN 1 AND 80),
  notes TEXT CHECK (notes IS NULL OR length(btrim(notes)) BETWEEN 1 AND 1000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_marketplace_rental_terms_active
  ON marketplace_rental_listing_terms(active, listing_id);
CREATE INDEX IF NOT EXISTS idx_marketplace_rental_terms_history
  ON marketplace_rental_listing_terms_history(listing_id, changed_at, id);
CREATE INDEX IF NOT EXISTS idx_marketplace_rental_runtime_status
  ON marketplace_rental_order_runtime(rental_status, start_date, end_date);
CREATE INDEX IF NOT EXISTS idx_marketplace_rental_event_order
  ON marketplace_rental_event(order_id, created_at, id);

CREATE OR REPLACE VIEW marketplace_order_checkout_runtime AS
SELECT
  order_id, checkout_id, lookup_token_hash, create_idempotency_key,
  create_request_sha256, fulfillment_method, fulfillment_status AS domain_status,
  hold_expires_at, tracking_reference, 'sale'::text AS order_kind
FROM marketplace_sale_order_runtime
UNION ALL
SELECT
  order_id, checkout_id, lookup_token_hash, create_idempotency_key,
  create_request_sha256, fulfillment_method, rental_status AS domain_status,
  hold_expires_at, NULL::text AS tracking_reference, 'rental'::text AS order_kind
FROM marketplace_rental_order_runtime;

CREATE OR REPLACE FUNCTION marketplace_validate_rental_terms()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE listing_purpose TEXT;
BEGIN
  SELECT purpose INTO listing_purpose FROM marketplace_listing WHERE id = NEW.listing_id;
  IF listing_purpose IS NULL OR lower(btrim(listing_purpose)) <> 'rent' THEN
    RAISE EXCEPTION 'Rental terms can only be attached to a rent listing';
  END IF;
  IF TG_OP = 'UPDATE'
     AND ROW(
       NEW.daily_rate_usd_cents, NEW.weekly_rate_usd_cents,
       NEW.security_deposit_usd_cents, NEW.late_fee_usd_cents,
       NEW.min_days, NEW.max_days, NEW.cancellation_window_hours,
       NEW.timezone, NEW.terms_summary
     ) IS DISTINCT FROM ROW(
       OLD.daily_rate_usd_cents, OLD.weekly_rate_usd_cents,
       OLD.security_deposit_usd_cents, OLD.late_fee_usd_cents,
       OLD.min_days, OLD.max_days, OLD.cancellation_window_hours,
       OLD.timezone, OLD.terms_summary
     )
     AND NEW.terms_version = OLD.terms_version THEN
    RAISE EXCEPTION 'Commercial rental terms require a new terms_version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_terms ON marketplace_rental_listing_terms;
CREATE TRIGGER trg_marketplace_validate_rental_terms
  BEFORE INSERT OR UPDATE ON marketplace_rental_listing_terms
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_rental_terms();

CREATE OR REPLACE FUNCTION marketplace_record_rental_terms_history()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO marketplace_rental_listing_terms_history(
    listing_id, daily_rate_usd_cents, weekly_rate_usd_cents,
    security_deposit_usd_cents, late_fee_usd_cents, min_days, max_days,
    cancellation_window_hours, timezone, terms_version, terms_summary,
    active, approved_at, approved_by, changed_by
  ) VALUES (
    NEW.listing_id, NEW.daily_rate_usd_cents, NEW.weekly_rate_usd_cents,
    NEW.security_deposit_usd_cents, NEW.late_fee_usd_cents, NEW.min_days, NEW.max_days,
    NEW.cancellation_window_hours, NEW.timezone, NEW.terms_version, NEW.terms_summary,
    NEW.active, NEW.approved_at, NEW.approved_by,
    COALESCE(NULLIF(current_setting('tdf.actor_id', TRUE), ''), NEW.approved_by, 'system')
  );
  RETURN NULL;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_record_rental_terms_history ON marketplace_rental_listing_terms;
CREATE TRIGGER trg_marketplace_record_rental_terms_history
  AFTER INSERT OR UPDATE ON marketplace_rental_listing_terms
  FOR EACH ROW EXECUTE FUNCTION marketplace_record_rental_terms_history();

CREATE OR REPLACE FUNCTION marketplace_validate_rental_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout commerce_checkout_session%ROWTYPE;
DECLARE rental_order marketplace_order%ROWTYPE;
DECLARE listed_asset UUID;
BEGIN
  IF EXISTS (SELECT 1 FROM marketplace_sale_order_runtime WHERE order_id = NEW.order_id) THEN
    RAISE EXCEPTION 'Marketplace order cannot be both a sale and a rental';
  END IF;
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO rental_order FROM marketplace_order WHERE id = NEW.order_id;
  SELECT asset_id INTO listed_asset FROM marketplace_listing WHERE id = NEW.listing_id;
  IF checkout.id IS NULL OR rental_order.id IS NULL OR listed_asset IS NULL THEN
    RAISE EXCEPTION 'Marketplace rental references are incomplete';
  END IF;
  IF checkout.domain_type <> 'marketplace_rental'
     OR checkout.domain_order_id <> NEW.order_id::text
     OR checkout.total_minor <> rental_order.total_usd_cents
     OR checkout.currency <> rental_order.currency
     OR listed_asset <> NEW.asset_id
     OR checkout.total_minor <> NEW.rental_charge_usd_cents + NEW.security_deposit_usd_cents THEN
    RAISE EXCEPTION 'Marketplace rental does not match its immutable checkout, order, asset, or price';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_runtime ON marketplace_rental_order_runtime;
CREATE TRIGGER trg_marketplace_validate_rental_runtime
  BEFORE INSERT OR UPDATE OF order_id, checkout_id, listing_id, asset_id,
    rental_charge_usd_cents, security_deposit_usd_cents
  ON marketplace_rental_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_rental_runtime();

CREATE OR REPLACE FUNCTION marketplace_rental_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN LANGUAGE sql IMMUTABLE AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('on_hold','confirmed'), ('on_hold','cancelled'), ('on_hold','expired'),
    ('confirmed','ready_for_handoff'), ('confirmed','cancellation_requested'), ('confirmed','no_show'),
    ('ready_for_handoff','checked_out'), ('ready_for_handoff','cancellation_requested'),
    ('ready_for_handoff','no_show'), ('checked_out','return_due'),
    ('checked_out','returned_pending_inspection'), ('checked_out','lost'), ('checked_out','disputed'),
    ('return_due','returned_pending_inspection'), ('return_due','lost'), ('return_due','disputed'),
    ('returned_pending_inspection','deposit_refund_due'),
    ('returned_pending_inspection','damage_review'), ('damage_review','deposit_refund_due'),
    ('damage_review','disputed'), ('deposit_refund_due','closed'),
    ('cancellation_requested','cancelled'), ('no_show','cancelled'), ('lost','disputed'),
    ('disputed','damage_review'), ('disputed','deposit_refund_due'), ('disputed','closed')
  );
$$;

CREATE OR REPLACE FUNCTION marketplace_validate_rental_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout_status TEXT;
BEGIN
  IF NOT marketplace_rental_transition_allowed(OLD.rental_status, NEW.rental_status) THEN
    RAISE EXCEPTION 'Invalid marketplace rental transition: % -> %', OLD.rental_status, NEW.rental_status;
  END IF;
  SELECT status INTO checkout_status FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  IF NEW.rental_status IN (
      'confirmed','ready_for_handoff','checked_out','return_due','returned_pending_inspection',
      'damage_review','deposit_refund_due','closed','lost','disputed'
    ) AND checkout_status NOT IN ('paid','partially_refunded','refunded','disputed','chargeback') THEN
    RAISE EXCEPTION 'Rental custody cannot advance without verified payment evidence';
  END IF;
  IF NEW.rental_status = 'checked_out' AND (NEW.condition_out IS NULL OR NEW.checked_out_at IS NULL) THEN
    RAISE EXCEPTION 'Rental handoff requires an outbound condition report';
  END IF;
  IF NEW.rental_status = 'returned_pending_inspection'
     AND (NEW.condition_in IS NULL OR NEW.returned_at IS NULL) THEN
    RAISE EXCEPTION 'Rental return requires an inbound condition report';
  END IF;
  IF NEW.rental_status = 'closed'
     AND NEW.security_deposit_usd_cents > 0
     AND NEW.deposit_status NOT IN ('refunded','partially_refunded','forfeited') THEN
    RAISE EXCEPTION 'Rental cannot close before deposit settlement reaches a verified terminal state';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_transition ON marketplace_rental_order_runtime;
CREATE TRIGGER trg_marketplace_validate_rental_transition
  BEFORE UPDATE OF rental_status ON marketplace_rental_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_rental_transition();

CREATE OR REPLACE FUNCTION marketplace_record_rental_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.rental_status IS DISTINCT FROM NEW.rental_status THEN
    INSERT INTO marketplace_rental_event(
      order_id, from_status, to_status, actor_type, actor_id, reason_code, notes
    ) VALUES (
      NEW.order_id, OLD.rental_status, NEW.rental_status,
      COALESCE(NULLIF(current_setting('tdf.actor_type', TRUE), ''), 'system'),
      NULLIF(current_setting('tdf.actor_id', TRUE), ''),
      NULLIF(current_setting('tdf.reason_code', TRUE), ''),
      NULLIF(current_setting('tdf.notes', TRUE), '')
    );
  END IF;
  IF NEW.rental_status IN ('cancelled','expired') THEN
    UPDATE commerce_reservation_hold SET status = 'released'
      WHERE checkout_id = NEW.checkout_id AND status = 'active';
  ELSIF NEW.rental_status = 'checked_out' THEN
    UPDATE asset SET status = 'Booked' WHERE id = NEW.asset_id;
  ELSIF NEW.rental_status = 'returned_pending_inspection' THEN
    UPDATE asset SET status = 'Active' WHERE id = NEW.asset_id;
  END IF;
  IF NEW.rental_status = 'returned_pending_inspection' THEN
    NEW.deposit_status := 'inspection_pending';
  ELSIF NEW.rental_status = 'damage_review' THEN
    NEW.deposit_status := 'deduction_proposed';
  ELSIF NEW.rental_status = 'deposit_refund_due' THEN
    NEW.deposit_status := CASE WHEN NEW.deposit_deduction_usd_cents = 0
      THEN 'refund_due' ELSE 'partial_refund_due' END;
  ELSIF NEW.rental_status = 'disputed' THEN
    NEW.deposit_status := 'disputed';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_record_rental_transition ON marketplace_rental_order_runtime;
CREATE TRIGGER trg_marketplace_record_rental_transition
  BEFORE UPDATE OF rental_status ON marketplace_rental_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_record_rental_transition();

CREATE OR REPLACE FUNCTION marketplace_sync_verified_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime_order UUID;
BEGIN
  IF NEW.domain_type NOT IN ('marketplace_sale','marketplace_rental') OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT order_id INTO runtime_order FROM marketplace_order_checkout_runtime WHERE checkout_id = NEW.id;
  IF runtime_order IS NULL THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    UPDATE commerce_reservation_hold SET status = 'consumed'
      WHERE checkout_id = NEW.id AND status = 'active';
    UPDATE marketplace_order
      SET status = 'paid', paid_at = COALESCE(paid_at, NEW.paid_at), updated_at = NOW()
      WHERE id = runtime_order;
    IF NEW.domain_type = 'marketplace_sale' THEN
      UPDATE marketplace_sale_order_runtime SET fulfillment_status = 'ready_to_fulfill'
        WHERE order_id = runtime_order AND fulfillment_status = 'on_hold';
    ELSE
      UPDATE marketplace_rental_order_runtime
        SET rental_status = 'confirmed', deposit_status = 'collected'
        WHERE order_id = runtime_order AND rental_status = 'on_hold';
    END IF;
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE marketplace_order SET status = NEW.status, updated_at = NOW()
      WHERE id = runtime_order AND paid_at IS NULL;
    IF NEW.domain_type = 'marketplace_sale' THEN
      UPDATE marketplace_sale_order_runtime SET fulfillment_status = NEW.status
        WHERE order_id = runtime_order AND fulfillment_status = 'on_hold';
    ELSE
      UPDATE marketplace_rental_order_runtime SET rental_status = NEW.status
        WHERE order_id = runtime_order AND rental_status = 'on_hold';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION marketplace_protect_canonical_payment_state()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE canonical_status TEXT;
BEGIN
  IF NEW.status = 'paid' AND OLD.status <> 'paid' AND EXISTS (
    SELECT 1 FROM marketplace_order_checkout_runtime WHERE order_id = NEW.id
  ) THEN
    SELECT checkout.status INTO canonical_status
      FROM marketplace_order_checkout_runtime runtime
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
      WHERE domain_type IN ('marketplace_sale','marketplace_rental')
        AND status IN ('holding','awaiting_payment','processing','failed')
        AND expires_at <= at_time
      RETURNING id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

INSERT INTO marketplace_rental_listing_terms(
  listing_id, daily_rate_usd_cents, weekly_rate_usd_cents,
  security_deposit_usd_cents, late_fee_usd_cents, min_days, max_days,
  cancellation_window_hours, timezone, terms_version, terms_summary, active,
  approved_at, approved_by
)
SELECT
  listing.id, listing.price_usd_cents, listing.price_usd_cents * 6,
  0, listing.price_usd_cents, 1, 30, 24, 'America/Guayaquil',
  'marketplace-rental-v1',
  'Tarifa diaria publicada; tarifa semanal de seis días. Sin depósito inicial. La entrega y devolución requieren informes de condición.',
  TRUE,
  NOW(),
  'system:marketplace-rental-rollout'
FROM marketplace_listing listing
WHERE lower(btrim(listing.purpose)) = 'rent'
ON CONFLICT (listing_id) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.marketplace_sales', TRUE, 'production',
   'Enabled by approved marketplace sales rollout; provider-specific and emergency kill switches remain authoritative'),
  ('commerce.marketplace_rentals', TRUE, 'production',
   'Enabled for approved rental listings with date, terms, deposit, availability, custody, and return controls')
ON CONFLICT (flag_key, environment) DO UPDATE
SET enabled = EXCLUDED.enabled, reason = EXCLUDED.reason, updated_at = NOW();

COMMIT;
