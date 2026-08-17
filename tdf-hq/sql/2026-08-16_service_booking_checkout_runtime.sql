-- Payable studio/DJ/service booking runtime.
--
-- Existing tentative bookings remain untouched. Only a booking created through
-- an approved policy and canonical checkout enters this runtime. Payment and
-- service fulfillment are intentionally separate state machines.
BEGIN;

CREATE EXTENSION IF NOT EXISTS btree_gist;

CREATE TABLE IF NOT EXISTS service_booking_commerce_policy (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  service_offering_id UUID NOT NULL REFERENCES service_offering(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL CHECK (length(btrim(policy_version)) BETWEEN 1 AND 80),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  rate_minor BIGINT NOT NULL CHECK (rate_minor > 0 AND rate_minor <= 2147483647),
  rate_unit_minutes INTEGER NOT NULL CHECK (rate_unit_minutes BETWEEN 1 AND 1440),
  tax_bps INTEGER NOT NULL CHECK (tax_bps BETWEEN 0 AND 10000),
  deposit_bps INTEGER NOT NULL DEFAULT 5000 CHECK (deposit_bps BETWEEN 1 AND 10000),
  hold_minutes INTEGER NOT NULL DEFAULT 15 CHECK (hold_minutes BETWEEN 5 AND 60),
  min_duration_minutes INTEGER NOT NULL CHECK (min_duration_minutes > 0),
  max_duration_minutes INTEGER NOT NULL CHECK (max_duration_minutes >= min_duration_minutes),
  duration_step_minutes INTEGER NOT NULL CHECK (duration_step_minutes > 0),
  cancellation_window_hours INTEGER NOT NULL DEFAULT 24
    CHECK (cancellation_window_hours BETWEEN 0 AND 8760),
  timezone TEXT NOT NULL DEFAULT 'America/Guayaquil',
  terms_version TEXT NOT NULL CHECK (length(btrim(terms_version)) BETWEEN 1 AND 80),
  terms_summary TEXT NOT NULL CHECK (length(btrim(terms_summary)) BETWEEN 1 AND 2000),
  approval_status TEXT NOT NULL DEFAULT 'draft'
    CHECK (approval_status IN ('draft','approved','retired')),
  active BOOLEAN NOT NULL DEFAULT FALSE,
  approved_at TIMESTAMPTZ,
  approved_by TEXT,
  effective_from TIMESTAMPTZ,
  effective_until TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (service_offering_id, policy_version),
  CHECK (max_duration_minutes <= 1440),
  CHECK (min_duration_minutes % duration_step_minutes = 0),
  CHECK (max_duration_minutes % duration_step_minutes = 0),
  CHECK (active = FALSE OR (approval_status = 'approved' AND approved_at IS NOT NULL AND approved_by IS NOT NULL)),
  CHECK (effective_until IS NULL OR effective_from IS NULL OR effective_until > effective_from)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_service_booking_active_policy
  ON service_booking_commerce_policy(service_offering_id)
  WHERE active;

CREATE TABLE IF NOT EXISTS service_booking_commerce_policy_history (
  id BIGSERIAL PRIMARY KEY,
  policy_id UUID NOT NULL,
  service_offering_id UUID NOT NULL,
  policy_version TEXT NOT NULL,
  snapshot JSONB NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  changed_by TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS service_booking_checkout_runtime (
  booking_id BIGINT PRIMARY KEY REFERENCES booking(id) ON DELETE RESTRICT,
  service_order_id BIGINT NOT NULL UNIQUE REFERENCES service_order(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  service_offering_id UUID NOT NULL REFERENCES service_offering(id) ON DELETE RESTRICT,
  policy_id UUID NOT NULL REFERENCES service_booking_commerce_policy(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL,
  lookup_token_hash TEXT NOT NULL UNIQUE CHECK (lookup_token_hash ~ '^[0-9a-f]{64}$'),
  create_idempotency_key TEXT NOT NULL UNIQUE CHECK (length(create_idempotency_key) BETWEEN 16 AND 128),
  create_request_sha256 TEXT NOT NULL CHECK (create_request_sha256 ~ '^[0-9a-f]{64}$'),
  fulfillment_status TEXT NOT NULL CHECK (fulfillment_status IN (
    'on_hold','confirmed','scheduled','in_progress','balance_due','completed',
    'reschedule_requested','cancellation_requested','cancelled','no_show',
    'overtime_review','disputed','expired'
  )),
  deposit_status TEXT NOT NULL CHECK (deposit_status IN (
    'awaiting_payment','processing','paid','partially_refunded','refunded','disputed','chargeback'
  )),
  balance_status TEXT NOT NULL CHECK (balance_status IN (
    'not_due','due','awaiting_payment','processing','paid','partially_refunded',
    'refunded','waived','written_off','disputed','chargeback'
  )),
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  timezone TEXT NOT NULL,
  duration_minutes INTEGER NOT NULL CHECK (duration_minutes > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  rate_minor BIGINT NOT NULL CHECK (rate_minor > 0),
  rate_unit_minutes INTEGER NOT NULL CHECK (rate_unit_minutes > 0),
  tax_bps INTEGER NOT NULL CHECK (tax_bps BETWEEN 0 AND 10000),
  deposit_bps INTEGER NOT NULL CHECK (deposit_bps BETWEEN 1 AND 10000),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor > 0),
  tax_minor BIGINT NOT NULL CHECK (tax_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor = subtotal_minor + tax_minor),
  deposit_minor BIGINT NOT NULL CHECK (deposit_minor > 0 AND deposit_minor <= total_minor),
  balance_minor BIGINT NOT NULL CHECK (balance_minor >= 0 AND balance_minor = total_minor - deposit_minor),
  terms_version TEXT NOT NULL,
  terms_accepted_at TIMESTAMPTZ NOT NULL,
  hold_expires_at TIMESTAMPTZ NOT NULL,
  confirmed_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (ends_at > starts_at),
  CHECK (duration_minutes = extract(epoch FROM (ends_at - starts_at))::INTEGER / 60),
  CHECK (hold_expires_at > created_at),
  CHECK (fulfillment_status <> 'completed' OR completed_at IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS service_booking_resource_allocation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  booking_id BIGINT NOT NULL REFERENCES booking(id) ON DELETE RESTRICT,
  resource_id BIGINT NOT NULL REFERENCES resource(id) ON DELETE RESTRICT,
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  allocation_status TEXT NOT NULL CHECK (allocation_status IN (
    'holding','reserved','released','cancelled','completed'
  )),
  hold_expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (booking_id, resource_id),
  CHECK (ends_at > starts_at),
  EXCLUDE USING gist (
    resource_id WITH =,
    tstzrange(starts_at, ends_at, '[)') WITH &&
  ) WHERE (allocation_status IN ('holding','reserved'))
);

CREATE TABLE IF NOT EXISTS service_booking_event (
  id BIGSERIAL PRIMARY KEY,
  booking_id BIGINT NOT NULL REFERENCES service_booking_checkout_runtime(booking_id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','operator','provider','customer')),
  actor_id TEXT,
  reason_code TEXT,
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_service_booking_policy_active
  ON service_booking_commerce_policy(active, service_offering_id);
CREATE INDEX IF NOT EXISTS idx_service_booking_runtime_status
  ON service_booking_checkout_runtime(fulfillment_status, starts_at, ends_at);
CREATE INDEX IF NOT EXISTS idx_service_booking_allocation_window
  ON service_booking_resource_allocation(resource_id, starts_at, ends_at);
CREATE INDEX IF NOT EXISTS idx_service_booking_event_booking
  ON service_booking_event(booking_id, created_at, id);

CREATE OR REPLACE FUNCTION service_booking_validate_policy()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.active AND NOT EXISTS (
    SELECT 1 FROM service_offering offering
    WHERE offering.id = NEW.service_offering_id
      AND offering.active
      AND offering.deprecated_at IS NULL
      AND offering.legacy_service_catalog_id IS NOT NULL
  ) THEN
    RAISE EXCEPTION 'Active booking policy requires an active service offering linked to its legacy service order catalog';
  END IF;
  IF NEW.min_duration_minutes % NEW.rate_unit_minutes <> 0
     OR NEW.max_duration_minutes % NEW.rate_unit_minutes <> 0 THEN
    RAISE EXCEPTION 'Booking duration bounds must align to the billing unit';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.approval_status = 'approved'
     AND ROW(
       NEW.service_offering_id, NEW.policy_version, NEW.currency, NEW.rate_minor,
       NEW.rate_unit_minutes, NEW.tax_bps, NEW.deposit_bps, NEW.hold_minutes,
       NEW.min_duration_minutes, NEW.max_duration_minutes, NEW.duration_step_minutes,
       NEW.cancellation_window_hours, NEW.timezone, NEW.terms_version, NEW.terms_summary
     ) IS DISTINCT FROM ROW(
       OLD.service_offering_id, OLD.policy_version, OLD.currency, OLD.rate_minor,
       OLD.rate_unit_minutes, OLD.tax_bps, OLD.deposit_bps, OLD.hold_minutes,
       OLD.min_duration_minutes, OLD.max_duration_minutes, OLD.duration_step_minutes,
       OLD.cancellation_window_hours, OLD.timezone, OLD.terms_version, OLD.terms_summary
     ) THEN
    RAISE EXCEPTION 'Approved booking policy terms are immutable; create a new version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_validate_policy ON service_booking_commerce_policy;
CREATE TRIGGER trg_service_booking_validate_policy
  BEFORE INSERT OR UPDATE ON service_booking_commerce_policy
  FOR EACH ROW EXECUTE FUNCTION service_booking_validate_policy();

CREATE OR REPLACE FUNCTION service_booking_record_policy_history()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO service_booking_commerce_policy_history(
    policy_id, service_offering_id, policy_version, snapshot, changed_by
  ) VALUES (
    NEW.id, NEW.service_offering_id, NEW.policy_version, to_jsonb(NEW),
    COALESCE(NULLIF(current_setting('tdf.actor_id', TRUE), ''), NEW.approved_by, 'system')
  );
  RETURN NULL;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_policy_history ON service_booking_commerce_policy;
CREATE TRIGGER trg_service_booking_policy_history
  AFTER INSERT OR UPDATE ON service_booking_commerce_policy
  FOR EACH ROW EXECUTE FUNCTION service_booking_record_policy_history();

CREATE OR REPLACE FUNCTION service_booking_validate_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout commerce_checkout_session%ROWTYPE;
DECLARE booked booking%ROWTYPE;
DECLARE service service_order%ROWTYPE;
DECLARE policy service_booking_commerce_policy%ROWTYPE;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO booked FROM booking WHERE id = NEW.booking_id;
  SELECT * INTO service FROM service_order WHERE id = NEW.service_order_id;
  SELECT * INTO policy FROM service_booking_commerce_policy WHERE id = NEW.policy_id;
  IF checkout.id IS NULL OR booked.id IS NULL OR service.id IS NULL OR policy.id IS NULL THEN
    RAISE EXCEPTION 'Service booking runtime references are incomplete';
  END IF;
  IF checkout.domain_type <> 'service_booking'
     OR checkout.domain_order_id <> NEW.booking_id::text
     OR checkout.total_minor <> NEW.deposit_minor
     OR checkout.currency <> NEW.currency
     OR booked.service_order_id IS DISTINCT FROM NEW.service_order_id
     OR booked.service_offering_id IS DISTINCT FROM NEW.service_offering_id
     OR service.service_offering_id IS DISTINCT FROM NEW.service_offering_id
     OR service.price_quoted_cents::BIGINT IS DISTINCT FROM NEW.total_minor
     OR policy.service_offering_id <> NEW.service_offering_id
     OR policy.policy_version <> NEW.policy_version THEN
    RAISE EXCEPTION 'Service booking runtime does not match its checkout, booking, service order, policy, or price';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_validate_runtime ON service_booking_checkout_runtime;
CREATE TRIGGER trg_service_booking_validate_runtime
  BEFORE INSERT OR UPDATE OF booking_id, service_order_id, checkout_id,
    service_offering_id, policy_id, policy_version, currency, total_minor, deposit_minor
  ON service_booking_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION service_booking_validate_runtime();

CREATE OR REPLACE FUNCTION service_booking_transition_allowed(from_status TEXT, to_status TEXT)
RETURNS BOOLEAN LANGUAGE sql IMMUTABLE AS $$
  SELECT from_status = to_status OR (from_status, to_status) IN (
    ('on_hold','confirmed'), ('on_hold','cancelled'), ('on_hold','expired'),
    ('confirmed','scheduled'), ('confirmed','reschedule_requested'),
    ('confirmed','cancellation_requested'), ('confirmed','no_show'),
    ('scheduled','in_progress'), ('scheduled','reschedule_requested'),
    ('scheduled','cancellation_requested'), ('scheduled','no_show'),
    ('in_progress','balance_due'), ('in_progress','overtime_review'),
    ('in_progress','disputed'), ('overtime_review','balance_due'),
    ('overtime_review','disputed'), ('balance_due','completed'),
    ('balance_due','disputed'), ('reschedule_requested','confirmed'),
    ('reschedule_requested','cancellation_requested'),
    ('cancellation_requested','cancelled'), ('no_show','balance_due'),
    ('no_show','cancelled'), ('disputed','balance_due'), ('disputed','cancelled')
  );
$$;

CREATE OR REPLACE FUNCTION service_booking_validate_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout_status TEXT;
BEGIN
  IF NOT service_booking_transition_allowed(OLD.fulfillment_status, NEW.fulfillment_status) THEN
    RAISE EXCEPTION 'Invalid service booking transition: % -> %', OLD.fulfillment_status, NEW.fulfillment_status;
  END IF;
  SELECT status INTO checkout_status FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  IF NEW.fulfillment_status IN (
      'confirmed','scheduled','in_progress','balance_due','completed','no_show',
      'overtime_review','disputed'
    ) AND checkout_status NOT IN ('paid','partially_refunded','refunded','disputed','chargeback') THEN
    RAISE EXCEPTION 'Service fulfillment cannot advance without verified deposit payment evidence';
  END IF;
  NEW.updated_at := NOW();
  IF NEW.fulfillment_status = 'confirmed' THEN
    NEW.confirmed_at := COALESCE(NEW.confirmed_at, NOW());
  END IF;
  IF NEW.fulfillment_status = 'completed' THEN
    NEW.completed_at := COALESCE(NEW.completed_at, NOW());
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_validate_transition ON service_booking_checkout_runtime;
CREATE TRIGGER trg_service_booking_validate_transition
  BEFORE UPDATE OF fulfillment_status ON service_booking_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION service_booking_validate_transition();

CREATE OR REPLACE FUNCTION service_booking_record_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.fulfillment_status IS DISTINCT FROM NEW.fulfillment_status THEN
    INSERT INTO service_booking_event(
      booking_id, from_status, to_status, actor_type, actor_id, reason_code, notes
    ) VALUES (
      NEW.booking_id, OLD.fulfillment_status, NEW.fulfillment_status,
      COALESCE(NULLIF(current_setting('tdf.actor_type', TRUE), ''), 'system'),
      NULLIF(current_setting('tdf.actor_id', TRUE), ''),
      NULLIF(current_setting('tdf.reason_code', TRUE), ''),
      NULLIF(current_setting('tdf.notes', TRUE), '')
    );
  END IF;
  IF NEW.fulfillment_status IN ('cancelled','expired') THEN
    UPDATE service_booking_resource_allocation
      SET allocation_status = 'released', updated_at = NOW()
      WHERE booking_id = NEW.booking_id AND allocation_status = 'holding';
  ELSIF NEW.fulfillment_status = 'confirmed' THEN
    UPDATE service_booking_resource_allocation
      SET allocation_status = 'reserved', updated_at = NOW()
      WHERE booking_id = NEW.booking_id AND allocation_status = 'holding';
  ELSIF NEW.fulfillment_status = 'completed' THEN
    UPDATE service_booking_resource_allocation
      SET allocation_status = 'completed', updated_at = NOW()
      WHERE booking_id = NEW.booking_id AND allocation_status = 'reserved';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_record_transition ON service_booking_checkout_runtime;
CREATE TRIGGER trg_service_booking_record_transition
  AFTER UPDATE OF fulfillment_status ON service_booking_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION service_booking_record_transition();

-- Every booking path, including the legacy tentative endpoint, writes through
-- the same exclusion-backed resource calendar. This closes the former
-- read-then-insert race between public and staff booking requests.
CREATE OR REPLACE FUNCTION service_booking_allocate_resource()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE booked booking%ROWTYPE;
DECLARE runtime service_booking_checkout_runtime%ROWTYPE;
DECLARE next_status TEXT;
DECLARE next_expiry TIMESTAMPTZ;
BEGIN
  SELECT * INTO booked FROM booking WHERE id = NEW.booking_id;
  IF booked.id IS NULL OR booked.status::text IN ('Cancelled','NoShow') THEN
    RETURN NEW;
  END IF;
  SELECT * INTO runtime FROM service_booking_checkout_runtime WHERE booking_id = NEW.booking_id;
  next_status := CASE WHEN runtime.booking_id IS NOT NULL
    AND runtime.fulfillment_status = 'on_hold' THEN 'holding' ELSE 'reserved' END;
  next_expiry := COALESCE(runtime.hold_expires_at, booked.ends_at);
  INSERT INTO service_booking_resource_allocation(
    booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at
  ) VALUES (
    NEW.booking_id, NEW.resource_id, booked.starts_at, booked.ends_at,
    next_status, next_expiry
  ) ON CONFLICT (booking_id, resource_id) DO NOTHING;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_allocate_resource ON booking_resource;
CREATE TRIGGER trg_service_booking_allocate_resource
  AFTER INSERT ON booking_resource
  FOR EACH ROW EXECUTE FUNCTION service_booking_allocate_resource();

CREATE OR REPLACE FUNCTION service_booking_sync_legacy_booking_allocation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status::text IN ('Cancelled','NoShow') AND OLD.status IS DISTINCT FROM NEW.status THEN
    UPDATE service_booking_resource_allocation
      SET allocation_status = 'released', updated_at = NOW()
      WHERE booking_id = NEW.id AND allocation_status IN ('holding','reserved');
  ELSIF NEW.status::text = 'Completed' AND OLD.status IS DISTINCT FROM NEW.status THEN
    UPDATE service_booking_resource_allocation
      SET allocation_status = 'completed', updated_at = NOW()
      WHERE booking_id = NEW.id AND allocation_status IN ('holding','reserved');
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_sync_legacy_allocation ON booking;
CREATE TRIGGER trg_service_booking_sync_legacy_allocation
  AFTER UPDATE OF status ON booking
  FOR EACH ROW EXECUTE FUNCTION service_booking_sync_legacy_booking_allocation();

INSERT INTO service_booking_resource_allocation(
  booking_id, resource_id, starts_at, ends_at, allocation_status, hold_expires_at
)
SELECT
  booked.id, relation.resource_id, booked.starts_at, booked.ends_at,
  CASE WHEN booked.status::text = 'Completed' OR booked.ends_at <= NOW()
    THEN 'completed' ELSE 'reserved' END,
  booked.ends_at
FROM booking booked
JOIN booking_resource relation ON relation.booking_id = booked.id
WHERE booked.status::text NOT IN ('Cancelled','NoShow')
ON CONFLICT (booking_id, resource_id) DO NOTHING;

CREATE OR REPLACE FUNCTION service_booking_sync_verified_checkout()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime_booking BIGINT;
BEGIN
  IF NEW.domain_type <> 'service_booking' OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT booking_id INTO runtime_booking
    FROM service_booking_checkout_runtime WHERE checkout_id = NEW.id;
  IF runtime_booking IS NULL THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    UPDATE service_booking_checkout_runtime
      SET fulfillment_status = 'confirmed', deposit_status = 'paid', updated_at = NOW()
      WHERE booking_id = runtime_booking AND fulfillment_status = 'on_hold';
    UPDATE booking SET status = 'Confirmed'
      WHERE id = runtime_booking AND status::text = 'Tentative';
    UPDATE service_order SET status = 'deposit_paid'
      WHERE id = (SELECT service_order_id FROM service_booking_checkout_runtime WHERE booking_id = runtime_booking)
        AND status = 'deposit_due';
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE service_booking_checkout_runtime
      SET fulfillment_status = NEW.status, updated_at = NOW()
      WHERE booking_id = runtime_booking AND fulfillment_status = 'on_hold';
    UPDATE booking SET status = 'Cancelled'
      WHERE id = runtime_booking AND status::text = 'Tentative';
    UPDATE service_order SET status = NEW.status
      WHERE id = (SELECT service_order_id FROM service_booking_checkout_runtime WHERE booking_id = runtime_booking)
        AND status = 'deposit_due';
  ELSIF NEW.status = 'processing' THEN
    UPDATE service_booking_checkout_runtime SET deposit_status = 'processing', updated_at = NOW()
      WHERE booking_id = runtime_booking AND deposit_status = 'awaiting_payment';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION service_booking_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'service_booking'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT EXISTS (
       SELECT 1
       FROM commerce_payment_attempt attempt
       JOIN commerce_provider_binding binding
         ON binding.payment_attempt_id = attempt.id
        AND binding.provider = attempt.provider
        AND binding.environment = attempt.environment
        AND binding.merchant_account_ref = attempt.merchant_account_ref
        AND binding.merchant_reference = NEW.domain_order_id
        AND binding.amount_minor = NEW.total_minor
        AND binding.currency = NEW.currency
       WHERE attempt.checkout_id = NEW.id
         AND attempt.status = 'succeeded'
         AND attempt.environment = NEW.environment
         AND attempt.amount_minor = NEW.total_minor
         AND attempt.currency = NEW.currency
         AND (
           (attempt.provider = 'datafast' AND binding.resource_type = 'checkout')
           OR (attempt.provider = 'paypal' AND binding.resource_type = 'capture')
           OR (attempt.provider = 'stripe' AND binding.resource_type = 'payment')
           OR
           (attempt.provider IN ('bank_transfer','cash','pos') AND EXISTS (
             SELECT 1 FROM commerce_manual_payment_evidence evidence
             WHERE evidence.checkout_id = NEW.id
               AND evidence.payment_attempt_id = attempt.id
               AND evidence.status = 'approved'
           ))
         )
     ) THEN
    RAISE EXCEPTION 'Service booking checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_service_booking_require_verified_payment ON commerce_checkout_session;
CREATE TRIGGER trg_service_booking_require_verified_payment
  BEFORE UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION service_booking_require_verified_payment();

DROP TRIGGER IF EXISTS trg_service_booking_sync_checkout ON commerce_checkout_session;
CREATE TRIGGER trg_service_booking_sync_checkout
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION service_booking_sync_verified_checkout();

CREATE OR REPLACE FUNCTION service_booking_expire_holds(at_time TIMESTAMPTZ DEFAULT NOW())
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE expired_count INTEGER;
BEGIN
  WITH expired AS (
    UPDATE commerce_checkout_session
      SET status = 'expired', updated_at = at_time
      WHERE domain_type = 'service_booking'
        AND status IN ('holding','awaiting_payment')
        AND expires_at <= at_time
      RETURNING id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

-- Preserve current catalog values as reviewable drafts. No policy is activated
-- by this migration: rate, tax, duration, terms, and production ownership must
-- be explicitly approved before public checkout can use it.
INSERT INTO service_booking_commerce_policy(
  service_offering_id, policy_version, currency, rate_minor, rate_unit_minutes,
  tax_bps, deposit_bps, hold_minutes, min_duration_minutes, max_duration_minutes,
  duration_step_minutes, cancellation_window_hours, timezone, terms_version,
  terms_summary, approval_status, active
)
SELECT
  offering.id,
  'catalog-v' || offering.version::text || '-booking-draft-v1',
  currency.code,
  offering.default_rate_cents,
  60,
  COALESCE(tax.rate_bps, 0),
  5000,
  15,
  60,
  840,
  60,
  24,
  'America/Guayaquil',
  'service-booking-terms-draft-v1',
  'Borrador migrado desde la tarifa publicada. Requiere aprobación operativa, fiscal y de cancelación antes de habilitar checkout.',
  'draft',
  FALSE
FROM service_offering offering
JOIN currency_reference currency ON currency.id = offering.currency_id AND currency.active
LEFT JOIN tax_rate_reference tax ON tax.id = offering.tax_rate_id AND tax.active
WHERE offering.active
  AND offering.deprecated_at IS NULL
  AND offering.legacy_service_catalog_id IS NOT NULL
  AND offering.default_rate_cents > 0
  AND offering.code IN (
    'band-recording','voice-recording','recording','audiovisual-live-recording',
    'podcast-recording','rehearsal','dj-booth-practice','event-production'
  )
ON CONFLICT (service_offering_id, policy_version) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.service_bookings', FALSE, 'production',
   'Requires approved active rate policies, Datafast/PayPal sandbox verification, cancellation ownership, and staged rollout')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
