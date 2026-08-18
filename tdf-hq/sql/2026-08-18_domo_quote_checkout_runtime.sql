-- Authoritative Domo quotes, atomic date holds, and deposit checkout linkage.
-- The existing historical rate card remains inactive until its independent
-- finance/operations/tax/legal comparison is approved.
BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

UPDATE commerce_product_version
SET pricing_rules = pricing_rules || jsonb_build_object(
      'max_guests', 220,
      'max_duration_hours', 24,
      'max_setup_hours', 12
    ),
    policy_snapshot = policy_snapshot || jsonb_build_object(
      'terms_version', 'domo-terms-legacy-draft-v1',
      'quote_expiry_minutes', checkout_hold_minutes,
      'timezone', 'America/Guayaquil',
      'minimum_lead_hours', 24,
      'maximum_advance_days', 730
    )
WHERE domain_type = 'domo'
  AND source = 'client_legacy_snapshot'
  AND status = 'pending_approval';

CREATE TABLE domo_event_quote_runtime (
  id UUID PRIMARY KEY,
  quote_id UUID NOT NULL UNIQUE REFERENCES commerce_quote(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  product_version_id UUID NOT NULL REFERENCES commerce_product_version(id) ON DELETE RESTRICT,
  venue_key TEXT NOT NULL DEFAULT 'domo-del-pululahua',
  lookup_token_hash TEXT NOT NULL UNIQUE,
  create_idempotency_key TEXT NOT NULL UNIQUE,
  create_request_sha256 TEXT NOT NULL,
  customer_name TEXT NOT NULL,
  customer_email TEXT NOT NULL,
  customer_phone TEXT,
  event_type TEXT NOT NULL,
  guests INT NOT NULL CHECK (guests > 0),
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  setup_starts_at TIMESTAMPTZ NOT NULL,
  duration_hours INT NOT NULL CHECK (duration_hours > 0),
  setup_hours INT NOT NULL CHECK (setup_hours >= 0),
  catering BOOLEAN NOT NULL,
  production BOOLEAN NOT NULL,
  transport BOOLEAN NOT NULL,
  customer_notes TEXT,
  quote_status TEXT NOT NULL CHECK (quote_status IN (
    'draft','sent','viewed','accepted','deposit_due','deposit_paid',
    'in_progress','balance_due','completed','cancelled','expired'
  )),
  fulfillment_status TEXT NOT NULL CHECK (fulfillment_status IN (
    'date_held','date_reserved','in_progress','balance_due','completed','cancelled','expired'
  )),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor > 0),
  tax_minor BIGINT NOT NULL CHECK (tax_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor > 0),
  deposit_minor BIGINT NOT NULL CHECK (deposit_minor > 0),
  balance_minor BIGINT NOT NULL CHECK (balance_minor >= 0),
  tax_basis_points INT NOT NULL CHECK (tax_basis_points BETWEEN 0 AND 10000),
  deposit_basis_points INT NOT NULL CHECK (deposit_basis_points BETWEEN 1 AND 10000),
  rate_card_version TEXT NOT NULL,
  rate_card_rules_sha256 TEXT NOT NULL,
  timezone TEXT NOT NULL,
  terms_version TEXT NOT NULL,
  terms_accepted_at TIMESTAMPTZ,
  hold_expires_at TIMESTAMPTZ NOT NULL,
  deposit_paid_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (ends_at > starts_at),
  CHECK (setup_starts_at <= starts_at),
  CHECK (total_minor = subtotal_minor + tax_minor),
  CHECK (deposit_minor + balance_minor = total_minor),
  CHECK (quote_status NOT IN ('deposit_due','deposit_paid','in_progress','balance_due','completed')
    OR terms_accepted_at IS NOT NULL),
  CHECK (quote_status NOT IN ('deposit_paid','in_progress','balance_due','completed')
    OR deposit_paid_at IS NOT NULL)
);

CREATE INDEX idx_domo_event_quote_customer
  ON domo_event_quote_runtime(customer_email, created_at DESC);
CREATE INDEX idx_domo_event_quote_hold_expiry
  ON domo_event_quote_runtime(hold_expires_at)
  WHERE quote_status IN ('sent','viewed','accepted','deposit_due');

ALTER TABLE domo_event_quote_runtime
  ADD CONSTRAINT ex_domo_event_quote_active_window
  EXCLUDE USING gist (
    venue_key WITH =,
    tstzrange(setup_starts_at, ends_at, '[)') WITH &&
  ) WHERE (fulfillment_status IN ('date_held','date_reserved','in_progress','balance_due'));

CREATE TABLE domo_quote_rate_limit (
  scope TEXT NOT NULL,
  subject_hash TEXT NOT NULL,
  window_started_at TIMESTAMPTZ NOT NULL,
  request_count INT NOT NULL CHECK (request_count > 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY(scope, subject_hash, window_started_at)
);

CREATE TABLE domo_quote_state_event (
  id BIGSERIAL PRIMARY KEY,
  domo_quote_id UUID NOT NULL REFERENCES domo_event_quote_runtime(id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','customer','operator','provider')),
  reason_code TEXT NOT NULL,
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE UNIQUE INDEX uq_domo_quote_deposit_paid_event
  ON domo_quote_state_event(domo_quote_id, to_status)
  WHERE to_status = 'deposit_paid';

CREATE OR REPLACE FUNCTION domo_validate_quote_runtime_insert()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  quote commerce_quote%ROWTYPE;
  product commerce_product_version%ROWTYPE;
  line_total BIGINT;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO quote FROM commerce_quote WHERE id = NEW.quote_id;
  SELECT * INTO product FROM commerce_product_version WHERE id = NEW.product_version_id;
  SELECT COALESCE(sum(subtotal_minor), 0) INTO line_total
    FROM commerce_quote_line WHERE quote_id = NEW.quote_id;

  IF checkout.id IS NULL OR quote.id IS NULL OR product.id IS NULL THEN
    RAISE EXCEPTION 'Domo quote runtime references missing canonical records';
  END IF;
  IF checkout.domain_type <> 'domo_event_quote'
     OR checkout.domain_order_id <> 'domo-quote:' || NEW.id::text
     OR checkout.quote_id <> NEW.quote_id
     OR checkout.status <> 'holding'
     OR checkout.total_minor <> NEW.deposit_minor
     OR checkout.currency <> NEW.currency THEN
    RAISE EXCEPTION 'Domo quote runtime does not match the immutable deposit checkout';
  END IF;
  IF quote.domain_type <> 'domo_event_quote'
     OR quote.domain_subject_id <> NEW.id::text
     OR quote.version <> 1
     OR quote.status <> 'sent'
     OR quote.currency <> NEW.currency
     OR quote.subtotal_minor <> NEW.subtotal_minor
     OR quote.tax_minor <> NEW.tax_minor
     OR quote.total_minor <> NEW.total_minor
     OR quote.deposit_minor <> NEW.deposit_minor
     OR quote.expires_at <> NEW.hold_expires_at
     OR line_total <> NEW.subtotal_minor THEN
    RAISE EXCEPTION 'Domo quote runtime does not match the immutable full quote and lines';
  END IF;
  IF product.domain_type <> 'domo'
     OR product.status <> 'active'
     OR product.approved_by IS NULL
     OR product.approved_at IS NULL
     OR product.currency <> NEW.currency
     OR product.deposit_basis_points <> NEW.deposit_basis_points
     OR (product.pricing_rules->>'legacy_tax_basis_points')::int <> NEW.tax_basis_points
     OR product.product_key || '-v' || product.version::text <> NEW.rate_card_version
     OR encode(digest(convert_to(product.pricing_rules::text, 'UTF8'), 'sha256'), 'hex')
          <> NEW.rate_card_rules_sha256
     OR (product.policy_snapshot->>'timezone') IS DISTINCT FROM NEW.timezone
     OR (product.policy_snapshot->>'terms_version') IS DISTINCT FROM NEW.terms_version
     OR NOT EXISTS (
       SELECT 1 FROM commerce_rate_card_review review
       WHERE review.product_version_id = product.id
         AND review.domain_type = product.domain_type
         AND review.status = 'approved'
         AND review.reviewed_by IS NOT NULL
         AND review.reviewed_at IS NOT NULL
     ) THEN
    RAISE EXCEPTION 'Domo quote runtime requires the exact approved active rate card';
  END IF;
  IF NEW.tax_minor <> (NEW.subtotal_minor * NEW.tax_basis_points + 5000) / 10000
     OR NEW.deposit_minor <> (NEW.total_minor * NEW.deposit_basis_points + 5000) / 10000 THEN
    RAISE EXCEPTION 'Domo quote tax or deposit does not match the approved basis points';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_domo_validate_quote_runtime_insert
  BEFORE INSERT ON domo_event_quote_runtime
  FOR EACH ROW EXECUTE FUNCTION domo_validate_quote_runtime_insert();

CREATE OR REPLACE FUNCTION domo_validate_quote_runtime_update()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout_status TEXT;
BEGIN
  IF (
    OLD.id, OLD.quote_id, OLD.checkout_id, OLD.product_version_id, OLD.venue_key,
    OLD.lookup_token_hash, OLD.create_idempotency_key, OLD.create_request_sha256,
    OLD.customer_name, OLD.customer_email, OLD.customer_phone, OLD.event_type,
    OLD.guests, OLD.starts_at, OLD.ends_at, OLD.setup_starts_at,
    OLD.duration_hours, OLD.setup_hours, OLD.catering, OLD.production, OLD.transport,
    OLD.customer_notes, OLD.currency, OLD.subtotal_minor, OLD.tax_minor,
    OLD.total_minor, OLD.deposit_minor, OLD.balance_minor, OLD.tax_basis_points,
    OLD.deposit_basis_points, OLD.rate_card_version, OLD.rate_card_rules_sha256,
    OLD.timezone,
    OLD.terms_version, OLD.hold_expires_at, OLD.created_at
  ) IS DISTINCT FROM (
    NEW.id, NEW.quote_id, NEW.checkout_id, NEW.product_version_id, NEW.venue_key,
    NEW.lookup_token_hash, NEW.create_idempotency_key, NEW.create_request_sha256,
    NEW.customer_name, NEW.customer_email, NEW.customer_phone, NEW.event_type,
    NEW.guests, NEW.starts_at, NEW.ends_at, NEW.setup_starts_at,
    NEW.duration_hours, NEW.setup_hours, NEW.catering, NEW.production, NEW.transport,
    NEW.customer_notes, NEW.currency, NEW.subtotal_minor, NEW.tax_minor,
    NEW.total_minor, NEW.deposit_minor, NEW.balance_minor, NEW.tax_basis_points,
    NEW.deposit_basis_points, NEW.rate_card_version, NEW.rate_card_rules_sha256,
    NEW.timezone,
    NEW.terms_version, NEW.hold_expires_at, NEW.created_at
  ) THEN
    RAISE EXCEPTION 'Domo quote snapshots are immutable; create a versioned change order';
  END IF;

  IF OLD.quote_status <> NEW.quote_status AND NOT (
    (OLD.quote_status = 'draft' AND NEW.quote_status IN ('sent','cancelled')) OR
    (OLD.quote_status = 'sent' AND NEW.quote_status IN ('viewed','accepted','expired','cancelled')) OR
    (OLD.quote_status = 'viewed' AND NEW.quote_status IN ('accepted','expired','cancelled')) OR
    (OLD.quote_status = 'accepted' AND NEW.quote_status IN ('deposit_due','expired','cancelled')) OR
    (OLD.quote_status = 'deposit_due' AND NEW.quote_status IN ('deposit_paid','expired','cancelled')) OR
    (OLD.quote_status = 'deposit_paid' AND NEW.quote_status IN ('in_progress','cancelled')) OR
    (OLD.quote_status = 'in_progress' AND NEW.quote_status IN ('balance_due','cancelled')) OR
    (OLD.quote_status = 'balance_due' AND NEW.quote_status IN ('completed','cancelled'))
  ) THEN
    RAISE EXCEPTION 'Invalid Domo quote transition: % -> %', OLD.quote_status, NEW.quote_status;
  END IF;

  IF NEW.quote_status IN ('deposit_paid','in_progress','balance_due','completed')
     AND OLD.quote_status NOT IN ('deposit_paid','in_progress','balance_due','completed') THEN
    SELECT status INTO checkout_status
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id;
    IF checkout_status <> 'paid' THEN
      RAISE EXCEPTION 'Domo deposit confirmation requires a verified paid checkout';
    END IF;
  END IF;

  RETURN NEW;
END $$;

CREATE TRIGGER trg_domo_validate_quote_runtime_update
  BEFORE UPDATE ON domo_event_quote_runtime
  FOR EACH ROW EXECUTE FUNCTION domo_validate_quote_runtime_update();

CREATE OR REPLACE FUNCTION domo_quote_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'domo_event_quote'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT EXISTS (
       SELECT 1 FROM domo_event_quote_runtime runtime
       WHERE runtime.checkout_id = NEW.id
         AND runtime.quote_status = 'deposit_due'
         AND runtime.fulfillment_status = 'date_held'
         AND runtime.hold_expires_at > COALESCE(NEW.paid_at, NOW())
     ) THEN
    RAISE EXCEPTION 'Domo deposit checkout is not attached to an accepted active date hold';
  END IF;
  IF NEW.domain_type = 'domo_event_quote'
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
           OR (
             attempt.provider IN ('bank_transfer','cash','pos')
             AND binding.resource_type = 'manual_evidence'
             AND EXISTS (
               SELECT 1 FROM commerce_manual_payment_evidence evidence
               WHERE evidence.checkout_id = NEW.id
                 AND evidence.payment_attempt_id = attempt.id
                 AND evidence.status = 'approved'
                 AND evidence.submitted_by IS NOT NULL
                 AND evidence.reviewed_by IS NOT NULL
                 AND evidence.submitted_by <> evidence.reviewed_by
             )
           )
         )
     ) THEN
    RAISE EXCEPTION 'Domo deposit checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_domo_quote_require_verified_payment
  BEFORE UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION domo_quote_require_verified_payment();

CREATE OR REPLACE FUNCTION domo_quote_sync_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime_quote UUID;
BEGIN
  IF NEW.domain_type <> 'domo_event_quote' OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT id INTO runtime_quote
    FROM domo_event_quote_runtime WHERE checkout_id = NEW.id;
  IF runtime_quote IS NULL THEN RETURN NEW; END IF;

  IF NEW.status = 'paid' THEN
    UPDATE domo_event_quote_runtime
      SET quote_status = 'deposit_paid', fulfillment_status = 'date_reserved',
          deposit_paid_at = COALESCE(deposit_paid_at, NEW.paid_at, NOW()), updated_at = NOW()
      WHERE id = runtime_quote AND quote_status = 'deposit_due'
        AND fulfillment_status = 'date_held';
    UPDATE commerce_reservation_hold SET status = 'consumed'
      WHERE checkout_id = NEW.id AND status = 'active';
    INSERT INTO domo_quote_state_event(
      domo_quote_id, from_status, to_status, actor_type, reason_code, notes
    ) VALUES (
      runtime_quote, 'deposit_due', 'deposit_paid', 'provider', 'verified_deposit',
      'Deposit verified independently; event fulfillment remains date_reserved'
    ) ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_domo_quote_sync_verified_payment
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION domo_quote_sync_verified_payment();

CREATE OR REPLACE FUNCTION domo_lock_commerce_quote_snapshot()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.domain_type = 'domo_event_quote' AND (
    OLD.id, OLD.domain_type, OLD.domain_subject_id, OLD.version, OLD.currency,
    OLD.subtotal_minor, OLD.discount_minor, OLD.tax_minor, OLD.fee_minor,
    OLD.total_minor, OLD.deposit_minor, OLD.expires_at, OLD.created_by, OLD.created_at
  ) IS DISTINCT FROM (
    NEW.id, NEW.domain_type, NEW.domain_subject_id, NEW.version, NEW.currency,
    NEW.subtotal_minor, NEW.discount_minor, NEW.tax_minor, NEW.fee_minor,
    NEW.total_minor, NEW.deposit_minor, NEW.expires_at, NEW.created_by, NEW.created_at
  ) THEN
    RAISE EXCEPTION 'Domo commerce quote economics are immutable';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_domo_lock_commerce_quote_snapshot
  BEFORE UPDATE ON commerce_quote
  FOR EACH ROW EXECUTE FUNCTION domo_lock_commerce_quote_snapshot();

CREATE OR REPLACE FUNCTION domo_lock_commerce_quote_line()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM commerce_quote quote
    WHERE quote.id = OLD.quote_id AND quote.domain_type = 'domo_event_quote'
  ) THEN
    RAISE EXCEPTION 'Domo commerce quote lines are immutable';
  END IF;
  RETURN OLD;
END $$;

CREATE TRIGGER trg_domo_lock_commerce_quote_line
  BEFORE UPDATE OR DELETE ON commerce_quote_line
  FOR EACH ROW EXECUTE FUNCTION domo_lock_commerce_quote_line();

CREATE OR REPLACE FUNCTION domo_quote_expire_holds(
  as_of TIMESTAMPTZ DEFAULT NOW(),
  only_quote_id UUID DEFAULT NULL
) RETURNS INT LANGUAGE plpgsql AS $$
DECLARE
  expired_count INT;
BEGIN
  WITH expired AS (
    UPDATE domo_event_quote_runtime runtime
    SET quote_status = 'expired', fulfillment_status = 'expired', updated_at = as_of
    FROM commerce_checkout_session checkout
    WHERE checkout.id = runtime.checkout_id
      AND runtime.hold_expires_at <= as_of
      AND runtime.quote_status IN ('sent','viewed','accepted','deposit_due')
      AND checkout.status IN ('holding','awaiting_payment','processing','failed')
      AND (only_quote_id IS NULL OR runtime.id = only_quote_id)
    RETURNING runtime.id, runtime.quote_id, runtime.checkout_id
  ), quote_updates AS (
    UPDATE commerce_quote quote
    SET status = 'expired'
    FROM expired
    WHERE quote.id = expired.quote_id AND quote.status IN ('sent','viewed','accepted')
  ), checkout_updates AS (
    UPDATE commerce_checkout_session checkout
    SET status = 'expired', updated_at = as_of
    FROM expired
    WHERE checkout.id = expired.checkout_id
      AND checkout.status IN ('holding','awaiting_payment','processing','failed')
  ), hold_updates AS (
    UPDATE commerce_reservation_hold hold
    SET status = 'expired'
    FROM expired
    WHERE hold.checkout_id = expired.checkout_id AND hold.status = 'active'
  )
  INSERT INTO domo_quote_state_event(
    domo_quote_id, from_status, to_status, actor_type, reason_code, notes
  )
  SELECT id, NULL, 'expired', 'system', 'hold_expired',
    'Date hold expired without verified deposit payment'
  FROM expired;

  GET DIAGNOSTICS expired_count = ROW_COUNT;
  RETURN expired_count;
END $$;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.domo_quotes', FALSE, 'production',
   'Requires approved active Domo rate card, provider sandbox evidence, calendar ownership, contract review, alerts, and controlled rollout')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
