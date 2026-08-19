-- Provider-neutral checkout and finance foundation.
-- Domain orders remain authoritative for fulfillment; checkout links by
-- (domain_type, domain_order_id) and stores an immutable financial snapshot.
BEGIN;

CREATE EXTENSION IF NOT EXISTS btree_gist;

CREATE TABLE IF NOT EXISTS commerce_quote (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  domain_type TEXT NOT NULL,
  domain_subject_id TEXT NOT NULL,
  version INT NOT NULL CHECK (version > 0),
  status TEXT NOT NULL CHECK (status IN ('draft','sent','viewed','accepted','expired','superseded','cancelled')),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor >= 0),
  discount_minor BIGINT NOT NULL DEFAULT 0 CHECK (discount_minor >= 0),
  tax_minor BIGINT NOT NULL DEFAULT 0 CHECK (tax_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor >= 0),
  deposit_minor BIGINT NOT NULL DEFAULT 0 CHECK (deposit_minor >= 0),
  accepted_terms_version TEXT,
  expires_at TIMESTAMPTZ NOT NULL,
  accepted_at TIMESTAMPTZ,
  created_by BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (domain_type, domain_subject_id, version),
  CHECK (total_minor = subtotal_minor - discount_minor + tax_minor + fee_minor),
  CHECK (deposit_minor <= total_minor)
);

CREATE TABLE IF NOT EXISTS commerce_quote_line (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  quote_id UUID NOT NULL REFERENCES commerce_quote(id) ON DELETE RESTRICT,
  line_number INT NOT NULL CHECK (line_number > 0),
  product_type TEXT NOT NULL,
  product_id TEXT NOT NULL,
  product_version TEXT NOT NULL,
  description TEXT NOT NULL,
  quantity INT NOT NULL CHECK (quantity > 0),
  unit_amount_minor BIGINT NOT NULL CHECK (unit_amount_minor >= 0),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor >= 0),
  tax_code TEXT,
  configuration JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (quote_id, line_number),
  CHECK (subtotal_minor = quantity::BIGINT * unit_amount_minor)
);

CREATE TABLE IF NOT EXISTS commerce_checkout_session (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  domain_type TEXT NOT NULL,
  domain_order_id TEXT NOT NULL,
  quote_id UUID REFERENCES commerce_quote(id) ON DELETE RESTRICT,
  status TEXT NOT NULL CHECK (status IN ('draft','validated','holding','awaiting_payment','processing','paid','failed','cancelled','expired','partially_refunded','refunded','disputed','chargeback')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor >= 0),
  discount_minor BIGINT NOT NULL DEFAULT 0 CHECK (discount_minor >= 0),
  tax_minor BIGINT NOT NULL DEFAULT 0 CHECK (tax_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor > 0),
  paid_minor BIGINT NOT NULL DEFAULT 0 CHECK (paid_minor >= 0),
  refunded_minor BIGINT NOT NULL DEFAULT 0 CHECK (refunded_minor >= 0),
  customer_email TEXT NOT NULL,
  customer_party_id BIGINT,
  lookup_token_hash TEXT NOT NULL UNIQUE,
  idempotency_key TEXT NOT NULL,
  expires_at TIMESTAMPTZ NOT NULL,
  paid_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (domain_type, idempotency_key),
  CHECK (total_minor = subtotal_minor - discount_minor + tax_minor + fee_minor),
  CHECK (paid_minor <= total_minor),
  CHECK (refunded_minor <= paid_minor)
);

CREATE INDEX IF NOT EXISTS idx_commerce_checkout_domain
  ON commerce_checkout_session(domain_type, domain_order_id);
CREATE INDEX IF NOT EXISTS idx_commerce_checkout_expiry
  ON commerce_checkout_session(expires_at)
  WHERE status IN ('holding','awaiting_payment','processing');

CREATE TABLE IF NOT EXISTS commerce_checkout_line_item (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  line_number INT NOT NULL CHECK (line_number > 0),
  product_type TEXT NOT NULL,
  product_id TEXT NOT NULL,
  product_version TEXT NOT NULL,
  description TEXT NOT NULL,
  quantity INT NOT NULL CHECK (quantity > 0),
  unit_amount_minor BIGINT NOT NULL CHECK (unit_amount_minor >= 0),
  subtotal_minor BIGINT NOT NULL CHECK (subtotal_minor >= 0),
  discount_minor BIGINT NOT NULL DEFAULT 0 CHECK (discount_minor >= 0),
  tax_minor BIGINT NOT NULL DEFAULT 0 CHECK (tax_minor >= 0),
  fee_minor BIGINT NOT NULL DEFAULT 0 CHECK (fee_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor >= 0),
  snapshot JSONB NOT NULL,
  UNIQUE (checkout_id, line_number),
  CHECK (subtotal_minor = quantity::BIGINT * unit_amount_minor),
  CHECK (total_minor = subtotal_minor - discount_minor + tax_minor + fee_minor)
);

CREATE TABLE IF NOT EXISTS commerce_payment_attempt (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  provider TEXT NOT NULL CHECK (provider IN ('datafast','paypal','stripe','bank_transfer','cash','pos','cardano')),
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  operation TEXT NOT NULL CHECK (operation IN ('create','authorize','capture','manual_verify')),
  status TEXT NOT NULL CHECK (status IN ('created','requires_customer_action','processing','succeeded','failed','cancelled','expired','requires_review')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  merchant_account_ref TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  failure_code TEXT,
  failure_summary TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, merchant_account_ref, operation, idempotency_key)
);

CREATE TABLE IF NOT EXISTS commerce_provider_binding (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider TEXT NOT NULL,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  merchant_account_ref TEXT NOT NULL,
  resource_type TEXT NOT NULL,
  provider_resource_id TEXT NOT NULL,
  provider_resource_path TEXT,
  merchant_reference TEXT NOT NULL,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, environment, merchant_account_ref, resource_type, provider_resource_id),
  UNIQUE (provider, environment, merchant_account_ref, resource_type, merchant_reference)
);

CREATE TABLE IF NOT EXISTS commerce_provider_event_inbox (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  provider TEXT NOT NULL,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','production')),
  merchant_account_ref TEXT NOT NULL,
  provider_event_id TEXT NOT NULL,
  event_type TEXT NOT NULL,
  signature_verified BOOLEAN NOT NULL,
  received_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  provider_created_at TIMESTAMPTZ,
  payload_ciphertext BYTEA NOT NULL,
  payload_sha256 TEXT NOT NULL,
  processing_status TEXT NOT NULL DEFAULT 'pending' CHECK (processing_status IN ('pending','processing','processed','retry','dead_letter','ignored')),
  attempt_count INT NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  next_attempt_at TIMESTAMPTZ,
  processed_at TIMESTAMPTZ,
  error_summary TEXT,
  UNIQUE (provider, environment, merchant_account_ref, provider_event_id),
  CHECK (environment <> 'production' OR signature_verified)
);

CREATE TABLE IF NOT EXISTS commerce_refund (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider_refund_id TEXT,
  status TEXT NOT NULL CHECK (status IN ('requested','approved','processing','succeeded','failed','cancelled')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  reason_code TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  requested_by BIGINT NOT NULL,
  approved_by BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMPTZ,
  UNIQUE (payment_attempt_id, idempotency_key),
  UNIQUE (provider_refund_id)
);

CREATE TABLE IF NOT EXISTS commerce_refund_allocation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  refund_id UUID NOT NULL REFERENCES commerce_refund(id) ON DELETE RESTRICT,
  line_item_id UUID NOT NULL REFERENCES commerce_checkout_line_item(id) ON DELETE RESTRICT,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  UNIQUE (refund_id, line_item_id)
);

CREATE TABLE IF NOT EXISTS commerce_dispute (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  provider_dispute_id TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('inquiry','dispute','chargeback')),
  status TEXT NOT NULL,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  reason_code TEXT,
  opened_at TIMESTAMPTZ NOT NULL,
  due_at TIMESTAMPTZ,
  closed_at TIMESTAMPTZ,
  UNIQUE (provider_dispute_id)
);

CREATE TABLE IF NOT EXISTS commerce_manual_payment_evidence (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  payment_attempt_id UUID NOT NULL REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT,
  evidence_object_key TEXT,
  customer_reference TEXT,
  submitted_amount_minor BIGINT,
  currency TEXT,
  status TEXT NOT NULL CHECK (status IN ('awaiting_evidence','submitted','under_review','approved','rejected')),
  submitted_at TIMESTAMPTZ,
  reviewed_by BIGINT,
  reviewed_at TIMESTAMPTZ,
  review_notes TEXT,
  CHECK (status <> 'approved' OR (
    reviewed_by IS NOT NULL
    AND reviewed_at IS NOT NULL
    AND submitted_amount_minor IS NOT NULL
    AND submitted_amount_minor > 0
    AND currency ~ '^[A-Z]{3}$'
  ))
);

CREATE TABLE IF NOT EXISTS commerce_receipt (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  receipt_number TEXT NOT NULL UNIQUE,
  kind TEXT NOT NULL CHECK (kind IN ('payment_receipt','credit_note','invoice_reference')),
  adapter TEXT NOT NULL,
  external_reference TEXT,
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  issued_at TIMESTAMPTZ NOT NULL,
  voided_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS commerce_ledger_transaction (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  transaction_type TEXT NOT NULL,
  source_type TEXT NOT NULL,
  source_id TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('draft','posted','reversed')),
  effective_at TIMESTAMPTZ NOT NULL,
  correlation_id TEXT NOT NULL,
  reversal_of UUID REFERENCES commerce_ledger_transaction(id) ON DELETE RESTRICT,
  created_by TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (source_type, source_id, transaction_type),
  CHECK (status <> 'reversed' OR reversal_of IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS commerce_ledger_entry (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  transaction_id UUID NOT NULL REFERENCES commerce_ledger_transaction(id) ON DELETE RESTRICT,
  account_code TEXT NOT NULL,
  party_id BIGINT,
  domain_type TEXT,
  domain_id TEXT,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  amount_minor BIGINT NOT NULL CHECK (amount_minor <> 0),
  memo TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_commerce_ledger_entry_account
  ON commerce_ledger_entry(account_code, currency, created_at);

CREATE TABLE IF NOT EXISTS commerce_reservation_hold (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  resource_type TEXT NOT NULL,
  resource_id TEXT NOT NULL,
  starts_at TIMESTAMPTZ,
  ends_at TIMESTAMPTZ,
  quantity INT NOT NULL CHECK (quantity > 0),
  status TEXT NOT NULL CHECK (status IN ('active','consumed','released','expired')),
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((starts_at IS NULL AND ends_at IS NULL) OR (starts_at IS NOT NULL AND ends_at > starts_at))
);
CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_active_hold
  ON commerce_reservation_hold(resource_type, resource_id, COALESCE(starts_at, '-infinity'::timestamptz), COALESCE(ends_at, 'infinity'::timestamptz))
  WHERE status = 'active';

ALTER TABLE commerce_reservation_hold
  ADD CONSTRAINT ex_commerce_active_hold_window
  EXCLUDE USING gist (
    resource_type WITH =,
    resource_id WITH =,
    tstzrange(
      COALESCE(starts_at, '-infinity'::timestamptz),
      COALESCE(ends_at, 'infinity'::timestamptz),
      '[)'
    ) WITH &&
  ) WHERE (status = 'active');

CREATE TABLE IF NOT EXISTS commerce_checkout_audit_event (
  id BIGSERIAL PRIMARY KEY,
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  event_type TEXT NOT NULL,
  from_status TEXT,
  to_status TEXT,
  actor_type TEXT NOT NULL,
  actor_id TEXT,
  correlation_id TEXT NOT NULL,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS commerce_idempotency_record (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  scope TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  request_sha256 TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('processing','completed','failed')),
  response_status INT,
  response_ciphertext BYTEA,
  locked_until TIMESTAMPTZ,
  expires_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (scope, idempotency_key)
);

CREATE TABLE IF NOT EXISTS commerce_reconciliation_exception (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  provider TEXT NOT NULL,
  environment TEXT NOT NULL,
  merchant_account_ref TEXT NOT NULL,
  exception_type TEXT NOT NULL,
  internal_reference TEXT,
  provider_reference TEXT,
  expected_amount_minor BIGINT,
  actual_amount_minor BIGINT,
  currency TEXT,
  status TEXT NOT NULL CHECK (status IN ('open','assigned','resolved','ignored')),
  assigned_to BIGINT,
  resolution_notes TEXT,
  detected_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  resolved_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS revenue_feature_flag (
  flag_key TEXT NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT FALSE,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','staging','production')),
  reason TEXT NOT NULL,
  updated_by BIGINT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (flag_key, environment)
);

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.datafast', FALSE, 'production', 'Requires merchant capability and production webhook verification'),
  ('checkout.paypal', FALSE, 'production', 'Requires merchant capability and production webhook verification'),
  ('distribution.partner_delivery', FALSE, 'production', 'Requires contracted partner profile, credentials, and authorization'),
  ('distribution.automatic_payouts', FALSE, 'production', 'Requires KYC, tax, banking, legal, reconciliation, and production authorization')
ON CONFLICT (flag_key, environment) DO NOTHING;

CREATE OR REPLACE FUNCTION commerce_validate_payment_attempt()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout_environment commerce_checkout_session.environment%TYPE;
  checkout_total_minor commerce_checkout_session.total_minor%TYPE;
  checkout_currency commerce_checkout_session.currency%TYPE;
BEGIN
  SELECT environment, total_minor, currency
    INTO checkout_environment, checkout_total_minor, checkout_currency
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id
    FOR UPDATE;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown checkout session %', NEW.checkout_id;
  END IF;
  IF NEW.environment <> checkout_environment
     OR NEW.amount_minor <> checkout_total_minor
     OR NEW.currency <> checkout_currency THEN
    RAISE EXCEPTION 'Payment attempt does not match checkout environment, amount, or currency';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_payment_attempt_binding
  BEFORE INSERT OR UPDATE OF checkout_id, environment, amount_minor, currency
  ON commerce_payment_attempt
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_payment_attempt();

CREATE OR REPLACE FUNCTION commerce_validate_provider_binding()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  attempt_provider commerce_payment_attempt.provider%TYPE;
  attempt_environment commerce_payment_attempt.environment%TYPE;
  attempt_merchant_account_ref commerce_payment_attempt.merchant_account_ref%TYPE;
  attempt_amount_minor commerce_payment_attempt.amount_minor%TYPE;
  attempt_currency commerce_payment_attempt.currency%TYPE;
BEGIN
  SELECT provider, environment, merchant_account_ref, amount_minor, currency
    INTO attempt_provider, attempt_environment, attempt_merchant_account_ref,
      attempt_amount_minor, attempt_currency
    FROM commerce_payment_attempt
    WHERE id = NEW.payment_attempt_id;
  IF NOT FOUND THEN
    RAISE EXCEPTION 'Unknown payment attempt %', NEW.payment_attempt_id;
  END IF;
  IF NEW.provider <> attempt_provider
     OR NEW.environment <> attempt_environment
     OR NEW.merchant_account_ref <> attempt_merchant_account_ref
     OR NEW.amount_minor <> attempt_amount_minor
     OR NEW.currency <> attempt_currency THEN
    RAISE EXCEPTION 'Provider binding does not match its payment attempt';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_provider_binding
  BEFORE INSERT OR UPDATE ON commerce_provider_binding
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_provider_binding();

CREATE OR REPLACE FUNCTION commerce_reject_immutable_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% records are immutable; append a compensating record', TG_TABLE_NAME;
END $$;

CREATE TRIGGER trg_commerce_checkout_line_immutable
  BEFORE UPDATE OR DELETE ON commerce_checkout_line_item
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();
CREATE TRIGGER trg_commerce_provider_binding_immutable
  BEFORE UPDATE OR DELETE ON commerce_provider_binding
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();

CREATE OR REPLACE FUNCTION commerce_protect_provider_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  IF (OLD.provider, OLD.environment, OLD.merchant_account_ref, OLD.provider_event_id,
      OLD.event_type, OLD.signature_verified, OLD.provider_created_at,
      OLD.payload_ciphertext, OLD.payload_sha256)
     IS DISTINCT FROM
     (NEW.provider, NEW.environment, NEW.merchant_account_ref, NEW.provider_event_id,
      NEW.event_type, NEW.signature_verified, NEW.provider_created_at,
      NEW.payload_ciphertext, NEW.payload_sha256) THEN
    RAISE EXCEPTION 'Provider inbox evidence is immutable';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_provider_event_immutable
  BEFORE UPDATE OR DELETE ON commerce_provider_event_inbox
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_provider_event();

CREATE OR REPLACE FUNCTION commerce_validate_ledger_posting()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status = 'posted' AND OLD.status <> 'posted' AND EXISTS (
    SELECT 1
    FROM commerce_ledger_entry
    WHERE transaction_id = NEW.id
    GROUP BY currency
    HAVING SUM(amount_minor) <> 0
  ) THEN
    RAISE EXCEPTION 'Ledger transaction % is not balanced by currency', NEW.id;
  END IF;
  IF NEW.status = 'posted' AND OLD.status <> 'posted' AND NOT EXISTS (
    SELECT 1 FROM commerce_ledger_entry WHERE transaction_id = NEW.id
  ) THEN
    RAISE EXCEPTION 'Ledger transaction % has no entries', NEW.id;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_ledger_posting
  BEFORE UPDATE OF status ON commerce_ledger_transaction
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_ledger_posting();

CREATE OR REPLACE FUNCTION commerce_protect_posted_ledger()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  transaction_status TEXT;
BEGIN
  SELECT status INTO transaction_status
    FROM commerce_ledger_transaction
    WHERE id = COALESCE(OLD.transaction_id, NEW.transaction_id);
  IF transaction_status IN ('posted', 'reversed') THEN
    RAISE EXCEPTION 'Posted ledger entries are immutable; create a reversing transaction';
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_commerce_ledger_entry_immutable
  BEFORE UPDATE OR DELETE ON commerce_ledger_entry
  FOR EACH ROW EXECUTE FUNCTION commerce_protect_posted_ledger();

COMMIT;
