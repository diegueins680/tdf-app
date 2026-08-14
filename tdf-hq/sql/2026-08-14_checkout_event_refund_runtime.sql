-- Runtime ownership for encrypted provider events and verified refunds.
-- Existing evidence remains untouched; new writes must satisfy the stronger contract.
BEGIN;

ALTER TABLE commerce_provider_event_inbox
  ADD COLUMN IF NOT EXISTS checkout_id UUID,
  ADD COLUMN IF NOT EXISTS payment_attempt_id UUID,
  ADD COLUMN IF NOT EXISTS refund_id UUID,
  ADD COLUMN IF NOT EXISTS provider_resource_id TEXT,
  ADD COLUMN IF NOT EXISTS processing_started_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS last_attempt_at TIMESTAMPTZ;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'fk_commerce_provider_event_checkout'
      AND conrelid = 'commerce_provider_event_inbox'::regclass
  ) THEN
    ALTER TABLE commerce_provider_event_inbox
      ADD CONSTRAINT fk_commerce_provider_event_checkout
      FOREIGN KEY (checkout_id) REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT;
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'fk_commerce_provider_event_attempt'
      AND conrelid = 'commerce_provider_event_inbox'::regclass
  ) THEN
    ALTER TABLE commerce_provider_event_inbox
      ADD CONSTRAINT fk_commerce_provider_event_attempt
      FOREIGN KEY (payment_attempt_id) REFERENCES commerce_payment_attempt(id) ON DELETE RESTRICT;
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'fk_commerce_provider_event_refund'
      AND conrelid = 'commerce_provider_event_inbox'::regclass
  ) THEN
    ALTER TABLE commerce_provider_event_inbox
      ADD CONSTRAINT fk_commerce_provider_event_refund
      FOREIGN KEY (refund_id) REFERENCES commerce_refund(id) ON DELETE RESTRICT;
  END IF;
END $$;

CREATE INDEX IF NOT EXISTS idx_commerce_provider_event_work
  ON commerce_provider_event_inbox(processing_status, next_attempt_at, received_at)
  WHERE processing_status IN ('pending', 'retry');
CREATE INDEX IF NOT EXISTS idx_commerce_provider_event_resource
  ON commerce_provider_event_inbox(provider, environment, merchant_account_ref, provider_resource_id)
  WHERE provider_resource_id IS NOT NULL;

CREATE TABLE IF NOT EXISTS commerce_refund_reason_code (
  reason_code TEXT PRIMARY KEY CHECK (reason_code ~ '^[a-z][a-z0-9_]{1,63}$'),
  name_es TEXT NOT NULL,
  name_en TEXT NOT NULL,
  active BOOLEAN NOT NULL DEFAULT TRUE,
  sort_order INT NOT NULL DEFAULT 0,
  requires_note BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

INSERT INTO commerce_refund_reason_code
  (reason_code, name_es, name_en, active, sort_order, requires_note) VALUES
  ('customer_request', 'Solicitud del cliente', 'Customer request', TRUE, 10, FALSE),
  ('duplicate', 'Pago duplicado', 'Duplicate payment', TRUE, 20, FALSE),
  ('fraud', 'Fraude confirmado', 'Confirmed fraud', TRUE, 30, TRUE),
  ('quality_issue', 'Problema de calidad', 'Quality issue', TRUE, 40, TRUE),
  ('service_cancelled', 'Servicio cancelado', 'Service cancelled', TRUE, 50, FALSE),
  ('other', 'Otro motivo documentado', 'Other documented reason', TRUE, 60, TRUE)
ON CONFLICT (reason_code) DO NOTHING;

ALTER TABLE commerce_refund
  ADD COLUMN IF NOT EXISTS provider TEXT,
  ADD COLUMN IF NOT EXISTS environment TEXT,
  ADD COLUMN IF NOT EXISTS merchant_account_ref TEXT,
  ADD COLUMN IF NOT EXISTS failure_code TEXT,
  ADD COLUMN IF NOT EXISTS failure_summary TEXT,
  ADD COLUMN IF NOT EXISTS updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW();

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'ck_commerce_refund_provider'
      AND conrelid = 'commerce_refund'::regclass
  ) THEN
    ALTER TABLE commerce_refund
      ADD CONSTRAINT ck_commerce_refund_provider
      CHECK (provider IS NULL OR provider IN ('datafast','paypal','stripe','bank_transfer','cash','pos'));
  END IF;
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'ck_commerce_refund_environment'
      AND conrelid = 'commerce_refund'::regclass
  ) THEN
    ALTER TABLE commerce_refund
      ADD CONSTRAINT ck_commerce_refund_environment
      CHECK (environment IS NULL OR environment IN ('sandbox','production'));
  END IF;
END $$;

CREATE INDEX IF NOT EXISTS idx_commerce_refund_checkout_status
  ON commerce_refund(checkout_id, status, created_at);

ALTER TABLE commerce_receipt
  ADD COLUMN IF NOT EXISTS refund_id UUID;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname = 'fk_commerce_receipt_refund'
      AND conrelid = 'commerce_receipt'::regclass
  ) THEN
    ALTER TABLE commerce_receipt
      ADD CONSTRAINT fk_commerce_receipt_refund
      FOREIGN KEY (refund_id) REFERENCES commerce_refund(id) ON DELETE RESTRICT;
  END IF;
END $$;

CREATE UNIQUE INDEX IF NOT EXISTS uq_commerce_credit_note_refund
  ON commerce_receipt(refund_id)
  WHERE refund_id IS NOT NULL AND kind = 'credit_note' AND voided_at IS NULL;

CREATE OR REPLACE FUNCTION commerce_validate_refund_write()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  attempt commerce_payment_attempt%ROWTYPE;
  allocated BIGINT;
  other_committed BIGINT;
BEGIN
  IF TG_OP = 'INSERT' THEN
    IF NEW.provider IS NULL OR NEW.environment IS NULL
       OR NULLIF(BTRIM(NEW.merchant_account_ref), '') IS NULL THEN
      RAISE EXCEPTION 'New refunds require provider, environment, and merchant binding';
    END IF;
    IF NEW.status <> 'requested' THEN
      RAISE EXCEPTION 'New refunds must begin in requested state';
    END IF;
    IF NOT EXISTS (
      SELECT 1 FROM commerce_refund_reason_code
      WHERE reason_code = NEW.reason_code AND active
    ) THEN
      RAISE EXCEPTION 'New refunds require an active configured reason code';
    END IF;
  ELSE
    IF (OLD.checkout_id, OLD.payment_attempt_id, OLD.amount_minor, OLD.currency,
        OLD.reason_code, OLD.idempotency_key, OLD.requested_by, OLD.created_at,
        OLD.provider, OLD.environment, OLD.merchant_account_ref)
       IS DISTINCT FROM
       (NEW.checkout_id, NEW.payment_attempt_id, NEW.amount_minor, NEW.currency,
        NEW.reason_code, NEW.idempotency_key, NEW.requested_by, NEW.created_at,
        NEW.provider, NEW.environment, NEW.merchant_account_ref) THEN
      RAISE EXCEPTION 'Refund request snapshots are immutable';
    END IF;
    IF OLD.status <> NEW.status AND (OLD.status, NEW.status) NOT IN (
      ('requested','approved'), ('requested','cancelled'),
      ('approved','processing'), ('approved','cancelled'),
      ('processing','succeeded'), ('processing','failed'),
      ('failed','processing'), ('failed','cancelled')
    ) THEN
      RAISE EXCEPTION 'Invalid refund transition from % to %', OLD.status, NEW.status;
    END IF;
  END IF;

  SELECT * INTO checkout
    FROM commerce_checkout_session
    WHERE id = NEW.checkout_id
    FOR UPDATE;
  SELECT * INTO attempt
    FROM commerce_payment_attempt
    WHERE id = NEW.payment_attempt_id;
  IF NOT FOUND OR attempt.checkout_id <> NEW.checkout_id
     OR attempt.status <> 'succeeded'
     OR attempt.provider <> NEW.provider
     OR attempt.environment <> NEW.environment
     OR attempt.merchant_account_ref <> NEW.merchant_account_ref
     OR attempt.currency <> NEW.currency THEN
    RAISE EXCEPTION 'Refund does not match a succeeded payment attempt';
  END IF;
  IF checkout.currency <> NEW.currency OR NEW.amount_minor > checkout.paid_minor THEN
    RAISE EXCEPTION 'Refund does not match the paid checkout currency or amount';
  END IF;
  IF NEW.status IN ('approved','processing','succeeded') AND
     (NEW.approved_by IS NULL OR NEW.approved_by = NEW.requested_by) THEN
    RAISE EXCEPTION 'Refund approval requires a different authenticated party';
  END IF;
  IF NEW.status = 'succeeded' AND OLD.status IS DISTINCT FROM 'succeeded' THEN
    IF NULLIF(BTRIM(NEW.provider_refund_id), '') IS NULL OR NEW.completed_at IS NULL THEN
      RAISE EXCEPTION 'Succeeded refund requires provider evidence and completion time';
    END IF;
    SELECT COALESCE(SUM(amount_minor), 0) INTO allocated
      FROM commerce_refund_allocation WHERE refund_id = NEW.id;
    IF allocated <> NEW.amount_minor THEN
      RAISE EXCEPTION 'Refund allocations must equal the verified refund amount';
    END IF;
    SELECT COALESCE(SUM(amount_minor), 0) INTO other_committed
      FROM commerce_refund
      WHERE checkout_id = NEW.checkout_id AND status = 'succeeded' AND id <> NEW.id;
    IF other_committed + NEW.amount_minor > checkout.paid_minor THEN
      RAISE EXCEPTION 'Verified refunds exceed the captured checkout amount';
    END IF;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_validate_refund_write ON commerce_refund;
CREATE TRIGGER trg_commerce_validate_refund_write
  BEFORE INSERT OR UPDATE ON commerce_refund
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_refund_write();

DROP TRIGGER IF EXISTS trg_commerce_refund_allocation_immutable ON commerce_refund_allocation;
CREATE TRIGGER trg_commerce_refund_allocation_immutable
  BEFORE UPDATE OR DELETE ON commerce_refund_allocation
  FOR EACH ROW EXECUTE FUNCTION commerce_reject_immutable_mutation();

CREATE OR REPLACE FUNCTION commerce_validate_credit_note()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  refund commerce_refund%ROWTYPE;
BEGIN
  IF NEW.refund_id IS NULL THEN
    RETURN NEW;
  END IF;
  SELECT * INTO refund FROM commerce_refund WHERE id = NEW.refund_id;
  IF NOT FOUND OR NEW.kind <> 'credit_note' OR refund.status <> 'succeeded'
     OR NEW.checkout_id <> refund.checkout_id
     OR NEW.amount_minor <> refund.amount_minor
     OR NEW.currency <> refund.currency THEN
    RAISE EXCEPTION 'Credit note must match a succeeded refund';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_commerce_validate_credit_note ON commerce_receipt;
CREATE TRIGGER trg_commerce_validate_credit_note
  BEFORE INSERT OR UPDATE ON commerce_receipt
  FOR EACH ROW EXECUTE FUNCTION commerce_validate_credit_note();

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('checkout.paypal.webhooks', FALSE, 'production',
    'Requires registered live webhook ID, signature verification evidence, and production authorization'),
  ('checkout.paypal.refunds', FALSE, 'production',
    'Requires merchant refund capability, two-person approval, reconciliation, and production authorization'),
  ('checkout.datafast.webhooks', FALSE, 'production',
    'No authenticated callback contract has been verified for this merchant'),
  ('checkout.datafast.refunds', FALSE, 'production',
    'No API refund capability has been verified for this merchant')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
