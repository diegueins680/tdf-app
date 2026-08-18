BEGIN;

-- A customer must be able to open a dispute after physical return or while a
-- deposit decision is pending. These are operational disputes only; they do
-- not mutate the provider payment state or claim a chargeback.
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
    ('returned_pending_inspection','damage_review'), ('returned_pending_inspection','disputed'),
    ('damage_review','deposit_refund_due'), ('damage_review','disputed'),
    ('deposit_refund_due','closed'), ('deposit_refund_due','disputed'),
    ('cancellation_requested','cancelled'), ('no_show','cancelled'), ('lost','disputed'),
    ('disputed','damage_review'), ('disputed','deposit_refund_due'), ('disputed','closed')
  );
$$;

CREATE TABLE IF NOT EXISTS marketplace_customer_request (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  order_kind TEXT NOT NULL CHECK (order_kind IN ('sale','rental')),
  request_type TEXT NOT NULL CHECK (request_type IN (
    'sale_cancellation','sale_return','rental_cancellation','rental_extension','rental_dispute'
  )),
  status TEXT NOT NULL DEFAULT 'submitted' CHECK (status IN (
    'submitted','needs_quote','approved','rejected'
  )),
  reason TEXT NOT NULL CHECK (length(btrim(reason)) BETWEEN 3 AND 1000),
  requested_end_date DATE,
  evidence_url TEXT CHECK (
    evidence_url IS NULL OR length(btrim(evidence_url)) BETWEEN 1 AND 2048
  ),
  idempotency_key TEXT NOT NULL CHECK (length(idempotency_key) BETWEEN 16 AND 128),
  request_sha256 TEXT NOT NULL CHECK (request_sha256 ~ '^[0-9a-f]{64}$'),
  requested_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewed_by BIGINT,
  reviewed_at TIMESTAMPTZ,
  review_notes TEXT CHECK (
    review_notes IS NULL OR length(btrim(review_notes)) BETWEEN 3 AND 1000
  ),
  UNIQUE (order_id, idempotency_key),
  CHECK ((request_type = 'rental_extension') = (requested_end_date IS NOT NULL)),
  CHECK (
    (status = 'submitted' AND reviewed_by IS NULL AND reviewed_at IS NULL AND review_notes IS NULL)
    OR
    (status <> 'submitted' AND reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL AND review_notes IS NOT NULL)
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_customer_request_open
  ON marketplace_customer_request(order_id, request_type)
  WHERE status IN ('submitted','needs_quote');

CREATE INDEX IF NOT EXISTS idx_marketplace_customer_request_queue
  ON marketplace_customer_request(status, requested_at, order_id);

CREATE TABLE IF NOT EXISTS marketplace_customer_request_event (
  id BIGSERIAL PRIMARY KEY,
  request_id UUID NOT NULL REFERENCES marketplace_customer_request(id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('customer','operator','system')),
  actor_id TEXT CHECK (actor_id IS NULL OR length(btrim(actor_id)) BETWEEN 1 AND 160),
  notes TEXT CHECK (notes IS NULL OR length(btrim(notes)) BETWEEN 1 AND 1000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_marketplace_customer_request_event
  ON marketplace_customer_request_event(request_id, created_at, id);

CREATE OR REPLACE FUNCTION marketplace_validate_customer_request()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime_kind TEXT;
  runtime_status TEXT;
  rental_end DATE;
BEGIN
  SELECT 'sale', fulfillment_status INTO runtime_kind, runtime_status
    FROM marketplace_sale_order_runtime
    WHERE order_id = NEW.order_id
    FOR UPDATE;
  IF runtime_kind IS NULL THEN
    SELECT 'rental', rental_status INTO runtime_kind, runtime_status
      FROM marketplace_rental_order_runtime
      WHERE order_id = NEW.order_id
      FOR UPDATE;
  END IF;
  IF runtime_kind IS NULL THEN
    RAISE EXCEPTION 'Marketplace customer requests require a canonical sale or rental order';
  END IF;
  IF NEW.order_kind <> runtime_kind THEN
    RAISE EXCEPTION 'Marketplace customer request order kind does not match its runtime';
  END IF;
  IF NEW.request_type IN ('sale_cancellation','sale_return') AND runtime_kind <> 'sale' THEN
    RAISE EXCEPTION 'Sale requests require a sale order';
  END IF;
  IF NEW.request_type IN ('rental_cancellation','rental_extension','rental_dispute')
     AND runtime_kind <> 'rental' THEN
    RAISE EXCEPTION 'Rental requests require a rental order';
  END IF;
  IF NEW.request_type = 'sale_cancellation'
     AND runtime_status NOT IN ('ready_to_fulfill','picking','ready_for_pickup') THEN
    RAISE EXCEPTION 'Sale cancellation can only be requested before shipment or delivery';
  ELSIF NEW.request_type = 'sale_return' AND runtime_status <> 'delivered' THEN
    RAISE EXCEPTION 'Sale return can only be requested after verified delivery';
  ELSIF NEW.request_type = 'rental_cancellation'
     AND runtime_status NOT IN ('confirmed','ready_for_handoff') THEN
    RAISE EXCEPTION 'Rental cancellation can only be requested before handoff';
  ELSIF NEW.request_type = 'rental_extension' THEN
    IF runtime_status NOT IN ('confirmed','ready_for_handoff','checked_out','return_due') THEN
      RAISE EXCEPTION 'Rental extension is unavailable in the current rental state';
    END IF;
    SELECT end_date INTO rental_end
      FROM marketplace_rental_order_runtime WHERE order_id = NEW.order_id;
    IF NEW.requested_end_date IS NULL OR NEW.requested_end_date <= rental_end THEN
      RAISE EXCEPTION 'Rental extension date must be later than the current return date';
    END IF;
  ELSIF NEW.request_type = 'rental_dispute'
     AND runtime_status NOT IN (
       'checked_out','return_due','returned_pending_inspection','damage_review',
       'deposit_refund_due','lost'
     ) THEN
    RAISE EXCEPTION 'Rental dispute is unavailable in the current rental state';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_customer_request ON marketplace_customer_request;
CREATE TRIGGER trg_marketplace_validate_customer_request
  BEFORE INSERT ON marketplace_customer_request
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_customer_request();

CREATE OR REPLACE FUNCTION marketplace_protect_customer_request()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Marketplace customer requests are immutable audit evidence';
  END IF;
  IF ROW(
       OLD.order_id, OLD.order_kind, OLD.request_type, OLD.reason,
       OLD.requested_end_date, OLD.evidence_url, OLD.idempotency_key,
       OLD.request_sha256, OLD.requested_at
     ) IS DISTINCT FROM ROW(
       NEW.order_id, NEW.order_kind, NEW.request_type, NEW.reason,
       NEW.requested_end_date, NEW.evidence_url, NEW.idempotency_key,
       NEW.request_sha256, NEW.requested_at
     ) THEN
    RAISE EXCEPTION 'Marketplace customer request evidence cannot be rewritten';
  END IF;
  IF OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  IF NOT (
    (OLD.status = 'submitted' AND NEW.status IN ('needs_quote','approved','rejected'))
    OR (OLD.status = 'needs_quote' AND NEW.status = 'rejected')
  ) THEN
    RAISE EXCEPTION 'Invalid marketplace customer request transition: % -> %', OLD.status, NEW.status;
  END IF;
  IF NEW.request_type = 'rental_extension' AND NEW.status = 'approved' THEN
    RAISE EXCEPTION 'Rental extensions require a versioned quote, atomic availability check, and payable change order';
  END IF;
  IF NEW.reviewed_by IS NULL OR NEW.reviewed_at IS NULL OR NEW.review_notes IS NULL THEN
    RAISE EXCEPTION 'Marketplace customer request review requires reviewer, timestamp, and notes';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_protect_customer_request ON marketplace_customer_request;
CREATE TRIGGER trg_marketplace_protect_customer_request
  BEFORE UPDATE OR DELETE ON marketplace_customer_request
  FOR EACH ROW EXECUTE FUNCTION marketplace_protect_customer_request();

CREATE OR REPLACE FUNCTION marketplace_record_customer_request_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  INSERT INTO marketplace_customer_request_event(
    request_id, from_status, to_status, actor_type, actor_id, notes
  ) VALUES (
    NEW.id,
    CASE WHEN TG_OP = 'INSERT' THEN NULL ELSE OLD.status END,
    NEW.status,
    COALESCE(NULLIF(current_setting('tdf.actor_type', TRUE), ''),
      CASE WHEN TG_OP = 'INSERT' THEN 'customer' ELSE 'operator' END),
    NULLIF(current_setting('tdf.actor_id', TRUE), ''),
    CASE WHEN TG_OP = 'INSERT' THEN NEW.reason ELSE NEW.review_notes END
  );
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_record_customer_request_event ON marketplace_customer_request;
CREATE TRIGGER trg_marketplace_record_customer_request_event
  AFTER INSERT OR UPDATE OF status ON marketplace_customer_request
  FOR EACH ROW
  EXECUTE FUNCTION marketplace_record_customer_request_event();

CREATE OR REPLACE FUNCTION marketplace_apply_approved_customer_request()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE affected_rows INTEGER;
BEGIN
  IF OLD.status = NEW.status OR NEW.status <> 'approved' THEN
    RETURN NEW;
  END IF;
  IF NEW.request_type = 'sale_cancellation' THEN
    UPDATE marketplace_sale_order_runtime SET fulfillment_status = 'cancellation_requested'
      WHERE order_id = NEW.order_id
        AND fulfillment_status IN ('ready_to_fulfill','picking','ready_for_pickup');
  ELSIF NEW.request_type = 'sale_return' THEN
    UPDATE marketplace_sale_order_runtime SET fulfillment_status = 'return_requested'
      WHERE order_id = NEW.order_id AND fulfillment_status = 'delivered';
  ELSIF NEW.request_type = 'rental_cancellation' THEN
    UPDATE marketplace_rental_order_runtime SET rental_status = 'cancellation_requested'
      WHERE order_id = NEW.order_id AND rental_status IN ('confirmed','ready_for_handoff');
  ELSIF NEW.request_type = 'rental_dispute' THEN
    UPDATE marketplace_rental_order_runtime SET rental_status = 'disputed'
      WHERE order_id = NEW.order_id
        AND rental_status IN (
          'checked_out','return_due','returned_pending_inspection','damage_review',
          'deposit_refund_due','lost'
        );
  ELSE
    RAISE EXCEPTION 'Unsupported approved marketplace customer request type %', NEW.request_type;
  END IF;
  GET DIAGNOSTICS affected_rows = ROW_COUNT;
  IF affected_rows <> 1 THEN
    RAISE EXCEPTION 'Marketplace order changed before the approved customer request could be applied';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_apply_approved_customer_request ON marketplace_customer_request;
CREATE TRIGGER trg_marketplace_apply_approved_customer_request
  AFTER UPDATE OF status ON marketplace_customer_request
  FOR EACH ROW EXECUTE FUNCTION marketplace_apply_approved_customer_request();

CREATE OR REPLACE FUNCTION marketplace_guard_sale_customer_request()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.fulfillment_status = NEW.fulfillment_status THEN RETURN NEW; END IF;
  IF EXISTS (
    SELECT 1 FROM marketplace_customer_request request
    WHERE request.order_id = NEW.order_id
      AND request.status IN ('submitted','needs_quote')
      AND (
        (request.request_type = 'sale_cancellation'
          AND NEW.fulfillment_status IN ('picking','ready_for_pickup','shipped','delivered'))
        OR (request.request_type = 'sale_return' AND NEW.fulfillment_status = 'closed')
      )
  ) THEN
    RAISE EXCEPTION 'Pending customer request blocks this sale fulfillment transition';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_guard_sale_customer_request ON marketplace_sale_order_runtime;
CREATE TRIGGER trg_marketplace_guard_sale_customer_request
  BEFORE UPDATE OF fulfillment_status ON marketplace_sale_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_guard_sale_customer_request();

CREATE OR REPLACE FUNCTION marketplace_guard_rental_customer_request()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.rental_status = NEW.rental_status THEN RETURN NEW; END IF;
  IF EXISTS (
    SELECT 1 FROM marketplace_customer_request request
    WHERE request.order_id = NEW.order_id
      AND request.status IN ('submitted','needs_quote')
      AND (
        (request.request_type = 'rental_cancellation'
          AND NEW.rental_status IN ('ready_for_handoff','checked_out','no_show'))
        OR (request.request_type = 'rental_dispute'
          AND NEW.rental_status IN ('damage_review','deposit_refund_due','closed'))
      )
  ) THEN
    RAISE EXCEPTION 'Pending customer request blocks this rental transition';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_guard_rental_customer_request ON marketplace_rental_order_runtime;
CREATE TRIGGER trg_marketplace_guard_rental_customer_request
  BEFORE UPDATE OF rental_status ON marketplace_rental_order_runtime
  FOR EACH ROW EXECUTE FUNCTION marketplace_guard_rental_customer_request();

CREATE TABLE IF NOT EXISTS marketplace_rental_deposit_settlement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  order_id UUID NOT NULL REFERENCES marketplace_order(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  deposit_amount_minor BIGINT NOT NULL CHECK (deposit_amount_minor > 0),
  deduction_amount_minor BIGINT NOT NULL CHECK (
    deduction_amount_minor >= 0 AND deduction_amount_minor <= deposit_amount_minor
  ),
  refund_amount_minor BIGINT NOT NULL CHECK (
    refund_amount_minor >= 0 AND refund_amount_minor = deposit_amount_minor - deduction_amount_minor
  ),
  settlement_method TEXT NOT NULL CHECK (settlement_method IN (
    'bank_transfer','cash','pos','forfeiture'
  )),
  external_reference TEXT NOT NULL CHECK (length(btrim(external_reference)) BETWEEN 3 AND 160),
  evidence_url TEXT NOT NULL CHECK (length(btrim(evidence_url)) BETWEEN 1 AND 2048),
  status TEXT NOT NULL DEFAULT 'submitted' CHECK (status IN (
    'submitted','verified','rejected','requires_reconciliation'
  )),
  idempotency_key TEXT NOT NULL CHECK (length(idempotency_key) BETWEEN 16 AND 128),
  request_sha256 TEXT NOT NULL CHECK (request_sha256 ~ '^[0-9a-f]{64}$'),
  submitted_by BIGINT NOT NULL,
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewed_by BIGINT,
  reviewed_at TIMESTAMPTZ,
  review_notes TEXT CHECK (
    review_notes IS NULL OR length(btrim(review_notes)) BETWEEN 3 AND 1000
  ),
  UNIQUE (order_id, idempotency_key),
  UNIQUE (settlement_method, external_reference),
  CHECK (
    (settlement_method = 'forfeiture' AND refund_amount_minor = 0)
    OR (settlement_method <> 'forfeiture' AND refund_amount_minor > 0)
  ),
  CHECK (
    (status = 'submitted' AND reviewed_by IS NULL AND reviewed_at IS NULL AND review_notes IS NULL)
    OR
    (status <> 'submitted' AND reviewed_by IS NOT NULL AND reviewed_at IS NOT NULL AND review_notes IS NOT NULL)
  )
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_rental_deposit_settlement_open
  ON marketplace_rental_deposit_settlement(order_id)
  WHERE status = 'submitted';
CREATE UNIQUE INDEX IF NOT EXISTS uq_marketplace_rental_deposit_settlement_verified
  ON marketplace_rental_deposit_settlement(order_id)
  WHERE status = 'verified';

CREATE INDEX IF NOT EXISTS idx_marketplace_rental_deposit_settlement_queue
  ON marketplace_rental_deposit_settlement(status, submitted_at, order_id);

CREATE TABLE IF NOT EXISTS marketplace_rental_deposit_settlement_event (
  id BIGSERIAL PRIMARY KEY,
  settlement_id UUID NOT NULL REFERENCES marketplace_rental_deposit_settlement(id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_id BIGINT NOT NULL,
  notes TEXT NOT NULL CHECK (length(btrim(notes)) BETWEEN 3 AND 1000),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_marketplace_rental_deposit_settlement_event
  ON marketplace_rental_deposit_settlement_event(settlement_id, created_at, id);

CREATE OR REPLACE FUNCTION marketplace_validate_rental_deposit_settlement()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime marketplace_rental_order_runtime%ROWTYPE;
  checkout commerce_checkout_session%ROWTYPE;
BEGIN
  SELECT * INTO runtime FROM marketplace_rental_order_runtime
    WHERE order_id = NEW.order_id FOR UPDATE;
  IF runtime.order_id IS NULL THEN
    RAISE EXCEPTION 'Deposit settlement requires a canonical rental order';
  END IF;
  SELECT * INTO checkout FROM commerce_checkout_session
    WHERE id = runtime.checkout_id FOR UPDATE;
  IF checkout.id IS NULL
     OR NEW.checkout_id <> runtime.checkout_id
     OR NEW.currency <> checkout.currency
     OR NEW.deposit_amount_minor <> runtime.security_deposit_usd_cents
     OR NEW.deduction_amount_minor <> runtime.deposit_deduction_usd_cents
     OR NEW.refund_amount_minor <> runtime.security_deposit_usd_cents - runtime.deposit_deduction_usd_cents THEN
    RAISE EXCEPTION 'Deposit settlement does not match the immutable rental and checkout snapshot';
  END IF;
  IF runtime.security_deposit_usd_cents <= 0
     OR runtime.rental_status <> 'deposit_refund_due'
     OR runtime.deposit_status NOT IN ('refund_due','partial_refund_due') THEN
    RAISE EXCEPTION 'Rental deposit is not due for settlement';
  END IF;
  IF checkout.status NOT IN ('paid','partially_refunded')
     OR checkout.paid_minor <> checkout.total_minor
     OR checkout.refunded_minor <> 0 THEN
    RAISE EXCEPTION 'Checkout refund state requires reconciliation before deposit settlement';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_validate_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
CREATE TRIGGER trg_marketplace_validate_rental_deposit_settlement
  BEFORE INSERT ON marketplace_rental_deposit_settlement
  FOR EACH ROW EXECUTE FUNCTION marketplace_validate_rental_deposit_settlement();

CREATE OR REPLACE FUNCTION marketplace_protect_rental_deposit_settlement()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime marketplace_rental_order_runtime%ROWTYPE;
  checkout commerce_checkout_session%ROWTYPE;
BEGIN
  IF TG_OP = 'DELETE' THEN
    RAISE EXCEPTION 'Rental deposit settlement evidence is immutable';
  END IF;
  IF ROW(
       OLD.order_id, OLD.checkout_id, OLD.currency, OLD.deposit_amount_minor,
       OLD.deduction_amount_minor, OLD.refund_amount_minor, OLD.settlement_method,
       OLD.external_reference, OLD.evidence_url, OLD.idempotency_key,
       OLD.request_sha256, OLD.submitted_by, OLD.submitted_at
     ) IS DISTINCT FROM ROW(
       NEW.order_id, NEW.checkout_id, NEW.currency, NEW.deposit_amount_minor,
       NEW.deduction_amount_minor, NEW.refund_amount_minor, NEW.settlement_method,
       NEW.external_reference, NEW.evidence_url, NEW.idempotency_key,
       NEW.request_sha256, NEW.submitted_by, NEW.submitted_at
     ) THEN
    RAISE EXCEPTION 'Rental deposit settlement evidence cannot be rewritten';
  END IF;
  IF OLD.status = NEW.status THEN RETURN NEW; END IF;
  IF OLD.status <> 'submitted'
     OR NEW.status NOT IN ('verified','rejected','requires_reconciliation') THEN
    RAISE EXCEPTION 'Invalid rental deposit settlement transition: % -> %', OLD.status, NEW.status;
  END IF;
  IF NEW.reviewed_by IS NULL OR NEW.reviewed_at IS NULL OR NEW.review_notes IS NULL THEN
    RAISE EXCEPTION 'Deposit settlement review requires reviewer, timestamp, and notes';
  END IF;
  IF NEW.reviewed_by = NEW.submitted_by THEN
    RAISE EXCEPTION 'Deposit settlement requires an independent reviewer';
  END IF;
  IF NEW.status = 'verified' THEN
    SELECT * INTO runtime FROM marketplace_rental_order_runtime
      WHERE order_id = NEW.order_id FOR UPDATE;
    SELECT * INTO checkout FROM commerce_checkout_session
      WHERE id = NEW.checkout_id FOR UPDATE;
    IF runtime.rental_status <> 'deposit_refund_due'
       OR runtime.deposit_status NOT IN ('refund_due','partial_refund_due')
       OR runtime.security_deposit_usd_cents <> NEW.deposit_amount_minor
       OR runtime.deposit_deduction_usd_cents <> NEW.deduction_amount_minor
       OR checkout.status NOT IN ('paid','partially_refunded')
       OR checkout.refunded_minor <> 0 THEN
      RAISE EXCEPTION 'Rental or checkout changed before deposit settlement approval';
    END IF;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_protect_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
CREATE TRIGGER trg_marketplace_protect_rental_deposit_settlement
  BEFORE UPDATE OR DELETE ON marketplace_rental_deposit_settlement
  FOR EACH ROW EXECUTE FUNCTION marketplace_protect_rental_deposit_settlement();

CREATE OR REPLACE FUNCTION marketplace_record_rental_deposit_settlement_event()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  INSERT INTO marketplace_rental_deposit_settlement_event(
    settlement_id, from_status, to_status, actor_id, notes
  ) VALUES (
    NEW.id,
    CASE WHEN TG_OP = 'INSERT' THEN NULL ELSE OLD.status END,
    NEW.status,
    CASE WHEN TG_OP = 'INSERT' THEN NEW.submitted_by ELSE NEW.reviewed_by END,
    CASE WHEN TG_OP = 'INSERT' THEN 'Deposit settlement evidence submitted' ELSE NEW.review_notes END
  );
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_record_rental_deposit_settlement_event
  ON marketplace_rental_deposit_settlement;
CREATE TRIGGER trg_marketplace_record_rental_deposit_settlement_event
  AFTER INSERT OR UPDATE OF status ON marketplace_rental_deposit_settlement
  FOR EACH ROW
  EXECUTE FUNCTION marketplace_record_rental_deposit_settlement_event();

CREATE OR REPLACE FUNCTION marketplace_apply_verified_rental_deposit_settlement()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE ledger_id UUID;
BEGIN
  IF OLD.status = NEW.status OR NEW.status <> 'verified' THEN RETURN NEW; END IF;

  INSERT INTO commerce_ledger_transaction(
    transaction_type, source_type, source_id, status, effective_at,
    correlation_id, created_by
  ) VALUES (
    'rental_deposit_settlement', 'marketplace_rental_deposit_settlement', NEW.id::text,
    'draft', NEW.reviewed_at, 'marketplace-rental-deposit:' || NEW.order_id::text,
    'staff_verified_manual'
  ) RETURNING id INTO ledger_id;

  INSERT INTO commerce_ledger_entry(
    transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo
  ) VALUES (
    ledger_id, 'liability.marketplace_rental_deposit', 'marketplace_rental',
    NEW.order_id::text, NEW.currency, NEW.deposit_amount_minor,
    'Release refundable rental deposit liability'
  );
  IF NEW.refund_amount_minor > 0 THEN
    INSERT INTO commerce_ledger_entry(
      transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo
    ) VALUES (
      ledger_id, 'cash.' || NEW.settlement_method, 'marketplace_rental',
      NEW.order_id::text, NEW.currency, -NEW.refund_amount_minor,
      'Verified manual rental deposit refund'
    );
  END IF;
  IF NEW.deduction_amount_minor > 0 THEN
    INSERT INTO commerce_ledger_entry(
      transaction_id, account_code, domain_type, domain_id, currency, amount_minor, memo
    ) VALUES (
      ledger_id, 'revenue.marketplace_rental_damage', 'marketplace_rental',
      NEW.order_id::text, NEW.currency, -NEW.deduction_amount_minor,
      'Approved rental deposit deduction'
    );
  END IF;
  UPDATE commerce_ledger_transaction SET status = 'posted' WHERE id = ledger_id;

  IF NEW.refund_amount_minor > 0 THEN
    UPDATE commerce_checkout_session
      SET refunded_minor = refunded_minor + NEW.refund_amount_minor,
          status = 'partially_refunded', updated_at = NEW.reviewed_at
      WHERE id = NEW.checkout_id;
    INSERT INTO commerce_receipt(
      checkout_id, receipt_number, kind, adapter, external_reference,
      amount_minor, currency, issued_at
    ) VALUES (
      NEW.checkout_id,
      'TDF-CN-RD-' || upper(substr(replace(NEW.id::text, '-', ''), 1, 16)),
      'credit_note', NEW.settlement_method, NEW.external_reference,
      NEW.refund_amount_minor, NEW.currency, NEW.reviewed_at
    );
  END IF;

  UPDATE marketplace_rental_order_runtime
    SET deposit_status = CASE
      WHEN NEW.refund_amount_minor = 0 THEN 'forfeited'
      WHEN NEW.deduction_amount_minor = 0 THEN 'refunded'
      ELSE 'partially_refunded'
    END,
    updated_at = NEW.reviewed_at
    WHERE order_id = NEW.order_id;

  INSERT INTO commerce_checkout_audit_event(
    checkout_id, event_type, from_status, to_status, actor_type, actor_id,
    correlation_id, metadata
  ) VALUES (
    NEW.checkout_id, 'rental_deposit_settlement_verified', NULL, NULL,
    'operator', NEW.reviewed_by::text,
    'marketplace-rental-deposit:' || NEW.order_id::text,
    jsonb_build_object(
      'settlement_id', NEW.id::text,
      'method', NEW.settlement_method,
      'refund_amount_minor', NEW.refund_amount_minor,
      'deduction_amount_minor', NEW.deduction_amount_minor
    )
  );
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_marketplace_apply_verified_rental_deposit_settlement
  ON marketplace_rental_deposit_settlement;
CREATE TRIGGER trg_marketplace_apply_verified_rental_deposit_settlement
  AFTER UPDATE OF status ON marketplace_rental_deposit_settlement
  FOR EACH ROW EXECUTE FUNCTION marketplace_apply_verified_rental_deposit_settlement();

CREATE OR REPLACE VIEW marketplace_rental_deposit_ledger_backfill_report AS
SELECT
  runtime.order_id,
  runtime.checkout_id,
  runtime.security_deposit_usd_cents AS deposit_amount_minor,
  checkout.currency,
  CASE
    WHEN runtime.security_deposit_usd_cents = 0 THEN 'no_deposit'
    WHEN EXISTS (
      SELECT 1 FROM commerce_ledger_entry entry
      JOIN commerce_ledger_transaction transaction ON transaction.id = entry.transaction_id
      WHERE entry.domain_type = 'marketplace_rental'
        AND entry.domain_id = runtime.order_id::text
        AND entry.account_code = 'liability.marketplace_rental_deposit'
        AND transaction.transaction_type = 'payment_capture'
        AND transaction.status = 'posted'
    ) THEN 'liability_recorded'
    WHEN checkout.status IN ('paid','partially_refunded','refunded') THEN 'requires_reclassification'
    ELSE 'unpaid'
  END AS backfill_disposition
FROM marketplace_rental_order_runtime runtime
JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.marketplace_manual_deposit_settlement', TRUE, 'production',
   'Enabled for evidence-backed manual settlement with independent staff review; provider refunds remain disabled')
ON CONFLICT (flag_key, environment) DO UPDATE
SET enabled = EXCLUDED.enabled, reason = EXCLUDED.reason, updated_at = NOW();

COMMIT;
