-- Canonical guest ticket checkout, versioned fee policy, expiring seat holds,
-- and organizer-payable accounting. Existing ticket orders remain untouched
-- unless they are explicitly linked through this runtime.
BEGIN;

CREATE TABLE IF NOT EXISTS event_ticket_checkout_policy (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL CHECK (length(btrim(policy_version)) BETWEEN 1 AND 80),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  buyer_fee_bps INTEGER NOT NULL DEFAULT 200 CHECK (buyer_fee_bps BETWEEN 0 AND 10000),
  organizer_fee_bps INTEGER NOT NULL DEFAULT 200 CHECK (organizer_fee_bps BETWEEN 0 AND 10000),
  tax_bps INTEGER NOT NULL DEFAULT 0 CHECK (tax_bps BETWEEN 0 AND 10000),
  hold_minutes INTEGER NOT NULL DEFAULT 15 CHECK (hold_minutes BETWEEN 5 AND 60),
  terms_version TEXT NOT NULL CHECK (length(btrim(terms_version)) BETWEEN 1 AND 80),
  terms_summary TEXT NOT NULL CHECK (length(btrim(terms_summary)) BETWEEN 1 AND 2000),
  refund_policy TEXT NOT NULL CHECK (length(btrim(refund_policy)) BETWEEN 1 AND 2000),
  transfer_allowed BOOLEAN NOT NULL DEFAULT TRUE,
  approval_status TEXT NOT NULL DEFAULT 'draft'
    CHECK (approval_status IN ('draft','approved','retired')),
  active BOOLEAN NOT NULL DEFAULT FALSE,
  approved_at TIMESTAMPTZ,
  approved_by TEXT,
  effective_from TIMESTAMPTZ,
  effective_until TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (event_id, policy_version),
  CHECK (
    active = FALSE
    OR (approval_status = 'approved' AND approved_at IS NOT NULL AND approved_by IS NOT NULL)
  ),
  CHECK (effective_until IS NULL OR effective_from IS NULL OR effective_until > effective_from)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_event_ticket_checkout_active_policy
  ON event_ticket_checkout_policy(event_id)
  WHERE active;

CREATE TABLE IF NOT EXISTS event_ticket_checkout_policy_history (
  id BIGSERIAL PRIMARY KEY,
  policy_id UUID NOT NULL,
  event_id BIGINT NOT NULL,
  policy_version TEXT NOT NULL,
  snapshot JSONB NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  changed_by TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS event_ticket_checkout_runtime (
  order_id BIGINT PRIMARY KEY REFERENCES event_ticket_order(id) ON DELETE RESTRICT,
  event_id BIGINT NOT NULL REFERENCES social_event(id) ON DELETE RESTRICT,
  tier_id BIGINT NOT NULL REFERENCES event_ticket_tier(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  policy_id UUID NOT NULL REFERENCES event_ticket_checkout_policy(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL,
  lookup_token_hash TEXT NOT NULL UNIQUE CHECK (lookup_token_hash ~ '^[0-9a-f]{64}$'),
  create_idempotency_key TEXT NOT NULL UNIQUE CHECK (length(create_idempotency_key) BETWEEN 16 AND 128),
  create_request_sha256 TEXT NOT NULL CHECK (create_request_sha256 ~ '^[0-9a-f]{64}$'),
  fulfillment_status TEXT NOT NULL DEFAULT 'seat_held' CHECK (fulfillment_status IN (
    'seat_held','issued','transfer_requested','transferred','checked_in',
    'cancelled','refunded','expired'
  )),
  payment_status TEXT NOT NULL DEFAULT 'awaiting_payment' CHECK (payment_status IN (
    'awaiting_payment','processing','paid','partially_refunded','refunded',
    'disputed','chargeback'
  )),
  quantity INTEGER NOT NULL CHECK (quantity BETWEEN 1 AND 100),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  unit_price_minor BIGINT NOT NULL CHECK (unit_price_minor > 0),
  gross_face_value_minor BIGINT NOT NULL
    CHECK (gross_face_value_minor = unit_price_minor * quantity),
  discount_minor BIGINT NOT NULL DEFAULT 0
    CHECK (discount_minor >= 0 AND discount_minor <= gross_face_value_minor),
  net_face_value_minor BIGINT NOT NULL
    CHECK (net_face_value_minor = gross_face_value_minor - discount_minor),
  buyer_fee_bps INTEGER NOT NULL CHECK (buyer_fee_bps BETWEEN 0 AND 10000),
  buyer_fee_minor BIGINT NOT NULL CHECK (buyer_fee_minor >= 0),
  organizer_fee_bps INTEGER NOT NULL CHECK (organizer_fee_bps BETWEEN 0 AND 10000),
  organizer_fee_minor BIGINT NOT NULL CHECK (organizer_fee_minor >= 0),
  tax_bps INTEGER NOT NULL CHECK (tax_bps BETWEEN 0 AND 10000),
  tax_minor BIGINT NOT NULL CHECK (tax_minor >= 0),
  checkout_total_minor BIGINT NOT NULL
    CHECK (checkout_total_minor = net_face_value_minor + buyer_fee_minor + tax_minor),
  organizer_payable_minor BIGINT NOT NULL
    CHECK (organizer_payable_minor = net_face_value_minor - organizer_fee_minor),
  platform_fee_minor BIGINT NOT NULL
    CHECK (platform_fee_minor = buyer_fee_minor + organizer_fee_minor),
  promo_code_id BIGINT REFERENCES promo_code(id) ON DELETE RESTRICT,
  terms_version TEXT NOT NULL,
  terms_accepted_at TIMESTAMPTZ NOT NULL,
  hold_expires_at TIMESTAMPTZ NOT NULL,
  issued_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (organizer_payable_minor >= 0),
  CHECK (hold_expires_at > created_at),
  CHECK (fulfillment_status <> 'issued' OR issued_at IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_event_ticket_checkout_capacity
  ON event_ticket_checkout_runtime(tier_id, fulfillment_status, hold_expires_at);
CREATE INDEX IF NOT EXISTS idx_event_ticket_checkout_payment
  ON event_ticket_checkout_runtime(payment_status, hold_expires_at);

CREATE TABLE IF NOT EXISTS event_ticket_fulfillment_event (
  id BIGSERIAL PRIMARY KEY,
  order_id BIGINT NOT NULL REFERENCES event_ticket_checkout_runtime(order_id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','operator','provider','customer')),
  actor_id TEXT,
  reason_code TEXT,
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_event_ticket_fulfillment_event_order
  ON event_ticket_fulfillment_event(order_id, created_at, id);

CREATE OR REPLACE FUNCTION event_ticket_checkout_validate_policy()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.active AND NOT EXISTS (
    SELECT 1
    FROM event_ticket_tier tier
    WHERE tier.event_id = NEW.event_id
      AND tier.is_active
      AND tier.price_cents > 0
      AND upper(tier.currency) = NEW.currency
  ) THEN
    RAISE EXCEPTION 'Active event ticket policy requires a matching payable ticket tier';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.approval_status = 'approved'
     AND ROW(
       NEW.event_id, NEW.policy_version, NEW.currency, NEW.buyer_fee_bps,
       NEW.organizer_fee_bps, NEW.tax_bps, NEW.hold_minutes,
       NEW.terms_version, NEW.terms_summary, NEW.refund_policy, NEW.transfer_allowed
     ) IS DISTINCT FROM ROW(
       OLD.event_id, OLD.policy_version, OLD.currency, OLD.buyer_fee_bps,
       OLD.organizer_fee_bps, OLD.tax_bps, OLD.hold_minutes,
       OLD.terms_version, OLD.terms_summary, OLD.refund_policy, OLD.transfer_allowed
     ) THEN
    RAISE EXCEPTION 'Approved event ticket policy is immutable; create a new version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_validate_policy
  ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_checkout_validate_policy
  BEFORE INSERT OR UPDATE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_validate_policy();

CREATE OR REPLACE FUNCTION event_ticket_checkout_policy_history_capture()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    INSERT INTO event_ticket_checkout_policy_history(
      policy_id, event_id, policy_version, snapshot, changed_by
    ) VALUES (OLD.id, OLD.event_id, OLD.policy_version, to_jsonb(OLD), current_user);
    RETURN OLD;
  END IF;
  INSERT INTO event_ticket_checkout_policy_history(
    policy_id, event_id, policy_version, snapshot, changed_by
  ) VALUES (NEW.id, NEW.event_id, NEW.policy_version, to_jsonb(NEW), current_user);
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_policy_history
  ON event_ticket_checkout_policy;
CREATE TRIGGER trg_event_ticket_checkout_policy_history
  AFTER INSERT OR UPDATE OR DELETE ON event_ticket_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_policy_history_capture();

CREATE OR REPLACE FUNCTION event_ticket_checkout_validate_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  ticket_order event_ticket_order%ROWTYPE;
  ticket_tier event_ticket_tier%ROWTYPE;
  policy event_ticket_checkout_policy%ROWTYPE;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO ticket_order FROM event_ticket_order WHERE id = NEW.order_id;
  SELECT * INTO ticket_tier FROM event_ticket_tier WHERE id = NEW.tier_id;
  SELECT * INTO policy FROM event_ticket_checkout_policy WHERE id = NEW.policy_id;
  IF checkout.id IS NULL OR ticket_order.id IS NULL OR ticket_tier.id IS NULL OR policy.id IS NULL THEN
    RAISE EXCEPTION 'Event ticket checkout runtime references missing canonical records';
  END IF;
  IF checkout.domain_type <> 'event_ticket_order'
     OR checkout.domain_order_id <> NEW.order_id::text
     OR checkout.total_minor <> NEW.checkout_total_minor
     OR checkout.currency <> NEW.currency THEN
    RAISE EXCEPTION 'Event ticket runtime does not match immutable checkout amount and identity';
  END IF;
  IF ticket_order.event_id <> NEW.event_id
     OR ticket_order.tier_id <> NEW.tier_id
     OR ticket_order.quantity <> NEW.quantity
     OR ticket_order.amount_cents <> NEW.checkout_total_minor
     OR upper(ticket_order.currency) <> NEW.currency
     OR ticket_order.promo_code_id IS DISTINCT FROM NEW.promo_code_id
     OR ticket_tier.event_id <> NEW.event_id
     OR ticket_tier.price_cents <> NEW.unit_price_minor
     OR upper(ticket_tier.currency) <> NEW.currency THEN
    RAISE EXCEPTION 'Event ticket runtime does not match immutable order and tier snapshots';
  END IF;
  IF policy.event_id <> NEW.event_id
     OR policy.policy_version <> NEW.policy_version
     OR policy.currency <> NEW.currency
     OR policy.buyer_fee_bps <> NEW.buyer_fee_bps
     OR policy.organizer_fee_bps <> NEW.organizer_fee_bps
     OR policy.tax_bps <> NEW.tax_bps
     OR policy.terms_version <> NEW.terms_version THEN
    RAISE EXCEPTION 'Event ticket runtime does not match the approved policy snapshot';
  END IF;
  IF TG_OP = 'INSERT' AND (
       policy.approval_status <> 'approved'
       OR NOT policy.active
       OR policy.approved_at IS NULL
       OR policy.approved_by IS NULL
     ) THEN
    RAISE EXCEPTION 'New event ticket checkout requires an approved active policy';
  END IF;
  IF NEW.fulfillment_status = 'seat_held' AND NEW.hold_expires_at <= NOW() THEN
    RAISE EXCEPTION 'Event ticket seat hold must expire in the future';
  END IF;
  IF NEW.fulfillment_status = 'issued' THEN
    NEW.issued_at := COALESCE(NEW.issued_at, NOW());
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_validate_runtime
  ON event_ticket_checkout_runtime;
CREATE TRIGGER trg_event_ticket_checkout_validate_runtime
  BEFORE INSERT OR UPDATE ON event_ticket_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_validate_runtime();

CREATE OR REPLACE FUNCTION event_ticket_checkout_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'event_ticket_order'
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
    RAISE EXCEPTION 'Event ticket checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_require_verified_payment
  ON commerce_checkout_session;
CREATE TRIGGER trg_event_ticket_checkout_require_verified_payment
  BEFORE UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_require_verified_payment();

CREATE OR REPLACE FUNCTION event_ticket_checkout_sync_payment_and_hold()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  runtime_order BIGINT;
  released_tier BIGINT;
  released_quantity INTEGER;
  released_promo BIGINT;
BEGIN
  IF NEW.domain_type <> 'event_ticket_order' OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT order_id INTO runtime_order
    FROM event_ticket_checkout_runtime WHERE checkout_id = NEW.id;
  IF runtime_order IS NULL THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    UPDATE event_ticket_checkout_runtime
      SET payment_status = 'paid', updated_at = NOW()
      WHERE order_id = runtime_order;
  ELSIF NEW.status = 'processing' THEN
    UPDATE event_ticket_checkout_runtime
      SET payment_status = 'processing', updated_at = NOW()
      WHERE order_id = runtime_order AND payment_status = 'awaiting_payment';
  ELSIF NEW.status IN ('partially_refunded','refunded','disputed','chargeback') THEN
    UPDATE event_ticket_checkout_runtime
      SET payment_status = NEW.status, updated_at = NOW()
      WHERE order_id = runtime_order;
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE event_ticket_checkout_runtime
      SET fulfillment_status = NEW.status, updated_at = NOW()
      WHERE order_id = runtime_order AND fulfillment_status = 'seat_held'
      RETURNING tier_id, quantity, promo_code_id
      INTO released_tier, released_quantity, released_promo;
    IF released_tier IS NOT NULL THEN
      UPDATE event_ticket_tier
        SET quantity_sold = quantity_sold - released_quantity, updated_at = NOW()
        WHERE id = released_tier AND quantity_sold >= released_quantity;
      UPDATE event_ticket_order
        SET status = 'cancelled', updated_at = NOW()
        WHERE id = runtime_order AND status = 'pending';
      IF released_promo IS NOT NULL THEN
        UPDATE promo_code SET current_redemptions = current_redemptions - 1,
          updated_at = NOW()
        WHERE id = released_promo AND current_redemptions > 0;
      END IF;
      INSERT INTO event_ticket_fulfillment_event(
        order_id, from_status, to_status, actor_type, reason_code, notes
      ) VALUES (
        runtime_order, 'seat_held', NEW.status, 'system',
        'checkout_' || NEW.status, 'Unpaid ticket seat hold released exactly once'
      );
    END IF;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_checkout_sync_payment_and_hold
  ON commerce_checkout_session;
CREATE TRIGGER trg_event_ticket_checkout_sync_payment_and_hold
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION event_ticket_checkout_sync_payment_and_hold();

CREATE OR REPLACE FUNCTION event_ticket_order_require_canonical_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status = 'paid' AND OLD.status <> 'paid'
     AND EXISTS (
       SELECT 1 FROM event_ticket_checkout_runtime WHERE order_id = NEW.id
     )
     AND NOT EXISTS (
       SELECT 1
       FROM event_ticket_checkout_runtime runtime
       JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id
       WHERE runtime.order_id = NEW.id AND checkout.status = 'paid'
     ) THEN
    RAISE EXCEPTION 'Canonical event ticket order cannot become paid before verified checkout payment';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_event_ticket_order_require_canonical_payment
  ON event_ticket_order;
CREATE TRIGGER trg_event_ticket_order_require_canonical_payment
  BEFORE UPDATE OF status ON event_ticket_order
  FOR EACH ROW EXECUTE FUNCTION event_ticket_order_require_canonical_payment();

CREATE OR REPLACE FUNCTION event_ticket_checkout_expire_holds(
  at_time TIMESTAMPTZ DEFAULT NOW(),
  target_tier_id BIGINT DEFAULT NULL
)
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE expired_count INTEGER;
BEGIN
  WITH expired AS (
    UPDATE commerce_checkout_session checkout
      SET status = 'expired', updated_at = at_time
      FROM event_ticket_checkout_runtime runtime
      WHERE checkout.id = runtime.checkout_id
        AND checkout.domain_type = 'event_ticket_order'
        AND checkout.status IN ('holding','awaiting_payment','failed')
        AND runtime.fulfillment_status = 'seat_held'
        AND runtime.hold_expires_at <= at_time
        AND (target_tier_id IS NULL OR runtime.tier_id = target_tier_id)
      RETURNING checkout.id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

-- Preserve the current 2% buyer/2% organizer split as inactive drafts. Each
-- event requires explicit operational, accounting, tax, refund, and terms
-- approval before its public checkout can be activated.
INSERT INTO event_ticket_checkout_policy(
  event_id, policy_version, currency, buyer_fee_bps, organizer_fee_bps,
  tax_bps, hold_minutes, terms_version, terms_summary, refund_policy,
  transfer_allowed, approval_status, active
)
SELECT
  tier.event_id,
  'ticket-v1-db-' || tier.event_id::text,
  min(upper(tier.currency)),
  200,
  200,
  0,
  15,
  'ticket-terms-draft-v1',
  'Borrador migrado desde el reparto publicado de la tarifa. Requiere aprobación operativa, contable, tributaria y legal.',
  'Política de reembolso pendiente de aprobación; no activar este borrador.',
  bool_and(tier.allow_transfers),
  'draft',
  FALSE
FROM event_ticket_tier tier
WHERE tier.is_active AND tier.price_cents > 0
GROUP BY tier.event_id
HAVING count(DISTINCT upper(tier.currency)) = 1
ON CONFLICT (event_id, policy_version) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.event_tickets', FALSE, 'production',
   'Requires approved event fee policy, provider sandbox evidence, organizer payable review, and a controlled owned-event pilot'),
  ('commerce.event_ticket_settlements', FALSE, 'production',
   'Organizer settlement remains manual and dual-controlled until legal, KYC, reconciliation, and payout rails are approved')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
