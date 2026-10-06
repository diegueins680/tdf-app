BEGIN;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM event_ticket_checkout_policy WHERE manual_transfer_hold_minutes IS NOT NULL)
     OR EXISTS (SELECT 1 FROM event_ticket_checkout_runtime
                WHERE manual_hold_expires_at IS NOT NULL OR manual_submitter_party_id IS NOT NULL) THEN
    RAISE EXCEPTION 'Retain manual transfer terms and extended holds; use a forward repair';
  END IF;
END $$;
DROP TRIGGER IF EXISTS trg_event_ticket_manual_hold_guard ON event_ticket_checkout_runtime;
DROP FUNCTION IF EXISTS event_ticket_manual_hold_guard();
DROP TRIGGER IF EXISTS trg_event_ticket_manual_transfer_policy_immutable ON event_ticket_checkout_policy;
DROP FUNCTION IF EXISTS event_ticket_manual_transfer_policy_immutable();

CREATE OR REPLACE FUNCTION event_ticket_checkout_validate_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  ticket_order event_ticket_order%ROWTYPE;
  ticket_tier event_ticket_tier%ROWTYPE;
  policy event_ticket_checkout_policy%ROWTYPE;
BEGIN
  IF TG_OP = 'UPDATE' THEN
    IF ROW(
      NEW.order_id, NEW.event_id, NEW.tier_id, NEW.checkout_id, NEW.policy_id,
      NEW.policy_version, NEW.lookup_token_hash, NEW.create_idempotency_key,
      NEW.create_request_sha256, NEW.quantity, NEW.currency, NEW.unit_price_minor,
      NEW.gross_face_value_minor, NEW.discount_minor, NEW.net_face_value_minor,
      NEW.buyer_fee_bps, NEW.buyer_fee_minor, NEW.organizer_fee_bps,
      NEW.organizer_fee_minor, NEW.tax_bps, NEW.tax_minor, NEW.checkout_total_minor,
      NEW.organizer_payable_minor, NEW.platform_fee_minor, NEW.promo_code_id,
      NEW.terms_version, NEW.terms_accepted_at, NEW.hold_expires_at, NEW.created_at
    ) IS DISTINCT FROM ROW(
      OLD.order_id, OLD.event_id, OLD.tier_id, OLD.checkout_id, OLD.policy_id,
      OLD.policy_version, OLD.lookup_token_hash, OLD.create_idempotency_key,
      OLD.create_request_sha256, OLD.quantity, OLD.currency, OLD.unit_price_minor,
      OLD.gross_face_value_minor, OLD.discount_minor, OLD.net_face_value_minor,
      OLD.buyer_fee_bps, OLD.buyer_fee_minor, OLD.organizer_fee_bps,
      OLD.organizer_fee_minor, OLD.tax_bps, OLD.tax_minor, OLD.checkout_total_minor,
      OLD.organizer_payable_minor, OLD.platform_fee_minor, OLD.promo_code_id,
      OLD.terms_version, OLD.terms_accepted_at, OLD.hold_expires_at, OLD.created_at
    ) THEN
      RAISE EXCEPTION 'Event ticket checkout snapshot is immutable after creation';
    END IF;
  ELSE
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
    IF policy.approval_status <> 'approved'
       OR NOT policy.active
       OR policy.approved_at IS NULL
       OR policy.approved_by IS NULL THEN
      RAISE EXCEPTION 'New event ticket checkout requires an approved active policy';
    END IF;
  END IF;
  IF TG_OP = 'UPDATE'
     AND NEW.payment_status = 'paid'
     AND OLD.payment_status <> 'paid'
     AND NOT EXISTS (
       SELECT 1 FROM commerce_checkout_session
       WHERE id = NEW.checkout_id AND status = 'paid'
     ) THEN
    RAISE EXCEPTION 'Event ticket runtime cannot become paid before canonical verified payment';
  END IF;
  IF TG_OP = 'UPDATE'
     AND NEW.fulfillment_status = 'issued'
     AND OLD.fulfillment_status <> 'issued'
     AND NOT EXISTS (
       SELECT 1 FROM commerce_checkout_session
       WHERE id = NEW.checkout_id AND status = 'paid'
     ) THEN
    RAISE EXCEPTION 'Event ticket cannot be issued before canonical verified payment';
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

CREATE OR REPLACE FUNCTION event_ticket_checkout_expire_holds(
  at_time TIMESTAMPTZ DEFAULT NOW(),
  target_tier_id BIGINT DEFAULT NULL,
  target_event_id BIGINT DEFAULT NULL
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
        AND (target_event_id IS NULL OR runtime.event_id = target_event_id)
      RETURNING checkout.id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

ALTER TABLE event_ticket_checkout_runtime
  DROP COLUMN manual_submitter_party_id,
  DROP COLUMN manual_hold_expires_at;
ALTER TABLE event_ticket_checkout_policy
  DROP CONSTRAINT IF EXISTS event_ticket_policy_manual_transfer_pair,
  DROP COLUMN manual_transfer_cutoff_at,
  DROP COLUMN manual_transfer_hold_minutes;
COMMIT;
